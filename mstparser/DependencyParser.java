package mstparser;

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.StringTokenizer;
import gnu.trove.*;

import mstparser.io.*;

public class DependencyParser {

    public static String trainfile = null;
    public static String testfile = null;
    public static String trainforest = null;
    public static String testforest = null;
    public static boolean train = false;
    public static boolean eval = false;
    public static boolean test = false;
    public static String modelName = "dep.model";
    public static String lossType = "punc";
    public static boolean createForest = true;
    public static String decodeType = "proj";
    public static String format = "CONLL";
    public static int numIters = 10;
    public static String outfile = "out.txt";
    public static String goldfile = null;
    public static int trainK = 1;
    public static int testK = 1;
    public static boolean secondOrder = false;

    private DependencyPipe pipe;
    private DependencyDecoder decoder;
    private Parameters params;

    public DependencyParser(DependencyPipe pipe) {
	this.pipe=pipe;
	// Set up arrays
	params = new Parameters(pipe.dataAlphabet.size());
	decoder = secondOrder ? new DependencyDecoder2O(pipe) : new DependencyDecoder(pipe);
    }

    public void train(int[] instanceLengths, String trainfile, String train_forest) 
	throws IOException {
		
	System.out.println("About to train");
	System.out.println("Num Feats: " + pipe.dataAlphabet.size());
		
	int i = 0;
	for(i = 0; i < numIters; i++) {
			
	    System.out.println("========================");
	    System.out.println("Iteration: " + i);
	    System.out.println("========================");
	    System.out.print("Processed: ");

	    long start = System.currentTimeMillis();

	    trainingIter(instanceLengths,trainfile,train_forest,i+1);

	    long end = System.currentTimeMillis();
	    System.out.println("Training iter took: " + (end-start));
			
	}

	params.averageParams(i*instanceLengths.length);
		
    }

    private void trainingIter(int[] instanceLengths, String trainfile, 
			      String train_forest, int iter) throws IOException {

	int numUpd = 0;
	ObjectInputStream in = new ObjectInputStream(new FileInputStream(train_forest));
	boolean evaluateI = true;

	int numInstances = instanceLengths.length;

	for(int i = 0; i < numInstances; i++) {
	    if((i+1) % 500 == 0)
		System.out.println("  "+(i+1)+" instances");

	    int length = instanceLengths[i];

	    // Get production crap.
	    FeatureVector[][][] fvs = new FeatureVector[length][length][2];
	    double[][][] probs = new double[length][length][2];
	    FeatureVector[][][][] nt_fvs = new FeatureVector[length][pipe.types.length][2][2];
	    double[][][][] nt_probs = new double[length][pipe.types.length][2][2];
	    FeatureVector[][][] fvs_trips = new FeatureVector[length][length][length];
	    double[][][] probs_trips = new double[length][length][length];
	    FeatureVector[][][] fvs_sibs = new FeatureVector[length][length][2];
	    double[][][] probs_sibs = new double[length][length][2];

	    DependencyInstance inst;

	    if(secondOrder) {
		inst = ((DependencyPipe2O)pipe).readInstance(in,length,fvs,probs,
							     fvs_trips,probs_trips,
							     fvs_sibs,probs_sibs,
							     nt_fvs,nt_probs,params);
	    }

	    else
		inst = pipe.readInstance(in,length,fvs,probs,nt_fvs,nt_probs,params);

	    double upd = (double)(numIters*numInstances - (numInstances*(iter-1)+(i+1)) + 1);
	    int K=trainK;
	    Object[][] d = null;
	    if(decodeType.equals("proj")) {
		if(secondOrder)
		    d = ((DependencyDecoder2O)decoder).decodeProjective(inst,fvs,probs,
									fvs_trips,probs_trips,
									fvs_sibs,probs_sibs,
									nt_fvs,nt_probs,K);
		else
		    d = decoder.decodeProjective(inst,fvs,probs,nt_fvs,nt_probs,K);
	    }
	    if(decodeType.equals("non-proj")) {
		if(secondOrder)
		    d = ((DependencyDecoder2O)decoder).decodeNonProjective(inst,fvs,probs,
								       fvs_trips,probs_trips,
								       fvs_sibs,probs_sibs,
								       nt_fvs,nt_probs,K);
		else
		    d = decoder.decodeNonProjective(inst,fvs,probs,nt_fvs,nt_probs,K);
	    }
	    params.updateParamsMIRA(inst,d,upd);

	}
	System.out.println("");
	
	System.out.println("  "+numInstances+" instances");
		
	in.close();

    }

    ///////////////////////////////////////////////////////
    // Saving and loading models
    ///////////////////////////////////////////////////////
    public void saveModel(String file) throws IOException {
	ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(file));
	out.writeObject(params.parameters);
	out.writeObject(pipe.dataAlphabet);
	out.writeObject(pipe.typeAlphabet);
	out.close();
    }

    public void loadModel(String file) throws Exception {
	ObjectInputStream in = new ObjectInputStream(new FileInputStream(file));
	params.parameters = (double[])in.readObject();
	pipe.dataAlphabet = (Alphabet)in.readObject();
	pipe.typeAlphabet = (Alphabet)in.readObject();
	in.close();
	pipe.closeAlphabets();
    }

    //////////////////////////////////////////////////////
    // Get Best Parses ///////////////////////////////////
    //////////////////////////////////////////////////////
    public void outputParses(String tFile, String file)
	throws IOException {

	long start = System.currentTimeMillis();

	pipe.initInputFile(tFile);
	pipe.initOutputFile(file);

	System.out.print("Processing Sentence: ");
	DependencyInstance instance = pipe.nextInstance();
	int cnt = 0;
	while(instance != null) {
	    cnt++;
	    System.out.print(cnt+" ");
	    String[] forms = instance.forms;
			
	    int length = forms.length;

	    FeatureVector[][][] fvs = new FeatureVector[forms.length][forms.length][2];
	    double[][][] probs = new double[forms.length][forms.length][2];
	    FeatureVector[][][][] nt_fvs = new FeatureVector[forms.length][pipe.types.length][2][2];
	    double[][][][] nt_probs = new double[forms.length][pipe.types.length][2][2];
	    FeatureVector[][][] fvs_trips = new FeatureVector[length][length][length];
	    double[][][] probs_trips = new double[length][length][length];
	    FeatureVector[][][] fvs_sibs = new FeatureVector[length][length][2];
	    double[][][] probs_sibs = new double[length][length][2];
	    if(secondOrder)
		((DependencyPipe2O)pipe).getFeatureVector(instance,fvs,probs,
							  fvs_trips,probs_trips,
							  fvs_sibs,probs_sibs,
							  nt_fvs,nt_probs,params);
	    else
		pipe.getFeatureVector(instance,fvs,probs,nt_fvs,nt_probs,params);

	    int K = testK;
	    Object[][] d = null;
	    if(decodeType.equals("proj")) {
		if(secondOrder)
		    d = ((DependencyDecoder2O)decoder).decodeProjective(instance,fvs,probs,
									fvs_trips,probs_trips,
									fvs_sibs,probs_sibs,
									nt_fvs,nt_probs,K);
		else
		    d = decoder.decodeProjective(instance,fvs,probs,nt_fvs,nt_probs,K);
	    }
	    if(decodeType.equals("non-proj")) {
		if(secondOrder)
		    d = ((DependencyDecoder2O)decoder).decodeNonProjective(instance,fvs,probs,
								       fvs_trips,probs_trips,
								       fvs_sibs,probs_sibs,
								       nt_fvs,nt_probs,K);
		else
		    d = decoder.decodeNonProjective(instance,fvs,probs,nt_fvs,nt_probs,K);
	    }

	    String[] res = ((String)d[0][1]).split(" ");

	    String[] pos = instance.cpostags;

	    String[] formsNoRoot = new String[forms.length-1];
	    String[] posNoRoot = new String[formsNoRoot.length];
	    String[] labels = new String[formsNoRoot.length];
	    int[] heads = new int[formsNoRoot.length];

	    Arrays.toString(forms);
	    Arrays.toString(res);
	    for(int j = 0; j < formsNoRoot.length; j++) {
		formsNoRoot[j] = forms[j+1];
		posNoRoot[j] = pos[j+1];

		String[] trip = res[j].split("[\\|:]");
		labels[j] = pipe.types[Integer.parseInt(trip[2])];
		heads[j] = Integer.parseInt(trip[0]);
	    }

	    pipe.outputInstance(new DependencyInstance(formsNoRoot, posNoRoot, labels, heads));

	    //String line1 = ""; String line2 = ""; String line3 = ""; String line4 = "";
	    //for(int j = 1; j < pos.length; j++) {
	    //	String[] trip = res[j-1].split("[\\|:]");
	    //	line1+= sent[j] + "\t"; line2 += pos[j] + "\t";
	    //	line4 += trip[0] + "\t"; line3 += pipe.types[Integer.parseInt(trip[2])] + "\t";
	    //}
	    //pred.write(line1.trim() + "\n" + line2.trim() + "\n"
	    //	       + (pipe.labeled ? line3.trim() + "\n" : "")
	    //	       + line4.trim() + "\n\n");

	    instance = pipe.nextInstance();
	}
	pipe.close();
		
	long end = System.currentTimeMillis();
	System.out.println("Took: " + (end-start));

    }

    /////////////////////////////////////////////////////
    // RUNNING THE PARSER
    ////////////////////////////////////////////////////
    public static void main (String[] args) throws FileNotFoundException, Exception
    {
		
	processArguments(args);

	if(train) {
		
	    DependencyPipe pipe = 
		secondOrder ? new DependencyPipe2O (createForest, format) 
		: new DependencyPipe (createForest, format);

	    
	    int[] instanceLengths = pipe.createInstances(trainfile,trainforest);
		
	    pipe.closeAlphabets();
	    
	    DependencyParser dp = new DependencyParser(pipe);
	    
	    int numFeats = pipe.dataAlphabet.size();
	    int numTypes = pipe.typeAlphabet.size();
	    System.out.println("Num Feats: " + numFeats);	
	    System.out.println("Num Edge Labels: " + numTypes);
	    
	    dp.train(instanceLengths,trainfile,trainforest);
	    
	    System.out.print("Saving model ... ");
	    dp.saveModel(modelName);
	    System.out.println("done.");
	    
	}
		
	if (test) {
	    DependencyPipe pipe = secondOrder ? new DependencyPipe2O (createForest, format) : new DependencyPipe (createForest, format);

	    DependencyParser dp = new DependencyParser(pipe);

	    System.out.println("\nLoading model ... ");
	    dp.loadModel(modelName);
	    System.out.println("done.");

	    pipe.closeAlphabets();

	    dp.outputParses(testfile,outfile);
	}
		
	if(eval) {
	    System.out.println("\nEVALUATION PERFORMANCE:");
	    DependencyEvaluator.evaluate(goldfile, outfile, format);
	}
    }

    public static void processArguments(String[] args) {
	for(int i = 0; i < args.length; i++) {
	    String[] pair = args[i].split(":");
	    if(pair[0].equals("train")) {
		train = true;
	    }
	    if(pair[0].equals("eval")) {
		eval = true;
	    }
	    if(pair[0].equals("test")) {
		test = true;
	    }
	    if(pair[0].equals("iters")) {
		numIters = Integer.parseInt(pair[1]);
	    }
	    if(pair[0].equals("output-file")) {
		outfile = pair[1];
	    }
	    if(pair[0].equals("gold-file")) {
		goldfile = pair[1];
	    }
	    if(pair[0].equals("train-file")) {
		trainfile = pair[1];
	    }
	    if(pair[0].equals("test-file")) {
		testfile = pair[1];
	    }
	    if(pair[0].equals("model-name")) {
		modelName = pair[1];
	    }
	    if(pair[0].equals("training-k")) {
		trainK = Integer.parseInt(pair[1]);
	    }
	    if(pair[0].equals("loss-type")) {
		lossType = pair[1];
	    }			
	    if(pair[0].equals("order") && pair[1].equals("2")) {
		secondOrder = true;
	    }			
	    if(pair[0].equals("create-forest")) {
		createForest = pair[1].equals("true") ? true : false;
	    }			
	    if(pair[0].equals("decode-type")) {
		decodeType = pair[1];
	    }			
	    if(pair[0].equals("format")) {
		format = pair[1];
	    }			
	}
	trainforest = trainfile == null ? null : trainfile+".forest";
	testforest = testfile == null ? null : testfile+".forest";
	
	System.out.println("------\nFLAGS\n------");
	System.out.println("train-file: " + trainfile);
	System.out.println("test-file: " + testfile);
	System.out.println("gold-file: " + goldfile);
	System.out.println("output-file: " + outfile);
	System.out.println("model-name: " + modelName);
	System.out.println("train: " + train);
	System.out.println("test: " + test);
	System.out.println("eval: " + eval);
	System.out.println("loss-type: " + lossType);
	System.out.println("second-order: " + secondOrder);
	System.out.println("training-iterations: " + numIters);
	System.out.println("training-k: " + trainK);
	System.out.println("decode-type: " + decodeType);
	System.out.println("create-forest: " + createForest);
	System.out.println("format: " + format);
	System.out.println("------\n");
    }
}
