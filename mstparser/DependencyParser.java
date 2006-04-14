package mstparser;

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.StringTokenizer;
import gnu.trove.*;

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
    public static int numIters = 10;
    public static String outfile = "out.txt";
    public static String goldfile = null;
    public static int trainK = 1;
    public static int testK = 1;

    //public static String pipeType = "standard";
    public static String pipeType = "extended";


    private DependencyPipe pipe;
    private DependencyDecoder decoder;
    private Parameters params;

    public DependencyParser(DependencyPipe pipe) {
	this.pipe=pipe;
	// Set up arrays
	params = new Parameters(pipe.dataAlphabet.size());
	decoder = new DependencyDecoder(pipe);
    }

    public void train(DependencyInstance[] il, String trainfile, String train_forest) throws IOException {
		
	System.out.println("About to train");
	System.out.println("Num Feats: " + pipe.dataAlphabet.size());
		
	int i = 0;
	for(i = 0; i < numIters; i++) {
			
	    System.out.println("========================");
	    System.out.println("Iteration: " + i);
	    System.out.println("========================");
	    System.out.print("Processed: ");

	    long start = System.currentTimeMillis();

	    trainingIter(il,trainfile,train_forest,i+1);

	    long end = System.currentTimeMillis();
	    System.out.println("Training iter took: " + (end-start));
			
	}

	params.averageParams(i*il.length);
		
    }

    private void trainingIter(DependencyInstance[] il, String trainfile, String train_forest, int iter) throws IOException {

	int numUpd = 0;
	ObjectInputStream in = new ObjectInputStream(new FileInputStream(train_forest));
	boolean evaluateI = true;

	for(int i = 0; i < il.length; i++) {
	    if((i+1) % 500 == 0)
		System.out.println("  "+(i+1)+" instances");

	    DependencyInstance inst = il[i];
		
	    int length = inst.length;

	    // Get production crap.
	    FeatureVector[][][] fvs = new FeatureVector[length][length][2];
	    double[][][] probs = new double[length][length][2];
	    FeatureVector[][][][] nt_fvs = new FeatureVector[length][pipe.types.length][2][2];
	    double[][][][] nt_probs = new double[length][pipe.types.length][2][2];
	    inst = pipe.getFeatureVector(in,inst,fvs,probs,nt_fvs,nt_probs,params);

	    double upd = (double)(numIters*il.length - (il.length*(iter-1)+(i+1)) + 1);
	    int K=trainK;
	    Object[][] d = null;
	    if(decodeType.equals("proj"))
		d = decoder.decodeProjective(inst,fvs,probs,nt_fvs,nt_probs,K); 
	    if(decodeType.equals("non-proj"))
		d = decoder.decodeNonProjective(inst,fvs,probs,nt_fvs,nt_probs,K);
	    params.updateParamsMIRA(inst,d,upd);

	}
	System.out.println("");
	
	System.out.println("  "+il.length+" instances");
		
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

	BufferedWriter pred = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file),"8859_2"));

	BufferedReader in =
	    new BufferedReader(new InputStreamReader(new FileInputStream(tFile),"8859_2"));
	System.out.print("Processing Sentence: ");
	DependencyInstance il = pipe.createInstance(in);
	int cnt = 0;
	while(il != null) {
	    cnt++;
	    System.out.print(cnt+" ");
	    String[] toks = il.get("tokens");
			
	    FeatureVector[][][] fvs = new FeatureVector[toks.length][toks.length][2];
	    double[][][] probs = new double[toks.length][toks.length][2];
	    FeatureVector[][][][] nt_fvs = new FeatureVector[toks.length][pipe.types.length][2][2];
	    double[][][][] nt_probs = new double[toks.length][pipe.types.length][2][2];
	    pipe.getFeatureVector(il,fvs,probs,nt_fvs,nt_probs,params);

	    int K = testK;
	    Object[][] d = null;
	    if(decodeType.equals("proj"))
		d = decoder.decodeProjective(il,fvs,probs,nt_fvs,nt_probs,K);
	    if(decodeType.equals("non-proj"))
		d = decoder.decodeNonProjective(il,fvs,probs,nt_fvs,nt_probs,K);

	    String[] res = ((String)d[0][1]).split(" ");
	    String[] sent = il.get("tokens");
	    String[] pos = il.get("pos");
	    String line1 = ""; String line2 = ""; String line3 = ""; String line4 = "";
	    for(int j = 1; j < pos.length; j++) {
		String[] trip = res[j-1].split("[\\|:]");
		line1+= sent[j] + "\t"; line2 += pos[j] + "\t";
		line4 += trip[0] + "\t"; line3 += pipe.types[Integer.parseInt(trip[2])] + "\t";
	    }
	    pred.write(line1.trim() + "\n" + line2.trim() + "\n"
		       + (pipe.labeled ? line3.trim() + "\n" : "")
		       + line4.trim() + "\n\n");
	    
	    il = pipe.createInstance(in);
	}
	System.out.println();
		
	pred.close();
	in.close();
		
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
		
	    DependencyPipe pipe;
	    if (pipeType.equals("standard"))
		pipe = new DependencyPipe (createForest);
	    else 
		pipe = new DependencyPipe (createForest);
	    //pipe = new ExtendedDependencyPipe (createForest);
			
	    pipe.setLabeled(trainfile);

	    DependencyInstance[] trainingData = pipe.createInstances(trainfile,trainforest);
			
	    pipe.closeAlphabets();
			
	    DependencyParser dp = new DependencyParser(pipe);

	    int numFeats = pipe.dataAlphabet.size();
	    int numTypes = pipe.typeAlphabet.size();
	    System.out.println("Num Feats: " + numFeats);	
	    System.out.println("Num Edge Labels: " + numTypes);

	    dp.train(trainingData,trainfile,trainforest);
	
	    System.out.print("Saving model ... ");
	    dp.saveModel(modelName);
	    System.out.println("done.");
			
	}
		
	if (test) {
	    DependencyPipe pipe;
	    if (pipeType.equals("standard"))
		pipe = new DependencyPipe (true);
	    else 
		pipe = new DependencyPipe (true);
	    //pipe = new ExtendedDependencyPipe (true);

	    pipe.setLabeled(testfile);
	    DependencyParser dp = new DependencyParser(pipe);

	    System.out.println("\nLoading model ... ");
	    dp.loadModel(modelName);
	    System.out.println("done.");

	    pipe.closeAlphabets();

	    dp.outputParses(testfile,outfile);
	}
		
	if(eval) {
	    System.out.println("\nEVALUATION PERFORMANCE:");
	    DependencyEvaluator.evaluate(goldfile,outfile);
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
	    if(pair[0].equals("create-forest")) {
		createForest = pair[1].equals("true") ? true : false;
	    }			
	    if(pair[0].equals("decode-type")) {
		decodeType = pair[1];
	    }			
	    if(pair[0].equals("pipe-type")) {
		pipeType = pair[1];
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
	System.out.println("training-iterations: " + numIters);
	System.out.println("training-k: " + trainK);
	System.out.println("decode-type: " + decodeType);
	System.out.println("create-forest: " + createForest);
	System.out.println("------\n");
    }
}
