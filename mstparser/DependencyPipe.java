package mstparser;

import mstparser.io.*;
import java.io.*;
import gnu.trove.*;
import java.util.*;

public class DependencyPipe {

    public Alphabet dataAlphabet;
	
    public Alphabet typeAlphabet;

    private DependencyReader depReader;
    private DependencyWriter depWriter;

    public String[] types;
    public int[] typesInt;
	
    public boolean labeled = false;
    private String format;
    private boolean isCONLL = true;

    public boolean createForest;
	
    public DependencyPipe() throws IOException {
	this(true, "CONLL");
    }

    public DependencyPipe(boolean createForest, String format) throws IOException {
	dataAlphabet = new Alphabet();
	typeAlphabet = new Alphabet();
	this.createForest = createForest;
	this.format = format;
	if (!format.equals("CONLL"))
	    isCONLL = false;
	depReader = DependencyReader.createDependencyReader(format);
    }

    public void initInputFile (String file) throws IOException {
	labeled = depReader.startReading(file);
    }

    public void initOutputFile (String file) throws IOException {
	depWriter = DependencyWriter.createDependencyWriter(format, labeled);
	depWriter.startWriting(file);
    }

    public void outputInstance (DependencyInstance instance) throws IOException {
	depWriter.write(instance);
    }

    public void close () throws IOException {
	if (null != depWriter) {
	    depWriter.finishWriting();
	}
    }

    public String getType (int typeIndex) {
	return types[typeIndex];
    }

    protected final DependencyInstance nextInstance() throws IOException {
	DependencyInstance instance = depReader.getNext();
	if (instance == null || instance.forms == null) return null;

	instance.setFeatureVector(createFeatureVector(instance));
	
	String[] labs = instance.deprels;
	int[] heads = instance.heads;

	StringBuffer spans = new StringBuffer(heads.length*5);
	for(int i = 1; i < heads.length; i++) {
	    spans.append(heads[i]).append("|").append(i).append(":").append(typeAlphabet.lookupIndex(labs[i])).append(" ");
	}
	instance.actParseTree = spans.substring(0,spans.length()-1);
	
	return instance;
    }


    public int[] createInstances(String file,
				 String featFileName) throws IOException {

	createAlphabet(file);

	System.out.println("Num Features: " + dataAlphabet.size());

	labeled = depReader.startReading(file);

	TIntArrayList lengths = new TIntArrayList();

	ObjectOutputStream out = createForest
	    ? new ObjectOutputStream(new FileOutputStream(featFileName))
	    : null;
		
	DependencyInstance instance = depReader.getNext();
	int num1 = 0;

	System.out.println("Creating Feature Vector Instances: ");
	while(instance != null) {
	    System.out.print(num1 + " ");
	    
	    instance.setFeatureVector(createFeatureVector(instance));
			
	    String[] labs = instance.deprels;
	    int[] heads = instance.heads;

	    StringBuffer spans = new StringBuffer(heads.length*5);
	    for(int i = 1; i < heads.length; i++) {
		spans.append(heads[i]).append("|").append(i).append(":").append(typeAlphabet.lookupIndex(labs[i])).append(" ");
	    }
	    instance.actParseTree = spans.substring(0,spans.length()-1);

	    lengths.add(instance.length());
			
	    if(createForest)
		writeInstance(instance,out);
	    instance = null;
			
	    instance = depReader.getNext();

	    num1++;
	}

	System.out.println();

	closeAlphabets();
		
	if(createForest)
	    out.close();

	return lengths.toNativeArray();
		
    }

    private final void createAlphabet(String file) throws IOException {

	System.out.print("Creating Alphabet ... ");

	labeled = depReader.startReading(file);

	DependencyInstance instance = depReader.getNext();

	while(instance != null) {
	    
	    String[] labs = instance.deprels;
	    for(int i = 0; i < labs.length; i++)
		typeAlphabet.lookupIndex(labs[i]);
			
	    createFeatureVector(instance);
			
	    instance = depReader.getNext();
	}

	closeAlphabets();

	System.out.println("Done.");
    }
	
    public void closeAlphabets() {
	dataAlphabet.stopGrowth();
	typeAlphabet.stopGrowth();
		
	types = new String[typeAlphabet.size()];
	Object[] keys = typeAlphabet.toArray();
	for(int i = 0; i < keys.length; i++) {
	    int indx = typeAlphabet.lookupIndex(keys[i]);
	    types[indx] = (String)keys[i];
	}
				
	KBestParseForest.rootType = typeAlphabet.lookupIndex("<root-type>");
    }


    // add with default 1.0
    public final FeatureVector add(String feat, FeatureVector fv) {
	int num = dataAlphabet.lookupIndex(feat);
	if(num >= 0)
	    fv = new FeatureVector(num, 1.0, fv);
	return fv;
    }

    public final FeatureVector add(String feat, double val, FeatureVector fv) {
	int num = dataAlphabet.lookupIndex(feat);
	if(num >= 0)
	    fv = new FeatureVector(num, val, fv);
	return fv;
    }

	
    public FeatureVector createFeatureVector(DependencyInstance instance) {

	final int instanceLength = instance.length();

	String[] labs = instance.deprels;
	int[] heads = instance.heads;

	FeatureVector fv = new FeatureVector();
	for(int i = 0; i < instanceLength; i++) {
	    if(heads[i] == -1)
		continue;
	    int small = i < heads[i] ? i : heads[i];
	    int large = i > heads[i] ? i : heads[i];
	    boolean attR = i < heads[i] ? false : true;
	    fv = addCoreFeatures(instance,small,large,attR,fv);
	    if(labeled) {
		fv = addLabeledFeatures(instance,i,labs[i],attR,true,fv);
		fv = addLabeledFeatures(instance,heads[i],labs[i],attR,false,fv);
	    }
	}

	fv = addExtendedFeatures(instance, fv);

	return fv;
    }

    protected FeatureVector addExtendedFeatures(DependencyInstance instance, 
						FeatureVector fv) {
	return fv;
    }


    public FeatureVector addCoreFeatures(DependencyInstance instance,
					 int small,
					 int large,
					 boolean attR,
					 FeatureVector fv) {

	String[] forms = instance.forms;
	String[] pos = instance.postags;
	String[] posA = instance.cpostags;

	String att = attR ? "RA" : "LA";

	int dist = Math.abs(large-small);
	String distBool = "0";
	if (dist > 10)
	    distBool = "10";
	else if (dist > 5)
	    distBool = "5";
	else
	    distBool = Integer.toString(dist-1);
		
	String attDist = "&"+att+"&"+distBool;

	fv = addLinearFeatures("WORD", forms, small, large, attDist, fv);
	//fv = addLinearFeatures("LEMMA", instance.lemmas, small, large, attDist, fv);
	fv = addLinearFeatures("POS", pos, small, large, attDist, fv);
	fv = addLinearFeatures("CPOS", posA, small, large, attDist, fv);
		
	//////////////////////////////////////////////////////////////////////
	
	int headIndex = small;
	int childIndex = large;
	if (!attR) {
	    headIndex = large;
	    childIndex = small;
	}	

	fv = addTwoFactorFeatures("HC", forms[headIndex], pos[headIndex], 
				  forms[childIndex], pos[childIndex], attDist, fv);

	if (isCONLL) {
	    fv = addTwoFactorFeatures("HCA", forms[headIndex], posA[headIndex], 
				      forms[childIndex], posA[childIndex], attDist, fv);

	    fv = addTwoFactorFeatures("HCB", forms[headIndex], instance.lemmas[headIndex],
				      forms[childIndex], instance.lemmas[childIndex], 
				      attDist, fv);

	    fv = addTwoFactorFeatures("HCC", instance.lemmas[headIndex], pos[headIndex], 
				      instance.lemmas[childIndex], pos[childIndex], 
				      attDist, fv);

	    fv = addTwoFactorFeatures("HCD", instance.lemmas[headIndex], posA[headIndex], 
				      instance.lemmas[childIndex], posA[childIndex], 
				      attDist, fv);

	    // Use this if your extra feature list has the same length
	    // for all items.
	    //
	    for (int i=0; i<instance.feats[headIndex].length; i++) {
		fv = addTwoFactorFeatures("FF"+i, 
					  instance.forms[headIndex], 
					  instance.feats[headIndex][i],
					  instance.forms[childIndex], 
					  instance.feats[childIndex][i], 
					  attDist, fv);
		fv = addTwoFactorFeatures("LF"+i, 
					  instance.lemmas[headIndex], 
					  instance.feats[headIndex][i],
					  instance.lemmas[childIndex], 
					  instance.feats[childIndex][i], 
					  attDist, fv);
		fv = addTwoFactorFeatures("PF"+i, 
					  pos[headIndex], 
					  instance.feats[headIndex][i],
					  pos[childIndex], 
					  instance.feats[childIndex][i], 
					  attDist, fv);
		fv = addTwoFactorFeatures("CPF"+i, 
					  posA[headIndex], 
					  instance.feats[headIndex][i],
					  posA[childIndex], 
					  instance.feats[childIndex][i], 
					  attDist, fv);

		for (int j=i+1; j<instance.feats[headIndex].length; j++) {

		    fv = addTwoFactorFeatures("CPF"+i, 
					      instance.feats[headIndex][i],
					      instance.feats[headIndex][j],
					      instance.feats[childIndex][i], 
					      instance.feats[childIndex][j], 
					      attDist, fv);
		}
	    }

	    // Use this if your extra feature lists can have different
	    // lengths for each item. For example, nouns might have a
	    // different number of morphological features than verbs.
	    //
	    //for (int i=0; i<instance.feats[headIndex].length; i++) {
	    //	for (int j=0; j<instance.feats[childIndex].length; j++) {
	    //	    fv = addTwoFactorFeatures("FF"+i+"*"+j, 
	    //				      instance.forms[headIndex], 
	    //				      instance.feats[headIndex][i],
	    //				      instance.forms[childIndex], 
	    //				      instance.feats[childIndex][j], 
	    //				      attDist, fv);
	    //	    fv = addTwoFactorFeatures("LF"+i+"*"+j, 
	    //				      instance.lemmas[headIndex], 
	    //				      instance.feats[headIndex][i],
	    //				      instance.lemmas[childIndex], 
	    //				      instance.feats[childIndex][j], 
	    //				      attDist, fv);
	    //	}
	    //}

	} else {
	    // Pick up stem features the way they used to be done. This
	    // will soon be phased out, but is kept for replicability
	    // of results for this version
	    int hL = forms[headIndex].length();
	    int cL = forms[childIndex].length();
	    if (hL > 5 || cL > 5) {
		fv = addOldMSTStemFeatures(instance.lemmas[headIndex], pos[headIndex],
					   instance.lemmas[childIndex], pos[childIndex],
					   attDist, hL, cL, fv);
	    }
	}				       
		
	return fv;
		
    }
 
    private final FeatureVector addLinearFeatures(String type, String[] factorVals, 
						  int first, int second,
						  String attachDistance,
						  FeatureVector fv) {
	
	String pLeft = first > 0 ? factorVals[first-1] : "STR";
	String pRight = second < factorVals.length-1 ? factorVals[second+1] : "END";
	String pLeftRight = first < second-1 ? factorVals[first+1] : "MID";
	String pRightLeft = second > first+1 ? factorVals[second-1] : "MID";

	// feature posR posMid posL
	StringBuilder featPos = 
	    new StringBuilder(type+"PC="+factorVals[first]+" "+factorVals[second]);

	for(int i = first+1; i < second; i++) {
	    String allPos = featPos.toString() + ' ' + factorVals[i];
	    fv = add(allPos, fv);
	    fv = add(allPos+attachDistance, fv);

	}

	fv = addCorePosFeatures(type+"PT", pLeft, factorVals[first], pLeftRight, 
				pRightLeft, factorVals[second], pRight, attachDistance, fv);

	return fv;

    }


    private final FeatureVector 
	addCorePosFeatures(String prefix,
			   String leftOf1, String one, String rightOf1, 
			   String leftOf2, String two, String rightOf2, 
			   String attachDistance, 
			   FeatureVector fv) {

	// feature posL-1 posL posR posR+1

	fv = add(prefix+"="+leftOf1+" "+one+" "+two+"*"+attachDistance, fv);

	StringBuilder feat = 
	    new StringBuilder(prefix+"1="+leftOf1+" "+one+" "+two);
	fv = add(feat.toString(), fv);
	feat.append(' ').append(rightOf2);
	fv = add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	fv = add(feat.toString(), fv);

	feat = new StringBuilder(prefix+"2="+leftOf1+" "+two+" "+rightOf2);
	fv = add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	fv = add(feat.toString(), fv);
	
	feat = new StringBuilder(prefix+"3="+leftOf1+" "+one+" "+rightOf2);
	fv = add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	fv = add(feat.toString(), fv);
	
	feat = new StringBuilder(prefix+"4="+one+" "+two+" "+rightOf2);
	fv = add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	fv = add(feat.toString(), fv);

	/////////////////////////////////////////////////////////////
	prefix = "A"+prefix;

	// feature posL posL+1 posR-1 posR
	fv = add(prefix+"1="+one+" "+rightOf1+" "+leftOf2+"*"+attachDistance, fv);

	feat = new StringBuilder(prefix+"1="+one+" "+rightOf1+" "+leftOf2);
	fv = add(feat.toString(), fv);
	feat.append(' ').append(two);
	fv = add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	fv = add(feat.toString(), fv);

	feat = new StringBuilder(prefix+"2="+one+" "+rightOf1+" "+two);
	fv = add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	fv = add(feat.toString(), fv);
	
	feat = new StringBuilder(prefix+"3="+one+" "+leftOf2+" "+two);
	fv = add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	fv = add(feat.toString(), fv);
	
	feat = new StringBuilder(prefix+"4="+rightOf1+" "+leftOf2+" "+two);
	fv = add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	fv = add(feat.toString(), fv);

	///////////////////////////////////////////////////////////////
	prefix = "B"+prefix;

	//// feature posL-1 posL posR-1 posR
	feat = new StringBuilder(prefix+"1="+leftOf1+" "+one+" "+leftOf2+" "+two);
	fv = add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	fv = add(feat.toString(), fv);

	//// feature posL posL+1 posR posR+1
	feat = new StringBuilder(prefix+"2="+one+" "+rightOf1+" "+two+" "+rightOf2);
	fv = add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	fv = add(feat.toString(), fv);

	return fv;

    }



    /**
     * Add features for two items, each with two factors, e.g. head,
     * head pos, child, and child pos.
     *
     * The use of StringBuilders is not yet as efficient as it could
     * be, but this is a start. (And it abstracts the logic so we can
     * add other features more easily based on other items and
     * factors.)
     **/
    private final FeatureVector addTwoFactorFeatures(String prefix, 
						     String item1F1, String item1F2, 
						     String item2F1, String item2F2, 
						     String attachDistance,
						     FeatureVector fv) {

	StringBuilder feat = new StringBuilder(prefix+"2FF1="+item1F1);
	fv = add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	fv = add(feat.toString(), fv);
	
	feat = new StringBuilder(prefix+"2FF1="+item1F1+" "+item1F2);
	fv = add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	fv = add(feat.toString(), fv);

	feat = new StringBuilder(prefix+"2FF1="+item1F1+" "+item1F2+" "+item2F2);
	fv = add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	fv = add(feat.toString(), fv);

	feat = new StringBuilder(prefix+"2FF1="+item1F1+" "+item1F2+" "+item2F2+" "+item2F1);
	fv = add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	fv = add(feat.toString(), fv);
	
	feat = new StringBuilder(prefix+"2FF2="+item1F1+" "+item2F1);
	fv = add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	fv = add(feat.toString(), fv);

	feat = new StringBuilder(prefix+"2FF3="+item1F1+" "+item2F2);
	fv = add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	fv = add(feat.toString(), fv);


	feat = new StringBuilder(prefix+"2FF4="+item1F2+" "+item2F1);
	fv = add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	fv = add(feat.toString(), fv);

	feat = new StringBuilder(prefix+"2FF4="+item1F2+" "+item2F1+" "+item2F2);
	fv = add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	fv = add(feat.toString(), fv);

	feat = new StringBuilder(prefix+"2FF5="+item1F2+" "+item2F2);
	fv = add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	fv = add(feat.toString(), fv);

	feat = new StringBuilder(prefix+"2FF6="+item2F1+" "+item2F2);
	fv = add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	fv = add(feat.toString(), fv);

	feat = new StringBuilder(prefix+"2FF7="+item1F2);
	fv = add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	fv = add(feat.toString(), fv);
	
	feat = new StringBuilder(prefix+"2FF8="+item2F1);
	fv = add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	fv = add(feat.toString(), fv);
	
	feat = new StringBuilder(prefix+"2FF9="+item2F2);
	fv = add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	fv = add(feat.toString(), fv);
	
	return fv;

    }


	
    public FeatureVector addLabeledFeatures(DependencyInstance instance,
					    int word,
					    String type,
					    boolean attR,
					    boolean childFeatures,
					    FeatureVector fv) {
		
	if(!labeled) return fv;

	String[] forms = instance.forms;
	String[] pos = instance.postags;
	    
	String att = "";
	if(attR)
	    att = "RA";
	else
	    att = "LA";

	att+="&"+childFeatures;
		
	String w = forms[word];
	String wP = pos[word];

	String wPm1 = word > 0 ? pos[word-1] : "STR";
	String wPp1 = word < pos.length-1 ? pos[word+1] : "END";

	fv = add("NTS1="+type+"&"+att,fv);
	fv = add("ANTS1="+type,fv);
	for(int i = 0; i < 2; i++) {
	    String suff = i < 1 ? "&"+att : "";
	    suff = "&"+type+suff;

	    fv = add("NTH="+w+" "+wP+suff,fv);
	    fv = add("NTI="+wP+suff,fv);
	    fv = add("NTIA="+wPm1+" "+wP+suff,fv);
	    fv = add("NTIB="+wP+" "+wPp1+suff,fv);
	    fv = add("NTIC="+wPm1+" "+wP+" "+wPp1+suff,fv);
	    fv = add("NTJ="+w+suff,fv); //this

	}
		
	return fv;
    }


    public void fillFeatureVectors(DependencyInstance instance,
				   FeatureVector[][][] fvs,
				   double[][][] probs,
				   FeatureVector[][][][] nt_fvs,
				   double[][][][] nt_probs, Parameters params) {

	final int instanceLength = instance.length();

	// Get production crap.		
	for(int w1 = 0; w1 < instanceLength; w1++) {
	    for(int w2 = w1+1; w2 < instanceLength; w2++) {
		for(int ph = 0; ph < 2; ph++) {
		    boolean attR = ph == 0 ? true : false;
		    
		    int childInt = attR ? w2 : w1;
		    int parInt = attR ? w1 : w2;
		    
		    FeatureVector prodFV = addCoreFeatures(instance,w1,w2,attR,
							   new FeatureVector());
		    double prodProb = params.getScore(prodFV);
		    fvs[w1][w2][ph] = prodFV;
		    probs[w1][w2][ph] = prodProb;
		}
	    }
	}

	if(labeled) {
	    for(int w1 = 0; w1 < instanceLength; w1++) {
		for(int t = 0; t < types.length; t++) {
		    String type = types[t];
		    for(int ph = 0; ph < 2; ph++) {						
			boolean attR = ph == 0 ? true : false;
			for(int ch = 0; ch < 2; ch++) {						
			    boolean child = ch == 0 ? true : false;			    
			    FeatureVector prodFV = addLabeledFeatures(instance,w1,
								      type,attR,child,
								      new FeatureVector());
			    
			    double nt_prob = params.getScore(prodFV);
			    nt_fvs[w1][t][ph][ch] = prodFV;
			    nt_probs[w1][t][ph][ch] = nt_prob;
			    
			}
		    }
		}
	    }
	}		
    }


    /**
     * Write an instance to an output stream for later reading.
     *
     **/
    protected void writeInstance(DependencyInstance instance, ObjectOutputStream out) {

	int instanceLength = instance.length();

	try {

	    for(int w1 = 0; w1 < instanceLength; w1++) {
		for(int w2 = w1+1; w2 < instanceLength; w2++) {
		    for(int ph = 0; ph < 2; ph++) {						
			boolean attR = ph == 0 ? true : false;
			FeatureVector prodFV = addCoreFeatures(instance,w1,w2,attR,
							       new FeatureVector());
			out.writeObject(prodFV.keys());
		    }
		}
	    }
	    out.writeInt(-3);

	    if(labeled) {
		for(int w1 = 0; w1 < instanceLength; w1++) {		    
		    for(int t = 0; t < types.length; t++) {
			String type = types[t];			
			for(int ph = 0; ph < 2; ph++) {
			    boolean attR = ph == 0 ? true : false;
			    for(int ch = 0; ch < 2; ch++) {
				boolean child = ch == 0 ? true : false;
				FeatureVector prodFV = 
				    addLabeledFeatures(instance,w1,
						       type, attR,child,
						       new FeatureVector());

				out.writeObject(prodFV.keys());
			    }
			}
		    }
		}
		out.writeInt(-3);
	    }

	    writeExtendedFeatures(instance, out);

	    out.writeObject(instance.fv.keys());
	    out.writeInt(-4);

	    out.writeObject(instance);
	    out.writeInt(-1);

	    out.reset();

	} catch (IOException e) {}
		
    }
	

    /**
     * Override this method if you have extra features that need to be
     * written to disk. For the basic DependencyPipe, nothing happens.
     *
     */
    protected void writeExtendedFeatures (DependencyInstance instance, ObjectOutputStream out) 
	throws IOException {}


    /**
     * Read an instance from an input stream.
     *
     **/
    public DependencyInstance readInstance(ObjectInputStream in,
					   int length,
					   FeatureVector[][][] fvs,
					   double[][][] probs,
					   FeatureVector[][][][] nt_fvs,
					   double[][][][] nt_probs,
					   Parameters params) throws IOException {

	try {

	    // Get production crap.		
	    for(int w1 = 0; w1 < length; w1++) {
		for(int w2 = w1+1; w2 < length; w2++) {
		    for(int ph = 0; ph < 2; ph++) {
			FeatureVector prodFV = new FeatureVector((int[])in.readObject());
			double prodProb = params.getScore(prodFV);
			fvs[w1][w2][ph] = prodFV;
			probs[w1][w2][ph] = prodProb;
		    }
		}
	    }
	    int last = in.readInt();
	    if(last != -3) { System.out.println("Error reading file."); System.exit(0); }
	    
	    if(labeled) {
		for(int w1 = 0; w1 < length; w1++) {
		    for(int t = 0; t < types.length; t++) {
			String type = types[t];
			
			for(int ph = 0; ph < 2; ph++) {						
			    for(int ch = 0; ch < 2; ch++) {
				FeatureVector prodFV = new FeatureVector((int[])in.readObject());
				double nt_prob = params.getScore(prodFV);
				nt_fvs[w1][t][ph][ch] = prodFV;
				nt_probs[w1][t][ph][ch] = nt_prob;
			    }
			}
		    }
		}
		last = in.readInt();
		if(last != -3) { System.out.println("Error reading file."); System.exit(0); }
	    }

	    FeatureVector nfv = new FeatureVector((int[])in.readObject());
	    last = in.readInt();
	    if(last != -4) { System.out.println("Error reading file."); System.exit(0); }

	    DependencyInstance marshalledDI;
	    marshalledDI = (DependencyInstance)in.readObject();
	    marshalledDI.setFeatureVector(nfv);	

	    last = in.readInt();
	    if(last != -1) { System.out.println("Error reading file."); System.exit(0); }

	    return marshalledDI;

	} catch(ClassNotFoundException e) { 
	    System.out.println("Error reading file."); System.exit(0); 
	} 

	// this won't happen, but it takes care of compilation complaints
	return null;
    }
		
    /**
     * Get features for stems the old way. The only way this differs
     * from calling addTwoFactorFeatures() is that it checks the
     * lengths of the full lexical items are greater than 5 before
     * adding features.
     *
     */
    private final FeatureVector 
	addOldMSTStemFeatures(String hLemma, String headP, 
			      String cLemma, String childP, String attDist, 
			      int hL, int cL, FeatureVector fv) {

	String all = hLemma + " " + headP + " " + cLemma + " " + childP;
	String hPos = headP + " " + cLemma + " " + childP;
	String cPos = hLemma + " " + headP + " " + childP;
	String hP = headP + " " + cLemma;
	String cP = hLemma + " " + childP;
	String oPos = headP + " " + childP;
	String oLex = hLemma + " " + cLemma;
	
	fv = add("SA="+all+attDist,fv); //this
	fv = add("SF="+oLex+attDist,fv); //this
	fv = add("SAA="+all,fv); //this
	fv = add("SFF="+oLex,fv); //this
	
	if(cL > 5) {
	    fv = add("SB="+hPos+attDist,fv);
	    fv = add("SD="+hP+attDist,fv);
	    fv = add("SK="+cLemma+" "+childP+attDist,fv);
	    fv = add("SM="+cLemma+attDist,fv); //this
	    fv = add("SBB="+hPos,fv);
	    fv = add("SDD="+hP,fv);
	    fv = add("SKK="+cLemma+" "+childP,fv);
	    fv = add("SMM="+cLemma,fv); //this
	}
	if(hL > 5) {
	    fv = add("SC="+cPos+attDist,fv);
	    fv = add("SE="+cP+attDist,fv);
	    fv = add("SH="+hLemma+" "+headP+attDist,fv);
	    fv = add("SJ="+hLemma+attDist,fv); //this
	    
	    fv = add("SCC="+cPos,fv);
	    fv = add("SEE="+cP,fv);
	    fv = add("SHH="+hLemma+" "+headP,fv);
	    fv = add("SJJ="+hLemma,fv); //this
	}

	return fv;
    }
		
}
