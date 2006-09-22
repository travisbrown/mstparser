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

	String att = "";
	if(attR)
	    att = "RA";
	else
	    att = "LA";
		
	int dist = Math.abs(large-small);
	String distBool = "0";
	if(dist > 1)
	    distBool = "1";
	if(dist > 2)
	    distBool = "2";
	if(dist > 3)
	    distBool = "3";
	if(dist > 4)
	    distBool = "4";
	if(dist > 5)
	    distBool = "5";
	if(dist > 10)
	    distBool = "10";
		
	String attDist = "&"+att+"&"+distBool;

	String pLeft = small > 0 ? pos[small-1] : "STR";
	String pRight = large < pos.length-1 ? pos[large+1] : "END";
	String pLeftRight = small < large-1 ? pos[small+1] : "MID";
	String pRightLeft = large > small+1 ? pos[large-1] : "MID";
	String pLeftA = small > 0 ? posA[small-1] : "STR";
	String pRightA = large < pos.length-1 ? posA[large+1] : "END";
	String pLeftRightA = small < large-1 ? posA[small+1] : "MID";
	String pRightLeftA = large > small+1 ? posA[large-1] : "MID";
		
	// feature posR posMid posL
	for(int i = small+1; i < large; i++) {
	    String allPos = pos[small]+" "+pos[i]+" "+pos[large];
	    String allPosA = posA[small]+" "+posA[i]+" "+posA[large];
	    fv = add("PC="+allPos+attDist,fv);
	    fv = add("1PC="+allPos,fv);
	    fv = add("XPC="+allPosA+attDist,fv);
	    fv = add("X1PC="+allPosA,fv);
	}

	// feature posL-1 posL posR posR+1
	fv = add("PT="+pLeft+" "+pos[small]+" "+pos[large]+" "+pRight+attDist,fv);
	fv = add("PT1="+pos[small]+" "+pos[large]+" " +pRight+attDist,fv);
	fv = add("PT2="+pLeft+" "+pos[small]+" "+pos[large]+attDist,fv);
	fv = add("PT3="+pLeft+" "+pos[large]+" "+pRight+attDist,fv);
	fv = add("PT4="+pLeft+" "+pos[small]+" "+pRight+attDist,fv);
		
	fv = add("1PT="+pLeft+" "+pos[small]+" "+pos[large]+" "+pRight,fv);
	fv = add("1PT1="+pos[small]+" "+pos[large]+" " +pRight,fv);
	fv = add("1PT2="+pLeft+" "+pos[small]+" "+pos[large],fv);
	fv = add("1PT3="+pLeft+" "+pos[large]+" "+pRight,fv);
	fv = add("1PT4="+pLeft+" "+pos[small]+" "+pRight,fv);
		
	fv = add("XPT="+pLeftA+" "+posA[small]+" "+posA[large]+" "+pRightA+attDist,fv);
	fv = add("XPT1="+posA[small]+" "+posA[large]+" " +pRightA+attDist,fv);
	fv = add("XPT2="+pLeftA+" "+posA[small]+" "+posA[large]+attDist,fv);
	fv = add("XPT3="+pLeftA+" "+posA[large]+" "+pRightA+attDist,fv);
	fv = add("XPT4="+pLeftA+" "+posA[small]+" "+pRightA+attDist,fv);
		
	fv = add("X1PT="+pLeftA+" "+posA[small]+" "+posA[large]+" "+pRightA,fv);
	fv = add("X1PT1="+posA[small]+" "+posA[large]+" " +pRightA,fv);
	fv = add("X1PT2="+pLeftA+" "+posA[small]+" "+posA[large],fv);
	fv = add("X1PT3="+pLeftA+" "+posA[large]+" "+pRightA,fv);
	fv = add("X1PT4="+pLeftA+" "+posA[small]+" "+pRightA,fv);
		
	// feature posL posL+1 posR-1 posR
	fv = add("APT="+pos[small]+" "+pLeftRight+" "
		 +pRightLeft+" "+pos[large]+attDist,fv);
	fv = add("APT1="+pos[small]+" "+pRightLeft+" "+pos[large]+attDist,fv);
	fv = add("APT2="+pos[small]+" "+pLeftRight+" "+pos[large]+attDist,fv);
	fv = add("APT3="+pLeftRight+" "+pRightLeft+" "+pos[large]+attDist,fv);
	fv = add("APT4="+pos[small]+" "+pLeftRight+" "+pRightLeft+attDist,fv);

	fv = add("1APT="+pos[small]+" "+pLeftRight+" "
		 +pRightLeft+" "+pos[large],fv);
	fv = add("1APT1="+pos[small]+" "+pRightLeft+" "+pos[large],fv);
	fv = add("1APT2="+pos[small]+" "+pLeftRight+" "+pos[large],fv);
	fv = add("1APT3="+pLeftRight+" "+pRightLeft+" "+pos[large],fv);
	fv = add("1APT4="+pos[small]+" "+pLeftRight+" "+pRightLeft,fv);
		
	fv = add("XAPT="+posA[small]+" "+pLeftRightA+" "
		 +pRightLeftA+" "+posA[large]+attDist,fv);
	fv = add("XAPT1="+posA[small]+" "+pRightLeftA+" "+posA[large]+attDist,fv);
	fv = add("XAPT2="+posA[small]+" "+pLeftRightA+" "+posA[large]+attDist,fv);
	fv = add("XAPT3="+pLeftRightA+" "+pRightLeftA+" "+posA[large]+attDist,fv);
	fv = add("XAPT4="+posA[small]+" "+pLeftRightA+" "+pRightLeftA+attDist,fv);

	fv = add("X1APT="+posA[small]+" "+pLeftRightA+" "
		 +pRightLeftA+" "+posA[large],fv);
	fv = add("X1APT1="+posA[small]+" "+pRightLeftA+" "+posA[large],fv);
	fv = add("X1APT2="+posA[small]+" "+pLeftRightA+" "+posA[large],fv);
	fv = add("X1APT3="+pLeftRightA+" "+pRightLeftA+" "+posA[large],fv);
	fv = add("X1APT4="+posA[small]+" "+pLeftRightA+" "+pRightLeftA,fv);
		
	// feature posL-1 posL posR-1 posR
	// feature posL posL+1 posR posR+1
	fv = add("BPT="+pLeft+" "+pos[small]+" "+pRightLeft+" "+pos[large]+attDist,fv);
	fv = add("1BPT="+pLeft+" "+pos[small]+" "+pRightLeft+" "+pos[large],fv);
	fv = add("CPT="+pos[small]+" "+pLeftRight+" "+pos[large]+" "+pRight+attDist,fv);
	fv = add("1CPT="+pos[small]+" "+pLeftRight+" "+pos[large]+" "+pRight,fv);
		
	fv = add("XBPT="+pLeftA+" "+posA[small]+" "+pRightLeftA+" "+posA[large]+attDist,fv);
	fv = add("X1BPT="+pLeftA+" "+posA[small]+" "+pRightLeftA+" "+posA[large],fv);
	fv = add("XCPT="+posA[small]+" "+pLeftRightA+" "+posA[large]+" "+pRightA+attDist,fv);
	fv = add("X1CPT="+posA[small]+" "+pLeftRightA+" "+posA[large]+" "+pRightA,fv);

	String head = attR ? forms[small] : forms[large];
	String headP = attR ? pos[small] : pos[large];
	String child = attR ? forms[large] : forms[small];
	String childP = attR ? pos[large] : pos[small];

	String all = head + " " + headP + " " + child + " " + childP;
	String hPos = headP + " " + child + " " + childP;
	String cPos = head + " " + headP + " " + childP;
	String hP = headP + " " + child;
	String cP = head + " " + childP;
	String oPos = headP + " " + childP;
	String oLex = head + " " + child;

	fv = add("A="+all+attDist,fv); //this
	fv = add("B="+hPos+attDist,fv);
	fv = add("C="+cPos+attDist,fv);
	fv = add("D="+hP+attDist,fv);
	fv = add("E="+cP+attDist,fv);
	fv = add("F="+oLex+attDist,fv); //this
	fv = add("G="+oPos+attDist,fv);
	fv = add("H="+head+" "+headP+attDist,fv);
	fv = add("I="+headP+attDist,fv);
	fv = add("J="+head+attDist,fv); //this
	fv = add("K="+child+" "+childP+attDist,fv);
	fv = add("L="+childP+attDist,fv);
	fv = add("M="+child+attDist,fv); //this

	fv = add("AA="+all,fv); //this
	fv = add("BB="+hPos,fv);
	fv = add("CC="+cPos,fv);
	fv = add("DD="+hP,fv);
	fv = add("EE="+cP,fv);
	fv = add("FF="+oLex,fv); //this
	fv = add("GG="+oPos,fv);
	fv = add("HH="+head+" "+headP,fv);
	fv = add("II="+headP,fv);
	fv = add("JJ="+head,fv); //this
	fv = add("KK="+child+" "+childP,fv);
	fv = add("LL="+childP,fv);
	fv = add("MM="+child,fv); //this

	int hL = head.length();
	int cL = child.length();

	if(isCONLL || hL > 5 || cL > 5) {
	    String[] lemmas = instance.lemmas;

	    String hLemma = attR ? lemmas[small] : lemmas[large];
	    String cLemma = attR ? lemmas[large] : lemmas[small];
		    
	    all = hLemma + " " + headP + " " + cLemma + " " + childP;
	    hPos = headP + " " + cLemma + " " + childP;
	    cPos = hLemma + " " + headP + " " + childP;
	    hP = headP + " " + cLemma;
	    cP = hLemma + " " + childP;
	    oPos = headP + " " + childP;
	    oLex = hLemma + " " + cLemma;
	
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
	}
		
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
		

		
}
