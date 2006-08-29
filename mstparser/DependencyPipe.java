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

    public boolean createForest;
	
    public DependencyPipe() throws IOException {
	this(true, "CONLL");
    }

    public DependencyPipe(boolean createForest, String format) throws IOException {
	dataAlphabet = new Alphabet();
	typeAlphabet = new Alphabet();
	this.createForest = createForest;
	this.format = format;
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

    public DependencyInstance nextInstance() throws IOException {
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

	while(instance != null) {
	    System.out.println("Creating Feature Vector Instance: " + num1);
	    
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
		possibleFeatures(instance,out);
	    instance = null;
			
	    instance = depReader.getNext();

	    num1++;
	}

	closeAlphabets();
		
	if(createForest)
	    out.close();

	return lengths.toNativeArray();
		
    }

    private void createAlphabet(String file) throws IOException {

	System.out.print("Creating Alphabet ... ");

	labeled = depReader.startReading(file);

	DependencyInstance instance = depReader.getNext();

	int cnt = 0;
		
	while(instance != null) {
	    
	    String[] labs = instance.deprels;

	    for(int i = 0; i < labs.length; i++)
		typeAlphabet.lookupIndex(labs[i]);
			
	    createFeatureVector(instance);
			
	    instance = depReader.getNext();

	    cnt++;
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

	
    public FeatureVector createFeatureVector(DependencyInstance instance) {

	int instanceLength = instance.length();

	String[] labs = instance.deprels;
	int[] heads = instance.heads;

	FeatureVector fv = new FeatureVector();
	for(int i = 0; i < instanceLength; i++) {
	    if(heads[i] == -1)
		continue;
	    int small = i < heads[i] ? i : heads[i];
	    int large = i > heads[i] ? i : heads[i];
	    boolean attR = i < heads[i] ? false : true;
	    fv = createFeatureVector(instance,small,large,attR,fv);
	    if(labeled) {
		fv = createFeatureVector(instance,i,labs[i],attR,true,fv);
		fv = createFeatureVector(instance,heads[i],labs[i],attR,false,fv);
	    }
	}
	return fv;
    }

    public FeatureVector add(String feat, double val, FeatureVector fv) {
	int num = dataAlphabet.lookupIndex(feat);
	if(num >= 0)
	    return new FeatureVector(num,val,fv);
	return fv;
    }

    public void possibleFeatures(DependencyInstance instance, 
				 ObjectOutputStream out) {

	int instanceLength = instance.length();

	try {

	    for(int w1 = 0; w1 < instanceLength; w1++) {
		for(int w2 = w1+1; w2 < instanceLength; w2++) {
					
		    for(int ph = 0; ph < 2; ph++) {						
			boolean attR = ph == 0 ? true : false;

			int childInt = attR ? w2 : w1;
			int parInt = attR ? w1 : w2;
						
			FeatureVector prodFV = createFeatureVector(instance,w1,w2,attR,
								   new FeatureVector());
								
			for(FeatureVector curr = prodFV; curr != null; curr = curr.next) {
			    if(curr.index >= 0)
				out.writeInt(curr.index);
			}
			out.writeInt(-2);
								
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
				    createFeatureVector(instance,w1,
							type, attR,child,
							new FeatureVector());

				for(FeatureVector curr = prodFV; curr != null; curr = curr.next) {
				    if(curr.index >= 0)
					out.writeInt(curr.index);
				}
				out.writeInt(-2);
				
			    }
			}
		    }
		    
		}
		
		out.writeInt(-3);
	    }

	    for(FeatureVector curr = instance.fv; curr.next != null; curr = curr.next)
		out.writeInt(curr.index);

	    out.writeInt(-4);
	    out.writeObject(instance);
			
	    out.writeInt(-1);
	    out.reset();

	} catch (IOException e) {}
		
    }
	
    public DependencyInstance getFeatureVector(ObjectInputStream in,
					       int length,
					       FeatureVector[][][] fvs,
					       double[][][] probs,
					       FeatureVector[][][][] nt_fvs,
					       double[][][][] nt_probs,
					       Parameters params) throws IOException {
	// Get production crap.		
	for(int w1 = 0; w1 < length; w1++) {
	    for(int w2 = w1+1; w2 < length; w2++) {
				
		for(int ph = 0; ph < 2; ph++) {

		    FeatureVector prodFV = new FeatureVector();
					
		    int indx = in.readInt();
		    while(indx != -2) {
			prodFV = new FeatureVector(indx,1.0,prodFV);
			indx = in.readInt();
		    }
					
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
			    
			    FeatureVector prodFV = new FeatureVector();
			    
			    int indx = in.readInt();
			    while(indx != -2) {
				prodFV = new FeatureVector(indx,1.0,prodFV);
				indx = in.readInt();
			    }
			    
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

	FeatureVector nfv = new FeatureVector();
	int next = in.readInt();
	while(next != -4) {
	    nfv = new FeatureVector(next,1.0,nfv);
	    next = in.readInt();
	}

	DependencyInstance marshalledDI;
	try {
	    marshalledDI = (DependencyInstance)in.readObject();
	    marshalledDI.setFeatureVector(nfv);	
	    next = in.readInt();
	    if(next != -1) { 
		System.out.println("Error reading file."); System.exit(0); 
	    }
	    return marshalledDI;
	} catch(ClassNotFoundException e) { 
	    System.out.println("Error reading file."); System.exit(0); 
	} 

	// this won't happen, but it takes care of compilation complaints
	return null;
    }
		
    public void getFeatureVector(DependencyInstance instance,
				 FeatureVector[][][] fvs,
				 double[][][] probs,
				 FeatureVector[][][][] nt_fvs,
				 double[][][][] nt_probs, Parameters params) {

	int instanceLength = instance.length();

	// Get production crap.		
	for(int w1 = 0; w1 < instanceLength; w1++) {
	    for(int w2 = w1+1; w2 < instanceLength; w2++) {
				
		for(int ph = 0; ph < 2; ph++) {
		    boolean attR = ph == 0 ? true : false;
		    
		    int childInt = attR ? w2 : w1;
		    int parInt = attR ? w1 : w2;
		    
		    FeatureVector prodFV = createFeatureVector(instance,w1,w2,attR,
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
			    FeatureVector prodFV = createFeatureVector(instance,w1,
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

    public FeatureVector createFeatureVector(DependencyInstance instance,
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
	    fv = add("PC="+allPos+attDist,1.0,fv);
	    fv = add("1PC="+allPos,1.0,fv);
	    fv = add("XPC="+allPosA+attDist,1.0,fv);
	    fv = add("X1PC="+allPosA,1.0,fv);
	}

	// feature posL-1 posL posR posR+1
	fv = add("PT="+pLeft+" "+pos[small]+" "+pos[large]+" "+pRight+attDist,1.0,fv);
	fv = add("PT1="+pos[small]+" "+pos[large]+" " +pRight+attDist,1.0,fv);
	fv = add("PT2="+pLeft+" "+pos[small]+" "+pos[large]+attDist,1.0,fv);
	fv = add("PT3="+pLeft+" "+pos[large]+" "+pRight+attDist,1.0,fv);
	fv = add("PT4="+pLeft+" "+pos[small]+" "+pRight+attDist,1.0,fv);
		
	fv = add("1PT="+pLeft+" "+pos[small]+" "+pos[large]+" "+pRight,1.0,fv);
	fv = add("1PT1="+pos[small]+" "+pos[large]+" " +pRight,1.0,fv);
	fv = add("1PT2="+pLeft+" "+pos[small]+" "+pos[large],1.0,fv);
	fv = add("1PT3="+pLeft+" "+pos[large]+" "+pRight,1.0,fv);
	fv = add("1PT4="+pLeft+" "+pos[small]+" "+pRight,1.0,fv);
		
	fv = add("XPT="+pLeftA+" "+posA[small]+" "+posA[large]+" "+pRightA+attDist,1.0,fv);
	fv = add("XPT1="+posA[small]+" "+posA[large]+" " +pRightA+attDist,1.0,fv);
	fv = add("XPT2="+pLeftA+" "+posA[small]+" "+posA[large]+attDist,1.0,fv);
	fv = add("XPT3="+pLeftA+" "+posA[large]+" "+pRightA+attDist,1.0,fv);
	fv = add("XPT4="+pLeftA+" "+posA[small]+" "+pRightA+attDist,1.0,fv);
		
	fv = add("X1PT="+pLeftA+" "+posA[small]+" "+posA[large]+" "+pRightA,1.0,fv);
	fv = add("X1PT1="+posA[small]+" "+posA[large]+" " +pRightA,1.0,fv);
	fv = add("X1PT2="+pLeftA+" "+posA[small]+" "+posA[large],1.0,fv);
	fv = add("X1PT3="+pLeftA+" "+posA[large]+" "+pRightA,1.0,fv);
	fv = add("X1PT4="+pLeftA+" "+posA[small]+" "+pRightA,1.0,fv);
		
	// feature posL posL+1 posR-1 posR
	fv = add("APT="+pos[small]+" "+pLeftRight+" "
		 +pRightLeft+" "+pos[large]+attDist,1.0,fv);
	fv = add("APT1="+pos[small]+" "+pRightLeft+" "+pos[large]+attDist,1.0,fv);
	fv = add("APT2="+pos[small]+" "+pLeftRight+" "+pos[large]+attDist,1.0,fv);
	fv = add("APT3="+pLeftRight+" "+pRightLeft+" "+pos[large]+attDist,1.0,fv);
	fv = add("APT4="+pos[small]+" "+pLeftRight+" "+pRightLeft+attDist,1.0,fv);

	fv = add("1APT="+pos[small]+" "+pLeftRight+" "
		 +pRightLeft+" "+pos[large],1.0,fv);
	fv = add("1APT1="+pos[small]+" "+pRightLeft+" "+pos[large],1.0,fv);
	fv = add("1APT2="+pos[small]+" "+pLeftRight+" "+pos[large],1.0,fv);
	fv = add("1APT3="+pLeftRight+" "+pRightLeft+" "+pos[large],1.0,fv);
	fv = add("1APT4="+pos[small]+" "+pLeftRight+" "+pRightLeft,1.0,fv);
		
	fv = add("XAPT="+posA[small]+" "+pLeftRightA+" "
		 +pRightLeftA+" "+posA[large]+attDist,1.0,fv);
	fv = add("XAPT1="+posA[small]+" "+pRightLeftA+" "+posA[large]+attDist,1.0,fv);
	fv = add("XAPT2="+posA[small]+" "+pLeftRightA+" "+posA[large]+attDist,1.0,fv);
	fv = add("XAPT3="+pLeftRightA+" "+pRightLeftA+" "+posA[large]+attDist,1.0,fv);
	fv = add("XAPT4="+posA[small]+" "+pLeftRightA+" "+pRightLeftA+attDist,1.0,fv);

	fv = add("X1APT="+posA[small]+" "+pLeftRightA+" "
		 +pRightLeftA+" "+posA[large],1.0,fv);
	fv = add("X1APT1="+posA[small]+" "+pRightLeftA+" "+posA[large],1.0,fv);
	fv = add("X1APT2="+posA[small]+" "+pLeftRightA+" "+posA[large],1.0,fv);
	fv = add("X1APT3="+pLeftRightA+" "+pRightLeftA+" "+posA[large],1.0,fv);
	fv = add("X1APT4="+posA[small]+" "+pLeftRightA+" "+pRightLeftA,1.0,fv);
		
	// feature posL-1 posL posR-1 posR
	// feature posL posL+1 posR posR+1
	fv = add("BPT="+pLeft+" "+pos[small]+" "+pRightLeft+" "+pos[large]+attDist,1.0,fv);
	fv = add("1BPT="+pLeft+" "+pos[small]+" "+pRightLeft+" "+pos[large],1.0,fv);
	fv = add("CPT="+pos[small]+" "+pLeftRight+" "+pos[large]+" "+pRight+attDist,1.0,fv);
	fv = add("1CPT="+pos[small]+" "+pLeftRight+" "+pos[large]+" "+pRight,1.0,fv);
		
	fv = add("XBPT="+pLeftA+" "+posA[small]+" "+pRightLeftA+" "+posA[large]+attDist,1.0,fv);
	fv = add("X1BPT="+pLeftA+" "+posA[small]+" "+pRightLeftA+" "+posA[large],1.0,fv);
	fv = add("XCPT="+posA[small]+" "+pLeftRightA+" "+posA[large]+" "+pRightA+attDist,1.0,fv);
	fv = add("X1CPT="+posA[small]+" "+pLeftRightA+" "+posA[large]+" "+pRightA,1.0,fv);

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

	fv = add("A="+all+attDist,1.0,fv); //this
	fv = add("B="+hPos+attDist,1.0,fv);
	fv = add("C="+cPos+attDist,1.0,fv);
	fv = add("D="+hP+attDist,1.0,fv);
	fv = add("E="+cP+attDist,1.0,fv);
	fv = add("F="+oLex+attDist,1.0,fv); //this
	fv = add("G="+oPos+attDist,1.0,fv);
	fv = add("H="+head+" "+headP+attDist,1.0,fv);
	fv = add("I="+headP+attDist,1.0,fv);
	fv = add("J="+head+attDist,1.0,fv); //this
	fv = add("K="+child+" "+childP+attDist,1.0,fv);
	fv = add("L="+childP+attDist,1.0,fv);
	fv = add("M="+child+attDist,1.0,fv); //this

	fv = add("AA="+all,1.0,fv); //this
	fv = add("BB="+hPos,1.0,fv);
	fv = add("CC="+cPos,1.0,fv);
	fv = add("DD="+hP,1.0,fv);
	fv = add("EE="+cP,1.0,fv);
	fv = add("FF="+oLex,1.0,fv); //this
	fv = add("GG="+oPos,1.0,fv);
	fv = add("HH="+head+" "+headP,1.0,fv);
	fv = add("II="+headP,1.0,fv);
	fv = add("JJ="+head,1.0,fv); //this
	fv = add("KK="+child+" "+childP,1.0,fv);
	fv = add("LL="+childP,1.0,fv);
	fv = add("MM="+child,1.0,fv); //this

	if(head.length() > 5 || child.length() > 5) {
	    int hL = head.length();
	    int cL = child.length();
		    
	    head = hL > 5 ? head.substring(0,5) : head;
	    child = cL > 5 ? child.substring(0,5) : child;
		    
	    all = head + " " + headP + " " + child + " " + childP;
	    hPos = headP + " " + child + " " + childP;
	    cPos = head + " " + headP + " " + childP;
	    hP = headP + " " + child;
	    cP = head + " " + childP;
	    oPos = headP + " " + childP;
	    oLex = head + " " + child;
	
	    fv = add("SA="+all+attDist,1.0,fv); //this
	    fv = add("SF="+oLex+attDist,1.0,fv); //this
	    fv = add("SAA="+all,1.0,fv); //this
	    fv = add("SFF="+oLex,1.0,fv); //this

	    if(cL > 5) {
		fv = add("SB="+hPos+attDist,1.0,fv);
		fv = add("SD="+hP+attDist,1.0,fv);
		fv = add("SK="+child+" "+childP+attDist,1.0,fv);
		fv = add("SM="+child+attDist,1.0,fv); //this
		fv = add("SBB="+hPos,1.0,fv);
		fv = add("SDD="+hP,1.0,fv);
		fv = add("SKK="+child+" "+childP,1.0,fv);
		fv = add("SMM="+child,1.0,fv); //this
	    }
	    if(hL > 5) {
		fv = add("SC="+cPos+attDist,1.0,fv);
		fv = add("SE="+cP+attDist,1.0,fv);
		fv = add("SH="+head+" "+headP+attDist,1.0,fv);
		fv = add("SJ="+head+attDist,1.0,fv); //this
			
		fv = add("SCC="+cPos,1.0,fv);
		fv = add("SEE="+cP,1.0,fv);
		fv = add("SHH="+head+" "+headP,1.0,fv);
		fv = add("SJJ="+head,1.0,fv); //this
	    }
	}
		
	return fv;
		
    }
	
    public FeatureVector createFeatureVector(DependencyInstance instance,
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

	fv = add("NTS1="+type+"&"+att,1.0,fv);
	fv = add("ANTS1="+type,1.0,fv);
	for(int i = 0; i < 2; i++) {
	    String suff = i < 1 ? "&"+att : "";
	    suff = "&"+type+suff;

	    fv = add("NTH="+w+" "+wP+suff,1.0,fv);
	    fv = add("NTI="+wP+suff,1.0,fv);
	    fv = add("NTIA="+wPm1+" "+wP+suff,1.0,fv);
	    fv = add("NTIB="+wP+" "+wPp1+suff,1.0,fv);
	    fv = add("NTIC="+wPm1+" "+wP+" "+wPp1+suff,1.0,fv);
	    fv = add("NTJ="+w+suff,1.0,fv); //this

	}
		
	return fv;
    }

		
}
