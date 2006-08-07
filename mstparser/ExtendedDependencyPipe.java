package mstparser;

import java.io.*;
import gnu.trove.*;
import java.util.*;

public class ExtendedDependencyPipe extends DependencyPipe {

    public ExtendedDependencyPipe() throws IOException {
	super(true);
    }

    public ExtendedDependencyPipe(boolean createForest) throws IOException {
	super(createForest);
    }

    protected void augmentInstance (DependencyInstance depinst) {

	String[] toks = depinst.get("tokens");

	String[] stems = new String[toks.length];
	String[] suffixes = new String[toks.length];

	for(int i = 0; i < toks.length; i++) {

	    String[] wordStem = toks[i].split("\\+");
	    if (wordStem.length ==1) {
		wordStem = new String[2];
		wordStem[0] = toks[i];
		wordStem[1] = toks[i];
	    }

	    if (wordStem[0].length() > wordStem[1].length())
		suffixes[i] = wordStem[0].substring(wordStem[1].length());
	    else
		suffixes[i] = "<no-suffix>";

	    toks[i] = wordStem[0];
	    stems[i] = wordStem[1];

	}

	depinst.put("stems", stems);
	depinst.put("suffixes", suffixes);

    }


    public FeatureVector createFeatureVector(DependencyInstance depinst,
					     int small,
					     int large,
					     boolean attR,
					     FeatureVector fv) {


	String[] toks = depinst.get("tokens");
	String[] pos = depinst.get("pos");
	String[] posA = depinst.get("posA");

	// Added by Jason - new feature types
	String[] stems = depinst.get("stems");
	String[] suffixes = depinst.get("suffixes");
	    
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

	String head = attR ? toks[small] : toks[large];
	String headP = attR ? pos[small] : pos[large];
	String child = attR ? toks[large] : toks[small];
	String childP = attR ? pos[large] : pos[small];

	// Added by Jason
	String headStem = attR ? stems[small] : stems[large];
	String childStem = attR ? stems[large] : stems[small];
	String headSuff = attR ? suffixes[small] : suffixes[large];
	String childSuff = attR ? suffixes[large] : suffixes[small];

	String all = head + " " + headP + " " + child + " " + childP;
	String hPos = headP + " " + child + " " + childP;
	String cPos = head + " " + headP + " " + childP;
	String hP = headP + " " + child;
	String cP = head + " " + childP;
	String oPos = headP + " " + childP;
	String oLex = head + " " + child;

	// Added by Jason
	String allStem = headStem + " " + headP + " " + childStem + " " + childP;
	String hPosStem = headP + " " + childStem + " " + childP;
	String cPosStem = headStem + " " + headP + " " + childP;
	String hPStem = headP + " " + childStem;
	String cPStem = headStem + " " + childP;
	String oLexStem = headStem + " " + childStem;

	// Added by Jason
	String allSuff = headSuff + " " + headP + " " + childSuff + " " + childP;
	String hPosSuff = headP + " " + childSuff + " " + childP;
	String cPosSuff = headSuff + " " + headP + " " + childP;
	String hPSuff = headP + " " + childSuff;
	String cPSuff = headSuff + " " + childP;
	String oLexSuff = headSuff + " " + childSuff;

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

	// Added by Jason
	fv = add("Stem-A="+allStem+attDist,1.0,fv); //this
	fv = add("Stem-B="+hPosStem+attDist,1.0,fv);
	fv = add("Stem-C="+cPosStem+attDist,1.0,fv);
	fv = add("Stem-D="+hPStem+attDist,1.0,fv);
	fv = add("Stem-E="+cPStem+attDist,1.0,fv);
	fv = add("Stem-F="+oLexStem+attDist,1.0,fv); //this
	fv = add("Stem-H="+headStem+" "+headP+attDist,1.0,fv);
	fv = add("Stem-J="+headStem+attDist,1.0,fv); //this
	fv = add("Stem-K="+childStem+" "+childP+attDist,1.0,fv);
	fv = add("Stem-M="+childStem+attDist,1.0,fv); //this

	// Added by Jason
	fv = add("Suff-A="+allSuff+attDist,1.0,fv); //this
	fv = add("Suff-B="+hPosSuff+attDist,1.0,fv);
	fv = add("Suff-C="+cPosSuff+attDist,1.0,fv);
	fv = add("Suff-D="+hPSuff+attDist,1.0,fv);
	fv = add("Suff-E="+cPSuff+attDist,1.0,fv);
	fv = add("Suff-F="+oLexSuff+attDist,1.0,fv); //this
	fv = add("Suff-H="+headSuff+" "+headP+attDist,1.0,fv);
	fv = add("Suff-J="+headSuff+attDist,1.0,fv); //this
	fv = add("Suff-K="+childSuff+" "+childP+attDist,1.0,fv);
	fv = add("Suff-M="+childSuff+attDist,1.0,fv); //this

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

	// Added by Jason
	fv = add("Stem-AA="+allStem,1.0,fv); //this
	fv = add("Stem-BB="+hPosStem,1.0,fv);
	fv = add("Stem-CC="+cPosStem,1.0,fv);
	fv = add("Stem-DD="+hPStem,1.0,fv);
	fv = add("Stem-EE="+cPStem,1.0,fv);
	fv = add("Stem-FF="+oLexStem,1.0,fv); //this
	fv = add("Stem-HH="+headStem+" "+headP,1.0,fv);
	fv = add("Stem-JJ="+headStem,1.0,fv); //this
	fv = add("Stem-KK="+childStem+" "+childP,1.0,fv);
	fv = add("Stem-MM="+childStem,1.0,fv); //this


	// Added by Jason
	fv = add("Suff-AA="+allSuff,1.0,fv); //this
	fv = add("Suff-BB="+hPosSuff,1.0,fv);
	fv = add("Suff-CC="+cPosSuff,1.0,fv);
	fv = add("Suff-DD="+hPSuff,1.0,fv);
	fv = add("Suff-EE="+cPSuff,1.0,fv);
	fv = add("Suff-FF="+oLexSuff,1.0,fv); //this
	fv = add("Suff-HH="+headSuff+" "+headP,1.0,fv);
	fv = add("Suff-JJ="+headSuff,1.0,fv); //this
	fv = add("Suff-KK="+childSuff+" "+childP,1.0,fv);
	fv = add("Suff-MM="+childSuff,1.0,fv); //this

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


    public FeatureVector createFeatureVector(DependencyInstance depinst,
					     int word,
					     String type,
					     boolean attR,
					     boolean childFeatures,
					     FeatureVector fv) {
		
	if(!labeled) return fv;


	String[] toks = depinst.get("tokens");
	String[] pos = depinst.get("pos");
	String[] posA = depinst.get("posA");
	    
	// Added by Jason
	String[] stems = depinst.get("stems");
	String[] suffixes = depinst.get("suffixes");

	String att = "";
	if(attR)
	    att = "RA";
	else
	    att = "LA";

	att+="&"+childFeatures;
		
	String w = toks[word];
	String wP = pos[word];

	// Added by Jason
	String stem = stems[word];
	String trueSuffix = suffixes[word];

	String wPm1 = word > 0 ? pos[word-1] : "STR";
	String wPp1 = word < pos.length-1 ? pos[word+1] : "END";

	String stemPm1 = word > 0 ? stems[word-1] : "STR";
	String stemPp1 = word < stems.length-1 ? stems[word+1] : "END";

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
	    
	    // Added by Jason
	    fv = add("Suff-NTJ="+w+trueSuffix,1.0,fv); //this

	    // Added by Jason
	    fv = add("Stem-NTH="+stem+" "+wP+suff,1.0,fv);
	    fv = add("Stem-NTJ="+stem+suff,1.0,fv); //this
	    fv = add("Stem-NTIA="+stemPm1+" "+wP+suff,1.0,fv);
	    fv = add("Stem-NTIB="+wP+" "+stemPp1+suff,1.0,fv);
	    fv = add("Stem-NTIC="+stemPm1+" "+wP+" "+wPp1+suff,1.0,fv);
	    fv = add("Stem-NTJ="+stem+suff,1.0,fv); //this
	    fv = add("Suff-Stem-NTJ="+stem+trueSuffix,1.0,fv); //this
			
	}
		
	return fv;
    }
	
}
