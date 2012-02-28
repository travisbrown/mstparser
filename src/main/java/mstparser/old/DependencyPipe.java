package mstparser.old;

import mstparser.ParserOptions;
import mstparser.io.*;
import java.io.*;
import java.util.*;

public abstract class DependencyPipe {
  public abstract ParserOptions getOptions();
  public abstract void add(String feat, mstparser.FeatureVector fv);
  public abstract void add(String feat, double val, mstparser.FeatureVector fv);

  protected abstract void addLinearFeatures(String type, String[] obsVals, 
					 int first, int second,
					 String attachDistance,
					 mstparser.FeatureVector fv);

    protected final void 
	addCorePosFeatures(String prefix,
			   String leftOf1, String one, String rightOf1, 
			   String leftOf2, String two, String rightOf2, 
			   String attachDistance, 
			   mstparser.FeatureVector fv) {

	// feature posL-1 posL posR posR+1

	add(prefix+"="+leftOf1+" "+one+" "+two+"*"+attachDistance, fv);

	StringBuilder feat = 
	    new StringBuilder(prefix+"1="+leftOf1+" "+one+" "+two);
	add(feat.toString(), fv);
	feat.append(' ').append(rightOf2);
	add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	add(feat.toString(), fv);

	feat = new StringBuilder(prefix+"2="+leftOf1+" "+two+" "+rightOf2);
	add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	add(feat.toString(), fv);
	
	feat = new StringBuilder(prefix+"3="+leftOf1+" "+one+" "+rightOf2);
	add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	add(feat.toString(), fv);
	
	feat = new StringBuilder(prefix+"4="+one+" "+two+" "+rightOf2);
	add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	add(feat.toString(), fv);

	/////////////////////////////////////////////////////////////
	prefix = "A"+prefix;

	// feature posL posL+1 posR-1 posR
	add(prefix+"1="+one+" "+rightOf1+" "+leftOf2+"*"+attachDistance, fv);

	feat = new StringBuilder(prefix+"1="+one+" "+rightOf1+" "+leftOf2);
	add(feat.toString(), fv);
	feat.append(' ').append(two);
	add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	add(feat.toString(), fv);

	feat = new StringBuilder(prefix+"2="+one+" "+rightOf1+" "+two);
	add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	add(feat.toString(), fv);
	
	feat = new StringBuilder(prefix+"3="+one+" "+leftOf2+" "+two);
	add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	add(feat.toString(), fv);
	
	feat = new StringBuilder(prefix+"4="+rightOf1+" "+leftOf2+" "+two);
	add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	add(feat.toString(), fv);

	///////////////////////////////////////////////////////////////
	prefix = "B"+prefix;

	//// feature posL-1 posL posR-1 posR
	feat = new StringBuilder(prefix+"1="+leftOf1+" "+one+" "+leftOf2+" "+two);
	add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	add(feat.toString(), fv);

	//// feature posL posL+1 posR posR+1
	feat = new StringBuilder(prefix+"2="+one+" "+rightOf1+" "+two+" "+rightOf2);
	add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	add(feat.toString(), fv);

    }



    /**
     * Add features for two items, each with two observations, e.g. head,
     * head pos, child, and child pos.
     *
     * The use of StringBuilders is not yet as efficient as it could
     * be, but this is a start. (And it abstracts the logic so we can
     * add other features more easily based on other items and
     * observations.)
     **/
    protected final void addTwoObsFeatures(String prefix, 
					 String item1F1, String item1F2, 
					 String item2F1, String item2F2, 
					 String attachDistance,
					 mstparser.FeatureVector fv) {

	StringBuilder feat = new StringBuilder(prefix+"2FF1="+item1F1);
	add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	add(feat.toString(), fv);
	
	feat = new StringBuilder(prefix+"2FF1="+item1F1+" "+item1F2);
	add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	add(feat.toString(), fv);

	feat = new StringBuilder(prefix+"2FF1="+item1F1+" "+item1F2+" "+item2F2);
	add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	add(feat.toString(), fv);

	feat = new StringBuilder(prefix+"2FF1="+item1F1+" "+item1F2+" "+item2F2+" "+item2F1);
	add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	add(feat.toString(), fv);
	
	feat = new StringBuilder(prefix+"2FF2="+item1F1+" "+item2F1);
	add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	add(feat.toString(), fv);

	feat = new StringBuilder(prefix+"2FF3="+item1F1+" "+item2F2);
	add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	add(feat.toString(), fv);


	feat = new StringBuilder(prefix+"2FF4="+item1F2+" "+item2F1);
	add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	add(feat.toString(), fv);

	feat = new StringBuilder(prefix+"2FF4="+item1F2+" "+item2F1+" "+item2F2);
	add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	add(feat.toString(), fv);

	feat = new StringBuilder(prefix+"2FF5="+item1F2+" "+item2F2);
	add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	add(feat.toString(), fv);

	feat = new StringBuilder(prefix+"2FF6="+item2F1+" "+item2F2);
	add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	add(feat.toString(), fv);

	feat = new StringBuilder(prefix+"2FF7="+item1F2);
	add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	add(feat.toString(), fv);
	
	feat = new StringBuilder(prefix+"2FF8="+item2F1);
	add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	add(feat.toString(), fv);
	
	feat = new StringBuilder(prefix+"2FF9="+item2F2);
	add(feat.toString(), fv);
	feat.append('*').append(attachDistance);
	add(feat.toString(), fv);
	
    }

    protected void addLabeledFeatures(mstparser.DependencyInstance instance,
				   int word,
				   String type,
				   boolean attR,
				   boolean childFeatures,
				   mstparser.FeatureVector fv) {
		
	String[] forms = instance.forms();
	String[] pos = instance.postags();
	    
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

	add("NTS1="+type+"&"+att,fv);
	add("ANTS1="+type,fv);
	for(int i = 0; i < 2; i++) {
	    String suff = i < 1 ? "&"+att : "";
	    suff = "&"+type+suff;

	    add("NTH="+w+" "+wP+suff,fv);
	    add("NTI="+wP+suff,fv);
	    add("NTIA="+wPm1+" "+wP+suff,fv);
	    add("NTIB="+wP+" "+wPp1+suff,fv);
	    add("NTIC="+wPm1+" "+wP+" "+wPp1+suff,fv);
	    add("NTJ="+w+suff,fv); //this

	}
    }


    protected void addDiscourseFeatures (mstparser.DependencyInstance instance, 
				       int small,
				       int large,
				       int headIndex,
				       int childIndex,
				       String attDist,
				       mstparser.FeatureVector fv) {
    
	addLinearFeatures("FORM", instance.forms(), small, large, attDist, fv);
	addLinearFeatures("LEMMA", instance.lemmas(), small, large, attDist, fv);
	
	addTwoObsFeatures("HCB1", instance.forms()[headIndex], 
			  instance.lemmas()[headIndex],
			  instance.forms()[childIndex], 
			  instance.lemmas()[childIndex], 
			  attDist, fv);
	
	addTwoObsFeatures("HCB2", instance.forms()[headIndex], 
			  instance.lemmas()[headIndex],
			  instance.forms()[childIndex], 
			  instance.postags()[childIndex], 
			  attDist, fv);
	
	addTwoObsFeatures("HCB3", instance.forms()[headIndex], 
			  instance.lemmas()[headIndex],
			  instance.forms()[childIndex], 
			  instance.cpostags()[childIndex], 
			  attDist, fv);
	
	addTwoObsFeatures("HC2", instance.forms()[headIndex], 
			  instance.postags()[headIndex], 
			  instance.forms()[childIndex], 
			  instance.cpostags()[childIndex], attDist, fv);
	
	addTwoObsFeatures("HCC2", instance.lemmas()[headIndex], 
			  instance.postags()[headIndex], 
			  instance.lemmas()[childIndex], 
			  instance.cpostags()[childIndex], 
			  attDist, fv);
	
	
	//// Use this if your extra feature lists all have the same length.
	for (int i=0; i<instance.feats().length; i++) {
	
		addLinearFeatures("F"+i, instance.feats()[i], small, large, attDist, fv);
	
		addTwoObsFeatures("FF"+i, 
				  instance.forms()[headIndex], 
				  instance.feats()[i][headIndex],
				  instance.forms()[childIndex], 
				  instance.feats()[i][childIndex],
				  attDist, fv);
		
		addTwoObsFeatures("LF"+i, 
				  instance.lemmas()[headIndex], 
				  instance.feats()[i][headIndex],
				  instance.lemmas()[childIndex], 
				  instance.feats()[i][childIndex],
				  attDist, fv);
		
		addTwoObsFeatures("PF"+i, 
				  instance.postags()[headIndex], 
				  instance.feats()[i][headIndex],
				  instance.postags()[childIndex], 
				  instance.feats()[i][childIndex],
				  attDist, fv);
		
		addTwoObsFeatures("CPF"+i, 
				  instance.cpostags()[headIndex], 
				  instance.feats()[i][headIndex],
				  instance.cpostags()[childIndex], 
				  instance.feats()[i][childIndex],
				  attDist, fv);
		
		
		for (int j=i+1; j<instance.feats().length; j++) {
		
		    addTwoObsFeatures("CPF"+i+"_"+j, 
				      instance.feats()[i][headIndex],
				      instance.feats()[j][headIndex],
				      instance.feats()[i][childIndex],
				      instance.feats()[j][childIndex],
				      attDist, fv);
		
		}
	
		for (int j=0; j<instance.feats().length; j++) {
	
		    addTwoObsFeatures("XFF"+i+"_"+j, 
				      instance.forms()[headIndex],
				      instance.feats()[i][headIndex],
				      instance.forms()[childIndex],
				      instance.feats()[j][childIndex],
				      attDist, fv);
	
		    addTwoObsFeatures("XLF"+i+"_"+j, 
				      instance.lemmas()[headIndex],
				      instance.feats()[i][headIndex],
				      instance.lemmas()[childIndex],
				      instance.feats()[j][childIndex],
				      attDist, fv);
	
		    addTwoObsFeatures("XPF"+i+"_"+j, 
				      instance.postags()[headIndex],
				      instance.feats()[i][headIndex],
				      instance.postags()[childIndex],
				      instance.feats()[j][childIndex],
				      attDist, fv);
	
	
		    addTwoObsFeatures("XCF"+i+"_"+j, 
				      instance.cpostags()[headIndex],
				      instance.feats()[i][headIndex],
				      instance.cpostags()[childIndex],
				      instance.feats()[j][childIndex],
				      attDist, fv);
	
	
		}
	
	}


	// Test out relational features
	if (this.getOptions().useRelationalFeatures) {

	    //for (int rf_index=0; rf_index<2; rf_index++) {
	    for (int rf_index=0; 
		 rf_index<instance.relFeats().length; 
		 rf_index++) {
		
		String headToChild = 
		    "H2C"+rf_index+instance.relFeats()[rf_index].getFeature(headIndex, childIndex);
	    
		addTwoObsFeatures("RFA1",
				  instance.forms()[headIndex], 
				  instance.lemmas()[headIndex],
				  instance.postags()[childIndex],
				  headToChild,
				  attDist, fv);
		
		addTwoObsFeatures("RFA2",
				  instance.postags()[headIndex], 
				  instance.cpostags()[headIndex],
				  instance.forms()[childIndex],
				  headToChild,
				  attDist, fv);
	    
	    	addTwoObsFeatures("RFA3",
				  instance.lemmas()[headIndex], 
				  instance.postags()[headIndex],
				  instance.forms()[childIndex],
				  headToChild,
				  attDist, fv);
	    	
	    	addTwoObsFeatures("RFB1",
				  headToChild,
				  instance.postags()[headIndex],
				  instance.forms()[childIndex], 
				  instance.lemmas()[childIndex],
				  attDist, fv);
	    	
	    	addTwoObsFeatures("RFB2",
				  headToChild,
				  instance.forms()[headIndex],
				  instance.postags()[childIndex], 
				  instance.cpostags()[childIndex],
				  attDist, fv);
	    	
	    	addTwoObsFeatures("RFB3",
				  headToChild,
				  instance.forms()[headIndex],
				  instance.lemmas()[childIndex], 
				  instance.postags()[childIndex],
				  attDist, fv);
		
	    }
	}
    }
		
    /**
     * Get features for stems the old way. The only way this differs
     * from calling addTwoObsFeatures() is that it checks the
     * lengths of the full lexical items are greater than 5 before
     * adding features.
     *
     */
    protected final void
	addOldMSTStemFeatures(String hLemma, String headP, 
			      String cLemma, String childP, String attDist, 
			      int hL, int cL, mstparser.FeatureVector fv) {

	String all = hLemma + " " + headP + " " + cLemma + " " + childP;
	String hPos = headP + " " + cLemma + " " + childP;
	String cPos = hLemma + " " + headP + " " + childP;
	String hP = headP + " " + cLemma;
	String cP = hLemma + " " + childP;
	String oPos = headP + " " + childP;
	String oLex = hLemma + " " + cLemma;
	
	add("SA="+all+attDist,fv); //this
	add("SF="+oLex+attDist,fv); //this
	add("SAA="+all,fv); //this
	add("SFF="+oLex,fv); //this
	
	if(cL > 5) {
	    add("SB="+hPos+attDist,fv);
	    add("SD="+hP+attDist,fv);
	    add("SK="+cLemma+" "+childP+attDist,fv);
	    add("SM="+cLemma+attDist,fv); //this
	    add("SBB="+hPos,fv);
	    add("SDD="+hP,fv);
	    add("SKK="+cLemma+" "+childP,fv);
	    add("SMM="+cLemma,fv); //this
	}
	if(hL > 5) {
	    add("SC="+cPos+attDist,fv);
	    add("SE="+cP+attDist,fv);
	    add("SH="+hLemma+" "+headP+attDist,fv);
	    add("SJ="+hLemma+attDist,fv); //this
	    
	    add("SCC="+cPos,fv);
	    add("SEE="+cP,fv);
	    add("SHH="+hLemma+" "+headP,fv);
	    add("SJJ="+hLemma,fv); //this
	}

    }		
}

