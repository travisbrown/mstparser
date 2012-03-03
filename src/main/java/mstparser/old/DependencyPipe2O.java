package mstparser.old;

import mstparser.DependencyInstance;
import mstparser.FeatureVector;
import mstparser.Parameters;

import java.io.IOException;
//import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

public abstract class DependencyPipe2O extends mstparser.DependencyPipe {
  public DependencyPipe2O(mstparser.ParserOptions options) throws IOException {
    super(options);
  }

    public void fillFeatureVectors(DependencyInstance instance,
				   FeatureVector[][][] fvs,
				   double[][][] probs,
				   FeatureVector[][][] fvs_trips,
				   double[][][] probs_trips,
				   FeatureVector[][][] fvs_sibs,
				   double[][][] probs_sibs,
				   FeatureVector[][][][] nt_fvs,
				   double[][][][] nt_probs, Parameters params) {

	fillFeatureVectors(instance, fvs, probs, nt_fvs, nt_probs, params);

	final int instanceLength = instance.length();

	for(int w1 = 0; w1 < instanceLength; w1++) {
	    for(int w2 = w1; w2 < instanceLength; w2++) {
		for(int w3 = w2+1; w3 < instanceLength; w3++) {
		    FeatureVector prodFV = new FeatureVector();
		    addTripFeatures(instance,w1,w2,w3,prodFV);
		    double prodProb = params.getScore(prodFV);
		    fvs_trips[w1][w2][w3] = prodFV;
		    probs_trips[w1][w2][w3] = prodProb;
		}
	    }
	    for(int w2 = w1; w2 >= 0; w2--) {
		for(int w3 = w2-1; w3 >= 0; w3--) {
		    FeatureVector prodFV = new FeatureVector();
		    addTripFeatures(instance,w1,w2,w3,prodFV);
		    double prodProb = params.getScore(prodFV);
		    fvs_trips[w1][w2][w3] = prodFV;
		    probs_trips[w1][w2][w3] = prodProb;
		}
	    }
	}
			
	for(int w1 = 0; w1 < instanceLength; w1++) {
	    for(int w2 = 0; w2 < instanceLength; w2++) {
		for(int wh = 0; wh < 2; wh++) {
		    if(w1 != w2) {
			FeatureVector prodFV = new FeatureVector();
			addSiblingFeatures(instance,w1,w2,wh == 0,prodFV);
			double prodProb = params.getScore(prodFV);
			fvs_sibs[w1][w2][wh] = prodFV;
			probs_sibs[w1][w2][wh] = prodProb;
		    }
		}
	    }
	}
    }

    protected final void addSiblingFeatures(DependencyInstance instance,
					  int ch1, int ch2,
					  boolean isST,
					  FeatureVector fv) {

	String[] forms = instance.forms();
	String[] pos = instance.postags();
		
	// ch1 is always the closes to par
	String dir = ch1 > ch2 ? "RA" : "LA";
		
	String ch1_pos = isST ? "STPOS" : pos[ch1];
	String ch2_pos = pos[ch2];
	String ch1_word = isST ? "STWRD" : forms[ch1];
	String ch2_word = forms[ch2];

	add("CH_PAIR="+ch1_pos+"_"+ch2_pos+"_"+dir,1.0,fv);
	add("CH_WPAIR="+ch1_word+"_"+ch2_word+"_"+dir,1.0,fv);
	add("CH_WPAIRA="+ch1_word+"_"+ch2_pos+"_"+dir,1.0,fv);
	add("CH_WPAIRB="+ch1_pos+"_"+ch2_word+"_"+dir,1.0,fv);
	add("ACH_PAIR="+ch1_pos+"_"+ch2_pos,1.0,fv);
	add("ACH_WPAIR="+ch1_word+"_"+ch2_word,1.0,fv);
	add("ACH_WPAIRA="+ch1_word+"_"+ch2_pos,1.0,fv);
	add("ACH_WPAIRB="+ch1_pos+"_"+ch2_word,1.0,fv);

	int dist = Math.max(ch1,ch2)-Math.min(ch1,ch2);
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
	add("SIB_PAIR_DIST="+distBool+"_"+dir,1.0,fv);
	add("ASIB_PAIR_DIST="+distBool,1.0,fv);
	add("CH_PAIR_DIST="+ch1_pos+"_"+ch2_pos+"_"+distBool+"_"+dir,1.0,fv);
	add("ACH_PAIR_DIST="+ch1_pos+"_"+ch2_pos+"_"+distBool,1.0,fv);
				
    }

    protected abstract void addTripFeatures(DependencyInstance instance,
				       int par,
				       int ch1, int ch2,
				       FeatureVector fv);
}

