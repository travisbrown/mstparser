package mstparser;

import java.io.*;
import gnu.trove.*;
import java.util.*;
import mstparser.io.*;

public class DependencyPipe2O extends DependencyPipe {

    public DependencyPipe2O() throws IOException {
	super();
    }

    public DependencyPipe2O(boolean createForest, DependencyReader dr) throws IOException {
	super(createForest, dr);
    }

    public FeatureVector createFeatureVector(DependencyInstance depinst,
					     int par,
					     int ch1, int ch2,
					     FeatureVector fv) {

	String[] toks = depinst.get("tokens");
	String[] pos = depinst.get("pos");
	String[] labs = depinst.get("labels");
	String[] posA = depinst.get("posA");

		
	// ch1 is always the closes to par
	String dir = par > ch2 ? "RA" : "LA";
		
	String par_pos = pos[par];
	String ch1_pos = ch1 == par ? "STPOS" : pos[ch1];
	String ch2_pos = pos[ch2];
	String ch1_word = ch1 == par ? "STWRD" : toks[ch1];
	String ch2_word = toks[ch2];

	String pTrip = par_pos+"_"+ch1_pos+"_"+ch2_pos;
	fv = add("POS_TRIP="+pTrip+"_"+dir,1.0,fv);
	fv = add("APOS_TRIP="+pTrip,1.0,fv);
		
	return fv;
    }
	
    public FeatureVector createFeatureVectorSib(DependencyInstance depinst,
						int ch1, int ch2,
						boolean isST,
						FeatureVector fv) {

	String[] toks = depinst.get("tokens");
	String[] pos = depinst.get("pos");
		
	// ch1 is always the closes to par
	String dir = ch1 > ch2 ? "RA" : "LA";
		
	String ch1_pos = isST ? "STPOS" : pos[ch1];
	String ch2_pos = pos[ch2];
	String ch1_word = isST ? "STWRD" : toks[ch1];
	String ch2_word = toks[ch2];

	fv = add("CH_PAIR="+ch1_pos+"_"+ch2_pos+"_"+dir,1.0,fv);
	fv = add("CH_WPAIR="+ch1_word+"_"+ch2_word+"_"+dir,1.0,fv);
	fv = add("CH_WPAIRA="+ch1_word+"_"+ch2_pos+"_"+dir,1.0,fv);
	fv = add("CH_WPAIRB="+ch1_pos+"_"+ch2_word+"_"+dir,1.0,fv);
	fv = add("ACH_PAIR="+ch1_pos+"_"+ch2_pos,1.0,fv);
	fv = add("ACH_WPAIR="+ch1_word+"_"+ch2_word,1.0,fv);
	fv = add("ACH_WPAIRA="+ch1_word+"_"+ch2_pos,1.0,fv);
	fv = add("ACH_WPAIRB="+ch1_pos+"_"+ch2_word,1.0,fv);

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
	fv = add("SIB_PAIR_DIST="+distBool+"_"+dir,1.0,fv);
	fv = add("ASIB_PAIR_DIST="+distBool,1.0,fv);
	fv = add("CH_PAIR_DIST="+ch1_pos+"_"+ch2_pos+"_"+distBool+"_"+dir,1.0,fv);
	fv = add("ACH_PAIR_DIST="+ch1_pos+"_"+ch2_pos+"_"+distBool,1.0,fv);
				
		
	return fv;
    }
			
    public FeatureVector createFeatureVector(DependencyInstance depinst,
					     int[] labs1,
					     int[] deps) {

	String[] labs = new String[labs1.length];
	for(int i = 0; i < labs.length; i++)
	    labs[i] = types[labs1[i]];

	depinst.put("labels", labs);

	return createFeatureVector(depinst, deps);
	    
    }

    public FeatureVector createFeatureVector(DependencyInstance depinst,
					     int[] deps) {
	    
	String[] toks = depinst.get("tokens");
	String[] pos = depinst.get("pos");
	String[] labs = depinst.get("labels");

	String[] posA = new String[pos.length];
	for(int i = 0; i < pos.length; i++) {
	    posA[i] = pos[i].substring(0,1);
	}

	depinst.put("posA", posA);

	FeatureVector fv = new FeatureVector(-1,-1.0,null);
	for(int i = 0; i < toks.length; i++) {
	    if(deps[i] == -1)
		continue;
	    int small = i < deps[i] ? i : deps[i];
	    int large = i > deps[i] ? i : deps[i];
	    boolean attR = i < deps[i] ? false : true;
	    fv = createFeatureVector(depinst,small,large,attR,fv);
	    if(labeled) {
		fv = createFeatureVector(depinst,i,labs[i],attR,true,fv);
		fv = createFeatureVector(depinst,deps[i],labs[i],attR,false,fv);
	    }
	}
	// find all trip features
	for(int i = 0; i < toks.length; i++) {
	    if(deps[i] == -1 && i != 0) continue;
	    // right children
	    int prev = i;
	    for(int j = i+1; j < toks.length; j++) {
		if(deps[j] == i) {
		    fv = createFeatureVector(depinst,i,prev,j,fv);
		    fv = createFeatureVectorSib(depinst,prev,j,prev==i,fv);
		    prev = j;
		}
	    }
	    prev = i;
	    for(int j = i-1; j >= 0; j--) {
		if(deps[j] == i) {
		    fv = createFeatureVector(depinst,i,prev,j,fv);
		    fv = createFeatureVectorSib(depinst,prev,j,prev==i,fv);
		    prev = j;
		}
	    }
	}
		
	return fv;
    }

    /*
      public FeatureVector createFeatureVector(int[] deps,
      NewFeatureVector[][][] fvs,
      NewFeatureVector[][][] fvs_trips,
      NewFeatureVector[][][] fvs_sibs,
      NewFeatureVector[][][] fvs_gp) {

      FeatureVector fv = new FeatureVector(-1,-1.0,null);
      for(int i = 0; i < deps.length; i++) {
      if(deps[i] == -1)
      continue;
      int small = i < deps[i] ? i : deps[i];
      int large = i > deps[i] ? i : deps[i];
      int attR = i < deps[i] ? 1 : 0;
      fv = NewFeatureVector.cat(fvs[small][large][attR],fv);
      }
      // find all trip features
      for(int i = 0; i < deps.length; i++) {
      // right children
      if(deps[i] == -1 && i != 0) continue;
      int prev = i;
      for(int j = i+1; j < deps.length; j++) {
      if(deps[j] == i) {
      fv = NewFeatureVector.cat(fvs_trips[i][prev][j],fv);
      fv = NewFeatureVector.cat(fvs_sibs[prev][j][prev == i ? 0 : 1],fv);
      prev = j;
      }
      }
      prev = i;
      for(int j = i-1; j >= 0; j--) {
      if(deps[j] == i) {
      fv = NewFeatureVector.cat(fvs_trips[i][prev][j],fv);
      fv = NewFeatureVector.cat(fvs_sibs[prev][j][prev == i ? 0 : 1],fv);
      prev = j;
      }
      }
      }
		
      return fv;
      }
    */

    public void possibleFeatures(DependencyInstance depinst, ObjectOutputStream out) {

	String[] toks = depinst.get("tokens");
	String[] pos = depinst.get("pos");
	String[] labs = depinst.get("labels");
		
	String[] posA = new String[pos.length];
	for(int i = 0; i < pos.length; i++) {
	    posA[i] = pos[i].substring(0,1);
	}

	depinst.put("posA", posA);

	try {

	    for(int w1 = 0; w1 < toks.length; w1++) {
		for(int w2 = w1+1; w2 < toks.length; w2++) {
					
		    for(int ph = 0; ph < 2; ph++) {						
			boolean attR = ph == 0 ? true : false;
							
			FeatureVector prodFV = createFeatureVector(depinst,w1,w2,attR,
								   new FeatureVector(-1,-1.0,null));
						
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
		for(int w1 = 0; w1 < toks.length; w1++) {
		    
		    for(int t = 0; t < types.length; t++) {
			String type = types[t];
			
			for(int ph = 0; ph < 2; ph++) {						
			    boolean attR = ph == 0 ? true : false;
			    
			    for(int ch = 0; ch < 2; ch++) {						
				boolean child = ch == 0 ? true : false;						
				
				FeatureVector prodFV = createFeatureVector(depinst,w1,
									   type,
									   attR,child,
									   new FeatureVector(-1,-1.0,null));
				
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

	    for(int w1 = 0; w1 < toks.length; w1++) {
		for(int w2 = w1; w2 < toks.length; w2++) {
		    for(int w3 = w2+1; w3 < toks.length; w3++) {
			FeatureVector prodFV = createFeatureVector(depinst,w1,w2,w3,
								   new FeatureVector(-1,-1.0,null));
			for(FeatureVector curr = prodFV; curr != null; curr = curr.next) {
			    if(curr.index >= 0)
				out.writeInt(curr.index);
			}
			out.writeInt(-2);
		    }
		}
		for(int w2 = w1; w2 >= 0; w2--) {
		    for(int w3 = w2-1; w3 >= 0; w3--) {
			FeatureVector prodFV = createFeatureVector(depinst,w1,w2,w3,
								   new FeatureVector(-1,-1.0,null));
			for(FeatureVector curr = prodFV; curr != null; curr = curr.next) {
			    if(curr.index >= 0)
				out.writeInt(curr.index);
			}
			out.writeInt(-2);
		    }
		}
	    }
			
	    out.writeInt(-3);
			
	    for(int w1 = 0; w1 < toks.length; w1++) {
		for(int w2 = 0; w2 < toks.length; w2++) {
		    for(int wh = 0; wh < 2; wh++) {
			if(w1 != w2) {
			    FeatureVector prodFV = createFeatureVectorSib(depinst,w1,w2,wh == 0,
									  new FeatureVector(-1,-1.0,null));
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
						
	    for(FeatureVector curr = depinst.fv; curr.next != null; curr = curr.next)
		out.writeInt(curr.index);

	    String[] keysToDI = depinst.keys();
	    for (int i=0; i<keysToDI.length; i++) {
		out.writeInt(-4);
		out.writeObject(keysToDI[i]);
		out.writeObject(depinst.get(keysToDI[i]));
	    }

	    out.writeInt(-5);
	    out.writeObject(depinst.actParseTree);
			
	    out.writeInt(-1);
	    out.reset();

	} catch (IOException e) {}
		
    }

    public DependencyInstance getFeatureVector(ObjectInputStream in,
					       DependencyInstance depinst,
					       FeatureVector[][][] fvs,
					       double[][][] probs,
					       FeatureVector[][][] fvs_trips,
					       double[][][] probs_trips,
					       FeatureVector[][][] fvs_sibs,
					       double[][][] probs_sibs,
					       FeatureVector[][][][] nt_fvs,
					       double[][][][] nt_probs,
					       Parameters params) throws IOException {
	int length = depinst.length;
		
	// Get production crap.		
	for(int w1 = 0; w1 < length; w1++) {
	    for(int w2 = w1+1; w2 < length; w2++) {
				
		for(int ph = 0; ph < 2; ph++) {

		    FeatureVector prodFV = new FeatureVector(-1,-1.0,null);
					
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
			    
			    FeatureVector prodFV = new FeatureVector(-1,-1.0,null);
			    
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

	for(int w1 = 0; w1 < length; w1++) {
	    for(int w2 = w1; w2 < length; w2++) {
		for(int w3 = w2+1; w3 < length; w3++) {
		    FeatureVector prodFV = new FeatureVector(-1,-1.0,null);
		    
		    int indx = in.readInt();
		    while(indx != -2) {
			prodFV = new FeatureVector(indx,1.0,prodFV);
			indx = in.readInt();
		    }

		    double prodProb = params.getScore(prodFV);
		    fvs_trips[w1][w2][w3] = prodFV;
		    probs_trips[w1][w2][w3] = prodProb;
		    

		}
	    }
	    for(int w2 = w1; w2 >= 0; w2--) {
		for(int w3 = w2-1; w3 >= 0; w3--) {
		    FeatureVector prodFV = new FeatureVector(-1,-1.0,null);
		    
		    int indx = in.readInt();
		    while(indx != -2) {
			prodFV = new FeatureVector(indx,1.0,prodFV);
			indx = in.readInt();
		    }

		    double prodProb = params.getScore(prodFV);
		    fvs_trips[w1][w2][w3] = prodFV;
		    probs_trips[w1][w2][w3] = prodProb;

		}
	    }
	}
			
	last = in.readInt();
	if(last != -3) { System.out.println("Error reading file."); System.exit(0); }
			
	for(int w1 = 0; w1 < length; w1++) {
	    for(int w2 = 0; w2 < length; w2++) {
		for(int wh = 0; wh < 2; wh++) {
		    if(w1 != w2) {
			FeatureVector prodFV = new FeatureVector(-1,-1.0,null);
			
			int indx = in.readInt();
			while(indx != -2) {
			    prodFV = new FeatureVector(indx,1.0,prodFV);
			    indx = in.readInt();
			}

			double prodProb = params.getScore(prodFV);
			fvs_sibs[w1][w2][wh] = prodFV;
			probs_sibs[w1][w2][wh] = prodProb;

		    }
		}
	    }
	}

	last = in.readInt();
	if(last != -3) { System.out.println("Error reading file."); System.exit(0); }

	FeatureVector nfv = new FeatureVector(-1,-1.0,null);
	int next = in.readInt();
	while(next != -4) {
	    nfv = new FeatureVector(next,1.0,nfv);
	    next = in.readInt();
	}

	DependencyInstance marshalledDI = new DependencyInstance();
	marshalledDI.setFeatureVector(nfv);

	String[] toks = null;
	String[] pos = null;
	String[] labs = null;
	String actParseTree = null;
	try {
	    while (next != -5) {
		marshalledDI.put((String)in.readObject(), 
				 (String[])in.readObject());
		next = in.readInt();
	    }

	    marshalledDI.actParseTree = (String)in.readObject();
	    next = in.readInt();

	}
	catch(ClassNotFoundException e) { System.out.println("Error reading file."); System.exit(0); }
		
	if(next != -1) { System.out.println("Error reading file."); System.exit(0); }

	return marshalledDI;
		
    }
		
    public void getFeatureVector(DependencyInstance depinst,
				 FeatureVector[][][] fvs,
				 double[][][] probs,
				 FeatureVector[][][] fvs_trips,
				 double[][][] probs_trips,
				 FeatureVector[][][] fvs_sibs,
				 double[][][] probs_sibs,
				 FeatureVector[][][][] nt_fvs,
				 double[][][][] nt_probs, Parameters params) {

	String[] toks = depinst.get("tokens");
	String[] pos = depinst.get("pos");
	String[] labs = depinst.get("labels");
		
	String[] posA = new String[pos.length];
	for(int i = 0; i < pos.length; i++) {
	    posA[i] = pos[i].substring(0,1);
	}

	depinst.put("posA",posA);

	// Get production crap.		
	for(int w1 = 0; w1 < toks.length; w1++) {
	    for(int w2 = w1+1; w2 < toks.length; w2++) {
				
		for(int ph = 0; ph < 2; ph++) {
		    boolean attR = ph == 0 ? true : false;
		    
		    int childInt = attR ? w2 : w1;
		    int parInt = attR ? w1 : w2;
		    
		    FeatureVector prodFV = createFeatureVector(depinst,w1,w2,attR,
							       new FeatureVector(-1,-1.0,null));
										
		    double prodProb = params.getScore(prodFV);
		    fvs[w1][w2][ph] = prodFV;
		    probs[w1][w2][ph] = prodProb;
		}
	    }
			
	}

	if(labeled) {
	    for(int w1 = 0; w1 < toks.length; w1++) {
		
		for(int t = 0; t < types.length; t++) {
		    String type = types[t];
		    
		    for(int ph = 0; ph < 2; ph++) {						
			boolean attR = ph == 0 ? true : false;
			
			for(int ch = 0; ch < 2; ch++) {						
			    boolean child = ch == 0 ? true : false;						
			    
			    FeatureVector prodFV = createFeatureVector(depinst,w1,
								       type,attR,child,
								       new FeatureVector(-1,-1.0,null));
			    
			    double nt_prob = params.getScore(prodFV);
			    nt_fvs[w1][t][ph][ch] = prodFV;
			    nt_probs[w1][t][ph][ch] = nt_prob;
			    
			}
		    }
		}
		
	    }
	}

		
	for(int w1 = 0; w1 < toks.length; w1++) {
	    for(int w2 = w1; w2 < toks.length; w2++) {
		for(int w3 = w2+1; w3 < toks.length; w3++) {
		    FeatureVector prodFV = createFeatureVector(depinst,w1,w2,w3,
							       new FeatureVector(-1,-1.0,null));
		    double prodProb = params.getScore(prodFV);
		    fvs_trips[w1][w2][w3] = prodFV;
		    probs_trips[w1][w2][w3] = prodProb;
		}
	    }
	    for(int w2 = w1; w2 >= 0; w2--) {
		for(int w3 = w2-1; w3 >= 0; w3--) {
		    FeatureVector prodFV = createFeatureVector(depinst,w1,w2,w3,
							       new FeatureVector(-1,-1.0,null));
		    double prodProb = params.getScore(prodFV);
		    fvs_trips[w1][w2][w3] = prodFV;
		    probs_trips[w1][w2][w3] = prodProb;
		}
	    }
	}
			
	for(int w1 = 0; w1 < toks.length; w1++) {
	    for(int w2 = 0; w2 < toks.length; w2++) {
		for(int wh = 0; wh < 2; wh++) {
		    if(w1 != w2) {
			FeatureVector prodFV = createFeatureVectorSib(depinst,w1,w2,wh == 0,
								      new FeatureVector(-1,-1.0,null));
			double prodProb = params.getScore(prodFV);
			fvs_sibs[w1][w2][wh] = prodFV;
			probs_sibs[w1][w2][wh] = prodProb;
		    }
		}
	    }
	}
    }
		
}
