package mstparser.old;

import mstparser.ParseForestItem;

public class KBestParseForest2O {

    protected ParseForestItem[][][][][] chart;
    private String[] sent,pos;
    protected int start,end;
    private int K;
	
    public KBestParseForest2O(int start, int end, mstparser.DependencyInstance inst, int K) {
	this.K = K;
	chart = new ParseForestItem[end+1][end+1][2][3][K];
	this.start = start;
	this.end = end;
	this.sent = inst.forms;
	this.pos = inst.postags;
    }

    public boolean add(int s, int type, int dir, double score, mstparser.FeatureVector fv) {
				
	boolean added = false;

	if(chart[s][s][dir][0][0] == null) {
	    for(int i = 0; i < K; i++)
		chart[s][s][dir][0][i] = new ParseForestItem(s,type,dir,Double.NEGATIVE_INFINITY,null);
	}
		
	if(chart[s][s][dir][0][K-1].prob > score)
	    return false;

	for(int i = 0; i < K; i++) {
	    if(chart[s][s][dir][0][i].prob < score) {
		ParseForestItem tmp = chart[s][s][dir][0][i];
		chart[s][s][dir][0][i] = new ParseForestItem(s,type,dir,score,fv);
		for(int j = i+1; j < K && tmp.prob != Double.NEGATIVE_INFINITY; j++) {
		    ParseForestItem tmp1 = chart[s][s][dir][0][j];
		    chart[s][s][dir][0][j] = tmp;
		    tmp = tmp1;
		}
		added = true;
		break;
	    }
	}

	return added;
    }

    public boolean add(int s, int r, int t, int type,
		       int dir, int comp, double score,
		       mstparser.FeatureVector fv,
		       ParseForestItem p1, ParseForestItem p2) {
		
	boolean added = false;

	if(chart[s][t][dir][comp][0] == null) {
	    for(int i = 0; i < K; i++)
		chart[s][t][dir][comp][i] =
		    new ParseForestItem(s,r,t,type,dir,comp,Double.NEGATIVE_INFINITY,null,null,null);
	}

	if(chart[s][t][dir][comp][K-1].prob > score)
	    return false;
		
	for(int i = 0; i < K; i++) {
	    if(chart[s][t][dir][comp][i].prob < score) {
		ParseForestItem tmp = chart[s][t][dir][comp][i];
		chart[s][t][dir][comp][i] = new ParseForestItem(s,r,t,type,dir,comp,score,fv,p1,p2);
		for(int j = i+1; j < K && tmp.prob != Double.NEGATIVE_INFINITY; j++) {
		    ParseForestItem tmp1 = chart[s][t][dir][comp][j];
		    chart[s][t][dir][comp][j] = tmp;
		    tmp = tmp1;
		}
		added = true;
		break;
	    }

	}

	return added;
		
    }

    public double getProb(int s, int t, int dir, int comp) {
	return getProb(s,t,dir,comp,0);
    }

    public double getProb(int s, int t, int dir, int comp, int i) {
	if(chart[s][t][dir][comp][i] != null)
	    return chart[s][t][dir][comp][i].prob;
	return Double.NEGATIVE_INFINITY;
    }

    public double[] getProbs(int s, int t, int dir, int comp) {
	double[] result = new double[K];
	for(int i = 0; i < K; i++)
	    result[i] =
		chart[s][t][dir][comp][i] != null ? chart[s][t][dir][comp][i].prob : Double.NEGATIVE_INFINITY;
	return result;
    }

    public mstparser.FeatureVector getFeatureVector(ParseForestItem pfi) {
	if(pfi.left == null)
	    return pfi.fv;

	return cat(pfi.fv,cat(getFeatureVector(pfi.left),getFeatureVector(pfi.right)));
    }

    public String getDepString(ParseForestItem pfi) {
	if(pfi.left == null)
	    return "";

	if(pfi.dir == 0 && pfi.comp == 1)
	    return ((getDepString(pfi.left)+" "+getDepString(pfi.right)).trim()+" "+pfi.s+"|"+pfi.t+":"+pfi.type).trim();
	else if(pfi.dir == 1 && pfi.comp == 1)
	    return (pfi.t+"|"+pfi.s+":"+pfi.type+" "+(getDepString(pfi.left)+" "+getDepString(pfi.right)).trim()).trim();
	return (getDepString(pfi.left) + " " + getDepString(pfi.right)).trim();
    }
	
    public mstparser.FeatureVector cat(mstparser.FeatureVector fv1, mstparser.FeatureVector fv2) {
	return fv1.cat(fv2);
    }
}

