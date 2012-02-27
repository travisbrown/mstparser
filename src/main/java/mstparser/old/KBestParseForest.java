package mstparser.old;

import mstparser.ParseForestItem;

import scala.Tuple2;
import scala.Some;

public class KBestParseForest {

    protected ParseForestItem[][][][][] chart;
    private String[] sent,pos;
    protected int start,end;
    private int K;
	
    public KBestParseForest(int start, int end, mstparser.DependencyInstance inst, int K) {
	this.K = K;
	chart = new ParseForestItem[end+1][end+1][2][2][K];
	//chart = new ParseForestItem[end+1][end+1][2][3][K];
	this.start = start;
	this.end = end;
	this.sent = inst.forms;
	this.pos = inst.postags;
    }

    public boolean add(int s, int type, int dir, double score, mstparser.FeatureVector fv) {

	boolean added = false;
		
	if(chart[s][s][dir][0][0] == null) {
	    for(int i = 0; i < K; i++)
		chart[s][s][dir][0][i] = new ParseForestItem(s,type,dir);
	}
		
	if(chart[s][s][dir][0][K-1].prob() > score)
	    return false;

	for(int i = 0; i < K; i++) {
	    if(chart[s][s][dir][0][i].prob() < score) {
		ParseForestItem tmp = chart[s][s][dir][0][i];
		chart[s][s][dir][0][i] = new ParseForestItem(s,type,dir,score,fv);
		for(int j = i+1; j < K && tmp.prob() != Double.NEGATIVE_INFINITY; j++) {
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
		    new ParseForestItem(s,r,t,type,dir,comp);
	}

	if(chart[s][t][dir][comp][K-1].prob() > score)
	    return false;
		
	for(int i = 0; i < K; i++) {
	    if(chart[s][t][dir][comp][i].prob() < score) {
		ParseForestItem tmp = chart[s][t][dir][comp][i];
		chart[s][t][dir][comp][i] =
		    new ParseForestItem(s,r,t,type,dir,comp,score,fv,new Some(new Tuple2(p1,p2)));
		for(int j = i+1; j < K && tmp.prob() != Double.NEGATIVE_INFINITY; j++) {
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
}
