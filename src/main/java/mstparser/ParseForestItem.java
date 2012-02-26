package mstparser;

public class ParseForestItem {

    public int s,r,t,dir;
    private int comp,type;
    public double prob;
    public FeatureVector fv;
    public ParseForestItem left, right;


    // productions
    public ParseForestItem(int i, int k, int j, int type,
			   int dir, int comp,
			   double prob, FeatureVector fv,
			   ParseForestItem left, ParseForestItem right) {
	this.s = i;
	this.r = k;
	this.t = j;
	this.dir = dir;
	this.comp = comp;
	this.type = type;
		
	this.prob = prob;
	this.fv = fv;

	this.left = left;
	this.right = right;
		
    }

    // preproductions
    public ParseForestItem(int s, int type, int dir, double prob, FeatureVector fv) {
      this(s, 0, 0, type, dir, 0, prob, fv, null, null);
    }

    public int comp() { return this.comp; }
    public int label() { return this.type; }

    // way forest works, only have to check rule and indeces
    // for equality.
    public boolean equals(ParseForestItem p) {
	return s == p.s && t == p.t && r == p.r
	    && dir == p.dir && comp == p.comp
	    && type == p.type;
    }
}

