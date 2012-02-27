package mstparser

import scala.collection.mutable.PriorityQueue

class KBestParseForest(private val end: Int, private val k: Int) {
  private val chart = Array.ofDim[ParseForestItem](this.end + 1, this.end + 1, 2, 2, this.k)

  def getItem(s: Int, t: Int, d: Int, c: Int): ParseForestItem = this.getItem(s, t, d, c, 0)
  def getItem(s: Int, t: Int, d: Int, c: Int, k: Int): ParseForestItem = this.chart(s)(t)(d)(c)(k)
  def getItems(s: Int, t: Int, d: Int, c: Int) =
    if (this.chart(s)(t)(d)(c)(0) != null) this.chart(s)(t)(d)(c) else null

  def getBestParses: Array[(FeatureVector, String)] =
    this.chart(0)(this.end)(0)(0).map { item =>
      if (item.prob > Double.NegativeInfinity)
        (item.getFeatureVector, item.getDepString)
      else (null, null)
    }

  def getProb(s: Int, t: Int, d: Int, c: Int): Double = this.getProb(s, t, d, c, 0)
  def getProb(s: Int, t: Int, d: Int, c: Int, i: Int): Double =
    Option(this.chart(s)(t)(d)(c)(i)).map(_.prob).getOrElse(Double.NegativeInfinity)

  def getKBestPairs(is: Array[ParseForestItem], js: Array[ParseForestItem]): Array[(java.lang.Integer, java.lang.Integer)] = {
    val result = Array.fill[(java.lang.Integer, java.lang.Integer)](this.k)(-1, -1)

    if (is != null && js != null && is(0) != null && js(0) != null) {
      val heap = PriorityQueue((is(0).prob + js(0).prob, (0, 0)))
      val beenPushed = Array.ofDim[Boolean](this.k, this.k)
      beenPushed(0)(0) = true

      var n = 0

      while (n < this.k && heap.head._1 > Double.NegativeInfinity) {
        val (v, (i, j)) = heap.dequeue

        result(n) = (i, j)
        n += 1

        if (n < this.k) {
          if (!beenPushed(i + 1)(j)) {
            heap += ((is(i + 1).prob + js(j).prob, (i + 1, j)))
            beenPushed(i + 1)(j) = true
          }

          if (!beenPushed(i)(j + 1)) {
            heap += ((is(i).prob + js(j + 1).prob, (i, j + 1)))
            beenPushed(i)(j + 1) = true
          }
        }
      }
    }

    result
  }

  def add(s: Int, label: Int, d: Int, score: Double, fv: FeatureVector) = {
    if (this.chart(s)(s)(d)(0)(0) == null) {
      this.chart(s)(s)(d)(0) = Array.fill(k)(new ParseForestItem(s, label, d))
    }

    if (this.chart(s)(s)(d)(0)(k - 1).prob > score) false
    else {
      var added = false
      var i = 0

      while (!added && i < this.k) {
        if (this.chart(s)(s)(d)(0)(i).prob < score) {
          var tmp = this.chart(s)(s)(d)(0)(i)
          this.chart(s)(s)(d)(0)(i) = new ParseForestItem(s, label, d, score, fv)

          var j = i + 1
          while (j < this.k && tmp.prob > Double.NegativeInfinity) {
		        val tmp1 = this.chart(s)(s)(d)(0)(j)
            this.chart(s)(s)(d)(0)(j) = tmp
            tmp = tmp1
            j += 1
          }
		      added = true
	      }
        i += 1
	    }

      added
    }
  }

  def add(s: Int, r: Int, t: Int, label: Int, d: Int, c: Int,
    score: Double, fv: FeatureVector, p: ParseForestItem, q: ParseForestItem) = {
    if (this.chart(s)(t)(d)(c)(0) == null) {
      this.chart(s)(t)(d)(c) = Array.fill(k)(new ParseForestItem(s, r, t, label, d, c))
    }

    if (this.chart(s)(t)(d)(c)(k - 1).prob > score) false
    else {
      var added = false
      var i = 0

      while (!added && i < this.k) {
        if (this.chart(s)(t)(d)(c)(i).prob < score) {
          var tmp = this.chart(s)(t)(d)(c)(i)
          this.chart(s)(t)(d)(c)(i) = new ParseForestItem(s, r, t, label, d, c, score, fv, Some(p, q))

          var j = i + 1
          while (j < this.k && tmp.prob > Double.NegativeInfinity) {
		        val tmp1 = this.chart(s)(t)(d)(c)(j)
            this.chart(s)(t)(d)(c)(j) = tmp
            tmp = tmp1
            j += 1
          }
		      added = true
	      }
        i += 1
	    }

      added
    }
  }
}

/*
	
    public ParseForestItem[][][][][] chart;
    private String[] sent,pos;
    private int start,end;
    private int K;
	
    public KBestParseForest(int start, int end, DependencyInstance inst, int K) {
	this.K = K;
	chart = new ParseForestItem[end+1][end+1][2][2][K];
	this.start = start;
	this.end = end;
	this.sent = inst.forms;
	this.pos = inst.postags;
    }
	
    public boolean add(int s, int type, int dir, double score, FeatureVector fv) {

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
		       FeatureVector fv,
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
		chart[s][t][dir][comp][i] =
		    new ParseForestItem(s,r,t,type,dir,comp,score,fv,p1,p2);
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
}	
*/

