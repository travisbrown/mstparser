package mstparser

import scala.collection.mutable.PriorityQueue

class KBestParseForest(start: Int, end: Int, instance: DependencyInstance, k: Int)
  extends old.KBestParseForest(start, end, instance, k) {

  def getItem(s: Int, t: Int, d: Int, c: Int): ParseForestItem = this.getItem(s, t, d, c, 0)
  def getItem(s: Int, t: Int, d: Int, c: Int, k: Int): ParseForestItem = this.chart(s)(t)(d)(c)(k)
  def getItems(s: Int, t: Int, d: Int, c: Int) =
    if (this.chart(s)(t)(d)(c)(0) != null) this.chart(s)(t)(d)(c) else null

  def getBestParses: Array[(FeatureVector, String)] =
    this.chart(0)(this.end)(0)(0).map { item =>
      if (item.prob > Double.NegativeInfinity)
        (this.getFeatureVector(item), this.getDepString(item))
      else (null, null)
    }

  def getProb(s: Int, t: Int, d: Int, c: Int): Double = this.getProb(s, t, d, c, 0)
  def getProb(s: Int, t: Int, d: Int, c: Int, i: Int): Double =
    Option(this.chart(s)(t)(d)(c)(i)).map(_.prob).getOrElse(Double.NegativeInfinity)


  def getFeatureVector(item: ParseForestItem): FeatureVector =
    Option(item.left).map(left => item.fv.cat(this.getFeatureVector(left).cat(this.getFeatureVector(item.right)))).getOrElse(item.fv)

  def getDepString(item: ParseForestItem): String = Option(item.left).map { left =>
    val ld = this.getDepString(left)
    val rd = this.getDepString(item.right)
    val cs = (ld + " " + rd).trim

    if (item.comp == 0) cs
    else if (item.dir == 0) "%s %d|%d:%s".format(cs, item.s, item.t, item.`type`).trim
    else "%d|%d:%s %s".format(item.t, item.s, item.`type`, cs).trim 
  }.getOrElse("")

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
    // returns pairs of indeces and -1,-1 if < K pairs
    public int[][] getKBestPairs(ParseForestItem[] items1, ParseForestItem[] items2) {
	// in this case K = items1.length

	boolean[][] beenPushed = new boolean[K][K];
		
	int[][] result = new int[K][2];
	for(int i = 0; i < K; i++) {
	    result[i][0] = -1;
	    result[i][1] = -1;
	}

	if(items1 == null || items2 == null || items1[0] == null || items2[0] == null)
	    return result;
		
	BinaryHeap heap = new BinaryHeap(K+1);
	int n = 0;
	ValueIndexPair vip = new ValueIndexPair(items1[0].prob+items2[0].prob,0,0);

	heap.add(vip);
	beenPushed[0][0] = true;
		
	while(n < K) {
	    vip = heap.removeMax();
			
	    if(vip.val == Double.NEGATIVE_INFINITY)
		break;
			
	    result[n][0] = vip.i1;
	    result[n][1] = vip.i2;

	    n++;
	    if(n >= K)
		break;
			
	    if(!beenPushed[vip.i1+1][vip.i2]) {
		heap.add(new ValueIndexPair(items1[vip.i1+1].prob+items2[vip.i2].prob,vip.i1+1,vip.i2));
		beenPushed[vip.i1+1][vip.i2] = true;
	    }
	    if(!beenPushed[vip.i1][vip.i2+1]) {
		heap.add(new ValueIndexPair(items1[vip.i1].prob+items2[vip.i2+1].prob,vip.i1,vip.i2+1));
		beenPushed[vip.i1][vip.i2+1] = true;
	    }

	}
		
	return result;
    }
}
	
class ValueIndexPair {
    public double val;
    public int i1, i2;
		
    public ValueIndexPair(double val, int i1, int i2) {
	this.val = val;
	this.i1 = i1;
	this.i2 = i2;
    }

    public int compareTo(ValueIndexPair other) {
	if(val < other.val)
	    return -1;
	if(val > other.val)
	    return 1;
	return 0;
    }
		
}

// Max Heap
// We know that never more than K elements on Heap
class BinaryHeap { 
    private int DEFAULT_CAPACITY; 
    private int currentSize; 
    private ValueIndexPair[] theArray;
  
    public BinaryHeap(int def_cap) {
	DEFAULT_CAPACITY = def_cap;
	theArray = new ValueIndexPair[DEFAULT_CAPACITY+1]; 
	// theArray[0] serves as dummy parent for root (who is at 1) 
	// "largest" is guaranteed to be larger than all keys in heap
	theArray[0] = new ValueIndexPair(Double.POSITIVE_INFINITY,-1,-1);          
	currentSize = 0; 
    } 
  
    public ValueIndexPair getMax() { 
	return theArray[1]; 
    }
  
    private int parent(int i) { return i / 2; } 
    private int leftChild(int i) { return 2 * i; } 
    private int rightChild(int i) { return 2 * i + 1; } 
  
    public void add(ValueIndexPair e) { 
   
	// bubble up: 
	int where = currentSize + 1; // new last place 
	while ( e.compareTo(theArray[parent(where)]) > 0 ){ 
	    theArray[where] = theArray[parent(where)]; 
	    where = parent(where); 
	} 
	theArray[where] = e; currentSize++;
    }
 
    public ValueIndexPair removeMax() {
	ValueIndexPair min = theArray[1];
	theArray[1] = theArray[currentSize];
	currentSize--;
	boolean switched = true;
	// bubble down
	for ( int parent = 1; switched && parent < currentSize; ) {
	    switched = false;
	    int leftChild = leftChild(parent);
	    int rightChild = rightChild(parent);

	    if(leftChild <= currentSize) {
		// if there is a right child, see if we should bubble down there
		int largerChild = leftChild;
		if ((rightChild <= currentSize) && 
		    (theArray[rightChild].compareTo(theArray[leftChild])) > 0){
		    largerChild = rightChild; 
		}
		if (theArray[largerChild].compareTo(theArray[parent]) > 0) {      
		    ValueIndexPair temp = theArray[largerChild];
		    theArray[largerChild] = theArray[parent];
		    theArray[parent] = temp;
		    parent = largerChild;
		    switched = true;
		}
	    }
	} 
	return min;
    }
 
}

*/

