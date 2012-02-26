package mstparser

import gnu.trove.list.TIntList
import gnu.trove.list.array.TIntArrayList
import gnu.trove.map.hash.TIntDoubleHashMap
import gnu.trove.procedure.TObjectProcedure


object FeatureVector {
  def fromKeys(keys: Array[Int]) = {
    val v = new FeatureVector()
    keys.foreach(k => v.add(new Feature(k, 1.0)))
    v
  }
}

class FeatureVector(fv1: FeatureVector, fv2: FeatureVector, negSecond: Boolean) extends old.FeatureVector(fv1, fv2, negSecond) {
  def this(fv1: FeatureVector, fv2: FeatureVector) = this(fv1, fv2, false)
  def this(fv1: FeatureVector) = this(fv1, null)
  def this() = this(null) 

  def cat(that: FeatureVector) = new FeatureVector(this, that)
  def getDistVector(that: FeatureVector) = new FeatureVector(this, that, true)

  def add(i: Int, v: Double) {
    this.add(new Feature(i, v))
  }

  def keys = {
    val keys = new TIntArrayList
    this.addKeysToList(keys)
	  keys.toArray
  }

  private def addKeysToList(keys: TIntArrayList) {
    Option(this.fv1).foreach(_.addKeysToList(keys))
    Option(this.fv2).foreach(_.addKeysToList(keys))

    this.forEachValue(new TObjectProcedure[Feature] {
      def execute(f: Feature) = {
        keys.add(f.index)
        true
      }
    })
  }

  def getScore(parameters: Array[Double]): Double = this.getScore(parameters, false)

  private def getScore(parameters: Array[Double], negate: Boolean): Double = {
    var score = Option(this.fv1).map(_.getScore(parameters, negate)).getOrElse(0.0) +
    Option(this.fv2).map(_.getScore(parameters, negate != this.negSecond)).getOrElse(0.0)

    var mult = if (negate) -1.0 else 1.0

    this.forEachValue(new TObjectProcedure[Feature] {
      def execute(f: Feature) = {
        score += mult * parameters(f.index) * f.value
        true
      }
    })

    score
  }

  def update(parameters: Array[Double], total: Array[Double], alpha: Double, update: Double) {
    this.update(parameters, total, alpha, update, false)
  }

  private def update(parameters: Array[Double], total: Array[Double], alpha: Double, update: Double, negate: Boolean) {
    Option(this.fv1).foreach(_.update(parameters, total, alpha, update, negate))
    Option(this.fv2).foreach(_.update(parameters, total, alpha, update, negate != this.negSecond))

    var mult = if (negate) -1.0 else 1.0

    this.forEachValue(new TObjectProcedure[Feature] {
      def execute(f: Feature) = {
        parameters(f.index) += mult * alpha * f.value
        total(f.index) += mult * alpha * f.value * update
        true
      }
    })
	}


  def dotProduct(that: FeatureVector) = {
    val m1 = new TIntDoubleHashMap(this.size)
    this.addFeaturesToMap(m1, false)
    m1.compact()

    val m2 = new TIntDoubleHashMap(that.size)
    that.addFeaturesToMap(m2, false)
    m2.compact()

    m1.keys.map(k => m1.get(k) * m2.get(k)).sum
  }

  private def addFeaturesToMap(map: TIntDoubleHashMap, negate: Boolean) {
    Option(this.fv1).foreach(_.addFeaturesToMap(map, negate))
    Option(this.fv2).foreach(_.addFeaturesToMap(map, negate != this.negSecond))

    var mult = if (negate) -1.0 else 1.0

    this.forEachValue(new TObjectProcedure[Feature] {
      def execute(f: Feature) = {
        if (!map.adjustValue(f.index, mult * f.value))
          map.put(f.index, mult * f.value)
        true
      }
    })
  }

  override def toString = {
    val builder = new StringBuilder
    this.toString(builder)
    builder.toString
  }

  private def toString(builder: StringBuilder) {
    Option(this.fv1).foreach(_.toString(builder))
    Option(this.fv2).foreach(_.toString(builder))

    this.forEachValue(new TObjectProcedure[Feature] {
      def execute(f: Feature) = {
        builder.append(f.toString).append(' ')
        true
      }
    })
  }
}

/*
public class FeatureVector extends TLinkedList<Feature> {

    public double dotProduct(FeatureVector fl2) {

	TIntDoubleHashMap hm1 = new TIntDoubleHashMap(this.size());
	addFeaturesToMap(hm1, false);
	hm1.compact();

	TIntDoubleHashMap hm2 = new TIntDoubleHashMap(fl2.size());
	fl2.addFeaturesToMap(hm2, false);
	hm2.compact();

	int[] keys = hm1.keys();

	double result = 0.0;
	for(int i = 0; i < keys.length; i++)
	    result += hm1.get(keys[i])*hm2.get(keys[i]);
		
	return result;
		
    }

    private void addFeaturesToMap(TIntDoubleHashMap map, boolean negate) {
	if (null != subfv1) {
	    subfv1.addFeaturesToMap(map, negate);

	    if (null != subfv2) {
		if (negate) {
		    subfv2.addFeaturesToMap(map, !negateSecondSubFV);
		} else {
		    subfv2.addFeaturesToMap(map, negateSecondSubFV);
		}
	    }
	}

	ListIterator it = listIterator();
	if (negate) {
	    while (it.hasNext()) {
		Feature f = (Feature)it.next();
		if (!map.adjustValue(f.getIndex(), -f.getValue()))
		    map.put(f.getIndex(), -f.getValue());
	    }
	} else {
	    while (it.hasNext()) {
		Feature f = (Feature)it.next();
		if (!map.adjustValue(f.getIndex(), f.getValue()))
		    map.put(f.getIndex(), f.getValue());
	    }
	}
    }


    public final String toString() {
	StringBuilder sb = new StringBuilder();
	toString(sb);
	return sb.toString();
    }

    private final void toString(StringBuilder sb) {
	if (null != subfv1) {
	    subfv1.toString(sb);

	    if (null != subfv2)
		subfv2.toString(sb);
	}
	ListIterator it = listIterator();
	while (it.hasNext())
	    sb.append(it.next().toString()).append(' ');
    }

}*/

