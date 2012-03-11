package mstparser

import gnu.trove.list.TIntList
import gnu.trove.list.array.TIntArrayList
import gnu.trove.list.linked.TLinkedList;
import gnu.trove.map.hash.TIntDoubleHashMap
import gnu.trove.procedure.TObjectProcedure

object FeatureVector {
  def fromKeys(keys: Array[Int]) = {
    val v = new FeatureVector
    keys.foreach(k => v.add(new Feature(k, 1.0)))
    v
  }
}

class FeatureVector(fv1: FeatureVector, fv2: FeatureVector, negSecond: Boolean) extends TLinkedList[Feature] {
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

    val mult = if (negate) -1.0 else 1.0

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

    val mult = if (negate) -1.0 else 1.0

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

    val mult = if (negate) -1.0 else 1.0

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

