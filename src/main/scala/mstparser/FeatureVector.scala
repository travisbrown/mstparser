package mstparser

import gnu.trove.list.array.TIntArrayList
import gnu.trove.map.hash.TIntDoubleHashMap

object FeatureVector {
  def fromKeys(keys: Array[Int]) = {
    val v = new FeatureVector
    keys.foreach(k => v.add(k, 1.0))
    v
  }
}

class FeatureVector(fv1: FeatureVector, fv2: FeatureVector, negSecond: Boolean) {
  private val features = scala.collection.mutable.Buffer.empty[Feature]
  def this(fv1: FeatureVector, fv2: FeatureVector) = this(fv1, fv2, false)
  def this(fv1: FeatureVector) = this(fv1, null)
  def this() = this(null)

  def cat(that: FeatureVector) = new FeatureVector(this, that)
  def getDistVector(that: FeatureVector) = new FeatureVector(this, that, true)

  def add(i: Int, v: Double) {
    this.features += new Feature(i, v)
  }

  def keys = {
    val keys = new TIntArrayList
    this.addKeysToList(keys)
    keys.toArray
  }

  private def addKeysToList(keys: TIntArrayList) {
    Option(this.fv1).foreach(_.addKeysToList(keys))
    Option(this.fv2).foreach(_.addKeysToList(keys))

    this.features.foreach(f => keys.add(f.index))
  }

  def getScore(parameters: Array[Double]): Double = this.getScore(parameters, false)

  private def getScore(parameters: Array[Double], negate: Boolean): Double = {
    var score = Option(this.fv1).map(_.getScore(parameters, negate)).getOrElse(0.0) +
      Option(this.fv2).map(_.getScore(parameters, negate != this.negSecond)).getOrElse(0.0)

    val mult = if (negate) -1.0 else 1.0
    score + this.features.map(f => mult * parameters(f.index) * f.value).sum
  }

  def update(parameters: Array[Double], total: Array[Double], alpha: Double, update: Double) {
    this.update(parameters, total, alpha, update, false)
  }

  private def update(parameters: Array[Double], total: Array[Double], alpha: Double, update: Double, negate: Boolean) {
    Option(this.fv1).foreach(_.update(parameters, total, alpha, update, negate))
    Option(this.fv2).foreach(_.update(parameters, total, alpha, update, negate != this.negSecond))

    val mult = if (negate) -1.0 else 1.0

    this.features.foreach { f =>
      parameters(f.index) += mult * alpha * f.value
      total(f.index) += mult * alpha * f.value * update
    }
  }

  def dotProduct(that: FeatureVector) = {
    val m1 = new TIntDoubleHashMap(this.features.size)
    this.addFeaturesToMap(m1, false)
    m1.compact()

    val m2 = new TIntDoubleHashMap(that.features.size)
    that.addFeaturesToMap(m2, false)
    m2.compact()

    m1.keys.map(k => m1.get(k) * m2.get(k)).sum
  }

  private def addFeaturesToMap(map: TIntDoubleHashMap, negate: Boolean) {
    Option(this.fv1).foreach(_.addFeaturesToMap(map, negate))
    Option(this.fv2).foreach(_.addFeaturesToMap(map, negate != this.negSecond))

    val mult = if (negate) -1.0 else 1.0

    this.features.foreach { f =>
      if (!map.adjustValue(f.index, mult * f.value))
        map.put(f.index, mult * f.value)
    }
  }

  override def toString = {
    val builder = new StringBuilder
    this.toString(builder)
    builder.toString
  }

  private def toString(builder: StringBuilder) {
    Option(this.fv1).foreach(_.toString(builder))
    Option(this.fv2).foreach(_.toString(builder))

    this.features.foreach(f => builder.append(f.toString).append(' '))
  }
}

