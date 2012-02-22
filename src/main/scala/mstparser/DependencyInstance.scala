package mstparser

import scala.reflect.BeanProperty

class DependencyInstance(
  _forms: Array[String],
  _lemmas: Array[String],
  _cpostags: Array[String],
  _postags: Array[String],
  _feats: Array[Array[String]],
  _deprels: Array[String],
  _heads: Array[Int],
  _relFeats: Array[RelationalFeature]
) extends old.DependencyInstance(_forms, _lemmas, _cpostags, _postags, _feats, _deprels, _heads, _relFeats) with Serializable {
  def this(
  forms: Array[String],
  lemmas: Array[String],
  cpostags: Array[String],
  postags: Array[String],
  feats: Array[Array[String]],
  deprels: Array[String],
  heads: Array[Int]) = this(forms, lemmas, cpostags, postags, feats, deprels, heads, null)

  @BeanProperty @transient var featureVector: FeatureVector = _
  @BeanProperty var parseTree: String = _
  def length = this.forms.length
  def size = this.forms.length - 1
  override def toString = "[%s]\n".format(this.forms.mkString(", "))

  def difference(that: DependencyInstance) = if (this.size == that.size) Some({
    val ps = this.heads.zip(that.heads).zip(this.deprels.zip(that.deprels)).tail.filter { case ((h0, h1), _) => h0 == h1 }
    (this.size - ps.size, (this.size - ps.size) + ps.filter { case (_, (l0, l1)) => l0 != l1 }.size)
  }) else None
}

