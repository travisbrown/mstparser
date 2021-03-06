package mstparser

class DependencyInstance(
  val forms: IndexedSeq[String],
  val lemmas: IndexedSeq[String],
  val cpostags: IndexedSeq[String],
  val postags: IndexedSeq[String],
  val feats: IndexedSeq[IndexedSeq[String]],
  var deprels: IndexedSeq[String],
  var heads: IndexedSeq[Int],
  val relFeats: IndexedSeq[RelationalFeature]
) {
  @transient var featureVector: FeatureVector = _
  def length = this.forms.length
  def size = this.forms.length - 1
  override def toString = "[%s]\n".format(this.forms.mkString(", "))

  def difference(that: DependencyInstance) = if (this.size == that.size) Some({
    val ps = this.heads.zip(that.heads).zip(this.deprels.zip(that.deprels)).tail.filter { case ((h0, h1), _) => h0 == h1 }
    (this.size - ps.size, (this.size - ps.size) + ps.filter { case (_, (l0, l1)) => l0 != l1 }.size)
  }) else None
}

