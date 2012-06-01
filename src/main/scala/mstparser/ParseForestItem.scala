package mstparser

trait ParseForestItem {
  def prob: Double
  def featureVector: FeatureVector
  def parse(size: Int): (IndexedSeq[Int], IndexedSeq[Int]) = {
    val parse = Array.fill(size)(-1) 
    val labels = Array.fill(size)(-1) 
    this.fill(parse, labels)
    (wrapIntArray(parse), wrapIntArray(labels))
  }
  def fill(parse: Array[Int], labels: Array[Int]): Unit = ()
}

trait ChildHavingItem extends ParseForestItem {
  def l: ParseForestItem
  def r: ParseForestItem
  override def fill(parse: Array[Int], labels: Array[Int]) {
    this.l.fill(parse, labels)
    this.r.fill(parse, labels)
  }
} 

case object EmptyItem extends ParseForestItem {
  val prob = 0.0
  val featureVector = new FeatureVector
}

case class CompleteItem(
  l: ParseForestItem,
  r: ParseForestItem
) extends ChildHavingItem {
  val prob = this.l.prob + this.r.prob
  val featureVector = this.l.featureVector.cat(this.r.featureVector)
}

case class ArcItem(
  s: Int,
  t: Int,
  label: Int,
  prob: Double,
  fv: FeatureVector,
  l: ParseForestItem,
  r: ParseForestItem
) extends ChildHavingItem {
  val featureVector = this.fv.cat(this.l.featureVector.cat(this.r.featureVector))
  override def fill(parse: Array[Int], labels: Array[Int]) {
    super.fill(parse, labels)
    parse(t) = this.s
    labels(t) = this.label
  }
}

