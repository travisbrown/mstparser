package mstparser

trait ParseForestItem {
  def prob: Double
  def featureVector: FeatureVector
  def depString(parse: Array[Int], labels: Array[Int]): Unit = ()
}

trait ChildHavingItem extends ParseForestItem {
  def l: ParseForestItem
  def r: ParseForestItem
  override def depString(parse: Array[Int], labels: Array[Int]) {
    this.l.depString(parse, labels)
    this.r.depString(parse, labels)
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
  override def depString(parse: Array[Int], labels: Array[Int]) {
    super.depString(parse, labels)
    parse(s) = this.t
    labels(s) = this.label
  }
}

