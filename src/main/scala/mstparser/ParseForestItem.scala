package mstparser

trait ParseForestItem {
  def prob: Double
  def featureVector: FeatureVector
  def depString(parse: Array[Int], labels: Array[Int]): Unit = ()
}

trait ChildHavingItem extends ParseForestItem {
  def l: ParseForestItem
  def r: ParseForestItem
  def fv: FeatureVector
  val featureVector = this.fv.cat(this.l.featureVector.cat(this.r.featureVector))
  override def depString(parse: Array[Int], labels: Array[Int]) {
    l.depString(parse, labels)
    r.depString(parse, labels)
  }
} 

case object EmptyItem extends ParseForestItem {
  val prob = Double.NegativeInfinity
  val featureVector: FeatureVector = null
}

case class PartialItem(
  prob: Double,
  featureVector: FeatureVector
) extends ParseForestItem

case class IncompleteItem(
  prob: Double,
  fv: FeatureVector,
  l: ParseForestItem,
  r: ParseForestItem
) extends ChildHavingItem

case class ArcItem(
  s: Int,
  t: Int,
  label: Int,
  prob: Double,
  fv: FeatureVector,
  l: ParseForestItem,
  r: ParseForestItem
) extends ChildHavingItem {
  override def depString(parse: Array[Int], labels: Array[Int]) {
    super.depString(parse, labels)
    parse(s) = this.t
    labels(s) = this.label
  }
}

