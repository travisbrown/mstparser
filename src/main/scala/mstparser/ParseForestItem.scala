package mstparser

trait ParseForestItem {
  def prob: Double
  def featureVector: FeatureVector
  def depString: String = ""
}

trait ChildHavingItem extends ParseForestItem {
  def l: ParseForestItem
  def r: ParseForestItem
  def fv: FeatureVector
  val featureVector = this.fv.cat(this.l.featureVector.cat(this.r.featureVector))
  override def depString = (this.l.depString + " " + this.r.depString).trim
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

case class LeftItem(
  s: Int,
  t: Int,
  label: Int,
  prob: Double,
  fv: FeatureVector,
  l: ParseForestItem,
  r: ParseForestItem
) extends ChildHavingItem {
  override val depString = "%s %d|%d:%d".format(super.depString, s, t, label).trim
}

case class RightItem(
  s: Int,
  t: Int,
  label: Int,
  prob: Double,
  fv: FeatureVector,
  l: ParseForestItem,
  r: ParseForestItem
) extends ChildHavingItem {
  override val depString = "%d|%d:%d %s".format(t, s, label, super.depString).trim
}

