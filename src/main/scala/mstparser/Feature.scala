package mstparser

import gnu.trove.list.TLinkable
import scala.reflect.BeanProperty

class Feature(val index: Int, val value: Double) extends TLinkable[Feature] {
  @BeanProperty var next: Feature = _
  @BeanProperty var previous: Feature = _

  override def toString = this.index + " " + this.value
}

class RelationalFeature(size: Int, declaration: String, lines: Array[String]) {
  require(lines.length == size)
  private val decFields = declaration.split(" ")
  private val name = this.decFields(2)
  private val values = lines.map(_.substring(2).split(" "))

  def getFeature(firstIndex: Int, secondIndex: Int) =
    if (firstIndex == 0 || secondIndex == 0) this.name + "=NULL"
    else this.name + "=" + this.values(firstIndex - 1)(secondIndex - 1)
}

