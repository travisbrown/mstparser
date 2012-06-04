package mstparser


class RelationalFeature(size: Int, declaration: String, lines: Array[String]) {
  require(lines.length == size)
  private val name = declaration.split(" ")(2)
  private val values = lines.map(_.substring(2).split(" "))

  def getFeature(firstIndex: Int, secondIndex: Int) =
    if (firstIndex == 0 || secondIndex == 0) this.name + "=NULL"
    else this.name + "=" + this.values(firstIndex - 1)(secondIndex - 1)
}

