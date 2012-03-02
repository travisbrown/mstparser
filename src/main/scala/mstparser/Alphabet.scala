package mstparser

import gnu.trove.map.hash.TObjectIntHashMap

class Alphabet(private val capacity: Int) extends Serializable {
  def this() = this(10000)

	private val map = new TObjectIntHashMap[String](capacity, 0.75F, -1)
  private var growing = true

  /** Return -1 if entry isn't present. */
  def lookupIndex(entry: String) = this.map.get(entry) match {
    case -1 if this.growing =>
      this.map.put(entry, this.map.size)
      this.map.size - 1
    case i => i
  }

  def toArray = this.map.keys(Array.empty[String])

  def contains(entry: String) = this.map.contains(entry)
  def size = this.map.size

  def getGrowing = this.growing

  def setGrowing(growing: Boolean) {
    if (!growing) this.map.compact()
    this.growing = growing
  }
}

