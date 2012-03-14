package mstparser

import gnu.trove.map.hash.TObjectIntHashMap

class Alphabet[A](private val capacity: Int) extends Serializable {
  def this() = this(10000)

	private val map = new TObjectIntHashMap[A](capacity, 0.75F, -1)
  private var growing = true
  private val _values = scala.collection.mutable.ArrayBuffer.empty[A]

  def lookupIndex(entry: A) = this.map.get(entry) match {
    case -1 if this.growing =>
      this._values += entry
      this.map.put(entry, this.map.size)
      this.map.size - 1
    case i => i
  }

  def values: IndexedSeq[A] = this._values

  def contains(entry: A) = this.map.contains(entry)
  def size = this.map.size

  def getGrowing = this.growing

  def setGrowing(growing: Boolean) {
    if (!growing) this.map.compact()
    this.growing = growing
  }
}

