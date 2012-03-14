package mstparser

import gnu.trove.map.hash.TObjectIntHashMap

class Alphabet[A](capacity: Int) extends Serializable {
  def this() = this(10000)

  private[this] val map = new TObjectIntHashMap[A](capacity, 0.75F, -1)
  private[this] var growing = true
  private[this] val _values = scala.collection.mutable.ArrayBuffer.empty[A]

  def lookupIndex(entry: A) = this.map.get(entry) match {
    case -1 if this.growing =>
      this._values += entry
      this.map.put(entry, this.map.size)
      this.map.size - 1
    case i => i
  }

  def values: IndexedSeq[A] = this._values
  def size = this.values.size

  def stopGrowth() {
    this.map.compact()
    this.growing = false
  }
}

