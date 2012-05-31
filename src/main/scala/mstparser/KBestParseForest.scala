package mstparser

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.PriorityQueue

class KBestParseForest(
  private[this] val end: Int,
  private[this] val k: Int,
  private[this] val extra: Boolean
) {
  def this(end: Int, k: Int) = this(end, k, false)
  val complete = Array.ofDim[IndexedSeq[ParseForestItem]](this.end + 1, this.end + 1)
  val incomplete = Array.ofDim[IndexedSeq[ParseForestItem]](this.end + 1, this.end + 1)
  val other = if (this.extra) Array.ofDim[IndexedSeq[ParseForestItem]](this.end + 1, this.end + 1) else null
  private[this] val empty = IndexedSeq(EmptyItem)

  (0 to this.end).foreach { i => this.complete(i)(i) = this.empty }

  def getBestParses: Seq[(FeatureVector, (IndexedSeq[Int], IndexedSeq[Int]))] =
    this.complete(0)(this.end).map { item =>
      val parse = Array.fill(this.end + 1)(-1) 
      val labels = Array.fill(this.end + 1)(-1) 
      item.depString(parse, labels)
      (item.featureVector, (wrapIntArray(parse), wrapIntArray(labels)))
    }

  def getKBestPairs(is: IndexedSeq[ParseForestItem], js: IndexedSeq[ParseForestItem]): IndexedSeq[(Int, Int)] = {
    val result = ArrayBuffer.empty[(Int, Int)]

    val heap = PriorityQueue((is(0).prob + js(0).prob, (0, 0)))
    val beenPushed = scala.collection.mutable.BitSet(0)

    var n = 0

    while (n < this.k) {
      val (_, (i, j)) = heap.dequeue

      result += i -> j
      n += 1

      if (n < this.k) {
        val x = (i + 1) * this.end + j
        if (!beenPushed(x)) {
          heap += ((is(i + 1).prob + js(j).prob, (i + 1, j)))
          beenPushed += x
        }

        val y = i * this.end + j + 1 
        if (!beenPushed(y)) {
          heap += ((is(i).prob + js(j + 1).prob, (i, j + 1)))
          beenPushed += y
        }
      }
    }

    result
  }

  //def add(s: Int, r: Int, t: Int, label: Int, d: Int, c: Int,
  //  score: Double, fv: FeatureVector, p: ParseForestItem, q: ParseForestItem) = true
}

