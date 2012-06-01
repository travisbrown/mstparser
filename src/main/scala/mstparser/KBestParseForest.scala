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
    this.complete(0)(this.end).map(item => (item.featureVector, item.parse(this.end + 1)))

  def bestPairs[A](is: IndexedSeq[A], js: IndexedSeq[A])(s: A => Double) = {
    val heap = scala.collection.mutable.PriorityQueue(
      (0, 0) -> (s(is(0)) + s(js(0)))
    )(Ordering.Double.on[((Int, Int), Double)](_._2))

    val added = scala.collection.mutable.Set(0 -> 0)

    new Iterator[((A, A), Double)] {
      def hasNext = heap.nonEmpty
      def next = {
        val ((i, j), v) = heap.dequeue

        if (i + 1 < is.size && !added((i + 1,  j))) {
          heap += (i + 1, j) -> (s(is(i + 1)) + s(js(j)))
          added += ((i + 1, j))
        }

        if (j + 1 < js.size && !added((i, j + 1))) {
          heap += (i, j + 1) -> (s(is(i)) + s(js(j + 1)))
          added += ((i, j + 1))
        }

        ((is(i), js(j)), v)
      }
    }
  }
}

