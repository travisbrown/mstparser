package mstparser

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.PriorityQueue

class KBestParseForest(
  private[this] val end: Int,
  private[this] val k: Int,
  private[this] val compC: Int
) {
  def this(end: Int, k: Int) = this(end, k, 2)
  private[this] val chart = Array.ofDim[IndexedSeq[ParseForestItem]](this.end + 1, this.end + 1, 6)
  private[this] val empty = IndexedSeq(EmptyItem)

  (0 to this.end).foreach { i =>
    this.chart(i)(i)(0) = this.empty
    this.chart(i)(i)(3) = this.empty
  }

  def getItems(s: Int, t: Int, d: Int, c: Int): IndexedSeq[ParseForestItem] = this.chart(s)(t)(d * 3 + c)

  def getBestParses: Seq[(FeatureVector, (IndexedSeq[Int], IndexedSeq[Int]))] =
    this.chart(0)(this.end)(0).map { item =>
      val parse = Array.fill(this.end + 1)(-1) 
      val labels = Array.fill(this.end + 1)(-1) 
      item.depString(parse, labels)
      (item.featureVector, (wrapIntArray(parse), wrapIntArray(labels)))
    }

  def getKBestPairs(is: IndexedSeq[ParseForestItem], js: IndexedSeq[ParseForestItem]): IndexedSeq[(Int, Int)] = {
    val result = ArrayBuffer.empty[(Int, Int)] //fill(this.k)(-1, -1)

    if (is.size > 0 && js.size > 0) {
      val heap = PriorityQueue((is(0).prob + js(0).prob, (0, 0)))
      val beenPushed = scala.collection.mutable.Set(0)

      var n = 0

      while (n < this.k && heap.head._1 > Double.NegativeInfinity) {
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
    }

    result
  }

  def add(s: Int, t: Int, d: Int, c: Int, is: IndexedSeq[ParseForestItem]) {
    this.chart(s)(t)(d * 3 + c) = is
  }

  def add(s: Int, r: Int, t: Int, label: Int, d: Int, c: Int,
    score: Double, fv: FeatureVector, p: ParseForestItem, q: ParseForestItem) = true /*{

    if (this.chart(s)(t)(d * 3 + c)(k - 1) != null && this.chart(s)(t)(d * 3 + c)(k - 1).prob > score) false
    else {
      var added = false
      var i = 0

      while (!added && i < this.k) {
        var tmp = this.chart(s)(t)(d * 3 + c)(i)
        if (tmp == null || tmp.prob < score) {
          this.chart(s)(t)(d * 3 + c)(i) = if (c != 1) IncompleteItem(score, fv, p, q)
          else if (d != 0) ArcItem(s, t, label, score, fv, p, q)
          else ArcItem(t, s, label, score, fv, p, q)

          var j = i + 1
          while (j < this.k && tmp != null) {
            val tmp1 = this.chart(s)(t)(d * 3 + c)(j)
            this.chart(s)(t)(d * 3 + c)(j) = tmp
            tmp = tmp1
            j += 1
          }
          added = true
        }
        i += 1
      }

      added
    }
  }*/
}

