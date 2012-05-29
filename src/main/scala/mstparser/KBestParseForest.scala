package mstparser

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.PriorityQueue

class KBestParseForest(
  private[this] val end: Int,
  private[this] val k: Int,
  private[this] val compC: Int
) {
  def this(end: Int, k: Int) = this(end, k, 2)
  private[this] val chart = IndexedSeq.fill(this.end + 1, this.end + 1, 2, compC)(ArrayBuffer.empty[ParseForestItem])

  def getItems(s: Int, t: Int, d: Int, c: Int): Seq[ParseForestItem] = this.chart(s)(t)(d)(c)

  def getBestParses: Seq[(FeatureVector, String)] =
    this.chart(0)(this.end)(0)(0).map { item =>
      if (item.prob > Double.NegativeInfinity)
        (item.featureVector, item.depString)
      else (null, null)
    }

  def getKBestPairs(is: Seq[ParseForestItem], js: Seq[ParseForestItem]): IndexedSeq[(Int, Int)] = {
    val result = ArrayBuffer.fill(this.k)(-1, -1)

    if (is(0) != null && js(0) != null) {
      val heap = PriorityQueue((is(0).prob + js(0).prob, (0, 0)))
      val beenPushed = scala.collection.mutable.Set((0, 0))

      var n = 0

      while (n < this.k && heap.head._1 > Double.NegativeInfinity) {
        val (_, (i, j)) = heap.dequeue

        result(n) = (i, j)
        n += 1

        if (n < this.k) {
          if (!beenPushed(i + 1, j)) {
            heap += ((is(i + 1).prob + js(j).prob, (i + 1, j)))
            beenPushed += ((i + 1, j))
          }

          if (!beenPushed(i, j + 1)) {
            heap += ((is(i).prob + js(j + 1).prob, (i, j + 1)))
            beenPushed += ((i, j + 1))
          }
        }
      }
    }

    result
  }

  def add(s: Int, label: Int, d: Int, score: Double, fv: FeatureVector) = {
    if (this.chart(s)(s)(d)(0).isEmpty) {
      this.chart(s)(s)(d)(0) ++= Seq.fill(k)(EmptyItem)
    }

    if (this.chart(s)(s)(d)(0)(k - 1).prob > score) false
    else {
      var added = false
      var i = 0

      while (!added && i < this.k) {
        if (this.chart(s)(s)(d)(0)(i).prob < score) {
          var tmp = this.chart(s)(s)(d)(0)(i)
          this.chart(s)(s)(d)(0)(i) = PartialItem(score, fv) //EmptyItem //new ParseForestItem(s, label, d != 0, score, fv)

          var j = i + 1
          while (j < this.k && tmp.prob > Double.NegativeInfinity) {
            val tmp1 = this.chart(s)(s)(d)(0)(j)
            this.chart(s)(s)(d)(0)(j) = tmp
            tmp = tmp1
            j += 1
          }
          added = true
        }
        i += 1
      }

      added
    }
  }

  def add(s: Int, r: Int, t: Int, label: Int, d: Int, c: Int,
    score: Double, fv: FeatureVector, p: ParseForestItem, q: ParseForestItem) = {
    if (this.chart(s)(t)(d)(c).isEmpty) {
      this.chart(s)(t)(d)(c) ++= Seq.fill(k)(EmptyItem)
    }

    if (this.chart(s)(t)(d)(c)(k - 1).prob > score) false
    else {
      var added = false
      var i = 0

      while (!added && i < this.k) {
        if (this.chart(s)(t)(d)(c)(i).prob < score) {
          var tmp = this.chart(s)(t)(d)(c)(i)
          this.chart(s)(t)(d)(c)(i) = if (c != 1)
            IncompleteItem(score, fv, p, q)
          else if (d != 0) 
            RightItem(s, t, label, score, fv, p, q)
          else
            LeftItem(s, t, label, score, fv, p, q)

          var j = i + 1
          while (j < this.k && tmp.prob > Double.NegativeInfinity) {
            val tmp1 = this.chart(s)(t)(d)(c)(j)
            this.chart(s)(t)(d)(c)(j) = tmp
            tmp = tmp1
            j += 1
          }
          added = true
        }
        i += 1
      }

      added
    }
  }
}

