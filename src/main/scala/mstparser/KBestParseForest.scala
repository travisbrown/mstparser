package mstparser

import scala.collection.mutable.PriorityQueue

class KBestParseForest(private val end: Int, private val k: Int, private val compC: Int) {
  def this(end: Int, k: Int) = this(end, k, 2)
  private val chart = Array.ofDim[ParseForestItem](this.end + 1, this.end + 1, 2, compC, this.k)

  def getItems(s: Int, t: Int, d: Int, c: Int) =
    if (this.chart(s)(t)(d)(c)(0) != null) this.chart(s)(t)(d)(c) else null

  def getBestParses: Array[(FeatureVector, String)] =
    this.chart(0)(this.end)(0)(0).map { item =>
      if (item.prob > Double.NegativeInfinity)
        (item.getFeatureVector, item.getDepString)
      else (null, null)
    }

  def getKBestPairs(is: Array[ParseForestItem], js: Array[ParseForestItem]) = {
    val result = Array.fill(this.k)(-1, -1)

    if (is(0) != null && js(0) != null) {
      val heap = PriorityQueue((is(0).prob + js(0).prob, (0, 0)))
      val beenPushed = Array.ofDim[Boolean](this.k, this.k)
      beenPushed(0)(0) = true

      var n = 0

      while (n < this.k && heap.head._1 > Double.NegativeInfinity) {
        val (v, (i, j)) = heap.dequeue

        result(n) = (i, j)
        n += 1

        if (n < this.k) {
          if (!beenPushed(i + 1)(j)) {
            heap += ((is(i + 1).prob + js(j).prob, (i + 1, j)))
            beenPushed(i + 1)(j) = true
          }

          if (!beenPushed(i)(j + 1)) {
            heap += ((is(i).prob + js(j + 1).prob, (i, j + 1)))
            beenPushed(i)(j + 1) = true
          }
        }
      }
    }

    result
  }

  def add(s: Int, label: Int, d: Int, score: Double, fv: FeatureVector) = {
    if (this.chart(s)(s)(d)(0)(0) == null) {
      this.chart(s)(s)(d)(0) = Array.fill(k)(new ParseForestItem(s, label, d))
    }

    if (this.chart(s)(s)(d)(0)(k - 1).prob > score) false
    else {
      var added = false
      var i = 0

      while (!added && i < this.k) {
        if (this.chart(s)(s)(d)(0)(i).prob < score) {
          var tmp = this.chart(s)(s)(d)(0)(i)
          this.chart(s)(s)(d)(0)(i) = new ParseForestItem(s, label, d, score, fv)

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
    if (this.chart(s)(t)(d)(c)(0) == null) {
      this.chart(s)(t)(d)(c) = Array.fill(k)(new ParseForestItem(s, t, label, d, c))
    }

    if (this.chart(s)(t)(d)(c)(k - 1).prob > score) false
    else {
      var added = false
      var i = 0

      while (!added && i < this.k) {
        if (this.chart(s)(t)(d)(c)(i).prob < score) {
          var tmp = this.chart(s)(t)(d)(c)(i)
          this.chart(s)(t)(d)(c)(i) = new ParseForestItem(s, t, label, d, c, score, fv, Some(p, q))

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

  def getKBestPairsJ(is: Array[ParseForestItem], js: Array[ParseForestItem]) =
    this.getKBestPairs(is, js).map {
      case (i, j) => (i.asInstanceOf[java.lang.Integer], j.asInstanceOf[java.lang.Integer])
    }
}

