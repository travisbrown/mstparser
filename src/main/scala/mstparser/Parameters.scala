package mstparser

class Parameters(size: Int) {
  var parameters = Array.ofDim[Double](size)
  private val total = Array.ofDim[Double](size)

  private val lossType = "punc"

  def updateParamsMIRA(instance: DependencyInstance, d: Seq[(FeatureVector, String)], update: Double) {
    val score = this.getScore(instance.featureVector)

    val (b, dist) = d.takeWhile(_._1 != null).map { case (f, p) => (
      this.numErrors(instance, p, instance.parseTree) - (score - this.getScore(f)),
      instance.featureVector.getDistVector(f)
    )}.unzip

    dist.zip(this.hildreth(dist, b)).foreach { case (f, a) =>
      f.update(this.parameters, this.total, a, update)
    }
  }

  def getScore(f: FeatureVector) = f.getScore(this.parameters)

  def averageParams(v: Double) {
    this.total.zipWithIndex.foreach { case (t, i) => this.total(i) /= v }
    this.parameters = this.total
  }

  def numErrors(instance: DependencyInstance, pred: String, gold: String) =
    this.lossType match {
      case "nopunc" => this.errors(instance, pred, gold)(!_.matches("[,:.'`]+"))
      case _ => this.errors(instance, pred, gold)(_ => true)
    }

  private def errors(instance: DependencyInstance, pred: String, gold: String)(p: String => Boolean) = {
    val items = instance.postags.tail.zip(pred.split(" ").zip(gold.split(" "))).filter(x => p(x._1))
    val (hs, ls) = items.map {
      case (_, (p, g)) =>
        val Array(ph, pl) = p.split(":")
        val Array(gh, gl) = g.split(":")
        (ph == gh, pl == gl)
    }.unzip

    ((items.size - hs.filter(x => x).size) + (items.size - ls.filter(x => x).size)).toDouble
  }

  private def hildreth(a: Seq[FeatureVector], b: Seq[Double]) = {
    val maxIter = 10000
    val eps = 0.00000001
    val zero = 0.000000000001

    val alpha = scala.collection.mutable.ArrayBuffer.fill(b.size)(0.0)
    val ff = b.toBuffer
    val kkt = b.toBuffer

    var (kktMaxV, kktMaxI) = kkt.zipWithIndex.maxBy(_._1)

    val k = a.size

    val aa = Array.ofDim[Double](k, k)
    val isComputed = Array.ofDim[Boolean](k)

    (0 until k).foreach(i => aa(i)(i) = a(i).dotProduct(a(i)))

    var iter = 0
    while (kktMaxV >= eps && iter < maxIter) {

      val diffA = if (aa(kktMaxI)(kktMaxI) <= zero) 0.0 else ff(kktMaxI) / aa(kktMaxI)(kktMaxI)
      val addA = if (alpha(kktMaxI) + diffA < 0.0) -1.0 * alpha(kktMaxI) else diffA

      alpha(kktMaxI) += addA

      if (!isComputed(kktMaxI)) {
        (0 until k).foreach { i =>
          aa(i)(kktMaxI) = a(i).dotProduct(a(kktMaxI))
          isComputed(kktMaxI) = true
        }
      }

      (0 until ff.size).foreach { i =>
        ff(i) -= addA * aa(i)(kktMaxI)
        kkt(i) = if (alpha(i) > zero) math.abs(ff(i)) else ff(i)
      }

      val (v, i) = kkt.zipWithIndex.maxBy(_._1)
      kktMaxV = v
      kktMaxI = i
      iter += 1
    }
    alpha
  }
}

