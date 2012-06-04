package mstparser

class Parameters(size: Int) {
  var parameters = Array.ofDim[Double](size)
  private val total = Array.ofDim[Double](size)

  private val lossType = "punc"

  def updateParamsMIRA(pipe: DependencyPipe, instance: DependencyInstance, d: Seq[(FeatureVector, (IndexedSeq[Int], IndexedSeq[Int]))], update: Double) {
    val score = this.getScore(instance.featureVector)

    val (b, dist) = d.takeWhile(_._1 != null).map { case (f, p) => (
      this.numErrors(pipe, instance, p) - (score - this.getScore(f)),
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

  def numErrors(pipe: DependencyPipe, instance: DependencyInstance, pred: (IndexedSeq[Int], IndexedSeq[Int])) =
    this.lossType match {
      case "nopunc" => this.errors(pipe, instance, pred)(!_.matches("[,:.'`]+"))
      case _ => this.errors(pipe, instance, pred)(_ => true)
    }

  private def errors(pipe: DependencyPipe, instance: DependencyInstance, pred: (IndexedSeq[Int], IndexedSeq[Int]))(p: String => Boolean) = {
    var he = 0
    var le = 0
    var i = 1

    while (i < instance.length) {
      if (p(instance.postags(i))) {
        if (instance.heads(i) != pred._1(i)) he += 1
        if (instance.deprels(i) != pipe.typeAlphabet.values(pred._2(i))) le += 1 
      }
      i += 1
    }

    (he + le).toDouble
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

