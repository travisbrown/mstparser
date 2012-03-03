package mstparser

import gnu.trove.map.TIntIntMap
import gnu.trove.map.hash.TIntIntHashMap
import gnu.trove.procedure.TIntIntProcedure

class DependencyDecoder2O(protected val pipe: DependencyPipe) extends old.DependencyDecoder2O with Decoder {
  def decodeNonProjective(
    instance: DependencyInstance,
    fvs: Array[Array[Array[FeatureVector]]],
    probs: Array[Array[Array[Double]]],
    fvsTr: Array[Array[Array[FeatureVector]]],
    probsTr: Array[Array[Array[Double]]],
    fvsSi: Array[Array[Array[FeatureVector]]],
    probsSi: Array[Array[Array[Double]]],
    fvsNt: Array[Array[Array[Array[FeatureVector]]]],
    probsNt: Array[Array[Array[Array[Double]]]],
    k: Int
  ) = {
    val orig = this.decodeProjective(instance.length, fvs, probs, fvsTr, probsTr, fvsSi, probsSi, fvsNt, probsNt, 1)

    val os = orig(0)._2.split(" ")
    val pars = -1 +: os.map(_.split("\\|")(0).toInt)
    var labs = 0 +: (if (this.pipe.getLabeled) os.map(_.split(":")(1).toInt) else Array.fill(os.length)(0))

    this.rearrange(probs, probsTr, probsSi, probsNt, pars, labs)

    instance.setHeads(pars)
    instance.setDeprels(labs.map(this.pipe.getType(_)))

    val parsString = pars.zip(labs).zipWithIndex.drop(1).map {
      case ((p, l), i) => "%d|%d:%d".format(p, i, l)
    }.mkString(" ")

    orig(0) = (this.pipe.createFeatureVector(instance), parsString)
    orig
  }

  protected def getSibs(ch: Int, par: Array[Int]) = (new java.lang.Integer((
    if (par(ch) > ch) ch + 1 until par(ch)
    else ch - 1 until par(ch) by -1
  ).find(par(ch) == par(_)).getOrElse(par(ch))), new java.lang.Integer((
    if (par(ch) < ch) ch + 1 until par.length
    else ch - 1 to 0 by -1 // TODO: Why 0 here?
  ).find(par(ch) == par(_)).getOrElse(ch)))
}

