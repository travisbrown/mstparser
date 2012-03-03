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
    else ch - 1 to 0 by -1
  ).find(par(ch) == par(_)).getOrElse(ch)))

  def decodeProjective(
    len: Int,
    fvs: Array[Array[Array[FeatureVector]]],
    probs: Array[Array[Array[Double]]],
    fvsTr: Array[Array[Array[FeatureVector]]],
    probsTr: Array[Array[Array[Double]]],
    fvsSi: Array[Array[Array[FeatureVector]]],
    probsSi: Array[Array[Array[Double]]],
    fvsNt: Array[Array[Array[Array[FeatureVector]]]],
    probsNt: Array[Array[Array[Array[Double]]]],
    kBest: Int
  ) = {
	  val staticTypes = if (this.pipe.getLabeled) Some(this.getTypes(probsNt, len)) else None
    val pf = new KBestParseForest(len - 1, kBest, 3)

    (0 until len).foreach { i =>
      pf.add(i, -1, 0, 0.0, new FeatureVector)
      pf.add(i, -1, 1, 0.0, new FeatureVector)
    }

    (1 until len).foreach { j =>
      (0 until len - j).foreach { s =>
        val t = s + j
        val (type1, type2) = staticTypes.map(ts => (ts(s)(t), ts(t)(s))).getOrElse((0, 0))
		
		    // case when r == s
        val b1 = pf.getItems(s, s, 0, 0)
        val c1 = pf.getItems(s + 1, t, 1, 0)

        if (b1 != null && c1 != null) {
          val sstFv = fvsTr(s)(s)(t).cat(fvsSi(s)(t)(0))
          val sstProb = probsTr(s)(s)(t) + probsSi(s)(t)(0)

          pf.getKBestPairs(b1, c1).takeWhile {
            case (comp1, comp2) => comp1 > -1 && comp2 > -1
          }.foreach {
            case (comp1, comp2) =>
              val bc = b1(comp1).prob + c1(comp2).prob
              var finProb = bc + probs(s)(t)(0) + sstProb
              var finFv = fvs(s)(t)(0).cat(sstFv)

              if (this.pipe.getLabeled) {
                finFv = fvsNt(s)(type1)(0)(1).cat(fvsNt(t)(type1)(0)(0).cat(finFv))
                finProb += probsNt(s)(type1)(0)(1) + probsNt(t)(type1)(0)(0)
              }
              pf.add(s, s, t, type1, 0, 1, finProb, finFv, b1(comp1), c1(comp2))
          }
        }

		    // case when r == t
        val b2 = pf.getItems(s, t - 1, 0, 0)
        val c2 = pf.getItems(t, t, 1, 0)

        if (b2 != null && c2 != null) {
          val sttFv = fvsTr(t)(t)(s).cat(fvsSi(t)(s)(0))
          val sttProb = probsTr(t)(t)(s) + probsSi(t)(s)(0)

          pf.getKBestPairs(b2, c2).takeWhile {
            case (comp1, comp2) => comp1 > -1 && comp2 > -1
          }.foreach {
            case (comp1, comp2) =>
              val bc = b2(comp1).prob + c2(comp2).prob
              var finProb = bc + probs(s)(t)(1) + sttProb
              var finFv = fvs(s)(t)(1).cat(sttFv)

              if (this.pipe.getLabeled) {
                finFv = fvsNt(t)(type2)(1)(1).cat(fvsNt(s)(type2)(1)(0).cat(finFv))
                finProb += probsNt(t)(type2)(1)(1) + probsNt(s)(type2)(1)(0)
              }
              pf.add(s, t, t, type2, 1, 1, finProb, finFv, b2(comp1), c2(comp2))
          }
        }

        (s until t).foreach { r =>
          // First case - create sibling.
          val b1 = pf.getItems(s, r, 0, 0)
          val c1 = pf.getItems(r + 1, t, 1, 0)

          if (b1 != null && c1 != null) {
            pf.getKBestPairs(b1, c1).takeWhile {
              case (comp1, comp2) => comp1 > -1 && comp2 > -1
            }.foreach {
              case (comp1, comp2) =>
                val bc = b1(comp1).prob + c1(comp2).prob
                pf.add(s, r, t, -1, 0, 2, bc, new FeatureVector, b1(comp1), c1(comp2))
                pf.add(s, r, t, -1, 1, 2, bc, new FeatureVector, b1(comp1), c1(comp2))
            }
          }
        }

        (s + 1 until t).foreach { r =>
		      // s -> (r,t)
		      val b1 = pf.getItems(s, r, 0, 1)
		      val c1 = pf.getItems(r, t, 0, 2)

          if (b1 != null && c1 != null) {
            pf.getKBestPairs(b1, c1).takeWhile {
              case (comp1, comp2) => comp1 > -1 && comp2 > -1
            }.foreach {
              case (comp1, comp2) =>
                var bc = b1(comp1).prob + c1(comp2).prob

                var finProb = bc + probs(s)(t)(0) + probsTr(s)(r)(t) + probsSi(r)(t)(1)
                var finFv = fvs(s)(t)(0).cat(fvsTr(s)(r)(t).cat(fvsSi(r)(t)(1)))

                if (this.pipe.getLabeled) {
                  finFv = fvsNt(s)(type1)(0)(1).cat(fvsNt(t)(type1)(0)(0).cat(finFv))
                  finProb += probsNt(s)(type1)(0)(1) + probsNt(t)(type1)(0)(0)
                }
                pf.add(s, r, t, type1, 0, 1, finProb, finFv, b1(comp1), c1(comp2))
            }
          }

		      // s -> (r,t)
		      val b2 = pf.getItems(s, r, 1, 2)
		      val c2 = pf.getItems(r, t, 1, 1)

          if (b2 != null && c2 != null) {
            pf.getKBestPairs(b2, c2).takeWhile {
              case (comp1, comp2) => comp1 > -1 && comp2 > -1
            }.foreach {
              case (comp1, comp2) =>
                var bc = b2(comp1).prob + c2(comp2).prob

                var finProb = bc + probs(s)(t)(1) + probsTr(t)(r)(s) + probsSi(r)(s)(1)
                var finFv = fvs(s)(t)(1).cat(fvsTr(t)(r)(s).cat(fvsSi(r)(s)(1)))

                if (this.pipe.getLabeled) {
                  finFv = fvsNt(t)(type2)(1)(1).cat(fvsNt(s)(type2)(1)(0).cat(finFv))
                  finProb += probsNt(t)(type2)(1)(1) + probsNt(s)(type2)(1)(0)
                }
                pf.add(s, r, t, type2, 1, 1, finProb, finFv, b2(comp1), c2(comp2))
            }
          }
        }

        (s to t).foreach { r =>
          if (r != s) {
            val b1 = pf.getItems(s, r, 0, 1)
            val c1 = pf.getItems(r, t, 0, 0)

            if (b1 != null && c1 != null) {
              pf.getKBestPairs(b1, c1).takeWhile {
                case (comp1, comp2) => comp1 > -1 && comp2 > -1
              }.takeWhile {
                case (comp1, comp2) =>
                  val bc = b1(comp1).prob + c1(comp2).prob
                  pf.add(s, r, t, -1, 0, 0, bc, new FeatureVector, b1(comp1), c1(comp2))
              }
            }
          }

          if (r != t) {
            val b1 = pf.getItems(s, r, 1, 0)
            val c1 = pf.getItems(r, t, 1, 1)

            if (b1 != null && c1 != null) {
              pf.getKBestPairs(b1, c1).takeWhile {
                case (comp1, comp2) => comp1 > -1 && comp2 > -1
              }.takeWhile {
                case (comp1, comp2) =>
                  val bc = b1(comp1).prob + c1(comp2).prob
                  pf.add(s, r, t, -1, 1, 0, bc, new FeatureVector, b1(comp1), c1(comp2))
              }
            }
          }
        }
      }
    }
	  pf.getBestParses
  }
}

