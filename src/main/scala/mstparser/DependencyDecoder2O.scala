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
    val parse = -1 +: os.map(_.split("\\|")(0).toInt)
    var labels = 0 +: (if (this.pipe.getLabeled) os.map(_.split(":")(1).toInt) else Array.fill(os.length)(0))

    val (nParseP, nLabelsP) = this.rearrange(probs, probsTr, probsSi, probsNt, parse, labels)
    val (nParse, nLabels) = this.rearrangex(probs, probsTr, probsSi, probsNt, parse, labels)
    if (!nParse.sameElements(nParseP)) {
      println(nParse.zip(nParseP).mkString(" "))
      println(nLabels.zip(nLabelsP).mkString(" "))
      println()
    } else {
      println("Fine.")
      println()
    }

    instance.setHeads(nParse)
    instance.setDeprels(nLabels.map(this.pipe.getType(_)))

    val parseString = nParse.zip(nLabels).zipWithIndex.drop(1).map {
      case ((p, l), i) => "%d|%d:%d".format(p, i, l)
    }.mkString(" ")

    orig(0) = (this.pipe.createFeatureVector(instance), parseString)
    orig
  }

  private def getSibs(ch: Int, par: Seq[Int]): (Int, Int) = ((
    if (par(ch) > ch) ch + 1 until par(ch)
    else ch - 1 until par(ch) by -1
  ).find(par(ch) == par(_)).getOrElse(par(ch)), (
    if (par(ch) < ch) ch + 1 until par.length
    else ch - 1 to 0 by -1
  ).find(par(ch) == par(_)).getOrElse(ch))

  private def allSibs(parse: Seq[Int]) =
    IndexedSeq.tabulate(parse.size, parse.size) {
      case (0, _) => (0, 0)
      case (i, j) => this.getSibs(i, parse.updated(i, j))
    }

  protected def oldAllSibs(parse: Array[Int]) = {
    val (as, bs) = this.allSibs(parse).map(_.unzip).unzip
    (as.map(_.toArray).toArray, bs.map(_.toArray).toArray)
  }

  /*protected def score(
    probs: Array[Array[Array[Double]]],
    probsTr: Array[Array[Array[Double]]],
    probsSi: Array[Array[Array[Double]]],
    probsNt: Array[Array[Array[Array[Double]]]],
    parse: IndexedSeq[Int],
    labels: IndexedSeq[Int]
  ) = {*/

  protected def rearrange(
    probs: Array[Array[Array[Double]]],
    probsTr: Array[Array[Array[Double]]],
    probsSi: Array[Array[Array[Double]]],
    probsNt: Array[Array[Array[Array[Double]]]],
    parse: IndexedSeq[Int],
    labels: IndexedSeq[Int]
  ) = {
    val len = parse.size
    val staticTypes = if (this.pipe.getLabeled) Some(this.getTypes(probsNt, len)) else None

    var isChild = this.calcChilds(parse)

    var max = Double.PositiveInfinity

    val nParse = parse.toBuffer
    val nLabels = labels.toBuffer

    while (max > 0.0) {
      max = Double.NegativeInfinity

      var wh = -1
      var nP = -1
      var nL = -1

      val sibs = this.allSibs(nParse)
      /*IndexedSeq.tabulate(len, len) {
        case (0, _) => (0, 0)
        case (i, j) => this.getSibs(i, nParse.updated(i, j))
      }*/

      val pairs = nParse.zipWithIndex.toIndexedSeq

      pairs.tail.foreach { case (p, i) =>
        // Calculate change of removing edge.
        val (a, b) = sibs(i)(p)
        val lDir = i < p

        val change0 =
            probs(if (lDir) i else p)(if (lDir) p else i)(if (lDir) 1 else 0)
          + probsTr(p)(a)(i)
          + probsSi(a)(i)(if (a == p) 0 else 1)
          + ( if (b != i)
              probsTr(p)(i)(b)
            + probsSi(i)(b)(1)
            - probsTr(p)(a)(b)
            - probsSi(a)(b)(if (a == p) 0 else 1)
              else 0.0
            )
          + ( if (this.pipe.getLabeled)
              probsNt(i)(nLabels(i))(if (lDir) 1 else 0)(0)
            + probsNt(p)(nLabels(i))(if (lDir) 1 else 0)(1)
              else 0.0
            )
        //System.err.println(change0)

        pairs.filter {
          case (_, j) => j != i && j != p && !isChild(i)(j)
        }.foreach { case (_, j) =>
          val (a, b) = sibs(i)(j)
          val lDir = i < j

          val change1 =
              probs(if (lDir) i else j)(if (lDir) j else i)(if (lDir) 1 else 0)
            + probsTr(j)(a)(i)
            + probsSi(a)(i)(if (a == j) 0 else 1)
            + ( if (b != i)
                probsTr(j)(i)(b)
              + probsSi(i)(b)(1)
              - probsTr(j)(a)(b)
              - probsSi(a)(b)(if (a == j) 0 else 1)
                else 0.0
              )
            + staticTypes.map(ts =>
                probsNt(i)(ts(j)(i))(if (lDir) 1 else 0)(0)
              + probsNt(j)(ts(j)(i))(if (lDir) 1 else 0)(1)
            ).getOrElse(0.0)

          if (max < change1 - change0) {
            max = change1 - change0
            wh = i
            nP = j
            nL = staticTypes.map(_(j)(i)).getOrElse(0)
            System.err.println("N: " + wh + " " + nP)
          }
        }
      }
      //System.err.println()

      if (max > 0.0) {
        nParse(wh) = nP
        nLabels(wh) = nL
        isChild = this.calcChilds(nParse)
      }
    }
    System.err.println()
    (nParse.toIndexedSeq, nLabels.toIndexedSeq)
  }

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

