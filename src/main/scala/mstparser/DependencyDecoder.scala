package mstparser

import scala.collection.mutable.Buffer
import com.google.common.collect.MinMaxPriorityQueue

class DependencyDecoder(protected val pipe: DependencyPipe) {
  protected def getTypes(probsNt: Array[Array[Array[Array[Double]]]], len: Int) =
    Array.tabulate(len, len) {
      case (i, j) if i == j => 0
      case (i, j) => probsNt(i).zip(probsNt(j)).zipWithIndex.maxBy {
        case ((pi, pj), _) => if (i < j) pi(0)(1) + pj(0)(0) else pi(1)(1) + pj(1)(0)
      }._2
    }

  protected def calcChilds(parse: Seq[Int]): Seq[Seq[Boolean]] = {
    val isChild = IndexedSeq.fill(parse.size)(Buffer.fill(parse.size)(false))

    parse.zipWithIndex.drop(1).foreach { case (v, i) =>
	    var l = v
	    while (l != -1) {
        isChild(l)(i) = true
        l = parse(l)
      }
    }
    isChild
  }

  // static type for each edge: run time O(n^3 + Tn^2) T is number of types
  def decodeProjective(
    len: Int,
    fvs: Array[Array[Array[FeatureVector]]],
    probs: Array[Array[Array[Double]]],
    fvsNt: Array[Array[Array[Array[FeatureVector]]]],
    probsNt: Array[Array[Array[Array[Double]]]],
    kBest: Int
  ) = {
	  val staticTypes = if (this.pipe.getLabeled) Some(this.getTypes(probsNt, len)) else None
    val pf = new KBestParseForest(len - 1, kBest)

    (0 until len).foreach { i =>
      pf.add(i, -1, 0, 0.0, new FeatureVector)
      pf.add(i, -1, 1, 0.0, new FeatureVector)
    }

    (1 until len).foreach { j =>
      (0 until len - j).foreach { s =>
        val t = s + j
        val (type1, type2) = staticTypes.map(ts => (ts(s)(t), ts(t)(s))).getOrElse((0, 0))
		
        (s to t).foreach { r =>
          if (r != t) {
            val b1 = pf.getItems(s, r, 0, 0)
            val c1 = pf.getItems(r + 1, t, 1, 0)

            if (b1 != null && c1 != null) {
              pf.getKBestPairs(b1, c1).takeWhile {
                case (comp1, comp2) => comp1 > -1 && comp2 > -1
              }.foreach {
                case (comp1, comp2) =>
                  val bc = b1(comp1).prob + c1(comp2).prob

                  var finProb = bc + probs(s)(t)(0)
                  var finFv = fvs(s)(t)(0)
                  if (this.pipe.getLabeled) {
                    finFv = fvsNt(s)(type1)(0)(1).cat(fvsNt(t)(type1)(0)(0).cat(finFv))
                    finProb += probsNt(s)(type1)(0)(1) + probsNt(t)(type1)(0)(0)
                  }
                  pf.add(s, r, t, type1, 0, 1, finProb, finFv, b1(comp1), c1(comp2))

                  finProb = bc + probs(s)(t)(1)
                  finFv = fvs(s)(t)(1)
                  if (this.pipe.getLabeled) {
                    finFv = fvsNt(t)(type2)(1)(1).cat(fvsNt(s)(type2)(1)(0).cat(finFv))
                    finProb += probsNt(t)(type2)(1)(1) + probsNt(s)(type2)(1)(0)
                  }
                  pf.add(s, r, t, type2, 1, 1, finProb, finFv, b1(comp1), c1(comp2))
              }
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

  protected def getKChanges(parse: Array[Int], scores: Array[Array[Double]], k: Int) = {
    val result = Array.fill(parse.size)(-1)
    val isChild = this.calcChilds(parse)

    val nParse = (-1 +: parse.zipWithIndex.tail.map { case (p, i) =>
      val valid = (0 until parse.size).filter(j =>
        i != j && p != j && !isChild(i)(j)
      )
      if (valid.isEmpty) -1 else valid.maxBy(j => scores(j)(i))
    }).toBuffer

    (0 until k).foreach { i =>
      val pairs = nParse.zipWithIndex.filter(_._1 > -1)
      if (!pairs.isEmpty) {
        val (mp, mi) = pairs.maxBy { case (np, ni) =>
          scores(np)(ni)
        }

        result(mi) = mp
        nParse(mi) = -1
      }
    }

    result
  }

  def decodeNonProjective(
    len: Int,
    fvs: Array[Array[Array[FeatureVector]]],
    probs: Array[Array[Array[Double]]],
    fvsNt: Array[Array[Array[Array[FeatureVector]]]],
    probsNt: Array[Array[Array[Array[Double]]]],
    kBest: Int
  ): Seq[(FeatureVector, String)] = {
    val staticTypes = if (this.pipe.getLabeled) Some(this.getTypes(probsNt, len)) else None

    val scores = Array.tabulate(len, len) {
      case (i, j) if i < j =>
        probs(i)(j)(0) + staticTypes.map(ts =>
          probsNt(i)(ts(i)(j))(0)(1) + probsNt(j)(ts(i)(j))(0)(0)
        ).getOrElse(0.0)

      case (i, j) =>
        probs(j)(i)(1) + staticTypes.map(ts =>
          probsNt(i)(ts(i)(j))(1)(1) + probsNt(j)(ts(i)(j))(1)(0)
        ).getOrElse(0.0)
    }

    val parse = Array.ofDim[Int](len)
    this.chuLiuEdmonds(scores).foreach { case (i, v) => parse(i) = v }

    val parFin = parse +: this.getKChanges(
      parse, scores, math.min(kBest, parse.length)
    ).zipWithIndex.filter(_._1 > -1).map {
      case (p, i) => parse.updated(i, p)
    }

    val fvFin = parFin.map { p =>
      p.zipWithIndex.map {
        case (-1, _) => new FeatureVector
        case (j, i)  =>
          val f = fvs(if (i < j) i else j)(if (i < j) j else i)(if (i < j) 1 else 0)
          staticTypes.map(ts =>
            f.cat(fvsNt(i)(ts(j)(i))(if (i < j) 1 else 0)(0)).cat(
                  fvsNt(j)(ts(j)(i))(if (i < j) 1 else 0)(1))
          ).getOrElse(f)
      }
    }

    val fin = Array.fill(parFin.size)(new FeatureVector)
    val result = Array.fill(parFin.size)("")
    fvFin.zip(parFin).zipWithIndex.foreach { case ((fvs, parses), i) =>
      fvs.zip(parses).zipWithIndex.drop(1).foreach { case ((fv, parse), j) =>
        fin(i) = fv.cat(fin(i))
        result(i) += "%d|%d:%d ".format(parse, j, staticTypes.map(_(parse)(j)).getOrElse(0))
      }
    }

    fin.zip(result.map(_.trim))
  }

  def chuLiuEdmonds(scores: Array[Array[Double]]) = {
    def cLE(
      scores: Array[Array[Double]],
      oldI: Array[Array[Int]],
      oldO: Array[Array[Int]],
      nodes: Set[Int],
      edges: Map[Int, Int],
      reps: IndexedSeq[Set[Int]],
      printing: Boolean
    ): Map[Int, Int] = {
      // Need to construct for each node list of nodes they represent (here only!)
      val len = scores.size
 
      val is = (0 until len)
      val as = is.filter(nodes)

      val parse = -1 +: is.tail.map {
        case i if !nodes(i) => 0
        case i => as.filter(_ != i).maxBy(scores(_)(i))
      }

      val cycles = Buffer.empty[Set[Int]]
      val added = collection.mutable.Set.empty[Int]

      var i = 0
      while (i < len && cycles.isEmpty) {
        if (!added(i) && nodes(i)) {
          added += i
          val cycle = collection.mutable.Set(i)

          var j = i
          var found = false
          while (!found) {
            if (parse(j) == -1) {
              added += j
              found = true
            } else {
              if (cycle(parse(j))) {
                cycle.clear()
                val k = parse(j)
                cycle += k
                added += k

                var l = parse(k)
                while (l != k) {
                  cycle += l
                  added += l
                  l = parse(l)
                }

                cycles += cycle.toSet
                found = true
              } else {
                cycle += j
                j = parse(j)
                if (added(j) && !cycle(j))
                  found = true
                else
                  added(j) = true
              }
            }
          }
        }
        i += 1
      }
      
      if (cycles.isEmpty) {
        edges ++
        parse.zipWithIndex.filter(pi => nodes(pi._2)).map {
          case (-1, _) => 0 -> -1
          case (p, i)  => oldO(p)(i) -> oldI(p)(i)
        }.toMap
      }
      else {
        val biggest = cycles.maxBy(_.size)
        val cNodes = biggest.toSeq.sorted.reverse
        val weight = cNodes.map(i => scores(parse(i))(i)).sum

        as.filterNot(biggest.contains).foreach { i =>
          val (ai, aw) = cNodes.map(j => j -> scores(j)(i)).maxBy(_._2)
          val (bi, bw) = cNodes.map(j => j -> (weight + scores(i)(j) - scores(parse(j))(j))).maxBy(_._2)

          scores(cNodes.head)(i) = aw
          oldI(cNodes.head)(i) = oldI(ai)(i)
          oldO(cNodes.head)(i) = oldO(ai)(i)
          
          scores(i)(cNodes.head) = bw
          oldI(i)(cNodes.head) = oldI(i)(bi)
          oldO(i)(cNodes.head) = oldO(i)(bi)
        }

        val nRep = cNodes.tail.map(reps).foldLeft(reps(cNodes.head))(_ ++ _)
 
        val nEdges = cLE(
          scores, oldI, oldO,
          nodes -- cNodes.tail,
          edges,
          reps.updated(cNodes.head, nRep),
          printing
        )

        // TODO: Could be more efficient, since we don't need the whole
        // intersection.
        val n = cNodes.find(i => !reps(i).intersect(nEdges.keySet).isEmpty).getOrElse(-1)

        nEdges ++ Stream.iterate(parse(n))(parse(_)).takeWhile(_ != n).map(i =>
          oldO(parse(i))(i) -> oldI(parse(i))(i)
        ).toMap
      }
    }

    val len = scores.size

    cLE(
      scores.map(_.clone),
      Array.tabulate(len, len)((i, _) => i),
      Array.tabulate(len, len)((_, j) => j),
      (0 until len).toSet,
      Map.empty[Int, Int],
      IndexedSeq.tabulate(len)(Set(_)),
      false
    )
  }
}

