package mstparser

import gnu.trove.map.TIntIntMap
import gnu.trove.map.hash.TIntIntHashMap
import gnu.trove.procedure.TIntIntProcedure

import scala.collection.mutable.Buffer
import com.google.common.collect.MinMaxPriorityQueue

class DependencyDecoder(protected val pipe: DependencyPipe) { // extends old.DependencyDecoder {
  protected def getTypes(probsNt: Array[Array[Array[Array[Double]]]], len: Int) =
    Array.tabulate(len, len) {
      case (i, j) if i == j => 0
      case (i, j) => probsNt(i).zip(probsNt(j)).zipWithIndex.maxBy {
        case ((pi, pj), _) => if (i < j) pi(0)(1) + pj(0)(0) else pi(1)(1) + pj(1)(0)
      }._2
    }

  protected def calcChilds(parse: Seq[Int]) = {
    val isChild = IndexedSeq.fill(parse.size)(Buffer.fill(parse.size)(false))

    parse.zipWithIndex.drop(1).foreach { case (v, i) =>
	    var l = v
	    while (l != -1) {
        isChild(l)(i) = true
        l = parse(l)
      }
    }
    isChild.map(_.toIndexedSeq)
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
  ) = {
    val staticTypes = if (this.pipe.getLabeled) Some(this.getTypes(probsNt, len)) else None

    val oldI = Array.tabulate(len, len)((i, _) => i)
    val oldO = Array.tabulate(len, len)((_, j) => j)
    val nodes = Array.fill(len)(true)
    val reps = Array.tabulate(len) { i =>
      val rep = new TIntIntHashMap
      rep.put(i, 0)
      rep
    }

    val scoreMatrix = Array.tabulate(len, len) {
      case (i, j) if i < j =>
        probs(i)(j)(0) + staticTypes.map(ts =>
          probsNt(i)(ts(i)(j))(0)(1) + probsNt(j)(ts(i)(j))(0)(0)
        ).getOrElse(0.0)

      case (i, j) =>
        probs(j)(i)(1) + staticTypes.map(ts =>
          probsNt(i)(ts(i)(j))(1)(1) + probsNt(j)(ts(i)(j))(1)(0)
        ).getOrElse(0.0)
    }

    val scoreMatrixOrig = scoreMatrix.map(_.clone)

    val parse = Array.ofDim[Int](len)
    old.DependencyDecoder.chuLiuEdmonds(
      scoreMatrix, nodes, oldI, oldO, false, new TIntIntHashMap, reps
    ).forEachEntry(new TIntIntProcedure {
      def execute(i: Int, v: Int) = {
        parse(i) = v
        true
      }
    })

    val parFin = parse +: this.getKChanges(
      parse, scoreMatrixOrig, math.min(kBest, parse.length)
    ).zipWithIndex.filter(_._1 > -1).map {
      case (p, i) => parse.updated(i, p)
    }
    val newK = parFin.size

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

    val fin = Array.fill(newK)(new FeatureVector)
    val result = Array.fill(newK)("")
    fvFin.zip(parFin).zipWithIndex.foreach { case ((fvs, parses), i) =>
      fvs.zip(parses).zipWithIndex.drop(1).foreach { case ((fv, parse), j) =>
        fin(i) = fv.cat(fin(i))
        result(i) += "%d|%d:%d ".format(parse, j, staticTypes.map(_(parse)(j)).getOrElse(0))
      }
    }

    fin.zip(result.map(_.trim))
  }

  private def chuLiuEdmonds(
    scoreMatrix: Array[Array[Double]],
    nodes: Array[Boolean],
    oldI: Array[Array[Int]],
    oldO: Array[Array[Int]],
    print: Boolean,
    edges: Map[Int, Int],
    reps: Array[TIntIntHashMap]
  ) = {
    // Need to construct for each node list of nodes they represent (here only!)
    val len = nodes.size
  
    val ns = nodes.zipWithIndex
    val nis = ns.filter(_._1).map(_._2)

    val parse = -1 +: ns.tail.map {
      case (false, _) => 0
      case (_, i)     => nis.filter(_ != i).maxBy(scoreMatrix(_)(i))
    }

    if (print) System.err.println("After init" + 
      parse.zipWithIndex.filter(p => nodes(p._2)).map {
        case (p, i) => p + "|" + i
      }.mkString(" ")
    )

  val cycles = Buffer.empty[TIntIntHashMap]
  val added = collection.mutable.Set.empty[Int]

  var i = 0
  while (i < len && cycles.isEmpty) {
    if (!added(i) && nodes(i)) {
      added += i
      val cycle = new TIntIntHashMap
      cycle.put(i, 0)

      var j = i
      var found = false
      while (!found) {
        if (parse(j) == -1) {
          added += j
          found = true
        } else {
          if (cycle.contains(parse(j))) {
            cycle.clear()
            cycle.put(parse(j), parse(parse(j)))
            added += parse(j)

            var k = parse(parse(j))
            while (k != parse(j)) {
              cycle.put(k, parse(k))
              added += k
              k = parse(k)
            }

            cycles += cycle
            found = true
          } else {
            cycle.put(j, 0)
            j = parse(j)
            if (added(j) && !cycle.contains(j)) found = true
            added(j) = true
          }
        }
      }
    }
  }
  }
  /*
  if (cycles.isEmpty) {
    parse.zipWithIndex.filter(p => nodes(p._1)).foreach {
      case (-1, _) => edges.put(0, -1)
      case (p, i)  => edges.put(oldO(p)(i), oldI(p)(i))
    }
    edges
  } else {
    val biggest =  
  int max_cyc = 0;
  int wh_cyc = 0;
  for(int i = 0; i < cycles.size(); i++) {
      TIntIntHashMap cycle = (TIntIntHashMap)cycles.get(i);
      if(cycle.size() > max_cyc) { max_cyc = cycle.size(); wh_cyc = i; }
  }
    
  TIntIntHashMap cycle = (TIntIntHashMap)cycles.get(wh_cyc);
  int[] cyc_nodes = cycle.keys();
  int rep = cyc_nodes[0];
    
  if(print) {
      System.out.println("Found Cycle");
      for(int i = 0; i < cyc_nodes.length; i++)
    System.out.print(cyc_nodes[i] + " ");
      System.out.println();
  }

  double cyc_weight = 0.0;
  for(int j = 0; j < cyc_nodes.length; j++) {
      cyc_weight += scoreMatrix[par[cyc_nodes[j]]][cyc_nodes[j]];
  }
    
    
  for(int i = 0; i < len; i++) {
      
      if(!curr_nodes[i] || cycle.contains(i)) continue;
      
      
      double max1 = Double.NEGATIVE_INFINITY;
      int wh1 = -1;
      double max2 = Double.NEGATIVE_INFINITY;
      int wh2 = -1;
      
      for(int j = 0; j < cyc_nodes.length; j++) {
    int j1 = cyc_nodes[j];
        
    if(scoreMatrix[j1][i] > max1) {
        max1 = scoreMatrix[j1][i];
        wh1 = j1;//oldI[j1][i];
    }
        
    // cycle weight + new edge - removal of old
    double scr = cyc_weight + scoreMatrix[i][j1] - scoreMatrix[par[j1]][j1];
    if(scr > max2) {
        max2 = scr;
        wh2 = j1;//oldO[i][j1];
    }
      }
      
      scoreMatrix[rep][i] = max1;
      oldI[rep][i] = oldI[wh1][i];//wh1;
      oldO[rep][i] = oldO[wh1][i];//oldO[wh1][i];
      scoreMatrix[i][rep] = max2;
      oldO[i][rep] = oldO[i][wh2];//wh2;
      oldI[i][rep] = oldI[i][wh2];//oldI[i][wh2];
      
  }
    
  TIntIntHashMap[] rep_cons = new TIntIntHashMap[cyc_nodes.length];
  for(int i = 0; i < cyc_nodes.length; i++) {
      rep_cons[i] = new TIntIntHashMap();
      int[] keys = reps[cyc_nodes[i]].keys();
      Arrays.sort(keys);
      if(print) System.out.print(cyc_nodes[i] + ": ");
      for(int j = 0; j < keys.length; j++) {
    rep_cons[i].put(keys[j],0);
    if(print) System.out.print(keys[j] + " ");
      }
      if(print) System.out.println();
  }
    
  // don't consider not representative nodes
  // these nodes have been folded
  for(int i = 1; i < cyc_nodes.length; i++) {
      curr_nodes[cyc_nodes[i]] = false;
      int[] keys = reps[cyc_nodes[i]].keys();
      for(int j = 0; j < keys.length; j++)
    reps[rep].put(keys[j],0);
  }
    
  this.chuLiuEdmonds(scoreMatrix, nodes, oldI, oldO, print, edges, reps)
    
  // check each node in cycle, if one of its representatives
  // is a key in the final_edges, it is the one.
  int wh = -1;
  boolean found = false;
  for(int i = 0; i < rep_cons.length && !found; i++) {
      int[] keys = rep_cons[i].keys();
      for(int j = 0; j < keys.length && !found; j++) {
    if(final_edges.contains(keys[j])) {
        wh = cyc_nodes[i];
        found = true;
    }
      }
  }
    
  int l = par[wh];
  while(l != wh) {
      int ch = oldO[par[l]][l];
      int pr = oldI[par[l]][l];
      final_edges.put(ch,pr);
      l = par[l];
  }
    
  if(print) {
      int[] keys = final_edges.keys();
      Arrays.sort(keys);
      for(int i = 0; i < keys.length; i++)
    System.out.print(final_edges.get(keys[i])+"|"+keys[i]+" ");
      System.out.println();
  }

  return final_edges;
  }*/
}

