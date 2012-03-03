package mstparser

import gnu.trove.map.TIntIntMap
import gnu.trove.map.hash.TIntIntHashMap
import gnu.trove.procedure.TIntIntProcedure

class DependencyDecoder(protected val pipe: DependencyPipe) extends Decoder

trait Decoder extends old.DependencyDecoder {
  protected def getTypes(probsNt: Array[Array[Array[Array[Double]]]], len: Int) =
    Array.tabulate(len, len) {
      case (i, j) if i == j => 0
      case (i, j) => probsNt(i).zip(probsNt(j)).zipWithIndex.maxBy {
        case ((pi, pj), k) => if (i < j) pi(0)(1) + pj(0)(0) else pi(1)(1) + pj(1)(0)
      }._2
		}

  protected def calcChilds(par: Array[Int]) = {
    val isChild = Array.ofDim[Boolean](par.length, par.length)

    par.zipWithIndex.drop(1).foreach { case (v, i) =>
	    var l = v
	    while (l != -1) {
        isChild(l)(i) = true
        l = par(l)
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

  /*def decodeNonProjective(
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
    val scoreMatrix = Array.ofDim[Double](len, len)
    val scoreMatrixOrig = Array.ofDim[Double](len, len)
    val nodes = Array.fill(len)(true)
    val reps = Array.fill(len) {
      val rep = new TIntIntHashMap
      (0 until len).foreach(rep.put(i, 0))
      rep
    }

    val scoreMatrix = Array.tabulate(len, len) {
      case (i, j) if i < j =>
        probs(i)(j)(0) + (if (this.pipe.getLabeled) {
          probsNt(i)(staticTypes(i)(j))(0)(1) + probsNt(j)(staticTypes(i)(j))(0)(0)
        } else 0.0)

      case (i, j) =>
        probs(j)(i)(1) + (if (this.pipe.getLabeled) {
          probsNt(i)(staticTypes(i)(j))(1)(1) + probsNt(j)(staticTypes(i)(j))(1)(0)
        } else 0.0)
    }

    val scoreMatrixOrig = scoreMatrix.map(_.clone)

    val par = Array.ofDim[Int](len)
    this.chuLiuEdmonds(
      scoreMatrix, nodes, oldI, oldO, false, new TIntIntHashMap, reps
    ).forEachEntry(new TIntIntProcedure {
      def execute(k: Int, v: Int) = {
        par(k) = v
        true
      }
    })

	  val parN = this.getKChanges(par, orig_scoreMatrix, math.min(k, par.length))
	  val newK = 1 + parN.filter(_ > -1).size

	// Create Feature Vectors;
	int[][] fin_par = new int[new_k][len];
	FeatureVector[][] fin_fv = new FeatureVector[new_k][len];
	fin_par[0] = par;
	int c = 1;
	for(int i = 0; i < n_par.length; i++) {
	    if(n_par[i] > -1) {
		int[] t_par = new int[par.length];
		for(int j = 0; j < t_par.length; j++)
		    t_par[j] = par[j];
		t_par[i] = n_par[i];
		fin_par[c] = t_par;
		c++;
	    }
	}

	for(int k = 0; k < fin_par.length; k++) {
	    for(int i = 0; i < fin_par[k].length; i++) {
		int ch = i; int pr = fin_par[k][i];
		if(pr != -1) {
		    fin_fv[k][ch] = fvs[ch < pr ? ch : pr][ch < pr ? pr : ch][ch < pr ? 1 : 0];
		    if(this.pipe().getLabeled()) {
			fin_fv[k][ch] = 
			    fin_fv[k][ch].cat(nt_fvs[ch][static_types[pr][ch]][ch < pr ? 1 : 0][0]);
			fin_fv[k][ch] = 
			    fin_fv[k][ch].cat(nt_fvs[pr][static_types[pr][ch]][ch < pr ? 1 : 0][1]);
		    }
		}
		else
		    fin_fv[k][ch] = new FeatureVector();
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
  }*/
}

/*    private int[] getKChanges(int[] par, double[][] scoreMatrix, int K) {
	int[] result = new int[par.length];
	int[] n_par = new int[par.length];
	double[] n_score = new double[par.length];
	for(int i = 0; i < par.length; i++) {
	    result[i] = -1;
	    n_par[i] = -1;
	    n_score[i] = Double.NEGATIVE_INFINITY;
	}

	boolean[][] isChild = calcChilds(par);

	for(int i = 1; i < n_par.length; i++) {
	    double max = Double.NEGATIVE_INFINITY;
	    int wh = -1;
	    for(int j = 0; j < n_par.length; j++) {
		if(i == j || par[i] == j || isChild[i][j]) continue;
		if(scoreMatrix[j][i] > max) { max = scoreMatrix[j][i]; wh = j; }
	    }
	    n_par[i] = wh;
	    n_score[i] = max;
	}

	for(int k = 0; k < K; k++) {
	    double max = Double.NEGATIVE_INFINITY;
	    int wh = -1;
	    int whI = -1;
	    for(int i = 0; i < n_par.length; i++) {
		if(n_par[i] == -1) continue;
		double score = scoreMatrix[n_par[i]][i];
		if(score > max) {
		    max = score; whI = i; wh = n_par[i];
		}
	    }

	    if(max == Double.NEGATIVE_INFINITY)
		break;
	    result[whI] = wh;
	    n_par[whI] = -1;
	}

	return result;
    }

    private static TIntIntHashMap chuLiuEdmonds(double[][] scoreMatrix, boolean[] curr_nodes, 
						int[][] oldI, int[][] oldO, boolean print,
						TIntIntHashMap final_edges, TIntIntHashMap[] reps) {
		
	// need to construct for each node list of nodes they represent (here only!)
		
	int[] par = new int[curr_nodes.length];
	int numWords = curr_nodes.length;
		
	// create best graph
	par[0] = -1;
	for(int i = 1; i < par.length; i++) {
	    // only interested in current nodes
	    if(!curr_nodes[i]) continue;
	    double maxScore = scoreMatrix[0][i];
	    par[i] = 0;
	    for(int j = 0; j < par.length; j++) {
		if(j == i) continue;
		if(!curr_nodes[j]) continue;
		double newScore = scoreMatrix[j][i];
		if(newScore > maxScore) {
		    maxScore = newScore;
		    par[i] = j;
		}
	    }
	}
		
	if(print) {
	    System.out.println("After init");
	    for(int i = 0; i < par.length; i++) {
		if(curr_nodes[i])
		    System.out.print(par[i] + "|" + i + " ");
	    }
	    System.out.println();
	}
		
	//Find a cycle
	ArrayList cycles = new ArrayList();
	boolean[] added = new boolean[numWords];
	for(int i = 0; i < numWords && cycles.size() == 0; i++) {
	    // if I have already considered this or
	    // This is not a valid node (i.e. has been contracted)
	    if(added[i] || !curr_nodes[i]) continue;
	    added[i] = true;
	    TIntIntHashMap cycle = new TIntIntHashMap();
	    cycle.put(i,0);
	    int l = i;
	    while(true) {
		if(par[l] == -1) {
		    added[l] = true;
		    break;
		}
		if(cycle.contains(par[l])) {
		    cycle = new TIntIntHashMap();
		    int lorg = par[l];
		    cycle.put(lorg,par[lorg]);
		    added[lorg] = true;
		    int l1 = par[lorg];
		    while(l1 != lorg) {
			cycle.put(l1,par[l1]);
			added[l1] = true;
			l1 = par[l1];
						
		    }
		    cycles.add(cycle);
		    break;
		}
		cycle.put(l,0);
		l = par[l];
		if(added[l] && !cycle.contains(l))
		    break;
		added[l] = true;
	    }
	}
		
	// get all edges and return them
	if(cycles.size() == 0) {
	    //System.out.println("TREE:");
	    for(int i = 0; i < par.length; i++) {
		if(!curr_nodes[i]) continue;
		if(par[i] != -1) {
		    int pr = oldI[par[i]][i];
		    int ch = oldO[par[i]][i];
		    final_edges.put(ch,pr);
		    //System.out.print(pr+"|"+ch + " ");
		}
		else
		    final_edges.put(0,-1);
	    }
	    //System.out.println();
	    return final_edges;
	}

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
		
		
	for(int i = 0; i < numWords; i++) {
			
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
		
	chuLiuEdmonds(scoreMatrix,curr_nodes,oldI,oldO,print,final_edges,reps);
		
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
    }
}*/

