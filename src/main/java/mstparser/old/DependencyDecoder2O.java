package mstparser.old;

import scala.Tuple2;

public abstract class DependencyDecoder2O extends DependencyDecoder {
  protected abstract Tuple2<Integer, Integer> oldGetSibs(int ch, int[] par);

  protected Tuple2<int[], int[]> rearrangex(double[][][] probs,
			   double[][][] probs_trips,
			   double[][][] probs_sibs, double[][][][] nt_probs, int[] par, int[] labs) {
		
	int[][] static_types = null;
	if (this.pipe().getLabeled()) static_types = getTypes(nt_probs,par.length);

	boolean[][] isChild = oldCalcChilds(par);
	boolean[][] isCross = null;
		
	while (true) {
	    int wh = -1;
	    int nPar = -1;
	    int nType = -1;
	    double max = Double.NEGATIVE_INFINITY;
	    int[][] aSibs = new int[par.length][par.length];
	    int[][] bSibs = new int[par.length][par.length];
	    for(int i = 1; i < par.length; i++) {
		for(int j = 0; j < par.length; j++) {
		    int oP = par[i];
		    par[i] = j;
		    Tuple2<Integer, Integer> sibs = oldGetSibs(i,par);
		    aSibs[i][j] = sibs._1(); bSibs[i][j] = sibs._2();
		    par[i] = oP;
		}
	    }
	    for(int ch = 1; ch < par.length; ch++) {
		// Calculate change of removing edge
		int aSib = aSibs[ch][par[ch]]; int bSib = bSibs[ch][par[ch]];
		boolean lDir = ch < par[ch];
		double change = 0.0 - probs[lDir ? ch : par[ch]][lDir ? par[ch] : ch][lDir ? 1 : 0]
		    - probs_trips[par[ch]][aSib][ch] - probs_sibs[aSib][ch][aSib == par[ch] ? 0 : 1]
		    - (bSib != ch ? probs_trips[par[ch]][ch][bSib] + probs_sibs[ch][bSib][1] : 0.0)
		    - (this.pipe().getLabeled() ? (nt_probs[ch][labs[ch]][lDir ? 1 : 0][0] + nt_probs[par[ch]][labs[ch]][lDir ? 1 : 0][1]) : 0.0)
		    + (bSib != ch ? probs_trips[par[ch]][aSib][bSib] + probs_sibs[aSib][bSib][aSib == par[ch] ? 0 : 1] : 0.0);
        System.err.println(change);
		for(int pa = 0; pa < par.length; pa++) {
		    if(ch == pa || pa == par[ch] || isChild[ch][pa]) continue;
		    aSib = aSibs[ch][pa]; bSib = bSibs[ch][pa];
		    boolean lDir1 = ch < pa;
		    double change1 = 0.0 + probs[lDir1 ? ch : pa][lDir1 ? pa : ch][lDir1 ? 1 : 0]
			+ probs_trips[pa][aSib][ch] + probs_sibs[aSib][ch][aSib == pa ? 0 : 1]
			+ (bSib != ch ? probs_trips[pa][ch][bSib] + probs_sibs[ch][bSib][1] : 0.0)
			+ (this.pipe().getLabeled() ? (nt_probs[ch][static_types[pa][ch]][lDir1 ? 1 : 0][0] + nt_probs[pa][static_types[pa][ch]][lDir1 ? 1 : 0][1]) : 0.0)
			- (bSib != ch ? probs_trips[pa][aSib][bSib] + probs_sibs[aSib][bSib][aSib == pa ? 0 : 1] : 0.0);
		    if(max < change+change1) {
			max = change+change1; wh = ch; nPar = pa; nType = this.pipe().getLabeled() ? static_types[pa][ch] : 0;
		    }
		}
	    }
      System.err.println();
	    if(max <= 0.0) break;
	    par[wh] = nPar;
	    labs[wh] = nType;
	    isChild = oldCalcChilds(par);
	    //System.out.println(max + " " + wh + " " + nPar + " " + nType);
	}
  return new Tuple2<int[], int[]>(par, labs);
    }
}

