package mstparser.old;

import scala.Tuple2;

public abstract class DependencyDecoder2O extends DependencyDecoder {
  protected abstract Tuple2<int[][], int[][]> oldAllSibs(int[] par);

  protected Tuple2<int[], int[]> rearrangex(
    double[][][] probs,
    double[][][] probsTr,
    double[][][] probsSi,
    double[][][][] probsNt,
    int[] parse,
    int[] labels
  ) { 
    int len = parse.length;
    int[][] staticTypes = this.pipe().getLabeled() ? this.getTypes(probsNt, len) : null;

    //boolean[][] isChild = this.oldCalcChilds(parse);
    
    double max = Double.POSITIVE_INFINITY;

    int[] nParse = (int[]) parse.clone();
    int[] nLabels = (int[]) labels.clone();

    while (max > 0.0) {
      max = Double.NEGATIVE_INFINITY;

      int wh = -1;
      int nP = -1;
      int nL = -1;

      Tuple2<int[][], int[][]> ss = this.oldAllSibs(nParse);
      int[][] aSibs = ss._1();
      int[][] bSibs = ss._2();
      




      for (int i = 1; i < nParse.length; i++) { int p = nParse[i];
        // Calculate change of removing edge
        int a = aSibs[i][p]; int b = bSibs[i][p];
        boolean lDir = i < p;

        double change0 =
            probs[lDir ? i : p][lDir ? p : i][lDir ? 1 : 0]
          + probsTr[p][a][i]
          + probsSi[a][i][a == p ? 0 : 1]
          + (b != i ?
              probsTr[p][i][b]
            + probsSi[i][b][1] 
            - probsTr[p][a][b]
            - probsSi[a][b][a == p ? 0 : 1]
          : 0.0)

          + (this.pipe().getLabeled() ?
              probsNt[i][nLabels[i]][lDir ? 1 : 0][0]
            + probsNt[p][nLabels[i]][lDir ? 1 : 0][1]
          : 0.0);

        //System.err.println(change);

        for (int j = 0; j < nParse.length; j++) {
          if (i == j || j == p || this.oldCalcChilds(nParse)[i][j]) continue;

          a = aSibs[i][j]; b = bSibs[i][j];
          lDir = i < j;

          double change1 =
              probs[lDir ? i : j][lDir ? j : i][lDir ? 1 : 0]
            + probsTr[j][a][i]
            + probsSi[a][i][a == j ? 0 : 1]
            + (b != i ?
                probsTr[j][i][b]
              + probsSi[i][b][1]
              - probsTr[j][a][b]
              - probsSi[a][b][a == j ? 0 : 1]
              : 0.0
              )
            + (this.pipe().getLabeled() ? 
                probsNt[i][staticTypes[j][i]][lDir ? 1 : 0][0]
              + probsNt[j][staticTypes[j][i]][lDir ? 1 : 0][1]
              : 0.0);

          if (max < change1 - change0) {
            max = change1 - change0;
            wh = i;
            nP = j;
            nL = this.pipe().getLabeled() ? staticTypes[j][i] : 0;
            System.err.println("O: " + wh + " " + nP);
          }
        }
      }
      //System.err.println();

      if (max > 0.0) {
        nParse[wh] = nP;
        nLabels[wh] = nL;
        //isChild = this.oldCalcChilds(nParse);
      }
      //System.out.println(max + " " + wh + " " + nPar + " " + nType);
    }
    System.err.println();
    return new Tuple2<int[], int[]>(nParse, nLabels);
  }
}

