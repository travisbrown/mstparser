package mstparser

class Parameters(size: Int) extends old.Parameters {
  var parameters = Array.ofDim[Double](size)
  private val total = Array.ofDim[Double](size)

  private var lossType = "punc"

  def updateParamsMIRA(instance: DependencyInstance, d: Array[(FeatureVector, String)], update: Double) {
    val score = this.getScore(instance.getFeatureVector)

    val (b, dist) = d.takeWhile(_._1 != null).map { case (f, p) => (
      this.numErrors(instance, p, instance.getParseTree) - (score - this.getScore(f)),
      instance.getFeatureVector.getDistVector(f)
    )}.unzip

    dist.zip(this.hildreth(dist.toArray, b.toArray)).foreach { case (f, a) =>
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

}

/*    private double SCORE = 0.0;

    public double[] parameters;
    public double[] total;
    public String lossType = "punc";

    public Parameters(int size) { 	
	parameters = new double[size];
	total = new double[size];
	for(int i = 0; i < parameters.length; i++) {
	    parameters[i] = 0.0;
	    total[i] = 0.0;
	}
	lossType = "punc";
    }

    private double[] hildreth(FeatureVector[] a, double[] b) {

	int i;
	int max_iter = 10000;
	double eps = 0.00000001;
	double zero = 0.000000000001;
		
	double[] alpha = new double[b.length];

	double[] F = new double[b.length];
	double[] kkt = new double[b.length];
	double max_kkt = Double.NEGATIVE_INFINITY;

	int K = a.length;
		
	double[][] A = new double[K][K];
	boolean[] is_computed = new boolean[K];
	for(i = 0; i < K; i++) {
	    A[i][i] = a[i].dotProduct(a[i]);
	    is_computed[i] = false;
	}
				
	int max_kkt_i = -1;

		
	for(i = 0; i < F.length; i++) {
	    F[i] = b[i];
	    kkt[i] = F[i];
	    if(kkt[i] > max_kkt) { max_kkt = kkt[i]; max_kkt_i = i; }
	}

	int iter = 0;
	double diff_alpha;
	double try_alpha;
	double add_alpha;
	
	while(max_kkt >= eps && iter < max_iter) {
			
	    diff_alpha = A[max_kkt_i][max_kkt_i] <= zero ? 0.0 : F[max_kkt_i]/A[max_kkt_i][max_kkt_i];
	    try_alpha = alpha[max_kkt_i] + diff_alpha;
	    add_alpha = 0.0;

	    if(try_alpha < 0.0)
		add_alpha = -1.0 * alpha[max_kkt_i];
	    else
		add_alpha = diff_alpha;

	    alpha[max_kkt_i] = alpha[max_kkt_i] + add_alpha;

	    if (!is_computed[max_kkt_i]) {
		for(i = 0; i < K; i++) {
		    A[i][max_kkt_i] = a[i].dotProduct(a[max_kkt_i]); // for version 1
		    is_computed[max_kkt_i] = true;
		}
	    }

	    for(i = 0; i < F.length; i++) {
		F[i] -= add_alpha * A[i][max_kkt_i];
		kkt[i] = F[i];
		if(alpha[i] > zero)
		    kkt[i] = Math.abs(F[i]);
	    }

	    max_kkt = Double.NEGATIVE_INFINITY;
	    max_kkt_i = -1;
	    for(i = 0; i < F.length; i++)
		if(kkt[i] > max_kkt) { max_kkt = kkt[i]; max_kkt_i = i; }

	    iter++;
	}

	return alpha;
    }

 double numErrorsDepNoPunc(DependencyInstance inst, String pred, String act) {
		
	String[] act_spans = act.split(" ");
	String[] pred_spans = pred.split(" ");
	
	String[] pos = inst.postags;
	
	int correct = 0;
	int numPunc = 0;

	for(int i = 0; i < pred_spans.length; i++) {
	    String p = pred_spans[i].split(":")[0]; String a = act_spans[i].split(":")[0];
	    if(pos[i+1].matches("[,:.'`]+")) {
		numPunc++;
		continue;
	    }
	    if(p.equals(a)) {
		correct++;
	    }
	}		

	return ((double)act_spans.length - numPunc - correct);
		
    }
	
    public double numErrorsLabelNoPunc(DependencyInstance inst, String pred, String act) {
		
	String[] act_spans = act.split(" ");
	String[] pred_spans = pred.split(" ");

	String[] pos = inst.postags;
	
	int correct = 0;
	int numPunc = 0;

	for(int i = 0; i < pred_spans.length; i++) {
	    String p = pred_spans[i].split(":")[1]; String a = act_spans[i].split(":")[1];
	    if(pos[i+1].matches("[,:.'`]+")) {
		numPunc++;
		continue;
	    }
	    if(p.equals(a)) {
		correct++;
	    }
	}		

	return ((double)act_spans.length - numPunc - correct);
		
    }
	
}*/

