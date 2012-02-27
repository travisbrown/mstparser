package mstparser.old;

import java.io.*;
import java.util.ArrayList;
import java.util.ListIterator;
import gnu.trove.*;

public abstract class Parameters {
    protected double[] hildreth(mstparser.FeatureVector[] a, double[] b) {
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
}

