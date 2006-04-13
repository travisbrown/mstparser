package mstparser;

import gnu.trove.*;
import java.util.*;

public class FeatureVector {

    public int index;
    public double value;
    public FeatureVector next;
	
    public FeatureVector(int i, double v, FeatureVector n) {
	index = i;
	value = v;
	next = n;
    }

    public static FeatureVector cat(FeatureVector fv1, FeatureVector fv2) {
	FeatureVector result = new FeatureVector(-1,-1.0,null);
	for(FeatureVector curr = fv1; curr.next != null; curr = curr.next) {
	    if(curr.index < 0)
		continue;
	    result = new FeatureVector(curr.index,curr.value,result);
	}
	for(FeatureVector curr = fv2; curr.next != null; curr = curr.next) {
	    if(curr.index < 0)
		continue;
	    result = new FeatureVector(curr.index,curr.value,result);
	}
	return result;
		
    }

    // fv1 - fv2
    public static FeatureVector getDistVector(FeatureVector fv1, FeatureVector fv2) {
	FeatureVector result = new FeatureVector(-1,-1.0,null);
	for(FeatureVector curr = fv1; curr.next != null; curr = curr.next) {
	    if(curr.index < 0)
		continue;
	    result = new FeatureVector(curr.index,curr.value,result);
	}
	for(FeatureVector curr = fv2; curr.next != null; curr = curr.next) {
	    if(curr.index < 0)
		continue;
	    result = new FeatureVector(curr.index,-curr.value,result);
	}
	return result;
    }
	
    public static double dotProduct(FeatureVector fv1, FeatureVector fv2) {
	double result = 0.0;
	TIntDoubleHashMap hm1 = new TIntDoubleHashMap();
	TIntDoubleHashMap hm2 = new TIntDoubleHashMap();

	for(FeatureVector curr = fv1; curr.next != null; curr = curr.next) {
	    if(curr.index < 0)
		continue;
	    hm1.put(curr.index,hm1.get(curr.index)+curr.value);
	}
	for(FeatureVector curr = fv2; curr.next != null; curr = curr.next) {
	    if(curr.index < 0)
		continue;
	    hm2.put(curr.index,hm2.get(curr.index)+curr.value);
	}

	int[] keys = hm1.keys();

	for(int i = 0; i < keys.length; i++) {
	    double v1 = hm1.get(keys[i]);
	    double v2 = hm2.get(keys[i]);
	    result += v1*v2;
	}
		
	return result;
		
    }

    public static double oneNorm(FeatureVector fv1) {
	double sum = 0.0;
	for(FeatureVector curr = fv1; curr.next != null; curr = curr.next) {
	    if(curr.index < 0)
		continue;
	    sum += curr.value;
	}
	return sum;
    }
	
    public static double twoNorm(FeatureVector fv1) {
	TIntDoubleHashMap hm = new TIntDoubleHashMap();
	double sum = 0.0;
	for(FeatureVector curr = fv1; curr.next != null; curr = curr.next) {
	    if(curr.index < 0)
		continue;
	    hm.put(curr.index,hm.get(curr.index)+curr.value);
	}
	int[] keys = hm.keys();

	for(int i = 0; i < keys.length; i++)
	    sum += Math.pow(hm.get(keys[i]),2.0);
		
	return Math.sqrt(sum);
    }

    public static FeatureVector twoNormalize(FeatureVector fv1) {
	double norm = twoNorm(fv1);
	FeatureVector result = new FeatureVector(-1,-1.0,null);
	for(FeatureVector curr = fv1; curr.next != null; curr = curr.next) {
	    if(curr.index < 0)
		continue;
	    result = new FeatureVector(curr.index,curr.value/norm,result);
	}
	return result;
    }

    public String toString() {
	if(next == null)
	    return ""+index;
	return index + " " + next.toString();
    }

    public int sum() {
	if(next == null)
	    return index >= 0 ? 1 : 0;
	return (index >= 0 ? 1 : 0) + next.sum();
    }

    public void sort() {
	int[] feats = new int[sum()];
	int j = 0;
	for(FeatureVector curr = this; curr.next != null; curr = curr.next) {
	    if(curr.index < 0)
		continue;
	    feats[j] = curr.index;
	    j++;
	}
	Arrays.sort(feats);
	FeatureVector result = new FeatureVector(-1,-1.0,null);
	for(int i = feats.length-1; i >= 0; i--)
	    result = new FeatureVector(feats[i],1.0,result);
	this.index = result.index; this.value = result.value; this.next = result.next;
    }
	
}
