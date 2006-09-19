package mstparser;

import gnu.trove.*;
import java.util.*;

public class FeatureVector {

    public int index;
    public double value;
    public FeatureVector next;
	

    public FeatureVector(int[] keys) {
	FeatureVector result = new FeatureVector();
	for (int i=0; i<keys.length-1; i++)
	    result = new FeatureVector(keys[i],1.0,result);

	index = keys[keys.length-1];
	value = 1.0;
	next = result;
    }

    public FeatureVector() {
	index = -1;
	value = -1.0;
	next = null;
    }
    
    public FeatureVector(int i, double v, FeatureVector n) {
	index = i;
	value = v;
	next = n;
    }

    public int[] keys() {
	TIntArrayList keys = new TIntArrayList();
	for(FeatureVector curr = this; curr.next != null; curr = curr.next) {
	    if(curr.index < 0)
		continue;
	    keys.add(curr.index);
	}
	return keys.toNativeArray();

    }

    public FeatureVector cat(FeatureVector fv2) {
	FeatureVector result = new FeatureVector();
	for(FeatureVector curr = this; curr.next != null; curr = curr.next) {
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
    public FeatureVector getDistVector(FeatureVector fv2) {
	FeatureVector result = new FeatureVector();
	for(FeatureVector curr = this; curr.next != null; curr = curr.next) {
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
	
    public double dotProduct(FeatureVector fv2) {
	double result = 0.0;
	TIntDoubleHashMap hm1 = new TIntDoubleHashMap();
	TIntDoubleHashMap hm2 = new TIntDoubleHashMap();

	for(FeatureVector curr = this; curr.next != null; curr = curr.next) {
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
	FeatureVector result = new FeatureVector();
	for(int i = feats.length-1; i >= 0; i--)
	    result = new FeatureVector(feats[i],1.0,result);
	this.index = result.index; this.value = result.value; this.next = result.next;
    }

    public String toString() {
	if(next == null)
	    return ""+index;
	return index + " " + next.toString();
    }

}
