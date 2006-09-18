package mstparser;

import gnu.trove.*;
import java.util.*;

public class FeatureVector extends gnu.trove.TIntDoubleHashMap {

    private static final double DEFAULT_VALUE = 1.0;

    //public FeatureVector add(String s, double v) {
    //	index = i;
    //	value = v;
    //}

    public FeatureVector () {
	super();
    }

    public FeatureVector (int[] initialKeys) {
	for (int i=0; i<initialKeys.length; i++)
	    put(initialKeys[i], DEFAULT_VALUE);
    }

    // add an index with the default value
    public void put(int index) {
	put(index, DEFAULT_VALUE);
    }

    public FeatureVector cat(FeatureVector fv2) {
	FeatureVector result = (FeatureVector)clone();

	int[] keys = fv2.keys();
	for (int i=0; i<keys.length; i++) {
	    int id = keys[i];
	    if (containsKey(id))
		result.put(id, get(id) + fv2.get(id));
	    else
		result.put(id, fv2.get(id));
	}

	return result;
		
    }

    // fv1 - fv2
    public FeatureVector getDistVector(FeatureVector fv2) {
	FeatureVector result = (FeatureVector)clone();

	int[] keys = fv2.keys();
	for (int i=0; i<keys.length; i++) {
	    int id = keys[i];
	    if (containsKey(id))
		result.put(id, get(id) - fv2.get(id));
	    else
		result.put(id, -fv2.get(id));
	}

	return result;
		
    }

    public double dotProduct(FeatureVector fv2) {
	double result = 0.0;

	int[] keys = keys();
	for(int i=0; i < keys.length; i++)
	    result += get(keys[i]) * fv2.get(keys[i]);
		
	return result;
    }


    public String toString() {
	return Arrays.toString(keys());
    }

}
