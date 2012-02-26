///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2007 University of Texas at Austin and (C) 2005
// University of Pennsylvania and Copyright (C) 2002, 2003 University
// of Massachusetts Amherst, Department of Computer Science.
//
// This software is licensed under the terms of the Common Public
// License, Version 1.0 or (at your option) any subsequent version.
// 
// The license is approved by the Open Source Initiative, and is
// available from their website at http://www.opensource.org.
///////////////////////////////////////////////////////////////////////////////

package mstparser.old;

import gnu.trove.list.linked.TLinkedList;
import gnu.trove.list.TIntList;
import gnu.trove.list.array.TIntArrayList;
import gnu.trove.map.hash.TIntDoubleHashMap;
import java.util.*;

import mstparser.Feature;


/**
 * A <tt>FeatureVector</tt> that can hold up to two
 * <tt>FeatureVector</tt> instances inside it, which allows for a very
 * quick concatenation operation.
 *
 * <p>Also, in order to avoid copies, the second of these internal
 * <tt>FeatureVector</tt> instances can be negated, so that it has the
 * effect of subtracting any values rather than adding them.
 *
 * <p>
 * Created: Sat Nov 10 15:25:10 2001
 * </p>
 *
 * @author Jason Baldridge
 * @version $Id$
 * @see mstparser.Feature
 */
public abstract class FeatureVector extends TLinkedList<Feature> {
    private FeatureVector subfv1 = null;
    private FeatureVector subfv2 = null;
    private boolean negateSecondSubFV = false;

    public FeatureVector (FeatureVector fv1, FeatureVector fv2, boolean negSecond) {
	subfv1 = fv1;
	subfv2 = fv2;
	negateSecondSubFV = negSecond;
    }

/*    public double dotProduct(FeatureVector fl2) {

	TIntDoubleHashMap hm1 = new TIntDoubleHashMap(this.size());
	addFeaturesToMap(hm1, false);
	hm1.compact();

	TIntDoubleHashMap hm2 = new TIntDoubleHashMap(fl2.size());
	fl2.addFeaturesToMap(hm2, false);
	hm2.compact();

	int[] keys = hm1.keys();

	double result = 0.0;
	for(int i = 0; i < keys.length; i++)
	    result += hm1.get(keys[i])*hm2.get(keys[i]);
		
	return result;
		
    }

    private void addFeaturesToMap(TIntDoubleHashMap map, boolean negate) {
	if (null != subfv1) {
	    subfv1.addFeaturesToMap(map, negate);

	    if (null != subfv2) {
		if (negate) {
		    subfv2.addFeaturesToMap(map, !negateSecondSubFV);
		} else {
		    subfv2.addFeaturesToMap(map, negateSecondSubFV);
		}
	    }
	}

	ListIterator it = listIterator();
	if (negate) {
	    while (it.hasNext()) {
		Feature f = (Feature)it.next();
		if (!map.adjustValue(f.getIndex(), -f.getValue()))
		    map.put(f.getIndex(), -f.getValue());
	    }
	} else {
	    while (it.hasNext()) {
		Feature f = (Feature)it.next();
		if (!map.adjustValue(f.getIndex(), f.getValue()))
		    map.put(f.getIndex(), f.getValue());
	    }
	}
    }


    public final String toString() {
	StringBuilder sb = new StringBuilder();
	toString(sb);
	return sb.toString();
    }

    private final void toString(StringBuilder sb) {
	if (null != subfv1) {
	    subfv1.toString(sb);

	    if (null != subfv2)
		subfv2.toString(sb);
	}
	ListIterator it = listIterator();
	while (it.hasNext())
	    sb.append(it.next().toString()).append(' ');
    }*/

}

