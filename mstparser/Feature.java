package mstparser;

import gnu.trove.*;

public final class Feature extends TLinkableAdaptor {

    public int index;
    public double value;

    public Feature (int i, double v) {
	index = i;
	value = v;
    }

    public final Feature clone () {
	return new Feature(index, value);
    }

    public final Feature negation () {
	return new Feature(index, -value);
    }

    public final String toString() {
	return index+"="+value;
    }

}
