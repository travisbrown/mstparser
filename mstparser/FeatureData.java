package mstparser;

import gnu.trove.*;
import java.util.*;

public class FeatureData {
    private THashMap typesToData = new THashMap();
    private int length = 0;


    public void put (String type, String[] data) {
	if (length == 0)
	    length = data.length;
	else 
	    if (length != data.length)
		System.out.println(
		   "Something wrong. Trying to add feature class \""+ type + 
		   "\" but the number of items (" + data.length +
		   ") is different from the number input for other feature classes ("
		   + length + "). Will add anyway, but you should expect problems.");
	typesToData.put(type, data);
    }

    public String[] get (String type) {
	return (String[])typesToData.get(type);
    }

    public int numFeatureClasses () {
	return typesToData.size();
    }

    public int length () {
	return length;
    }


}
