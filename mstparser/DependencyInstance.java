package mstparser;

import gnu.trove.*;
import java.util.*;

public class DependencyInstance {

    public FeatureVector fv;
    public String actParseTree;
    public int length;

    private THashMap typesToData = new THashMap();

    public DependencyInstance() {}
    
    public DependencyInstance(int length) { this.length = length; }

    public DependencyInstance(String[] sentence, FeatureVector fv) {
	this(sentence.length);
	put("tokens",sentence);
	this.fv = fv;
    }
    
    public DependencyInstance(String[] sentence, String[] pos, FeatureVector fv) {
	this(sentence, fv);
	put("pos",pos);
    }
    
    public DependencyInstance(String[] sentence, String[] pos, 
			      String[] labs, FeatureVector fv) {
	this(sentence, pos, fv);
	put("labels",labs);
    }

    public DependencyInstance(String[] sentence, String[] pos, 
			      String[] labs, String[] deps) {
	put("tokens",sentence);
	put("pos",pos);
	put("labels",labs);
	put("deps",deps);
    }

    public void setFeatureVector (FeatureVector fv) {
	this.fv = fv;
    }


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

    public String[] keys () {
	String[] keys = new String[typesToData.size()];
	typesToData.keySet().toArray(keys);
	return keys;
    }

    public int numFeatureClasses () {
	return typesToData.size();
    }

    public int length () {
	return length;
    }

    
}
