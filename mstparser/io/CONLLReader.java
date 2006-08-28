package mstparser.io;

import mstparser.DependencyInstance;
import mstparser.Util;

import java.io.*;
import java.util.*;

public class CONLLReader extends DependencyReader {


    public DependencyInstance getNext() throws IOException {

	ArrayList<String[]> lineList = new ArrayList<String[]>();

	String line = inputReader.readLine();
	while (line != null && !line.equals("")) {
	    lineList.add(line.split("\t"));
	    line = inputReader.readLine();
	}

	int length = lineList.size();

	if(length == 0) {
	    inputReader.close();
	    return null;
	}


	String[] toks = new String[length+1];
	String[] pos = new String[length+1];
	String[] labs = new String[length+1];
	int[] deps = new int[length+1];

	toks[0] = "<root>";
	pos[0] = "<root-POS>";
	labs[0] = "<no-type>";
	deps[0] = -1;

	for(int i = 0; i < length; i++) {
	    String[] info = lineList.get(i);
	    toks[i+1] = normalize(info[1]);
	    pos[i+1] = info[4];
	    labs[i+1] = labeled ? info[7] : "<no-type>";
	    deps[i+1] = Integer.parseInt(info[6]);
	}

	return new DependencyInstance(toks, pos, labs, deps);

    }


    protected boolean fileContainsLabels (String file) throws IOException {
	BufferedReader in = new BufferedReader(new FileReader(file));
	String line = in.readLine();
	in.close();

	if(line.trim().length() > 0) 
	    return true;
	else
	    return false;
    }

}
