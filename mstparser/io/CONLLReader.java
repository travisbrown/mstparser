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

	String[] forms = new String[length+1];
	String[] lemmas = new String[length+1];
	String[] cpos = new String[length+1];
	String[] pos = new String[length+1];
	String[][] feats = new String[length+1][];
	String[] deprels = new String[length+1];
	int[] heads = new int[length+1];

	forms[0] = "<root>";
	lemmas[0] = "<root-LEMMA>";
	cpos[0] = "<root-CPOS>";
	pos[0] = "<root-POS>";
	deprels[0] = "<no-type>";
	heads[0] = -1;

	for(int i = 0; i < length; i++) {
	    String[] info = lineList.get(i);
	    forms[i+1] = normalize(info[1]);
	    lemmas[i+1] = normalize(info[2]);
	    cpos[i+1] = info[3];
	    pos[i+1] = info[4];
	    feats[i+1] = info[5].split("\\|");
	    deprels[i+1] = labeled ? info[7] : "<no-type>";
	    heads[i+1] = Integer.parseInt(info[6]);
	}

	feats[0] = new String[feats[1].length];
	for (int i = 0; i< feats[1].length; i++)
	    feats[0][i] = "<root-feat>"+i;

	return new DependencyInstance(forms, lemmas, cpos, pos, feats, deprels, heads);

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
