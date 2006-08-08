package mstparser.io;

import mstparser.DependencyInstance;
import java.io.*;

public class MSTReader extends DependencyReader {


    public DependencyInstance getNext() throws IOException {

	String line = inputReader.readLine();
	String pos_line = inputReader.readLine();
	String lab_line = labeled ? inputReader.readLine() : pos_line;
	String deps_line = inputReader.readLine();
	inputReader.readLine(); // blank line

	if(line == null) {
	    inputReader.close();
	    return null;
	}

	String[] toks = line.split("\t");
	String[] pos = pos_line.split("\t");
	String[] labs = lab_line.split("\t");
	String[] deps = deps_line.split("\t");

	String[] toks_new = new String[toks.length+1];
	String[] pos_new = new String[pos.length+1];
	String[] labs_new = new String[labs.length+1];
	String[] deps_new = new String[deps.length+1];

	toks_new[0] = "<root>";
	pos_new[0] = "<root-POS>";
	labs_new[0] = "<no-type>";
	deps_new[0] = "-1";
	for(int i = 0; i < toks.length; i++) {
	    toks_new[i+1] = normalize(toks[i]);
	    pos_new[i+1] = pos[i];
	    labs_new[i+1] = labeled ? labs[i] : "<no-type>";
	    deps_new[i+1] = deps[i];
	}

	//System.out.println(java.util.Arrays.toString(toks_new));

	return new DependencyInstance(toks_new, pos_new, labs_new, deps_new);

    }


    protected boolean fileContainsLabels (String file) throws IOException {
	BufferedReader in = new BufferedReader(new FileReader(file));
	in.readLine(); in.readLine(); in.readLine();
	String line = in.readLine();
	in.close();

	if(line.trim().length() > 0) 
	    return true;
	else
	    return false;
    }

}
