package mstparser.io;

import mstparser.DependencyInstance;
import mstparser.Util;
import java.io.*;

public class MSTReader extends DependencyReader {


    public DependencyInstance getNext() throws IOException {

	String line = inputReader.readLine();
	String pos_line = inputReader.readLine();
	String deprel_line = labeled ? inputReader.readLine() : pos_line;
	String heads_line = inputReader.readLine();
	inputReader.readLine(); // blank line

	if(line == null) {
	    inputReader.close();
	    return null;
	}

	String[] forms = line.split("\t");
	String[] pos = pos_line.split("\t");
	String[] deprels = deprel_line.split("\t");
	int[] heads = Util.stringsToInts(heads_line.split("\t"));

	String[] forms_new = new String[forms.length+1];
	String[] pos_new = new String[pos.length+1];
	String[] deprels_new = new String[deprels.length+1];
	int[] heads_new = new int[heads.length+1];

	forms_new[0] = "<root>";
	pos_new[0] = "<root-POS>";
	deprels_new[0] = "<no-type>";
	heads_new[0] = -1;
	for(int i = 0; i < forms.length; i++) {
	    forms_new[i+1] = normalize(forms[i]);
	    pos_new[i+1] = pos[i];
	    deprels_new[i+1] = labeled ? deprels[i] : "<no-type>";
	    heads_new[i+1] = heads[i];
	}

	DependencyInstance instance = 
	    new DependencyInstance(forms_new, pos_new, deprels_new, heads_new);

	// set up the course pos tags as just the first letter of the fine-grained ones
	String[] cpostags = new String[pos_new.length];
	cpostags[0] = "<root-CPOS>";
	for(int i = 1; i < pos_new.length; i++)
	    cpostags[i] = pos_new[i].substring(0,1);
	instance.cpostags = cpostags;

	return instance;
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
