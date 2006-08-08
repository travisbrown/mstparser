package mstparser.io;

import java.io.*;
import mstparser.DependencyInstance;

public class CONLLWriter extends DependencyWriter {

    public CONLLWriter (boolean labeled) {
	this.labeled = labeled;
    }

    public void write(DependencyInstance instance) throws IOException {
	
	String[] toks = instance.get("tokens");
	String[] pos = instance.get("pos");
	String[] labs = instance.get("labels");
	String[] deps = instance.get("deps");

	for (int i=0; i<instance.length(); i++) {
	    writer.write(Integer.toString(i+1)); writer.write('\t');
	    writer.write(toks[i]);               writer.write('\t');
	    writer.write(toks[i]);               writer.write('\t');
	    writer.write(pos[i]);                writer.write('\t');
	    writer.write(pos[i]);                writer.write('\t');
	    writer.write("-\t");                 writer.write('\t');
	    writer.write(deps[i]);               writer.write('\t');
	    writer.write(labs[i]);               writer.write('\t');
	    writer.newLine();
	}
	writer.newLine();

    }


}
