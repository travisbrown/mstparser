package mstparser.io;

import java.io.*;
import mstparser.DependencyInstance;

public class CONLLWriter extends DependencyWriter {

    public CONLLWriter (boolean labeled) {
	this.labeled = labeled;
    }

    public void write(DependencyInstance instance) throws IOException {
	
	for (int i=0; i<instance.length(); i++) {
	    writer.write(Integer.toString(i+1));                writer.write('\t');
	    writer.write(instance.forms[i]);                    writer.write('\t');
	    writer.write(instance.forms[i]);                    writer.write('\t');
	    //writer.write(instance.cpostags[i]);                 writer.write('\t');
	    writer.write(instance.postags[i]);                  writer.write('\t');
	    writer.write(instance.postags[i]);                  writer.write('\t');
	    writer.write("-");                                  writer.write('\t');
	    writer.write(Integer.toString(instance.heads[i]));  writer.write('\t');
	    writer.write(instance.deprels[i]);                  writer.write('\t');
	    writer.write("-\t-");
	    writer.newLine();
	}
	writer.newLine();

    }


}
