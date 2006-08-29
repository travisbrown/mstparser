package mstparser.io;

import mstparser.DependencyInstance;
import mstparser.Util;

import java.io.*;

public class MSTWriter extends DependencyWriter {

    public MSTWriter (boolean labeled) {
	this.labeled = labeled;
    }

    public void write(DependencyInstance instance) throws IOException {
	writer.write(Util.join(instance.forms, '\t') + "\n");
	writer.write(Util.join(instance.postags, '\t') + "\n");
	if (labeled)
	    writer.write(Util.join(instance.deprels, '\t') + "\n");
	writer.write(Util.join(instance.heads, '\t') + "\n\n");
    }

}
