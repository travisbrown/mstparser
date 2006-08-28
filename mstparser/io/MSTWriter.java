package mstparser.io;

import mstparser.DependencyInstance;
import mstparser.Util;

import java.io.*;

public class MSTWriter extends DependencyWriter {

    public MSTWriter (boolean labeled) {
	this.labeled = labeled;
    }

    public void write(DependencyInstance instance) throws IOException {
	writer.write(Util.join(instance.get("tokens"), '\t') + "\n");
	writer.write(Util.join(instance.get("pos"), '\t') + "\n");
	if (labeled)
	    writer.write(Util.join(instance.get("labels"), '\t') + "\n");
	writer.write(Util.join(instance.getDeps(), '\t') + "\n\n");
    }

}
