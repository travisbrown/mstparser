package mstparser.io;

import java.io.*;
import mstparser.DependencyInstance;

public class MSTWriter extends DependencyWriter {

    public MSTWriter (boolean labeled) {
	this.labeled = labeled;
    }

    public void write(DependencyInstance instance) throws IOException {
	writer.write(join(instance.get("tokens"), '\t') + "\n");
	writer.write(join(instance.get("pos"), '\t') + "\n");
	if (labeled)
	    writer.write(join(instance.get("labels"), '\t') + "\n");
	writer.write(join(instance.get("deps"), '\t') + "\n\n");
    }

}
