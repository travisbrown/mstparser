package mstparser.io;

import java.io.*;
import mstparser.DependencyInstance;

public class CONLLWriter extends DependencyWriter {

    public CONLLWriter (boolean labeled) {
	this.labeled = labeled;
    }

    public void write(DependencyInstance instance) throws IOException {
	return;
    }

}
