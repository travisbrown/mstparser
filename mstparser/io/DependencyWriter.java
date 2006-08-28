package mstparser.io;

import java.io.*;
import mstparser.*;

public abstract class DependencyWriter {

    protected BufferedWriter writer;
    protected boolean labeled = false;

    public static DependencyWriter createDependencyWriter (String format, boolean labeled) throws IOException {
	if (format.equals("MST")) {
	    return new MSTWriter(labeled);
	} else if (format.equals("CONLL")) {
	    return new CONLLWriter(labeled);
	} else {
	    System.out.println("!!!!!!!  Not a supported format: " + format);
	    System.out.println("********* Assuming CONLL format. **********");
	    return new CONLLWriter(labeled);
	}
    }

    public void startWriting (String file) throws IOException {
	writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file),"UTF8"));
    }

    public void finishWriting () throws IOException {
	writer.flush();
	writer.close();
    }

    public boolean isLabeled() {
	return labeled;
    }

    public abstract void write(DependencyInstance instance) throws IOException;

}
