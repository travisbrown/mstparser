package mstparser.io;

import java.io.*;

public abstract class DependencyReader {

    protected BufferedReader inputReader;
    protected boolean labeled = true;

    public static DependencyReader createDependencyReader (String format) throws IOException {
	if (format.equals("MST")) {
	    return new MSTReader();
	} else if (format.equals("CONLL")) {
	    return new CONLLReader();
	} else {
	    System.out.println("!!!!!!!  Not a supported format: " + format);
	    System.out.println("********* Assuming CONLL format. **********");
	    return new CONLLReader();
	}
    }

    public boolean startReading (String file) throws IOException {
	labeled = fileContainsLabels(file);
	inputReader = new BufferedReader(new InputStreamReader(new FileInputStream(file),"UTF8"));
	return labeled;
    }

    public boolean isLabeled() {
	return labeled;
    }

    public abstract mstparser.DependencyInstance getNext() throws IOException;

    protected abstract boolean fileContainsLabels(String filename) throws IOException;


    protected String normalize (String s) {
	if(s.matches("[0-9]+|[0-9]+\\.[0-9]+|[0-9]+[0-9,]+"))
	    return "<num>";

	return s;
    }	

}
