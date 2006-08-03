package mstparser;

import java.io.*;

public class DependencyEvaluator {
	
    public static void evaluate(String act_file, String pred_file) throws IOException {
	boolean labeled = false;
	BufferedReader act_in = new BufferedReader(new FileReader(act_file));
	act_in.readLine(); act_in.readLine(); act_in.readLine();
	String l = act_in.readLine();
	if(l.trim().length() > 0) labeled = true;

	int total = 0; int corr = 0; int corrL = 0;
	int numsent = 0; int corrsent = 0; int corrsentL = 0;
	int root_act = 0; int root_guess = 0; int root_corr = 0;

	act_in = new BufferedReader(new FileReader(act_file));
	BufferedReader pred_in = new BufferedReader(new FileReader(pred_file));

	act_in.readLine(); String[] pos = act_in.readLine().split("\t");
	pred_in.readLine(); pred_in.readLine();
	String act_lab = labeled ? act_in.readLine().trim() : "" ; String act_dep = act_in.readLine().trim();
	String pred_lab = labeled ? pred_in.readLine().trim() : ""; String pred_dep = pred_in.readLine().trim();
	act_in.readLine(); pred_in.readLine();

	while(act_dep != null) {

	    String[] act_labs = null;
	    String[] pred_labs = null;
	    if(labeled) {
		act_labs = act_lab.split("\t");
		pred_labs = pred_lab.split("\t");
	    }
	    String[] act_deps = act_dep.split("\t");
	    String[] pred_deps = pred_dep.split("\t");
	    if(act_deps.length != pred_deps.length) System.out.println("Lengths do not match on sentence "+numsent);

	    boolean whole = true;
	    boolean wholeL = true;

	    for(int i = 0; i < act_deps.length; i++) {
		if(pred_deps[i].equals(act_deps[i])) {
		    corr++;
		    if(labeled) {
			if(act_labs[i].equals(pred_labs[i])) corrL++;
			else wholeL = false;
		    }
		}
		else { whole = false; wholeL = false; }
	    }
	    total += act_deps.length;

	    if(whole) corrsent++;
	    if(wholeL) corrsentL++;
	    numsent++;
						
	    act_in.readLine(); try {pos = act_in.readLine().split("\t");} catch(Exception e){}
	    pred_in.readLine(); pred_in.readLine();
	    act_lab = labeled ? act_in.readLine() : ""; act_dep = act_in.readLine();
	    pred_lab = labeled ? pred_in.readLine() : ""; pred_dep = pred_in.readLine();
	    act_in.readLine(); pred_in.readLine();
	}

	System.out.println("Tokens: " + total);
	System.out.println("Correct: " + corr);
	System.out.println("Unlabeled Accuracy: " + ((double)corr/total));
	System.out.println("Unlabeled Complete Correct: " + ((double)corrsent/numsent));
	if(labeled) {
	    System.out.println("Labeled Accuracy: " + ((double)corrL/total));
	    System.out.println("Labeled Complete Correct: " + ((double)corrsentL/numsent));
	}
		
    }

    public static void main (String[] args) throws IOException {
	evaluate(args[0], args[1]);
    }

}
