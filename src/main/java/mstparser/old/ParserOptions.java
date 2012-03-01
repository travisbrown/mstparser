///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2007 University of Texas at Austin and (C) 2005
// University of Pennsylvania and Copyright (C) 2002, 2003 University
// of Massachusetts Amherst, Department of Computer Science.
//
// This software is licensed under the terms of the Common Public
// License, Version 1.0 or (at your option) any subsequent version.
// 
// The license is approved by the Open Source Initiative, and is
// available from their website at http://www.opensource.org.
///////////////////////////////////////////////////////////////////////////////

package mstparser.old;

import java.io.File;

public class ParserOptions {
    public String trainfile = null;
    public String testfile = null;
    public File trainforest = null;
    public File testforest = null;
    public boolean train = false;
    public boolean eval = false;
    public boolean test = false;
    public String modelName = "dep.model";
    public String lossType = "punc";
    public boolean createForest = true;
    public String decodeType = "proj";
    public String format = "CONLL";
    public int numIters = 10;
    public String outfile = "out.txt";
    public String goldfile = null;
    public int trainK = 1;
    public int testK = 1;
    public boolean secondOrder = false;
    public boolean useRelationalFeatures = false;
    public boolean discourseMode = false;
}

