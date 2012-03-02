package mstparser.old;

import scala.collection.IndexedSeq;

public abstract class DependencyPipe {
  public abstract mstparser.ParserOptions getOptions();
  public abstract void add(String feat, mstparser.FeatureVector fv);
  public abstract void add(String feat, double val, mstparser.FeatureVector fv);

/*  protected abstract void addLinearFeatures(String type, IndexedSeq<String> obsVals, 
					 int first, int second,
					 String attachDistance,
					 mstparser.FeatureVector fv);*/

    /**
     * Add features for two items, each with two observations, e.g. head,
     * head pos, child, and child pos.
     *
     * The use of StringBuilders is not yet as efficient as it could
     * be, but this is a start. (And it abstracts the logic so we can
     * add other features more easily based on other items and
     * observations.)
     **/
    protected abstract void addTwoObsFeatures(String prefix, 
					 String item1F1, String item1F2, 
					 String item2F1, String item2F2, 
					 String attachDistance,
					 mstparser.FeatureVector fv);

    protected void addDiscourseFeatures (mstparser.DependencyInstance instance, 
				       int small,
				       int large,
				       int headIndex,
				       int childIndex,
				       String attDist,
				       mstparser.FeatureVector fv) {
 /*   
	addLinearFeatures("FORM", instance.forms(), small, large, attDist, fv);
	addLinearFeatures("LEMMA", instance.lemmas(), small, large, attDist, fv);
	
	addTwoObsFeatures("HCB1", instance.forms()[headIndex], 
			  instance.lemmas()[headIndex],
			  instance.forms()[childIndex], 
			  instance.lemmas()[childIndex], 
			  attDist, fv);
	
	addTwoObsFeatures("HCB2", instance.forms()[headIndex], 
			  instance.lemmas()[headIndex],
			  instance.forms()[childIndex], 
			  instance.postags()[childIndex], 
			  attDist, fv);
	
	addTwoObsFeatures("HCB3", instance.forms()[headIndex], 
			  instance.lemmas()[headIndex],
			  instance.forms()[childIndex], 
			  instance.cpostags()[childIndex], 
			  attDist, fv);
	
	addTwoObsFeatures("HC2", instance.forms()[headIndex], 
			  instance.postags()[headIndex], 
			  instance.forms()[childIndex], 
			  instance.cpostags()[childIndex], attDist, fv);
	
	addTwoObsFeatures("HCC2", instance.lemmas()[headIndex], 
			  instance.postags()[headIndex], 
			  instance.lemmas()[childIndex], 
			  instance.cpostags()[childIndex], 
			  attDist, fv);
	
	
	//// Use this if your extra feature lists all have the same length.
	for (int i=0; i<instance.feats().length; i++) {
	
		addLinearFeatures("F"+i, instance.feats()[i], small, large, attDist, fv);
	
		addTwoObsFeatures("FF"+i, 
				  instance.forms()[headIndex], 
				  instance.feats()[i][headIndex],
				  instance.forms()[childIndex], 
				  instance.feats()[i][childIndex],
				  attDist, fv);
		
		addTwoObsFeatures("LF"+i, 
				  instance.lemmas()[headIndex], 
				  instance.feats()[i][headIndex],
				  instance.lemmas()[childIndex], 
				  instance.feats()[i][childIndex],
				  attDist, fv);
		
		addTwoObsFeatures("PF"+i, 
				  instance.postags()[headIndex], 
				  instance.feats()[i][headIndex],
				  instance.postags()[childIndex], 
				  instance.feats()[i][childIndex],
				  attDist, fv);
		
		addTwoObsFeatures("CPF"+i, 
				  instance.cpostags()[headIndex], 
				  instance.feats()[i][headIndex],
				  instance.cpostags()[childIndex], 
				  instance.feats()[i][childIndex],
				  attDist, fv);
		
		for (int j=i+1; j<instance.feats().length; j++) {
		
		    addTwoObsFeatures("CPF"+i+"_"+j, 
				      instance.feats()[i][headIndex],
				      instance.feats()[j][headIndex],
				      instance.feats()[i][childIndex],
				      instance.feats()[j][childIndex],
				      attDist, fv);
		}
	
		for (int j=0; j<instance.feats().length; j++) {
	
		    addTwoObsFeatures("XFF"+i+"_"+j, 
				      instance.forms()[headIndex],
				      instance.feats()[i][headIndex],
				      instance.forms()[childIndex],
				      instance.feats()[j][childIndex],
				      attDist, fv);
	
		    addTwoObsFeatures("XLF"+i+"_"+j, 
				      instance.lemmas()[headIndex],
				      instance.feats()[i][headIndex],
				      instance.lemmas()[childIndex],
				      instance.feats()[j][childIndex],
				      attDist, fv);
	
		    addTwoObsFeatures("XPF"+i+"_"+j, 
				      instance.postags()[headIndex],
				      instance.feats()[i][headIndex],
				      instance.postags()[childIndex],
				      instance.feats()[j][childIndex],
				      attDist, fv);
	
	
		    addTwoObsFeatures("XCF"+i+"_"+j, 
				      instance.cpostags()[headIndex],
				      instance.feats()[i][headIndex],
				      instance.cpostags()[childIndex],
				      instance.feats()[j][childIndex],
				      attDist, fv);
		}
	}

	// Test out relational features
	if (this.getOptions().useRelationalFeatures()) {

	    //for (int rf_index=0; rf_index<2; rf_index++) {
	    for (int rf_index=0; 
		 rf_index<instance.relFeats().length; 
		 rf_index++) {
		
		String headToChild = 
		    "H2C"+rf_index+instance.relFeats()[rf_index].getFeature(headIndex, childIndex);
	    
		addTwoObsFeatures("RFA1",
				  instance.forms()[headIndex], 
				  instance.lemmas()[headIndex],
				  instance.postags()[childIndex],
				  headToChild,
				  attDist, fv);
		
		addTwoObsFeatures("RFA2",
				  instance.postags()[headIndex], 
				  instance.cpostags()[headIndex],
				  instance.forms()[childIndex],
				  headToChild,
				  attDist, fv);
	    
	    	addTwoObsFeatures("RFA3",
				  instance.lemmas()[headIndex], 
				  instance.postags()[headIndex],
				  instance.forms()[childIndex],
				  headToChild,
				  attDist, fv);
	    	
	    	addTwoObsFeatures("RFB1",
				  headToChild,
				  instance.postags()[headIndex],
				  instance.forms()[childIndex], 
				  instance.lemmas()[childIndex],
				  attDist, fv);
	    	
	    	addTwoObsFeatures("RFB2",
				  headToChild,
				  instance.forms()[headIndex],
				  instance.postags()[childIndex], 
				  instance.cpostags()[childIndex],
				  attDist, fv);
	    	
	    	addTwoObsFeatures("RFB3",
				  headToChild,
				  instance.forms()[headIndex],
				  instance.lemmas()[childIndex], 
				  instance.postags()[childIndex],
				  attDist, fv);
		
	    }
	}*/
    }
}

