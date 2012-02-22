package mstparser.old;

import mstparser.*;

public class DependencyInstance implements java.io.Serializable {

    // The various data types. Here's an example from Portuguese:
    //
    // 3  eles ele   pron       pron-pers M|3P|NOM 4    SUBJ   _     _
    // ID FORM LEMMA COURSE-POS FINE-POS  FEATURES HEAD DEPREL PHEAD PDEPREL
    //
    // We ignore PHEAD and PDEPREL for now. 

    // FORM: the forms - usually words, like "thought"
    public String[] forms;

    // LEMMA: the lemmas, or stems, e.g. "think"
    public String[] lemmas;

    // COURSE-POS: the course part-of-speech tags, e.g."V"
    public String[] cpostags;

    // FINE-POS: the fine-grained part-of-speech tags, e.g."VBD"
    public String[] postags;

    // FEATURES: some features associated with the elements separated by "|", e.g. "PAST|3P"
    public String[][] feats;

    // HEAD: the IDs of the heads for each element
    public int[] heads;

    // DEPREL: the dependency relations, e.g. "SUBJ"
    public String[] deprels;

    // RELATIONAL FEATURE: relational features that hold between items
    public RelationalFeature[] relFeats;

    public DependencyInstance(String[] forms, String[] lemmas, String[] cpostags, 
			      String[] postags, String[][] feats, String[] labs, int[] heads,
			      RelationalFeature[] relFeats) {
	this.forms = forms;
	this.lemmas = lemmas;
  this.cpostags = cpostags;
	this.postags = postags;
  this.feats = feats;
	this.deprels = labs;
	this.heads = heads;
	this.relFeats = relFeats;
    }
}

