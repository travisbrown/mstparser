#!/usr/bin/python

import sys
import optparse
import fileinput

## Get options

parser = optparse.OptionParser()
#parser.add_option("-r", "--right_links_only", action="store_true", default=False,
#                      help="Only score sentences with only rightward links.")

(options, args) = parser.parse_args()


## Process file

for sentence_deps in fileinput.input(args):

    dep_list = sentence_deps.split()

    remapped_ids = []
    skipped_items = 0 # keep track of the number of unconnected items    
    for i in range(len(dep_list)):
        id, word, target, rel, postag = dep_list[i].split(",")
        id, target = int(id), int(target)

        if target == 0:
            skipped_items += 1
	    remapped_ids.append(0)
	else:
	    remapped_ids.append(id-skipped_items)

    for i in range(len(dep_list)):
        id, word, target, rel, postag = dep_list[i].split(",")
        id, target = int(id), int(target)

	#print id, "**",remapped_ids[id-1],"::",target,"**",remapped_ids[target-1]

	new_id = remapped_ids[id-1]
	new_target = remapped_ids[target-1]

        true_word = word
        if word == "+comma+":
            true_word = ","

        true_postag = postag
        if postag == "Punc":
            true_postag = true_word
            if true_postag == "-":
                true_postag = ":"

        if target != 0:
            print ",".join([str(new_id), word, str(new_target), rel, postag])+" ",

    print


