#!/usr/bin/python

import os
import sys
import optparse

## Check that MSTPARSER_DIR environment variable is set and get it
global mstparser_dir
mstparser_dir = ''
if os.environ.has_key('MSTPARSER_DIR'):
    mstparser_dir = os.environ['MSTPARSER_DIR']
else:
    print "Please set the MSTPARSER_DIR environment variable to where you have Dan Bikel's parser installed."
    exit(1)


###########################################################################
#
# Run a single fold. This could actually be not a "fold" per se, but
# actually explicitly provided training and test files.
#
###########################################################################

def run_single_train_and_test(options, train_filename,
                              test_filename, output_filename, args):
    # Train the parser
    print "  Training and evaluating..."
    train_command = 'mst_parse.sh train train-file:%s model-name:%s/dep.model decode-type:%s test test-file:%s output-file:%s %s' % (train_filename, options.output_dir, options.decoder_type, test_filename, output_filename, " ".join(args[1:]))
    if options.verbose:
	print train_command
	os.system(train_command)
    else:
	os.system(train_command+' &>/dev/null')
    

###################### END FUNCTION DEFINITIONS ########################


## Get options

opt_parser = optparse.OptionParser()
opt_parser.add_option("-l", "--language", action="store", default='Unspecified',
		  help="use configurations specific to LANGUAGE",
		  metavar="LANGUAGE")
opt_parser.add_option("-e", "--eval_file", action="store", default='Generated',
		  help="read evaluation sentences from FILE",
		  metavar="FILE")
opt_parser.add_option("-d", "--decoder_type", action="store",
                      choices=['proj', 'non-proj'],
		      default="proj",
		      help="Use a projective or non-projective algorithm.E",
		      metavar="FILE")
opt_parser.add_option("-o", "--output_dir", action="store", default='output',
		  help="save parser output to DIR",
		  metavar="DIR")
opt_parser.add_option("-f", "--num_folds", action="store", default=10,
		  help="The number of folds to use in cross-validation (Default=10). Set the number of folds to be zero if you want don't want cross-validation.",
		  metavar="NUM")
opt_parser.add_option("-v", "--verbose", action="store_true", default=False,
                      help="be verbose")

opt_parser.add_option("-t", "--tag_source", choices=['Raw','Gold','OTK_Tagger'],
                   default='Raw',
                   help="use tags from Gold standard or from a tagger (Raw (def), Gold, OTK_Tagger)",
                   metavar="SOURCE")

(options, args) = opt_parser.parse_args()

#Convert from FP to Int
options.num_folds = int(options.num_folds)

# Check that the requested output directory doesn't exist and isn't a
# file. If it's okay, create the directory.
output_dir = options.output_dir
if os.path.isdir(output_dir):
    os.system("rm -rf %s" % output_dir)
elif os.path.isfile(output_dir):
    raise OSError("A file with the same name as the desired dir, " \
		  "'%s', already exists." % output_dir)
os.makedirs(output_dir)


# This file accumulates the results across all folds.
model_output_filename = output_dir+"/model_out"
os.system('touch %s' % model_output_filename)

## Process files

train_filename = args[0]

# If a source of gold dependencies is given, set some flags and start
# up the file for reading them in.
#evaluate_deps = False
#gold_deps_source = None
#if options.gold_dependency_source != "None":
#    evaluate_deps = True

# This file accumulates the gold dependencies across all folds.
gold_deps_filename = output_dir+"/gold.deps"

num_folds = int(options.num_folds)

if num_folds != 0:

    print "Running a %d-fold evaluation on file %s" \
          % (num_folds, train_filename)
    print

    # Align parses with their corresponding sentences and assign a
    # partition id to them.

    train_file = file(train_filename)

    examples = []

    next_example = train_file.readline()

    counter = 0
    while next_example:
        partition = counter % num_folds

        examples.append((partition, next_example, train_file.readline(), 
			 train_file.readline(), train_file.readline()))

	train_file.readline() 	# eat up blank line

        next_example = train_file.readline()

        counter += 1


    # Close the sentences file and delete it. (It was either copied or
    # generated, so it's okay.)
    train_file.close()

    # Train/test on each partion

    gold_deps = open(gold_deps_filename,"w")

    # Run each fold. The output from each fold is appended to gold.deps
    # and model.deps
    #for test_partition in range(1):
    for test_partition in range(num_folds):

        print "Fold",test_partition

        train_filename = output_dir+"/train"
        train_set = open(train_filename, "w")

        test_filename = output_dir+"/test"
        test_set = open(test_filename, "w")

	counter = 0
        for ex in examples:
            if ex[0] == test_partition:
                test_set.write("".join(ex[1:5])+"\n")
                gold_deps.write("".join(ex[1:5])+"\n")
            else:
                train_set.write("".join(ex[1:5])+"\n")

	    counter += 1

        train_set.close()
        test_set.close()

        # Run the fold.
	output_filename = output_dir+"/output"
        run_single_train_and_test(options, train_filename, test_filename, output_filename, args)

        # Pile this fold's output onto the accumulating result file.
        os.system('cat %s >> %s' % (output_filename, model_output_filename))

        gold_deps.flush()

    gold_deps.close()


################## EVALUATION ###################

print "Evaluating. If anything here dies, you can still look at the output files in the directory '%s'." % (output_dir)

# Get dependency results.

os.system("mst_score.sh %s %s" % (gold_deps_filename, model_output_filename))

