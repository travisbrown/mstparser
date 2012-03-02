package mstparser

import java.io.File

class ParserOptions(args: Array[String]) {
  var trainfile: String = _
  var testfile: String = _
  var trainforest: File = _
  var testforest: File = _
  var train = false
  var eval = false
  var test = false
  var modelName = "dep.model"
  var lossType = "punc"
  var createForest = true
  var decodeType = "proj"
  var format = "CONLL"
  var numIters = 10
  var outfile = "out.txt"
  var goldfile: String = _
  var trainK = 1
  var testK = 1
  var secondOrder = false
  var useRelationalFeatures = false
  var discourseMode = false

  this.args.map(_.split(":")).foreach {
    case Array(k) =>
      k match {
        case "train" => this.train = true
        case "test"  => this.test = true
        case "eval"  => this.eval = true
      }
    case Array(k, v) =>
      k match {
        case "iters"               => this.numIters = v.toInt
        case "training-k"          => this.trainK = v.toInt
        case "output-file"         => this.outfile = v
        case "gold-file"           => this.goldfile = v
        case "train-file"          => this.trainfile = v
        case "test-file"           => this.testfile = v
        case "model-name"          => this.modelName = v
        case "loss-type"           => this.lossType = v
        case "decode-type"         => this.decodeType = v
        case "format"              => this.format = v
        case "order" if v == "2"   => this.secondOrder = true
        case "create-forest"       => this.createForest = v == "true"
        case "relational-features" => this.useRelationalFeatures = v == "true"
        case "discourse-mode"      => this.discourseMode = v == "true"
      }
  }

	try {
    Option(this.trainfile).foreach { f =>
      this.trainforest = File.createTempFile("train", ".forest")
      this.trainforest.deleteOnExit()
	  }

    Option(this.testfile).foreach { f =>
      this.testforest = File.createTempFile("test", ".forest")
      this.testforest.deleteOnExit()
	  }
	} catch { case e: java.io.IOException =>
	  println("Unable to create tmp files for feature forests!")
	  println(e)
	  sys.exit(0)
	}

  override def toString = "FLAGS [%s]\n".format(List(
    "train-file"          -> this.trainfile,
    "test-file"           -> this.testfile,
    "gold-file"           -> this.goldfile,
    "output-file"         -> this.outfile,
    "model-name"          -> this.modelName,
    "train"               -> this.train,
    "test"                -> this.test,
    "eval"                -> this.eval,
    "loss-type"           -> this.lossType,
    "second-order"        -> this.secondOrder,
    "training-iterations" -> this.numIters,
    "training-k"          -> this.trainK,
    "decode-type"         -> this.decodeType,
    "create-forest"       -> this.createForest,
    "format"              -> this.format,
    "relational-features" -> this.useRelationalFeatures,
    "discourse-mode"      -> this.discourseMode
  ).map { case (k, v) => k + ": " + v }.mkString(" | "))
}

