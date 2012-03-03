package mstparser

import java.io.File

class ParserOptions(private val args: Array[String]) {
  val pairs = this.args.map(_.split(":")).map {
    case Array(k) => (k, "true")
    case Array(k, v) => (k, v)
  }.toMap

  val train = this.pairs.get("train").map(_ == "true").getOrElse(false)
  val test = this.pairs.get("test").map(_ == "true").getOrElse(false)
  val eval = this.pairs.get("eval").map(_ == "true").getOrElse(false)
  val trainFile = this.pairs.get("train-file")
  val testFile = this.pairs.get("test-file")
  val goldFile = this.pairs.get("gold-file")
  val outFile = this.pairs.getOrElse("output-file", "out.txt")
  val modelName = this.pairs.getOrElse("model-name", "dep.model")
  val createForest = this.pairs.get("create-forest").map(_ == "true").getOrElse(true)
  val secondOrder = this.pairs.get("order").map(_ == "2").getOrElse(false)
  val useRelationalFeatures = this.pairs.get("relational-features").map(_ == "true").getOrElse(false)
  val discourseMode = this.pairs.get("discourse-mode").map(_ == "true").getOrElse(false)
  val lossType = this.pairs.getOrElse("loss-type", "punc")
  val decodeType = this.pairs.getOrElse("decode-type", "proj")
  val format = this.pairs.getOrElse("format", "CONLL")
  val numIters = this.pairs.get("iters").map(_.toInt).getOrElse(10)
  val trainK = this.pairs.get("training-k").map(_.toInt).getOrElse(1)
  val testK = this.pairs.get("test-k").map(_.toInt).getOrElse(1)

	val (trainForest, testForest) = try {(
    this.trainFile.map { f =>
      val tmp = File.createTempFile("train", ".forest")
      tmp.deleteOnExit()
      tmp
	  },
    this.testFile.map { f =>
      val tmp = File.createTempFile("test", ".forest")
      tmp.deleteOnExit()
      tmp
	  }
  )} catch { case e: java.io.IOException =>
	  println("Unable to create temporary files for feature forests!")
	  println(e)
	  sys.exit(0)
	}

  override def toString = "FLAGS [%s]\n".format(List(
    "train-file"          -> this.trainFile.orNull,
    "test-file"           -> this.testFile.orNull,
    "gold-file"           -> this.goldFile.orNull,
    "output-file"         -> this.outFile,
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

