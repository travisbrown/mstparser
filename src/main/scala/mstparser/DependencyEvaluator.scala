package mstparser

import mstparser.io.DependencyReader

object DependencyEvaluator {
  def evaluate(actFile: String, predFile: String, format: String) = {
    val goldReader = DependencyReader.createDependencyReader(format)
    val labeled = goldReader.startReading(actFile)

    val predReader = DependencyReader.createDependencyReader(format)
    val predLabeled = predReader.startReading(predFile)

    if (labeled != predLabeled)
	    println("Gold file and predicted file appear to differ on whether or not they are labeled. Expect problems!!!")

    val (corr, corrL, corrSent, corrSentL) =
      goldReader.zip(predReader).zipWithIndex.foldLeft(0, 0, 0, 0) {
        case ((c, cL, cs, csL), ((goldInstance, predInstance), i)) =>
          goldInstance.difference(predInstance) match {
            case Some((d, dL)) => (
              c + goldInstance.size - d,
              cL + goldInstance.size - dL,
              cs + (if (d == 0) 1 else 0),
              csL + (if (dL == 0) 1 else 0)
            )
            case None =>
              println("Lengths do not match on sentence " + i)
              (c, cL, cs, csL)
	    }
    }

    println("Tokens: " + goldReader.tokenCount)
    println("Correct: " + corr)
    println("Unlabeled Accuracy: " + corr.toDouble / goldReader.tokenCount)
    println("Unlabeled Complete Correct: " + corrSent.toDouble / goldReader.instanceCount)
    if (labeled) {
      println("Labeled Accuracy: " + corrL.toDouble / goldReader.tokenCount)
      println("Labeled Complete Correct: " + corrSentL.toDouble / goldReader.instanceCount)
    }
  }

  def main(args: Array[String]) {
    val format = if (args.length > 2) args(2) else "CONLL"
    this.evaluate(args(0), args(1), format)
  }
}

