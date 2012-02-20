package mstparser

import java.io._
import mstparser.io._

object DependencyEvaluator {
  def evaluate(actFile: String, predFile: String, format: String) = {
    val goldReader = DependencyReader.createDependencyReader(format)
    val labeled = goldReader.startReading(actFile)

    val predReader = DependencyReader.createDependencyReader(format)
    val predLabeled = predReader.startReading(predFile)

    if (labeled != predLabeled)
	    println("Gold file and predicted file appear to differ on whether or not they are labeled. Expect problems!!!")

    var corr = 0
    var corrL = 0
    var corrSent = 0
    var corrSentL = 0

    goldReader.zip(predReader).zipWithIndex.foreach { case ((goldInstance, predInstance), i) =>
      goldInstance.difference(predInstance) match {
        case Some((d, dL)) =>
          if (d == 0) corrSent += 1
          if (dL == 0) corrSentL += 1

          corr += goldInstance.size - d
          corrL += goldInstance.size - dL
        case None =>
          println("Lengths do not match on sentence " + i)
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

