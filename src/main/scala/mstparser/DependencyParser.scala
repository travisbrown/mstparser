package mstparser;

import java.io._
import java.util.ArrayList
import java.util.Arrays
import java.util.StringTokenizer
import gnu.trove._

import mstparser.io._

class DependencyParser(
  private val pipe: DependencyPipe,
  private val options: ParserOptions
) {

	val params = new Parameters(this.pipe.dataAlphabet.size)
  val decoder = if (options.secondOrder) new DependencyDecoder2O(this.pipe) else new DependencyDecoder(this.pipe)

  def train(instanceLengths: Array[Int], trainfile: String, train_forest: File) {
    (0 until options.numIters).map { i =>
      print(" Iteration %d[".format(i))
      val start = System.currentTimeMillis()
      this.trainingIter(instanceLengths, trainfile, train_forest, i + 1)
      val end = System.currentTimeMillis()
      println("|Time:%d]".format(end - start))
      end - start
    }

    this.params.averageParams(options.numIters * instanceLengths.length)
  }

  private def trainingIter(instanceLengths: Array[Int], trainfile: String, train_forest: File, iter: Int) {
    val in = new ObjectInputStream(new FileInputStream(train_forest))

    instanceLengths.zipWithIndex.foreach { case (length, i) =>
      if ((i + 1) % 500 == 0) print("%d,".format(i + 1))

      val (fvs, probs, nt_fvs, nt_probs, fvs_trips, probs_trips, fvs_sibs, probs_sibs) = this.createArrays(length)

      val instance = if (options.secondOrder) this.pipe.asInstanceOf[DependencyPipe2O].readInstance(
        in, length, fvs, probs, fvs_trips, probs_trips, fvs_sibs, probs_sibs, nt_fvs, nt_probs, params
      ) else this.pipe.readInstance(in, length, fvs, probs, nt_fvs, nt_probs, params)

      val upd = (options.numIters * instanceLengths.length - (instanceLengths.length * (iter - 1) + (i + 1)) + 1).toDouble

      val d = options.decodeType match {
        case "proj" =>
          if (options.secondOrder)
            decoder.asInstanceOf[DependencyDecoder2O].decodeProjective(
              instance, fvs, probs,
              fvs_trips, probs_trips,
              fvs_sibs, probs_sibs,
              nt_fvs,nt_probs, options.trainK
            )
          else decoder.decodeProjective(instance, fvs, probs, nt_fvs, nt_probs, options.trainK)
        case "non-proj" =>
          if (options.secondOrder)
            decoder.asInstanceOf[DependencyDecoder2O].decodeNonProjective(
              instance, fvs, probs,
              fvs_trips, probs_trips,
              fvs_sibs, probs_sibs,
              nt_fvs,nt_probs, options.trainK
            )
          else decoder.decodeNonProjective(instance, fvs, probs, nt_fvs, nt_probs, options.trainK)
        case _ => null
      }

      this.params.updateParamsMIRA(instance, d, upd)
    }
    System.out.print(instanceLengths.length)
    in.close()
  }

  private def createArrays(length: Int) = (
    Array.ofDim[FeatureVector](length, length, 2),
    Array.ofDim[Double](length, length, 2),
    Array.ofDim[FeatureVector](length, pipe.types.length, 2, 2),
    Array.ofDim[Double](length, pipe.types.length, 2, 2),

    Array.ofDim[FeatureVector](length, length, length),
    Array.ofDim[Double](length, length, length),

    Array.ofDim[FeatureVector](length, length, 2),
    Array.ofDim[Double](length, length, 2)
  )

  def saveModel(file: String) {
    val out = new ObjectOutputStream(new FileOutputStream(file))
    out.writeObject(this.params.parameters)
    out.writeObject(this.pipe.dataAlphabet)
    out.writeObject(this.pipe.typeAlphabet)
    out.close()
  }

  def loadModel(file: String) {
    val in = new ObjectInputStream(new FileInputStream(file))
    this.params.parameters = in.readObject().asInstanceOf[Array[Double]]
    this.pipe.dataAlphabet = in.readObject().asInstanceOf[Alphabet]
    this.pipe.typeAlphabet = in.readObject().asInstanceOf[Alphabet]
    in.close()
    this.pipe.closeAlphabets()
  }

  private implicit def pipeToIterator(pipe: DependencyPipe) = new Iterator[DependencyInstance] {
    var nextInstance = pipe.nextInstance
    def hasNext = this.nextInstance != null
    def next = {
      val current = this.nextInstance
      this.nextInstance = pipe.nextInstance
      current
    }
  }

  def outputParses() = {
    val start = System.currentTimeMillis

    this.pipe.initInputFile(options.testfile)
    this.pipe.initOutputFile(options.outfile)

    print("Processing Sentence: ")

    pipe.zipWithIndex.foreach { case (instance, cnt) => 
      print("%d ".format(cnt + 1))
      val forms = instance.forms
			
      val (fvs, probs, nt_fvs, nt_probs, fvs_trips, probs_trips, fvs_sibs, probs_sibs) = this.createArrays(instance.forms.length)

      if (options.secondOrder) this.pipe.asInstanceOf[DependencyPipe2O].fillFeatureVectors(
        instance, fvs, probs, fvs_trips, probs_trips, fvs_sibs, probs_sibs, nt_fvs, nt_probs, params
      ) else this.pipe.fillFeatureVectors(instance, fvs, probs, nt_fvs, nt_probs, params)

      val d = options.decodeType match {
        case "proj" =>
          if (options.secondOrder)
            decoder.asInstanceOf[DependencyDecoder2O].decodeProjective(
              instance, fvs, probs,
              fvs_trips, probs_trips,
              fvs_sibs, probs_sibs,
              nt_fvs,nt_probs, options.testK
            )
          else decoder.decodeProjective(instance, fvs, probs, nt_fvs, nt_probs, options.testK)
        case "non-proj" =>
          if (options.secondOrder)
            decoder.asInstanceOf[DependencyDecoder2O].decodeNonProjective(
              instance, fvs, probs,
              fvs_trips, probs_trips,
              fvs_sibs, probs_sibs,
              nt_fvs,nt_probs, options.testK
            )
          else decoder.decodeNonProjective(instance, fvs, probs, nt_fvs, nt_probs, options.testK)
        case _ => null
      }

      val res = d(0)(1).asInstanceOf[String].split(" ")
      val formsNoRoot = Array.ofDim[String](instance.forms.length - 1)
      val posNoRoot = Array.ofDim[String](instance.forms.length - 1)
      val labels = Array.ofDim[String](instance.forms.length - 1)
      val heads = Array.ofDim[Int](instance.forms.length - 1)

      (0 until instance.forms.length - 1).foreach { j =>
        formsNoRoot(j) = instance.forms(j + 1)
        posNoRoot(j) = instance.cpostags(j + 1)

        val trip = res(j).split("[\\|:]")
        labels(j) = this.pipe.types(trip(2).toInt)
        heads(j) = trip(0).toInt
      }

      pipe.outputInstance(new DependencyInstance(formsNoRoot, null, null, posNoRoot, null, labels, heads))
    }
    pipe.close()
		
    val end = System.currentTimeMillis()
    println("Took: %d".format(end - start))
    end - start
  }
}

object DependencyParser {
  def main(args: Array[String]) {
    val options = new ParserOptions(args)
    if (options.train) {
      val pipe =
        if (options.secondOrder) new DependencyPipe2O(options)
        else new DependencyPipe(options)

      val instanceLengths = pipe.createInstances(options.trainfile, options.trainforest)
	  	pipe.closeAlphabets()

      val dp = new DependencyParser(pipe, options)

      val numFeats = pipe.dataAlphabet.size
      val numTypes = pipe.typeAlphabet.size
      println("Num Feats: %d.\tNum Edge Labels: %d".format(numFeats, numTypes))

      dp.train(instanceLengths, options.trainfile, options.trainforest)

      print("Saving model...")
      dp.saveModel(options.modelName)
      print("done.")
	  }

    if (options.test) {
      val pipe =
        if (options.secondOrder) new DependencyPipe2O(options)
        else new DependencyPipe(options)

      val dp = new DependencyParser(pipe, options)
      print("\tLoading model...")
      dp.loadModel(options.modelName)
      println("done.")
      pipe.closeAlphabets()
      dp.outputParses()
    }

    println()

    if (options.eval) {
      println("\nEVALUATION PERFORMANCE:")
      DependencyEvaluator.evaluate(options.goldfile, options.outfile, options.format)
    }
  }
}

