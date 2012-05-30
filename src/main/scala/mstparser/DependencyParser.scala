package mstparser

import java.io.BufferedInputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream

class DependencyParser(
  private var pipe: DependencyPipe,
  private val options: ParserOptions
) {
  val params = new Parameters(this.pipe.dataAlphabet.size)

  def train(instances: Seq[DependencyInstance], trainForest: File) {
    (0 until options.numIters).map { i =>
      print(" Iteration %d[".format(i))
      val start = System.currentTimeMillis()
      this.trainingIter(instances, trainForest, i + 1)
      val end = System.currentTimeMillis()
      println("|Time:%d]".format(end - start))
      end - start
    }

    this.params.averageParams(options.numIters * instances.size)
  }

  private def trainingIter(instances: Seq[DependencyInstance], trainForest: File, iter: Int) {
    val in = new ObjectInputStream(new BufferedInputStream(new FileInputStream(trainForest), 65536))
    val decoder = if (options.secondOrder) new DependencyDecoder2O(this.pipe) else new DependencyDecoder(this.pipe)

    instances.zipWithIndex.foreach { case (instance, i) =>
      if ((i + 1) % 500 == 0) print("%d,".format(i + 1))

      val (fvs, probs, nt_fvs, nt_probs, fvs_trips, probs_trips, fvs_sibs, probs_sibs) = this.createArrays(instance.length)

      this.pipe.readInstance(in, instance.length, fvs, probs, fvs_trips, probs_trips, fvs_sibs, probs_sibs, nt_fvs, nt_probs, params)

      val upd = (options.numIters * instances.size - (instances.size * (iter - 1) + (i + 1)) + 1).toDouble

      val d = options.decodeType match {
        case "non-proj" =>
          if (options.secondOrder)
            decoder.asInstanceOf[DependencyDecoder2O].decodeNonProjective(
              instance, fvs, probs,
              fvs_trips, probs_trips,
              fvs_sibs, probs_sibs,
              nt_fvs, nt_probs, options.trainK
            )
          else decoder.decodeNonProjective(instance.length, fvs, probs, nt_fvs, nt_probs, options.trainK)
        case _ =>
          if (options.secondOrder)
            decoder.asInstanceOf[DependencyDecoder2O].decodeProjective(
              instance.length, fvs, probs,
              fvs_trips, probs_trips,
              fvs_sibs, probs_sibs,
              nt_fvs, nt_probs, options.trainK
            )
          else decoder.decodeProjective(instance.length, fvs, probs, nt_fvs, nt_probs, options.trainK)
      }

      this.params.updateParamsMIRA(this.pipe, instance, d, upd)
    }
    System.out.print(instances.size)
    in.close()
  }

  private def createArrays(length: Int) = (
    Array.ofDim[FeatureVector](length, length, 2),
    Array.ofDim[Double](length, length, 2),
    Array.ofDim[FeatureVector](length, pipe.typeAlphabet.size, 2, 2),
    Array.ofDim[Double](length, pipe.typeAlphabet.size, 2, 2),

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
    out.writeBoolean(this.pipe.labeled)
    out.close()
  }

  def loadModel(file: String) {
    val in = new ObjectInputStream(new BufferedInputStream(new FileInputStream(file)))
    this.params.parameters = in.readObject().asInstanceOf[Array[Double]]
    this.pipe = if (this.options.secondOrder)
      new DependencyPipe2O(this.options,
        in.readObject().asInstanceOf[Alphabet[String]],
        in.readObject().asInstanceOf[Alphabet[String]],
        in.readBoolean()
      )
    else
      new DependencyPipe(this.options,
        in.readObject().asInstanceOf[Alphabet[String]],
        in.readObject().asInstanceOf[Alphabet[String]],
        in.readBoolean()
      )
    //this.pipe.dataAlphabet = in.readObject().asInstanceOf[Alphabet[String]]
    //this.pipe.typeAlphabet = in.readObject().asInstanceOf[Alphabet[String]]
    //this.pipe.labeled = in.readBoolean()

    in.close()
  }

  def outputParses() = {
    val start = System.currentTimeMillis

    this.pipe.initInputFile(options.testFile.get)
    this.pipe.initOutputFile(options.outFile)
    val instances = pipe.createInstances(options.testFile.get, null, false)
    val decoder = if (options.secondOrder) new DependencyDecoder2O(this.pipe) else new DependencyDecoder(this.pipe)

    print("Processing Sentence: ")

    instances.zipWithIndex.foreach { case (instance, cnt) =>
      print("%d ".format(cnt + 1))
      val forms = instance.forms

      val (fvs, probs, nt_fvs, nt_probs, fvs_trips, probs_trips, fvs_sibs, probs_sibs) = this.createArrays(instance.forms.length)
      this.pipe.fillFeatureVectors(instance, fvs, probs, fvs_trips, probs_trips, fvs_sibs, probs_sibs, nt_fvs, nt_probs, params)

      val d = options.decodeType match {
        case "non-proj" =>
          if (options.secondOrder)
            decoder.asInstanceOf[DependencyDecoder2O].decodeNonProjective(
              instance, fvs, probs,
              fvs_trips, probs_trips,
              fvs_sibs, probs_sibs,
              nt_fvs,nt_probs, options.testK
            )
          else decoder.decodeNonProjective(instance.length, fvs, probs, nt_fvs, nt_probs, options.testK)
        case _ =>
          if (options.secondOrder)
            decoder.asInstanceOf[DependencyDecoder2O].decodeProjective(
              instance.length, fvs, probs,
              fvs_trips, probs_trips,
              fvs_sibs, probs_sibs,
              nt_fvs,nt_probs, options.testK
            )
          else decoder.decodeProjective(instance.length, fvs, probs, nt_fvs, nt_probs, options.testK)
      }

      val (parse, labels) = d.head._2
      pipe.outputInstance(new DependencyInstance(instance.forms.tail, null, null, instance.cpostags.tail, null, labels.tail.map(this.pipe.typeAlphabet.values), parse.tail, null))
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
    val pw = new java.io.PrintWriter("opts.txt")
    pw.write(options.toString)
    pw.close
    if (options.train) {
      val pipe =
        if (options.secondOrder) new DependencyPipe2O(options)
        else new DependencyPipe(options)

      pipe.createAlphabets(options.trainFile.get)
      val instances = pipe.createInstances(options.trainFile.get, options.trainForest.get, options.createForest)

      val dp = new DependencyParser(pipe, options)

      val numFeats = pipe.dataAlphabet.size
      val numTypes = pipe.typeAlphabet.size
      println("Num Feats: %d.\tNum Edge Labels: %d".format(numFeats, numTypes))

      dp.train(instances, options.trainForest.get)

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
      dp.outputParses()
    }

    println()

    if (options.eval) {
      println("\nEVALUATION PERFORMANCE:")
      DependencyEvaluator.evaluate(options.goldFile.get, options.outFile, options.format)
    }
  }
}

