package mstparser

import java.io.BufferedOutputStream
import java.io.File
import java.io.FileOutputStream
import java.io.IOException
import java.io.ObjectInputStream
import java.io.ObjectOutputStream

import mstparser.io.DependencyReader
import mstparser.io.DependencyWriter

class DependencyPipe(
  protected val options: ParserOptions,
  val dataAlphabet: Alphabet[String],
  val typeAlphabet: Alphabet[String],
  var labeled: Boolean
) {
  def this(options: ParserOptions) = this(options, new Alphabet, new Alphabet, false)

  private val depReader = DependencyReader.createDependencyReader(this.options.format, this.options.discourseMode)
  private var depWriter: DependencyWriter = _
  private var instances: IndexedSeq[DependencyInstance] = _

  def initInputFile(file: String) {
    this.labeled = this.depReader.startReading(file)
  }

  def initOutputFile(file: String) {
    this.depWriter = DependencyWriter.createDependencyWriter(this.options.format, this.labeled)
    this.depWriter.startWriting(file)
  }

  def outputInstance(instance: DependencyInstance) {
    this.depWriter.write(instance)
  }

  def close() {
    if (this.depWriter != null) this.depWriter.finishWriting()
  }

  def add(f: String, fv: FeatureVector) {
    this.add(f, 1.0, fv)
  }

  def add(f: String, v: Double, fv: FeatureVector) {
    val i = this.dataAlphabet.lookupIndex(f)
    if (i >= 0) fv.add(i, v)
  }

  def createAlphabets(file: String) {
    print("Creating Alphabet ... ")

    this.labeled = this.depReader.startReading(file)

    depReader.foreach { instance =>
      instance.deprels.foreach(this.typeAlphabet.lookupIndex(_))
      this.createFeatureVector(instance)
    }

    this.dataAlphabet.stopGrowth()
    this.typeAlphabet.stopGrowth()
    println("Done.")
  }

  def createFeatureVector(instance: DependencyInstance) = {
    val fv = new FeatureVector

    instance.heads.zip(instance.deprels).zipWithIndex.filter(_._1._1 > -1).foreach {
      case ((h, l), i) =>
        val small = math.min(h, i)
        val large = math.max(h, i)
        val attR = i >= h
        this.addCoreFeatures(instance, small, large, attR, fv)
        if (this.labeled) {
          this.addLabeledFeatures(instance, i, l, attR, true, fv)
          this.addLabeledFeatures(instance, h, l, attR, false, fv)
        }
    }

    this.addExtendedFeatures(instance, fv)

    fv
  }

  protected def addExtendedFeatures(instance: DependencyInstance, fv: FeatureVector) {}

  def createInstances(file: String, featFile: File, createForest: Boolean) = {
    this.labeled = this.depReader.startReading(file)

    val out = if (createForest)
      new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(featFile), 65536))
    else null

    println("Creating Feature Vector Instances: ")
    this.instances = this.depReader.zipWithIndex.map { case (instance, i) =>
      print(i + " ")

      instance.featureVector = this.createFeatureVector(instance)

      if (createForest) this.writeInstance(instance, out)
      instance
    }.toIndexedSeq

    println()

    if (createForest) out.close()
    this.instances
  }

  protected def writeInstance(instance: DependencyInstance, out: ObjectOutputStream) {

    (0 until instance.length).foreach { w1 =>
      ((w1 + 1) until instance.length).foreach { w2 =>
        val prodFV1 = new FeatureVector
        this.addCoreFeatures(instance, w1, w2, true, prodFV1)
        out.writeObject(prodFV1.keys)

        val prodFV2 = new FeatureVector
        this.addCoreFeatures(instance, w1, w2, false, prodFV2)
        out.writeObject(prodFV2.keys)
      }
    }

    out.writeInt(-3)

    if (this.labeled) {
      (0 until instance.length).foreach { w1 =>
        this.typeAlphabet.values.foreach { t =>
          val prodFV1 = new FeatureVector
          this.addLabeledFeatures(instance, w1, t, true, true, prodFV1)
          out.writeObject(prodFV1.keys)

          val prodFV2 = new FeatureVector
          this.addLabeledFeatures(instance, w1, t, true, false, prodFV2)
          out.writeObject(prodFV2.keys)

          val prodFV3 = new FeatureVector
          this.addLabeledFeatures(instance, w1, t, false, true, prodFV3)
          out.writeObject(prodFV3.keys)

          val prodFV4 = new FeatureVector
          this.addLabeledFeatures(instance, w1, t, false, false, prodFV4)
          out.writeObject(prodFV4.keys)
        }
      }
      out.writeInt(-3)
    }

    this.writeExtendedFeatures(instance, out)

    out.reset()
  }

  protected def writeExtendedFeatures(instance: DependencyInstance, out: ObjectOutputStream) {}

  def fillFeatureVectors(
    instance: DependencyInstance,
    fvs: Array[Array[Array[FeatureVector]]],
    probs: Array[Array[Array[Double]]],
    fvsTr: Array[Array[Array[FeatureVector]]],
    probsTr: Array[Array[Array[Double]]],
    fvsSi: Array[Array[Array[FeatureVector]]],
    probsSi: Array[Array[Array[Double]]],
    fvsNt: Array[Array[Array[Array[FeatureVector]]]],
    probsNt: Array[Array[Array[Array[Double]]]],
    params: Parameters
  ) {
    (0 until instance.length).foreach { w1 =>
      ((w1 + 1) until instance.length).foreach { w2 =>
        val prodFV1 = new FeatureVector
        this.addCoreFeatures(instance, w1, w2, true, prodFV1)
        fvs(w1)(w2)(0) = prodFV1
        probs(w1)(w2)(0) = params.getScore(prodFV1)

        val prodFV2 = new FeatureVector
        this.addCoreFeatures(instance, w1, w2, false, prodFV2)
        fvs(w1)(w2)(1) = prodFV2
        probs(w1)(w2)(1) = params.getScore(prodFV2)
      }
    }

    if (this.labeled) {
      (0 until instance.length).foreach { w1 =>
        this.typeAlphabet.values.zipWithIndex.foreach { case (t, i) =>
          val prodFV1 = new FeatureVector
          this.addLabeledFeatures(instance, w1, t, true, true, prodFV1)
          fvsNt(w1)(i)(0)(0) = prodFV1
          probsNt(w1)(i)(0)(0) = params.getScore(prodFV1)

          val prodFV2 = new FeatureVector
          this.addLabeledFeatures(instance, w1, t, true, false, prodFV2)
          fvsNt(w1)(i)(0)(1) = prodFV2
          probsNt(w1)(i)(0)(1) = params.getScore(prodFV2)

          val prodFV3 = new FeatureVector
          this.addLabeledFeatures(instance, w1, t, false, true, prodFV3)
          fvsNt(w1)(i)(1)(0) = prodFV3
          probsNt(w1)(i)(1)(0) = params.getScore(prodFV3)

          val prodFV4 = new FeatureVector
          this.addLabeledFeatures(instance, w1, t, false, false, prodFV4)
          fvsNt(w1)(i)(1)(1) = prodFV4
          probsNt(w1)(i)(1)(1) = params.getScore(prodFV4)
        }
      }
    }
  }

  def readInstance(
    in: ObjectInputStream, len: Int,
    fvs: Array[Array[Array[FeatureVector]]],
    probs: Array[Array[Array[Double]]],
    fvsTr: Array[Array[Array[FeatureVector]]],
    probsTr: Array[Array[Array[Double]]],
    fvsSi: Array[Array[Array[FeatureVector]]],
    probsSi: Array[Array[Array[Double]]],
    fvsNt: Array[Array[Array[Array[FeatureVector]]]],
    probsNt: Array[Array[Array[Array[Double]]]],
    params: Parameters
  ) {
    try {
      (0 until len).foreach { w1 =>
        ((w1 + 1) until len).foreach { w2 =>
          val prodFV1 = FeatureVector.fromKeys(in.readObject().asInstanceOf[Array[Int]])
          fvs(w1)(w2)(0) = prodFV1
          probs(w1)(w2)(0) = params.getScore(prodFV1)

          val prodFV2 = FeatureVector.fromKeys(in.readObject().asInstanceOf[Array[Int]])
          fvs(w1)(w2)(1) = prodFV2
          probs(w1)(w2)(1) = params.getScore(prodFV2)
        }
      }

      if (in.readInt() != -3) { println("Error reading file."); sys.exit(0) }

      if (this.labeled) {
        (0 until len).foreach { w1 =>
          this.typeAlphabet.values.zipWithIndex.foreach { case (t, i) =>
            val prodFV1 = FeatureVector.fromKeys(in.readObject().asInstanceOf[Array[Int]])
            fvsNt(w1)(i)(0)(0) = prodFV1
            probsNt(w1)(i)(0)(0) = params.getScore(prodFV1)

            val prodFV2 = FeatureVector.fromKeys(in.readObject().asInstanceOf[Array[Int]])
            fvsNt(w1)(i)(0)(1) = prodFV2
            probsNt(w1)(i)(0)(1) = params.getScore(prodFV2)

            val prodFV3 = FeatureVector.fromKeys(in.readObject().asInstanceOf[Array[Int]])
            fvsNt(w1)(i)(1)(0) = prodFV3
            probsNt(w1)(i)(1)(0) = params.getScore(prodFV3)

            val prodFV4 = FeatureVector.fromKeys(in.readObject().asInstanceOf[Array[Int]])
            fvsNt(w1)(i)(1)(1) = prodFV4
            probsNt(w1)(i)(1)(1) = params.getScore(prodFV4)
          }
        }

        if (in.readInt() != -3) { println("Error reading file."); sys.exit(0) }
      }
    } catch { case e: IOException => println("Error reading file."); sys.exit(0) }
  }

  private def addCoreFeatures(instance: DependencyInstance, small: Int, large: Int, attR: Boolean, fv: FeatureVector) {
    val attDist = "&%s&%d".format(
      if (attR) "RA" else "LA",
      math.abs(large - small) match {
        case dist if dist > 10 => 10
        case dist if dist > 5 => 5
        case dist => dist - 1
      }
    )

    this.addLinearFeatures("POS", instance.postags, small, large, attDist, fv)
    this.addLinearFeatures("CPOS", instance.cpostags, small, large, attDist, fv)

    val (headIndex, childIndex) = if (attR) (small, large) else (large, small)

    this.addTwoObsFeatures("HC",
      instance.forms(headIndex),
      instance.postags(headIndex),
      instance.forms(childIndex),
      instance.postags(childIndex),
      attDist,
      fv
    )

    if (this.options.format == "CONLL") {
      this.addTwoObsFeatures("HCA",
        instance.forms(headIndex),
        instance.cpostags(headIndex),
        instance.forms(childIndex),
        instance.cpostags(childIndex),
        attDist,
        fv
      )

      this.addTwoObsFeatures("HCC",
        instance.lemmas(headIndex),
        instance.postags(headIndex),
        instance.lemmas(childIndex),
        instance.postags(childIndex),
        attDist,
        fv
      )

      this.addTwoObsFeatures("HCD",
        instance.lemmas(headIndex),
        instance.cpostags(headIndex),
        instance.lemmas(childIndex),
        instance.cpostags(childIndex),
        attDist,
        fv
      )

      if (options.discourseMode) {
        /* Note: The features invoked here are designed for discourse parsing
         * (as opposed to sentential parsing). It is conceivable that they
         * could help for sentential parsing, but current testing indicates
         * that they hurt sentential parsing performance.
         */
         this.addDiscourseFeatures(instance, small, large, headIndex, childIndex, attDist, fv)
      } else {
        /* Add in features from the feature lists. It assumes the feature
         * lists can have different lengths for each item. For example, nouns
         * might have a different number of morphological features than verbs.
         */
        for {
          i <- 0 until instance.feats(headIndex).length
          j <- 0 until instance.feats(childIndex).length
        } {
          this.addTwoObsFeatures("FF" + i + "*" + j,
            instance.forms(headIndex),
            instance.feats(headIndex)(i),
            instance.forms(childIndex),
            instance.feats(childIndex)(j),
            attDist, fv
          )

          this.addTwoObsFeatures("LF" + i + "*" + j,
            instance.lemmas(headIndex),
            instance.feats(headIndex)(i),
            instance.lemmas(childIndex),
            instance.feats(childIndex)(j),
            attDist, fv
          )
        }
      }
    } else {
      /* We are using the old MST format. Pick up stem features the way they
       * used to be done. This is kept for replicability of results for old
       * versions.
       */
      val hL = instance.forms(headIndex).length
      val cL = instance.forms(childIndex).length
      if (hL > 5 || cL > 5) {
        this.addOldMSTStemFeatures(
          instance.lemmas(headIndex),
          instance.postags(headIndex),
          instance.lemmas(childIndex),
          instance.postags(childIndex),
          attDist, hL, cL, fv
        )
      }
    }
  }

  protected def addLinearFeatures(
    label: String,
    vals: IndexedSeq[String],
    first: Int,
    second: Int,
    attDist: String,
    fv: FeatureVector
  ) {
    val pL = if (first > 0) vals(first - 1) else "STR"
    val pR = if (second < vals.length - 1) vals(second + 1) else "END"
    val pLR = if (first < second - 1) vals(first + 1) else "MID"
    val pRL = if (second > first + 1) vals(second - 1) else "MID"

    val pos = label + "PC=" + vals(first) + " " + vals(second)

    vals.drop(first + 1).take(second - first - 1).map(pos + " " + _).foreach { allPos =>
      this.add(allPos, fv)
      this.add(allPos + attDist, fv)
    }

    this.addCorePosFeatures(label + "PT", pL, vals(first), pLR, pRL, vals(second), pR, attDist, fv)
  }

  private def addLabeledFeatures(
    instance: DependencyInstance,
    word: Int,
    label: String,
    attR: Boolean,
    childFeatures: Boolean,
    fv: FeatureVector
  ) {
    val att = (if (attR) "RA" else "LA") + "&" + childFeatures
    val w = instance.forms(word)
    val wP = instance.postags(word)
    val wPm1 = if (word > 0) instance.postags(word - 1) else "STR"
    val wPp1 = if (word < instance.size) instance.postags(word + 1) else "END"

    this.add("NTS1=" + label + "&" + att, fv)
    this.add("ANTS1=" + label, fv)

    Seq("&" + att, "").map("&" + label + _).foreach { suff =>
      this.add("NTH=" + w + " " + wP + suff, fv)
      this.add("NTI=" + wP + suff, fv)
      this.add("NTIA=" + wPm1 + " " + wP + suff, fv)
      this.add("NTIB=" + wP + " " + wPp1 + suff, fv)
      this.add("NTIC=" + wPm1 + " " + wP + " " + wPp1 + suff, fv)
      this.add("NTJ=" + w + suff, fv)
    }
  }

  private def addWithAtt(feat: String, att: String, fv: FeatureVector) {
    this.add(feat, fv)
    this.add(feat + att, fv)
  }

  private def addCorePosFeatures(
    prefix: String,
    leftOf1: String, one: String, rightOf1: String,
    leftOf2: String, two: String, rightOf2: String,
    attDistance: String,
    fv: FeatureVector
  ) {
    val att = "*" + attDistance
    this.add(prefix + "=" + leftOf1 + " " + one + " " + two + att, fv)

    var feat = prefix + "1=" + leftOf1+ " " +one+ " " +two
    this.add(feat, fv)
    feat += " " + rightOf2
    this.addWithAtt(feat, att, fv)

    this.addWithAtt(prefix + "2=" + leftOf1 + " " + two + " " + rightOf2, att, fv)
    this.addWithAtt(prefix + "3=" + leftOf1 + " " + one + " " + rightOf2, att, fv)
    this.addWithAtt(prefix + "4=" + one + " " + two + " " + rightOf2, att, fv)

    val prefix2 = "A" + prefix

    feat = prefix2 + "1=" + one + " " + rightOf1 + " " + leftOf2
    this.addWithAtt(feat, att, fv)
    feat += " " + two
    this.addWithAtt(feat, att, fv)

    this.addWithAtt(prefix2 + "2=" + one + " " + rightOf1 + " " + two, att, fv)
    this.addWithAtt(prefix2 + "3=" + one + " " + leftOf2 + " " + two, att, fv)
    this.addWithAtt(prefix2 + "4=" + rightOf1 + " " + leftOf2 + " " + two, att, fv)

    val prefix3 = "B" + prefix2

    this.addWithAtt(prefix3 + "1=" + leftOf1 + " " + one + " " + leftOf2 + " " + two, att, fv)
    this.addWithAtt(prefix3 + "2=" + one + " " + rightOf1 + " " + two + " " + rightOf2, att, fv)
  }

  protected def addTwoObsFeatures(
    prefix: String,
    item1F1: String, item1F2: String,
    item2F1: String, item2F2: String,
    attDistance: String,
    fv: FeatureVector
  ) {
    val att = "*" + attDistance
    this.addWithAtt(prefix + "2FF1=" + item1F1, att, fv)
    this.addWithAtt(prefix + "2FF1=" + item1F1 + " " + item1F2, att, fv)
    this.addWithAtt(prefix + "2FF1=" + item1F1 + " " + item1F2 + " " + item2F2, att, fv)
    this.addWithAtt(prefix + "2FF1=" + item1F1 + " " + item1F2 + " " + item2F2 + " " + item2F1, att, fv)
    this.addWithAtt(prefix + "2FF2=" + item1F1 + " " + item2F1, att, fv)
    this.addWithAtt(prefix + "2FF3=" + item1F1 + " " + item2F2, att, fv)
    this.addWithAtt(prefix + "2FF4=" + item1F2 + " " + item2F1, att, fv)
    this.addWithAtt(prefix + "2FF4=" + item1F2 + " " + item2F1 + " " + item2F2, att, fv)
    this.addWithAtt(prefix + "2FF5=" + item1F2 + " " + item2F2, att, fv)
    this.addWithAtt(prefix + "2FF6=" + item2F1 + " " + item2F2, att, fv)
    this.addWithAtt(feat = prefix + "2FF7=" + item1F2, att, fv)
    this.addWithAtt(prefix + "2FF8=" + item2F1, att, fv)
    this.addWithAtt(prefix + "2FF9=" + item2F2, att, fv)
  }

  private def addDiscourseFeatures(
    instance: DependencyInstance,
    small: Int,
    large: Int,
    headIndex: Int,
    childIndex: Int,
    attDist: String,
    fv: FeatureVector
  ) {

  addLinearFeatures("FORM", instance.forms, small, large, attDist, fv);
  addLinearFeatures("LEMMA", instance.lemmas, small, large, attDist, fv);

  addTwoObsFeatures("HCB1", instance.forms(headIndex),
        instance.lemmas(headIndex),
        instance.forms(childIndex),
        instance.lemmas(childIndex),
        attDist, fv);

  addTwoObsFeatures("HCB2", instance.forms(headIndex),
        instance.lemmas(headIndex),
        instance.forms(childIndex),
        instance.postags(childIndex),
        attDist, fv);

  addTwoObsFeatures("HCB3", instance.forms(headIndex),
        instance.lemmas(headIndex),
        instance.forms(childIndex),
        instance.cpostags(childIndex),
        attDist, fv);

  addTwoObsFeatures("HC2", instance.forms(headIndex),
        instance.postags(headIndex),
        instance.forms(childIndex),
        instance.cpostags(childIndex), attDist, fv);

  addTwoObsFeatures("HCC2", instance.lemmas(headIndex),
        instance.postags(headIndex),
        instance.lemmas(childIndex),
        instance.cpostags(childIndex),
        attDist, fv);


  //// Use this if your extra feature lists all have the same length.
  (0 until instance.feats.size).foreach { i =>
    addLinearFeatures("F" +i, instance.feats(i), small, large, attDist, fv);

    addTwoObsFeatures("FF" +i,
          instance.forms(headIndex),
          instance.feats(i)(headIndex),
          instance.forms(childIndex),
          instance.feats(i)(childIndex),
          attDist, fv);

    addTwoObsFeatures("LF" +i,
          instance.lemmas(headIndex),
          instance.feats(i)(headIndex),
          instance.lemmas(childIndex),
          instance.feats(i)(childIndex),
          attDist, fv);

    addTwoObsFeatures("PF" +i,
          instance.postags(headIndex),
          instance.feats(i)(headIndex),
          instance.postags(childIndex),
          instance.feats(i)(childIndex),
          attDist, fv);

    addTwoObsFeatures("CPF" +i,
          instance.cpostags(headIndex),
          instance.feats(i)(headIndex),
          instance.cpostags(childIndex),
          instance.feats(i)(childIndex),
          attDist, fv);


    (i + 1 until instance.feats.size).foreach { j =>
        addTwoObsFeatures("CPF" +i+ "_" +j,
              instance.feats(i)(headIndex),
              instance.feats(j)(headIndex),
              instance.feats(i)(childIndex),
              instance.feats(j)(childIndex),
              attDist, fv);
    }

    (0 until instance.feats.size).foreach { j =>
        addTwoObsFeatures("XFF" +i+ "_" +j,
              instance.forms(headIndex),
              instance.feats(i)(headIndex),
              instance.forms(childIndex),
              instance.feats(j)(childIndex),
              attDist, fv);

        addTwoObsFeatures("XLF" +i+ "_" +j,
              instance.lemmas(headIndex),
              instance.feats(i)(headIndex),
              instance.lemmas(childIndex),
              instance.feats(j)(childIndex),
              attDist, fv);

        addTwoObsFeatures("XPF" +i+ "_" +j,
              instance.postags(headIndex),
              instance.feats(i)(headIndex),
              instance.postags(childIndex),
              instance.feats(j)(childIndex),
              attDist, fv);


        addTwoObsFeatures("XCF" +i+ "_" +j,
              instance.cpostags(headIndex),
              instance.feats(i)(headIndex),
              instance.cpostags(childIndex),
              instance.feats(j)(childIndex),
              attDist, fv);
    }
  }

  if (options.useRelationalFeatures) {

      //for (int rf_index=0; rf_index<2; rf_index++) {
      (0 until instance.relFeats.size).foreach { rf_index =>

        val headToChild =
        "H2C" +rf_index+instance.relFeats(rf_index).getFeature(headIndex, childIndex);

    addTwoObsFeatures("RFA1",
          instance.forms(headIndex),
          instance.lemmas(headIndex),
          instance.postags(childIndex),
          headToChild,
          attDist, fv);

    addTwoObsFeatures("RFA2",
          instance.postags(headIndex),
          instance.cpostags(headIndex),
          instance.forms(childIndex),
          headToChild,
          attDist, fv);

        addTwoObsFeatures("RFA3",
          instance.lemmas(headIndex),
          instance.postags(headIndex),
          instance.forms(childIndex),
          headToChild,
          attDist, fv);

        addTwoObsFeatures("RFB1",
          headToChild,
          instance.postags(headIndex),
          instance.forms(childIndex),
          instance.lemmas(childIndex),
          attDist, fv);

        addTwoObsFeatures("RFB2",
          headToChild,
          instance.forms(headIndex),
          instance.postags(childIndex),
          instance.cpostags(childIndex),
          attDist, fv);

        addTwoObsFeatures("RFB3",
          headToChild,
          instance.forms(headIndex),
          instance.lemmas(childIndex),
          instance.postags(childIndex),
          attDist, fv);

      }
  }
    }

  private def addOldMSTStemFeatures(
    hLemma: String,
    headP: String,
    cLemma: String,
    childP: String,
    attDist: String,
    hL: Int,
    cL: Int,
    fv: FeatureVector
  ) {
    val all = hLemma + " " + headP + " " + cLemma + " " + childP
    val hPos = headP + " " + cLemma + " " + childP
    val cPos = hLemma + " " + headP + " " + childP
    val hP = headP + " " + cLemma
    val cP = hLemma + " " + childP
    val oPos = headP + " " + childP
    val oLex = hLemma + " " + cLemma

    this.add("SA=" + all + attDist, fv)
    this.add("SF=" + oLex + attDist, fv)
    this.add("SAA=" + all, fv)
    this.add("SFF=" + oLex, fv)

    if (cL > 5) {
      this.add("SB=" + hPos + attDist, fv)
      this.add("SD=" + hP + attDist, fv)
      this.add("SK=" + cLemma + " " + childP+attDist, fv)
      this.add("SM=" + cLemma+ attDist, fv)
      this.add("SBB=" + hPos, fv)
      this.add("SDD=" + hP, fv)
      this.add("SKK=" + cLemma + " " + childP, fv)
      this.add("SMM=" + cLemma, fv)
    }

    if (hL > 5) {
      this.add("SC=" + cPos + attDist, fv)
      this.add("SE=" + cP + attDist, fv)
      this.add("SH=" + hLemma + " " + headP + attDist, fv)
      this.add("SJ=" + hLemma + attDist, fv)

      this.add("SCC=" + cPos, fv)
      this.add("SEE=" + cP, fv)
      this.add("SHH=" + hLemma + " " + headP, fv)
      this.add("SJJ=" + hLemma, fv)
    }
  }
}

