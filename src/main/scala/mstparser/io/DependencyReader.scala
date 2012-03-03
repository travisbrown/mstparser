package mstparser.io

import scala.io.Source

import mstparser.DependencyInstance
import mstparser.RelationalFeature

/**
 * A class that defines common behavior and abstract methods for
 * readers for different formats.
 *
 * <p>
 * Created: Sat Nov 10 15:25:10 2001
 * </p>
 *
 * @author Jason Baldridge
 * @version $Id$
 */
object DependencyReader {
  def createDependencyReader(format: String, discourseMode: Boolean) = format match {
    case "MST" => new MSTReader()
    case "CONLL" => new CONLLReader(discourseMode)
    case _ =>
      println("!!!!!!!  Not a supported format: " + format)
      println("********* Assuming CONLL format. **********")
      new CONLLReader(discourseMode)
  }

  def createDependencyReader(format: String): DependencyReader = this.createDependencyReader(format, false)
}

abstract class DependencyReader extends Iterator[DependencyInstance] {
  protected var source: Source = _
  protected var lines: Iterator[String] = _
  private var labeled = true
  var instanceCount = 0
  var tokenCount = 0

  def isLabeled = this.labeled

  def startReading(file: String) = {
    this.labeled = this.fileContainsLabels(file)
    this.source = Source.fromFile(file, "UTF-8")
    this.lines = this.source.getLines
    this.isLabeled
  }

  def hasNext = this.lines.hasNext

  def next = {
    val instance = this.nextInstance
    this.tokenCount += instance.size
    this.instanceCount += 1
    instance
  }

  def close() { this.source.close() }

  protected def nextInstance: DependencyInstance
  protected def fileContainsLabels(filename: String): Boolean

  protected def normalize(s: String) =
    if (s.matches("[0-9]+|[0-9]+\\.[0-9]+|[0-9]+[0-9,]+")) "<num>"
    else s
}

class CONLLReader(discourseMode: Boolean) extends DependencyReader {
  protected def nextInstance = {
    val instance = this.lines.takeWhile(
      line => !line.isEmpty && !line.startsWith("*")
    ).map(_.split("\t")).toList

      val forms = Array.ofDim[String](instance.size + 1)
      val lemmas = Array.ofDim[String](instance.size + 1)
      val cpos = Array.ofDim[String](instance.size + 1)
      val pos = Array.ofDim[String](instance.size + 1)
      var feats = Array.ofDim[IndexedSeq[String]](instance.size + 1)
      val deprels = Array.ofDim[String](instance.size + 1)
      val heads = Array.ofDim[Int](instance.size + 1)

      forms(0) = "<root>"
      lemmas(0) = "<root-LEMMA>"
      cpos(0) = "<root-CPOS>"
      pos(0) = "<root-POS>"
      deprels(0) = "<no-type>"
      heads(0) = -1

      instance.zipWithIndex.foreach { case (info, i) =>
	      forms(i + 1) = this.normalize(info(1))
	      lemmas(i + 1) = this.normalize(info(2))
        cpos(i + 1) = info(3)
        pos(i + 1) = info(4)
        feats(i + 1) = info(5).split("\\|")
        deprels(i + 1) = if (this.isLabeled) info(7) else "<no-type>"
        heads(i + 1) = info(6).toInt
      }

      feats(0) = feats(1).zipWithIndex.map {
        case (_, i) => "<root-feat>%d".format(i)
      }

      // The following stuff is for discourse and can be safely
      // ignored if you are doing sentential parsing. (In theory it
      // could be useful for sentential parsing.)
      //if (this.discourseMode)
      //  feats = Array.tabulate(feats(0).length, instance.size + 1) { (i, j) => feats(j)(i) }

      //val rfeatsList = this.lines.takeWhile(!_.isEmpty).toArray
      val rfeatsList = Array.empty[RelationalFeature]

      new DependencyInstance(forms, lemmas, cpos, pos, feats, deprels, heads, rfeatsList)
  }

  protected def fileContainsLabels(filename: String) = {
    val in = Source.fromFile(filename, "UTF-8")
    val lines = in.getLines
    val line = lines.next
    in.close()
    line.trim.length > 0
  }
}

class MSTReader extends DependencyReader {
  protected def nextInstance = {
      val forms = this.lines.next.split("\t").map(this.normalize)
      val pos = this.lines.next.split("\t")
      val deprels = if (this.isLabeled) this.lines.next.split("\t") else Array.fill(pos.length)("<no-type>")
      val heads = this.lines.next.split("\t").map(_.toInt)
      this.lines.next

      val lemmas = forms.map(form => if (form.length > 5) form.substring(0, 5) else form)
      val cpostags = pos.map(_.substring(0, 1))

	    new DependencyInstance(
        "<root>" +: forms,
        "<root-LEMMA>" +: lemmas,
        "<root-CPOS>" +: cpostags,
        "<root-POS>" +: pos,
        IndexedSeq.empty[IndexedSeq[String]],
        "<no-type>" +: deprels,
        -1 +: heads,
        null
      )
  }

  protected def fileContainsLabels(filename: String) = {
    val in = Source.fromFile(filename, "UTF-8")
    val rows = in.getLines.takeWhile(_.trim.length > 0).size
    in.close()
    rows == 4
  }
}

