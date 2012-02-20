///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2007 University of Texas at Austin and (C) 2005
// University of Pennsylvania and Copyright (C) 2002, 2003 University
// of Massachusetts Amherst, Department of Computer Science.
//
// This software is licensed under the terms of the Common Public
// License, Version 1.0 or (at your option) any subsequent version.
// 
// The license is approved by the Open Source Initiative, and is
// available from their website at http://www.opensource.org.
///////////////////////////////////////////////////////////////////////////////

package mstparser.io

import java.io.PrintWriter

import mstparser.DependencyInstance

/**
 * A class that defines common behavior and abstract methods for
 * writers for different formats.
 *
 * <p>
 * Created: Sat Nov 10 15:25:10 2001
 * </p>
 *
 * @author Jason Baldridge
 * @version $Id$
 */
object DependencyWriter {
  def createDependencyWriter(format: String, labeled: Boolean) = format match {
    case "MST" => new MSTWriter(labeled)
    case "CONLL" => new CONLLWriter(labeled)
    case _ =>
      println("!!!!!!!  Not a supported format: " + format)
      println("********* Assuming CONLL format. **********")
      new CONLLWriter(labeled)
  }
}

abstract class DependencyWriter(private val labeled: Boolean) {
  protected var writer: PrintWriter = _

  def startWriting(file: String) {
    this.writer = new PrintWriter(file, "UTF-8")
  }

  def finishWriting() {
    writer.close()
  }

  def isLabeled: Boolean = this.labeled

  def write(instance: DependencyInstance): Unit
}

class MSTWriter(labeled: Boolean) extends DependencyWriter(labeled) {
  def write(instance: DependencyInstance) {
    this.writer.println(instance.forms.mkString("\t"))
    this.writer.println(instance.postags.mkString("\t"))
    if (labeled) this.writer.println(instance.deprels.mkString("\t"))
    this.writer.println(instance.heads.mkString("\t"))
    this.writer.println()
  }
}

class CONLLWriter(labeled: Boolean) extends DependencyWriter(labeled) {
  def write(instance: DependencyInstance) {
    (0 until instance.length).foreach { i =>
      val line = List(
        (i + 1).toString,
        instance.forms(i),
        instance.forms(i),
        //instance.cpostags(i),
        instance.postags(i),
        instance.postags(i),
        "-",
        instance.heads(i).toString,
        instance.deprels(i),
        "-",
        "-"
      ).mkString("\t")
      this.writer.println(line)
    }
    this.writer.println()
  }
}

