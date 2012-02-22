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

package mstparser

import gnu.trove.list.TLinkable
import scala.reflect.BeanProperty

class Feature(
  @BeanProperty val index: Int,
  @BeanProperty val value: Double
) extends TLinkable[Feature] {
  @BeanProperty var next: Feature = _ 
  @BeanProperty var previous: Feature = _ 

  override def toString = this.index + " " + this.value
}

class RelationalFeature(size: Int, declaration: String, lines: Array[String]) {
  require(lines.length == size)
  private val decFields = declaration.split(" ")
  private val name = this.decFields(2)
  private val values = lines.map(_.substring(2).split(" "))

  def getFeature(firstIndex: Int, secondIndex: Int) =
    if (firstIndex == 0 || secondIndex == 0) this.name + "=NULL"
    else this.name + "=" + this.values(firstIndex - 1)(secondIndex - 1)
}
