/* Copyright (C) 2002 Univ. of Massachusetts Amherst, Computer Science Dept.
   This file is part of "MALLET" (MAchine Learning for LanguagE Toolkit).
   http://www.cs.umass.edu/~mccallum/mallet
   This software is provided under the terms of the Common Public License,
   version 1.0, as published by http://www.opensource.org.  For further
   information, see the file `LICENSE' included with this distribution. */




/** 
    @author Andrew McCallum <a href="mailto:mccallum@cs.umass.edu">mccallum@cs.umass.edu</a>
*/

package mstparser

import gnu.trove.map.hash.TObjectIntHashMap

import scala.reflect.BeanProperty

class Alphabet(private val capacity: Int) extends Serializable {
  def this() = this(10000)

	private val map = new TObjectIntHashMap[String](capacity, 0.75F, -1)
  private var growing = true

  /** Return -1 if entry isn't present. */
  def lookupIndex(entry: String) = {
    require(entry != null, "Can't lookup \"null\" in an Alphabet.")

    this.map.get(entry) match {
      case -1 if this.growing =>
        this.map.put(entry, this.map.size)
        this.map.size - 1
      case i => i
    }
  }

  def toArray = this.map.keys(Array.empty[String])

  def contains(entry: String) = this.map.contains(entry)
  def size = this.map.size 

  def getGrowing = this.growing

  def setGrowing(growing: Boolean) {
    if (!growing) this.map.compact()
    this.growing = growing
  }
}

