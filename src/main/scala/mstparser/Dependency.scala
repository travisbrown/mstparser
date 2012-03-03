package mstparser

import mstparser.Instance

import scala.reflect.BeanProperty

case class Word(
  @BeanProperty val form: String,
  @BeanProperty val lemma: String,
  @BeanProperty val pos: String,
  @BeanProperty val cPos: String
) {
}

abstract class DependencyInstance extends Instance[Word] {
}

