package mstparser

trait Parser[A] {
  def k: Int
  def parse[B <: Instance[A], C <: B with Parsed[A]](instance: B): C
}

trait LabelingParser[A, B] extends Parser[A] {
  def parseAndLabel[C <: Instance[A], D <: C with Labeled[A, B]](instance: C): D
}

