package mstparser

trait Evaluator {
  def evaluateParsed[A, B <: GoldParsed[A] with PredParsed[A]](instance: B) =
    instance.goldParse.parents.zip(instance.predParses.head._1.parents).map {
      case (g, p) => g != p
    }.size

  def evaluateLabeled[A, B, C <: GoldLabeled[A, B] with PredLabeled[A, B]](instance: C) =
    instance.goldParse.labels.zip(instance.predParses.head._1.labels).map {
      case (g, p) => g != p
    }.size
}

