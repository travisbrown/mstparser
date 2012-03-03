package mstparser

import scala.collection.IndexedSeqOptimized
import scala.collection.SeqLike
import scala.collection.mutable.Builder

trait Corpus[A, B <: Instance[A]] extends Seq[B]
  with SeqLike[B, Corpus[A, B]] {
  override def newBuilder: Builder[B, Corpus[A, B]] =
    Seq.newBuilder[B].mapResult(Corpus.fromSeq)
}

trait GoldParsedCorpus[A, B <: GoldParsed[A]] extends Corpus[A, B]
trait GoldLabeledCorpus[A, B, C <: GoldLabeled[A, B]] extends GoldParsedCorpus[A, C]

trait PredParsedCorpus[A, B <: PredParsed[A]] extends Corpus[A, B]
trait PredLabeledCorpus[A, B, C <: PredLabeled[A, C]] extends PredParsedCorpus[A, C]

object Corpus {
  def fromSeq[A, B <: Instance[A]](s: Seq[B]) = new Corpus[A, B] {
    def apply(i: Int) = s(i)
    def length = s.length
    def iterator = s.iterator
  }
}

