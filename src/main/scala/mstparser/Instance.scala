package mstparser

import scala.collection.IndexedSeqOptimized
import scala.collection.SeqLike
import scala.collection.mutable.Builder

trait Instance[A] extends IndexedSeq[A]
  with IndexedSeqOptimized[A, Instance[A]] {
  override def newBuilder: Builder[A, Instance[A]] =
    IndexedSeq.newBuilder[A].mapResult(Instance.fromIndexedSeq)

  trait Parsing {
    protected def parents: IndexedSeq[Int]
    def getParent(i: Int) = this.parents(i)
    def withParents = Instance.this.zip(this.parents)    
  }

  trait Labeling[B] extends Parsing {
    protected def labels: IndexedSeq[B]
    def getLabel(i: Int) = this.labels(i)
    def withRelations = Instance.this.zip(this.parents.zip(this.labels))
  }
}

trait GoldParsed[A] extends Instance[A] {
  def goldParse: Parsing
}

trait GoldLabeled[A, B] extends GoldParsed[A] {
  def goldParse: Labeling[B]
}

trait PredParsed[A] extends Instance[A] {
  def k = this.predParses.size
  def predParses: Seq[(Parsing, Double)]
}

trait PredLabeled[A, B] extends PredParsed[A] {
  def predParses: Seq[(Labeling[B], Double)]
}

object Instance {
  def fromIndexedSeq[A](s: IndexedSeq[A]) = new Instance[A] {
    def apply(i: Int) = s(i)
    def length = s.length
  }
}

