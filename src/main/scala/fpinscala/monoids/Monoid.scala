package fpinscala.monoids

import fpinscala.parallel.Par
import fpinscala.testing.Gen.forAll
import fpinscala.testing.{Gen, Prop}

/**
  * @author caleb
  */
trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  import MonoidInstances._
  import Par._

  def foldMap[A, B](as: List[A], M: Monoid[B])(f: A => B): B = as.foldLeft(M.zero)((b, a) => M.op(b, f(a)))

  def foldMapV[A, B](v: IndexedSeq[A], M: Monoid[B])(f: A => B): B = v match {
    case IndexedSeq() => M.zero
    case IndexedSeq(a) => f(a)
    case IndexedSeq(a, b) => M.op(f(a), f(b))
    case _ =>
      val (v1, v2) = v.splitAt(v.length / 2)
      M.op(foldMapV(v1, M)(f), foldMapV(v2, M)(f))
  }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as, EndoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = foldMap(as, dual(EndoMonoid[B]))(a => b => f(b, a))(z)

  def par[A](M: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def zero: Par[A] = Par.unit(M.zero)

    override def op(p1: Par[A], p2: Par[A]): Par[A] = for {
      a1 <- p1
      a2 <- p2
    } yield M.op(a1, a2)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], M: Monoid[B])(f: A => B): Par[B] = foldMapV(v, par(M))(a => Par.unit(f(a)))

  trait MOrdering
  case object Ascending extends MOrdering
  case object Equal extends MOrdering
  case object Descending extends MOrdering
  case object Unordered extends MOrdering

  def ordered[A: Ordering](v: IndexedSeq[A]): Boolean = if (v.length <= 1)
    true
  else {
    val m = new Monoid[MOrdering] {
      val zero = Equal

      def op(a1: MOrdering, a2: MOrdering): MOrdering = {
        if (a1 == a2) a1
        else if (a1 == Equal) a2
        else if (a2 == Equal) a1
        else Unordered
      }
    }
    val comps = v.sliding(2).toIndexedSeq.map { case IndexedSeq(x, y) => Ordering[A].compare(x, y) }
    foldMapV(comps, m) {
      case -1 => Descending
      case 0 => Equal
      case 1 => Ascending
    } != Unordered
  }

  object Laws {
    def associativity[A](M: Monoid[A])(g: Gen[A]): Prop = forAll(for {
      a <- g
      b <- g
      c <- g
    } yield (a, b, c)) {
      case (a, b, c) => M.op(M.op(a, b), c) == M.op(a, M.op(b, c))
    }

    def identity[A](M: Monoid[A])(g: Gen[A]): Prop = forAll(g) { a =>
      M.op(a, M.zero) == a && M.op(M.zero, a) == a
    }

    def homomorphism[A, B](M: Monoid[A], N: Monoid[B])(f: A => B)(g: Gen[A]): Prop = forAll(g.tupled) { case(a1, a2) =>
        N.op(f(a1), f(a2)) == f(M.op(a1, a2))
    }

    def isomorphism[A, B](M: Monoid[A], N: Monoid[B])(f: A => B, g: B => A)(ga: Gen[A], gb: Gen[B]): Prop =
      forAll(ga)(a => (f andThen g)(a) == a) &&
      forAll(gb)(b => (g andThen f)(b) == b) &&
      homomorphism(M, N)(f)(ga) &&
      homomorphism(N, M)(g)(gb)
  }
}

object MonoidInstances {
  import Monoid._

  object StringMonoid extends Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    override val zero: String = ""
  }

  type ListMonoid[A] = Monoid[List[A]]
  def ListMonoid[A]: ListMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    override val zero: List[A] = Nil
  }

  object IntAddition extends Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override val zero: Int = 0
  }

  object IntMultiplication extends Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override val zero: Int = 1
  }

  object BooleanOr extends Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override val zero: Boolean = false
  }

  object BooleanAnd extends Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override val zero: Boolean = true
  }

  type OptionMonoid[A] = Monoid[Option[A]]
  def OptionMonoid[A]: OptionMonoid[A] = new Monoid[Option[A]] {
    override def op(o1: Option[A], o2: Option[A]): Option[A] = o1 orElse o2
    override val zero: Option[A] = None
  }

  type EndoMonoid[A] = Monoid[A => A]
  def EndoMonoid[A]: EndoMonoid[A] = new Monoid[A => A] {
    override def op(f: A => A, g: A => A): A => A = f compose g
    override val zero: A => A = { a => a }
  }

  type ProductMonoid[A, B] = Monoid[(A, B)]
  def ProductMonoid[A, B](A: Monoid[A], B: Monoid[B]): ProductMonoid[A, B] = new Monoid[(A, B)] {
    override def zero: (A, B) = (A.zero, B.zero)

    override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
  }

  type MapMergeMonoid[K, V] = Monoid[Map[K, V]]
  def MapMergeMonoid[K, V](M: Monoid[V]): MapMergeMonoid[K, V] = new Monoid[Map[K, V]] {
    override def zero: Map[K, V] = Map[K, V]()

    override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] = {
      val keys = a1.keySet ++ a2.keySet
      val merged = scala.collection.mutable.Map[K, V]()

      keys.foldLeft(merged) { (merged, k) =>
        merged += ((k, M.op(a1.getOrElse(k, M.zero),
                            a2.getOrElse(k, M.zero))))
      }

      merged.toMap
    }
  }

  type FunctionMonoid[A, B] = Monoid[A => B]
  def FunctionMonoid[A, B](M: Monoid[B]): FunctionMonoid[A, B] = new Monoid[A => B] {
    override def zero: A => B = _ => M.zero

    override def op(f: A => B, g: A => B): A => B = a => M.op(f(a), g(a))
  }

  def dual[A](M: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a1: A, a2: A): A = M.op(a2, a1)
    override val zero: A = M.zero
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    foldMapV(as, MapMergeMonoid[A, Int](IntAddition))(a => Map(a -> 1))
  }

}