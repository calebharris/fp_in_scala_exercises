package fpinscala.monoids

import fpinscala.datastructures.Tree
import fpinscala.monoids.Monoid.foldMapV

import scala.language.higherKinds

/**
  * @author caleb
  */
trait Foldable[F[_]] {
  import MonoidInstances._

  def foldRight[A,B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(EndoMonoid[B])(z)

  def foldLeft[A,B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(EndoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(M: Monoid[B]): B

  def concatenate[A](as: F[A])(M: Monoid[A]): A = foldLeft(as)(M.zero)(M.op)

  def toList[A](fa: F[A]): List[A] = foldRight(fa)(List[A]())(_ :: _)

}

object FoldableInstances {
  import MonoidInstances._

  object ListFoldable extends Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: A => B)(M: Monoid[B]): B = as.foldLeft(M.zero)((b, a) => M.op(b, f(a)))
  }

  object IndexedSeqFoldable extends Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(M: Monoid[B]): B = foldMapV(as, M)(f)
  }

  object StreamFoldable extends Foldable[Stream] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: Stream[A])(f: A => B)(M: Monoid[B]): B = as.foldLeft(M.zero)((b, a) => M.op(b, f(a)))
  }

  object TreeFoldable extends Foldable[Tree] {
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = foldMap(as)(f.curried)(EndoMonoid[B])(z)

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
      foldMap(as)((a: A) => (b: B) => f(b, a))(dual(EndoMonoid[B]))(z)

    override def foldMap[A, B](as: Tree[A])(f: A => B)(M: Monoid[B]): B = Tree.fold(as)(f)(M.op)
  }

  object OptionFoldable extends Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as.map(f(_, z)).getOrElse(z)

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as.map(f(z, _)).getOrElse(z)

    override def foldMap[A, B](as: Option[A])(f: A => B)(M: Monoid[B]): B = as.map(f).getOrElse(M.zero)
  }
}