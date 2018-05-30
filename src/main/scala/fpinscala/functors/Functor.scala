package fpinscala.functors

import fpinscala.testing.{Gen, Prop}

import scala.language.higherKinds

/**
  * @author caleb
  */
trait Functor[F[_]] {
  // abstract functions
  def map[A, B](fa: F[A])(f: A => B): F[B]

  // derived functions
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  implicit val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }

  object Laws {
    def mapIdentity[F[_], A](g: Gen[F[A]])(implicit F: Functor[F]): Prop = Gen.forAll(g) { fa =>
      F.map(fa)(a => a) == fa
    }
  }
}

object FunctorSyntax {
  implicit class FunctorOps[F[_], A](ref: F[A])(implicit F: Functor[F]) {
    def fmap[B](f: A => B): F[B] = F.map(ref)(f)
  }

  implicit class TupleFunctorOps[F[_], A, B](ref: F[(A, B)])(implicit F: Functor[F]) {
    def distribute: (F[A], F[B]) = F.distribute(ref)
  }
}
