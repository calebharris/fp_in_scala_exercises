package fpinscala.applicatives

import fpinscala.functors.Functor
import fpinscala.monoids.Monoid
import fpinscala.testing.{Gen, Prop}

import scala.language.{higherKinds, implicitConversions, reflectiveCalls}

/**
  * Applicative Functor trait
  *
  * @author caleb
  */
trait Applicative[F[_]] extends Functor[F] { self =>
  //primitive combinators (aka abstract methods)
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]

  //derived combinators
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_(_))

  def assoc[A, B, C](p: (A, (B, C))): ((A, B), C) =
    p match { case (a, (b, c)) => ((a, b), c)}

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fa, fb)(G.map2(_, _)(f))

      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
    }

  def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    as.foldRight(unit(Nil: List[A])) { (h, t) =>
      map2(f(h), t) { (b, as1) =>
        if (b) h :: as1 else as1
      }
    }

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def map3[A, B, C, D](fa: F[A],
                       fb: F[B],
                       fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A],
                          fb: F[B],
                          fc: F[C],
                          fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
        (self.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))

      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
    }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    if (n <= 0) unit(List[A]()) else map2(ma, replicateM(n - 1, ma))(_ :: _)

  def sequence[A](lma: List[F[A]]): F[List[A]] = traverse(lma)(fa => fa)

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K,V]] =
    ofa.foldLeft(unit(Map[K, V]())) { (fm, p) =>
      val (k, fv) = p
      map2(fm, fv)((map, v) => map + (k -> v))
    }

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  //lets define lots of stuff in terms of other stuff (Ex. 12.2)
  def mapViaApply[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  def map2ViaApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(apply(unit(f.curried))(fa))(fb)
}

object Applicative {
  type Id[A] = A

  implicit val idApplicative: Applicative[Id] = new Applicative[Id] {
    override def unit[A](a: => A): Id[A] = a

    override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] =
      f(fa, fb)
  }

  type Const[M, B] = M

  implicit def monoidApp[M](M: Monoid[M]): Applicative[({type f[x] = Const[M, x]})#f] =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      override def unit[A](a: => A): M = M.zero
      override def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): Const[M, C] = M.op(m1, m2)
    }

  implicit def validationApp[E]: Applicative[({type f[x] = Validation[E, x]})#f] =
    new Applicative[({type f[x] = Validation[E, x]})#f] {
      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
        Validation.map2(fa, fb)(f)

      override def unit[A](a: => A): Validation[E, A] = Success(a)
    }

  object Laws {
    def productF[I, O, I2, O2](f: I => O, g: I2 => O2): (I, I2) => (O, O2) = (i, i2) => (f(i), g(i2))

    def leftMapIdentity[F[_], A](g: Gen[F[A]])(implicit F: Applicative[F]): Prop =
      Gen.forAll(g)(fa => F.map2(F.unit(()), fa)((_, a) => a) == fa)

    def rightMapIdentity[F[_], A](g: Gen[F[A]])(implicit F: Applicative[F]): Prop =
      Gen.forAll(g)(fa => F.map2(fa, F.unit(()))((a, _) => a) == fa)

    def associativity[F[_], A, B, C](ga: Gen[F[A]],
                                     gb: Gen[F[B]],
                                     gc: Gen[F[C]])(implicit F: Applicative[F]): Prop =
      Gen.forAll((ga ** gb) ** gc) { case ((fa, fb), fc) =>
        F.product(F.product(fa, fb), fc) == F.map(F.product(fa, F.product(fb, fc)))(F.assoc)
      }

    def naturality[F[_], A, B, C, D](ga: Gen[F[A]],
                                     gb: Gen[F[B]])
                                    (f: A => C,
                                     g: B => D)(implicit F: Applicative[F]): Prop =
      Gen.forAll(ga ** gb) { case (fa, fb) =>
        F.map2(fa, fb)(productF(f, g)) == F.product(F.map(fa)(f), F.map(fb)(g))
      }
  }
}
