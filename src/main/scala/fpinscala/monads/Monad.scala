package fpinscala.monads

import fpinscala.applicatives.Applicative
import fpinscala.free.{FlatMap, Free, Return}
import fpinscala.parallel.Par
import fpinscala.parsing.Parser
import fpinscala.parsing.StateParsers
import fpinscala.state.State
import fpinscala.testing.Gen

import scala.language.{higherKinds, implicitConversions, reflectiveCalls}

/**
  * @author caleb
  */
trait Monad[F[_]] extends Applicative[F] { self =>
  // abstract functions

  // Declared in Applicative
  override def unit[A](a: => A): F[A]

  def as[A, B](fa: F[A])(b: B): F[B] = map(fa)(_ => b)

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  // derived functions
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    f andThen { flatMap(_)(g) }

  def composeViaJoinMap[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    f andThen { fb => join(map(fb)(g)) }

  def doWhile[A](fa: F[A])(cond: A => F[Boolean]): F[Unit] = for {
    a <- fa
    ok <- cond(a)
    _ <- if (ok) doWhile(fa)(cond) else unit(())
  } yield ()

  def foldM[A, B](l: Stream[A])(z: B)(f: (B, A) => F[B]): F[B] =
    l match {
      case h #:: t => flatMap(f(z, h))(z2 => foldM(t)(z2)(f))
      case _ => unit(z)
    }

  def foldM_[A, B](l: Stream[A])(z: B)(f: (B, A) => F[B]): F[Unit] =
    skip { foldM(l)(z)(f) }

  def foreachM[A](l: Stream[A])(f: A => F[Unit]): F[Unit] = foldM_(l)(())((_, a) => f(a))

  def forever[A, B](fa: F[A]): F[B] = {
    lazy val t: F[B] = forever(fa)
    flatMap(fa)(_ => t)
  }

  def flatMapViaCompose[A, B](fa: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => fa, f)(())

  def flatMapViaJoinMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  // Declared in Applicative
  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  // Must be overridden, because default implementation in Applicative
  // calls map2 (which calls map, which calls map2, which calls map, ...)
  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))

  def sequence_[A](fs: Stream[F[A]]): F[Unit] = foreachM(fs)(skip)

  def sequence_[A](fs: F[A]*): F[Unit] = sequence_(fs.toStream)

  def skip[A](fa: F[A]): F[Unit] = as(fa)(())

  def when[A](b: Boolean)(fa: => F[A]): F[Boolean] =
    if (b) as(fa)(true) else unit(false)

  implicit def wrapWithOps[A](fa: F[A]): MonadOps[F, A] = new MonadOps[F, A] {
    override def flatMap[B](f: A => F[B]): F[B] = self.flatMap(fa)(f)
    override def map[B](f: A => B): F[B] = self.map(fa)(f)
  }

//  Everything below moved to Applicative
//    def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
//      as.foldRight(unit(Nil: List[A])) { (h, t) =>
//        map2(f(h), t) { (b, as1) =>
//          if (b) h :: as1 else as1
//        }
//      }

//  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

//  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
//    if (n <= 0) unit(List[A]()) else map2(ma, replicateM(n - 1, ma))(_ :: _)

//  def sequence[A](lma: List[F[A]]): F[List[A]] = traverse(lma)(fa => fa)

//  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
//    as.foldRight(unit(Nil: List[B]))((h, t) => map2(f(h), t)(_ :: _))
}

trait MonadOps[F[_], A] {
  def flatMap[B](f: A => F[B]): F[B]
  def map[B](f: A => B): F[B]
}

object Monad {
  import Par._

  implicit val function0Monad: Monad[Function0] = new Monad[Function0] {
    override def flatMap[A, B](fa: () => A)(f: A => () => B): () => B = () => f(fa())()

    override def unit[A](a: => A): () => A = () => a
  }

  implicit val genMonad: Monad[Gen] = new Monad[Gen] {
    override def flatMap[A, B](ga: Gen[A])(f: A => Gen[B]): Gen[B] = ga flatMap f

    override def unit[A](a: => A): Gen[A] = Gen.unit(a)
  }

  implicit val idMonad: Monad[Id] = new Monad[Id] {
    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa flatMap f

    override def unit[A](a: => A): Id[A] = Id(a)
  }

  implicit val listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as flatMap f

    override def unit[A](a: => A): List[A] = List(a)
  }

  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](maybeA: Option[A])(f: A => Option[B]): Option[B] = maybeA flatMap f

    override def unit[A](a: => A): Option[A] = Option(a)
  }

  implicit val parMonad: Monad[Par] = new Monad[Par] {
    override def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = Par.fork { Par.flatMap(pa)(f) }

    override def unit[A](a: => A): Par[A] = Par.unit(a)
  }

  implicit val parserMonad: Monad[Parser] = new Monad[Parser] {
    override def flatMap[A, B](pa: Parser[A])(f: A => Parser[B]): Parser[B] = StateParsers.flatMap(pa)(f)

    override def unit[A](a: => A): Parser[A] = StateParsers.unit(a)
  }

  implicit val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def flatMap[A, B](as: Stream[A])(f: A => Stream[B]): Stream[B] = as flatMap f

    override def unit[A](a: => A): Stream[A] = Stream(a)
  }

  implicit def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({type f[x] = Either[E, x]})#f] {
      override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa flatMap f

      override def unit[A](a: => A): Either[E, A] = Right(a)
    }

  implicit def stateMonad[S]: Monad[({type f[x] = State[S, x]})#f] =
    new Monad[({type f[x] = State[S, x]})#f] {
      override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa flatMap f

      override def unit[A](a: => A): State[S, A] = State.unit(a)
    }

  implicit def freeMonad[F[_]]: Monad[({type f[x] = Free[F, x]})#f] =
    new Monad[({type f[x] = Free[F, x]})#f] {
      override def unit[A](a: => A): Free[F, A] = Return[F, A](a)

      override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
        fa flatMap f
    }
}

case class Id[A](value: A) {
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  def map[B](f: A => B): Id[B] = Id(f(value))
}

case class Reader[R, A](run: R => A) {
  def flatMap[B](f: A => Reader[R, B]): Reader[R, B] = Reader(r => f(run(r)).run(r))
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = st flatMap f
  }
}