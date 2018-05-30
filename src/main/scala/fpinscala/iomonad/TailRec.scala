package fpinscala.iomonad

import fpinscala.monads.Monad

import scala.io.StdIn._

/**
  * @author caleb
  */

object tailrec {

  sealed trait TailRec[A] {
    self =>
    def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))

    def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(self, f)
  }

  /**
    * A pure computation that immediately returns an `A` without any further steps.
    * When `run` sees this constructor, it knows the computation has finished.
    */
  case class Return[A](a: A) extends TailRec[A]

  /**
    * A suspension of the computation where `resume` is a function that takes no
    * arguments but has an effect and yields a result.
    */
  case class Suspend[A](resume: () => A) extends TailRec[A]

  /**
    * A composition of two steps. Reifies `flatMap` as a data constructor rather
    * than a function. When `run` sees this, it should first process the sub-
    * computation `sub` and then continue with `k` once `sub` produces a result.
    */
  case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

  object TailRec extends Monad[TailRec] {

    sealed class IORef[A](var value: A) {
      def set(a: A): TailRec[A] = TailRec {
        value = a; a
      }

      def get: TailRec[A] = TailRec {
        value
      }

      def modify(f: A => A): TailRec[A] = get flatMap (a => set(f(a)))
    }

    override def unit[A](a: => A): TailRec[A] = Return(a)

    override def flatMap[A, B](fa: TailRec[A])(f: A => TailRec[B]): TailRec[B] =
      fa flatMap f

    def apply[A](a: => A): TailRec[A] = unit(a)

    def ref[A](a: A): TailRec[IORef[A]] = TailRec {
      new IORef(a)
    }

    @annotation.tailrec
    def run[A](t: TailRec[A]): A = t match {
      case Return(a) => a
      case Suspend(r) => r()
      case FlatMap(x, f) => x match {
        case Return(a) => run(f(a))
        case Suspend(r) => run(f(r()))
        case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
      }
    }

    def ReadLine: TailRec[String] = TailRec {
      readLine
    }

    def PrintLine(msg: String): TailRec[Unit] = Suspend(() => println(msg))
  }

}

