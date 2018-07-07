package fpinscala.mutability

import fpinscala.monads.Monad

import scala.language.reflectiveCalls

/**
  * @author caleb
  */
trait ST[S, A] { self =>

  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = (s: S) => {
    val (a, s1) = self.run(s)
    (f(a), s1)
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = (s: S) => {
    val (a, s1) = self.run(s)
    f(a).run(s1)
  }



}

object ST {
  def apply[S, A](a: => A): ST[S, A] = {
    lazy val memo = a
    s: S => (memo, s)
  }

  def noop[S]: ST[S, Unit] = ST(())

  def noop[S](f: => Unit): ST[S, Unit] = {f; ST(())}

  def runST[A](st: RunnableST[A]): A = st[Unit].run(())._1

  implicit def monad[S]: Monad[({type f[x] = ST[S, x]})#f] =
    new Monad[({type f[x] = ST[S, x]})#f] {
      override def unit[A](a: => A): ST[S, A] = ST(a)

      override def flatMap[A, B](sa: ST[S, A])(f: A => ST[S, B]): ST[S, B] = sa flatMap f
    }
}

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}