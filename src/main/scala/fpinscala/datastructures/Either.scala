package fpinscala.datastructures

import scala.{Either => _, Left => _, Right => _, _}

/**
  * @author caleb
  */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C) =
    for { a <- this; b1 <- b } yield f(a, b1)
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(e => e)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    List.foldRight[A, Either[E, List[B]]](as, Right(Nil))((h, et) => f(h).map2(et)(Cons(_, _)))

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = Try(x / y)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)
}

object Insurance {
  import Either.Try

  def parseRateQuote(age: String, numberOfTickets: String): Either[Exception, Double] =
    for {
      a <- Try { age.toInt }
      n <- Try { numberOfTickets.toInt }
    } yield rateQuote(a, n)

  def rateQuote(age: Int, numberOfTickets: Int): Double = numberOfTickets * 100.0 / age
}
