package fpinscala.datastructures

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Either => _, None => _, Option => _, Some => _, _}

/**
  * @author caleb
  */
sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(av => b.map(bv => f(av, bv)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(b => b)

  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    List.foldRight[A,Option[List[B]]](a, Some(Nil))((h, t) => map2(f(h), t)(Cons(_, _)))

  def parseInts(a: List[String]): Option[List[Int]] = traverse(a)(i => Try(i.toInt))

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap((m) => {
      Some(xs.foldLeft(0.0)((z: Double, x: Double) => z + math.pow(m - x, 2)) / xs.length)
    })
}

