package fpinscala.laziness

import Stream._

import scala.collection.mutable.ListBuffer
sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = {
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def loop(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        loop(t())
      case _ => buf.toList
    }
    loop(this)
  }

  def foldRight[B](name: String, z: => B)(f: (A, => B) => B): B = {
//    println("foldRight: " + name)
    this match {
      case Cons(h, t) => f(h(), t().foldRight(name, z)(f))
      case _ => z
    }
  }

  def exists(p: A => Boolean): Boolean = foldRight("exists", false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight("forAll", true)((a, b) => p(a) && b)

  def takeWhileViaFold(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]]("takeWhile", empty)((a, b) => if (p(a)) cons(a, b) else empty)

  def headOptionViaFold: Option[A] = foldRight[Option[A]]("headOption", None)((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] = foldRight("map", empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] = foldRight("filter", empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  def append[B >: A](s1: => Stream[B]): Stream[B] = foldRight("append", s1)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight("flatMap", empty[B])((a, b) => f(a) append b)

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  def mapUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1)  => Some(h(), (empty, 0))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B,C](rs: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, rs)) {
      case (Cons(lh, lt), Cons(rh, rt)) => Some((f(lh(), rh()), (lt(), rt())))
      case _ => None
    }

  def zip[B](rs: Stream[B]): Stream[(A, B)] = zipWith(rs)((_, _))

  def zipAll[B](rs: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, rs)) {
      case (Cons(lh, lt), Cons(rh, rt)) => Some(((Some(lh()), Some(rh())), (lt(), rt())))
      case (Cons(lh, lt), _) => Some(((Some(lh()), None), (lt(), empty)))
      case (_, Cons(rh, rt)) => Some(((None, Some(rh())), (empty, rt())))
      case _ => None
    }

  def startsWith[B >: A](s: Stream[B]): Boolean = zipAll(s) takeWhile (_._2.isDefined) forAll {
    case (h1, h2) => h1 == h2
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case s => Some((s, s drop 1))
  }

  def hasSubsequence[B >: A](s: Stream[B]): Boolean = tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight("scanRight", Stream(z)) { (a, bs) =>
    lazy val bs1 = bs
    cons(f(a, bs1.headOptionViaFold.get), bs1)
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

/**
  * @author caleb
  */
object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def loop(n1: Int, n2: Int): Stream[Int] = cons(n1, loop(n2, n1 + n2))
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, t)) => cons(h, unfold(t)(f))
    case _ => empty
  }

  def constantUnfold[A](a: A): Stream[A] = unfold(a)(Some(_, a))

  def onesUnfold = constantUnfold(1)

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(z => Some(z, z + 1))

  def fibsUnfold: Stream[Int] = unfold((0, 1)) { case(f0, f1) => Some(f0, (f1, f0 + f1)) }

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
