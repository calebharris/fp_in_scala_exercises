package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def append[A](asl: List[A], asr: List[A]): List[A] = foldRight(asl, asr)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case _ => l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  def tail(l: List[_]): List[_] = drop(l, 1)

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs)(f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def setHead[A](l: List[A], e: A): List[A] = l match {
    case Nil => Cons(e, Nil)
    case Cons(_, xs) => Cons(e, xs)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

//  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
//    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
//    case _ => z
//  }


  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((t, h) => Cons(h, t))

  def sum(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)

  def sum2(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def product(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  def product2(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, b) => 1 + b)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((b, _) => 1 + b)


  def incrementAll(l: List[Int]): List[Int] = map(l)(_ + 1)

  def stringifyDs(ds: List[Double]): List[String] = map(ds)(_.toString())

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil:List[B])((a, bs) => Cons(f(a), bs))

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft(as, Nil:List[B])((bs, a) => append(bs, f(a)))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil:List[A])(
    (a, as) => if (f(a)) Cons(a, as) else as
  )

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)((a) => if (f(a)) List(a) else Nil)

  def addLists(ls: List[Int], rs: List[Int]): List[Int] = zipWith(ls, rs)(_ + _)

  def zipWith[A,B,C](ls: List[A], rs: List[B])(f: (A, B) => C): List[C] = {
    @annotation.tailrec
    def loop(ls: List[A], rs: List[B], acc: List[C]): List[C] = (ls, rs) match {
      case (Nil, _) => acc
      case (_, Nil) => acc
      case (Cons(lh, lt), Cons(rh, rt)) => loop(lt, rt, Cons(f(lh, rh), acc))
    }
    reverse(loop(ls, rs, Nil:List[C]))
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => false
    case (Cons(_, t)) => foldLeft(zipWith(sup, sub)(_ == _), true)(_ && _) || hasSubsequence(t, sub)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
