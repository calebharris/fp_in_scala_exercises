package fpinscala.datastructures

/**
  * @author caleb
  */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = {
    def loop(t: Tree[A], acc: Int): Int = t match {
      case Leaf(_) => acc + 1
      case Branch(left, right) => loop(right, 1 + loop(left, acc))
    }
    loop(t, 0)
  }

  def maximum(t: Tree[Int]): Int = {
    def loop(t: Tree[Int], acc: Int): Int = t match {
      case Leaf(n) => n max acc
      case Branch(left, right) => loop(right, loop(left, acc))
    }
    loop(t, Int.MinValue)
  }

  def depth[A](t: Tree[A]): Int = {
    def loop(t: Tree[A], depth: Int, maxSoFar: Int): Int = t match {
      case Leaf(_) => maxSoFar max (depth + 1)
      case Branch(left, right) => loop(right, depth + 1, loop(left, depth + 1, maxSoFar))
    }
    loop(t, 0, 0)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)

  def depthViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ max _ + 1)

  def maximumViaFold(t: Tree[Int]): Int = fold(t)(n => n)(_ max _)

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Tree(f(a)))(Branch(_, _))

  def apply[A](a: A): Tree[A] = Leaf(a)

  def apply[A](l: A, r: A): Tree[A] = Tree(Tree(l), Tree(r))

  def apply[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def apply[A](left: A, right: Tree[A]): Tree[A] = Tree(Tree(left), right)

  def apply[A](left: Tree[A], right: A): Tree[A] = Tree(left, Tree(right))
}
