import fpinscala.datastructures.{Cons, List, Nil}
import List._

val ex1: List[Double] = Nil
val ex2: List[Int] = Cons(1, Nil)
val ex3: List[String] = Cons("a", Cons("b", Nil))

val n = List(1, 2, 3, 4, 5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + sum(t)
  case _ => 101
}

val t1 = tail(ex1)
val t2 = tail(ex3)
val ex4 = setHead(ex3, "c")
val ex5 = drop(ex3, 1)
val ex6 = dropWhile(ex3, (x: String) => x == "a")
