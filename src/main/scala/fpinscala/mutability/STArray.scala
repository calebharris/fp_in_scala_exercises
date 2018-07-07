package fpinscala.mutability

import fpinscala.mutability.ST.noop

/**
  * @author caleb
  */
sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]

  def size: ST[S, Int] = ST(value.length)

  def write(i: Int, a: A): ST[S, Unit] = (s: S) => {
    value(i) = a
    ((), s)
  }

  def read(i: Int): ST[S, A] = ST(value(i))

  def freeze: ST[S, List[A]] = ST(value.toList)

  def myFill(xs: Map[Int, A]): ST[S, Unit] = {
    val M = ST.monad[S]
    val ss = xs.toList.map { case (i, a) => write(i, a) }
    M.sequence_(ss: _*)
  }

  def fill(xs: Map[Int, A]): ST[S, Unit] = {
    xs.foldRight(ST[S, Unit]( () )) {
      case ((k, v), st) => st flatMap (_ => write(k, v))
    }
  }

  def swap(i: Int, j: Int): ST[S, Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()
}

object STArray {
  def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value = Array.fill(sz)(v)
    })

  def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value = xs.toArray
    })

  def partition[S, A: Ordering](xs: STArray[S, A], n: Int, r: Int, pivot: Int): ST[S,Int] = for {
    j <- STRef(n)
    p <- xs.read(pivot)
    _ <- xs.swap(pivot, r)
    l <- xs.freeze
    _ <- noop[S](println(s"swapped pivot=$pivot and r=$r; $l"))
    _ <- (n until r).foldLeft(noop[S])((st, i) => for {
      _ <- st
      iVal <- xs.read(i)
      _ <- if (Ordering[A].compare(iVal, p) < 0) for {
        jVal <- j.read
        _ <- xs.swap(i, jVal)
        m <- xs.freeze
        _ <- noop[S](println(s"swapped $i and $jVal; $m"))
        _ <- j.modify(_ + 1)
      } yield () else noop[S]
    } yield ())
    jVal <- j.read
    _ <- xs.swap(jVal, r)
    n <- xs.freeze
    _ <- noop[S](println(s"swapped j=$jVal and r=$r; $n"))
  } yield jVal

  def qs[S, A: Ordering](a: STArray[S, A], n: Int, r: Int): ST[S,Unit] = if (n < r) {
    for {
      pi <- partition(a, n, r, n + (r - n) / 2)
      l <- a.freeze
      _ <- noop[S](println(s"n=${n}; r=${r}; pivot=${n + (r - n) / 2}; ${l}"))
      _ <- qs(a, n, pi - 1)
      _ <- qs(a, pi + 1, r)
    } yield ()
  } else {
    ST.noop[S]
  }

  def quicksort[S, A : Manifest : Ordering](as: List[A]): List[A] = ST.runST(new RunnableST[List[A]] {
    override def apply[S]: ST[S, List[A]] = for {
      aa <- STArray.fromList(as)
      _ <- qs(aa, 0, as.length - 1)
      ys <- aa.freeze
    } yield ys
  })
}
