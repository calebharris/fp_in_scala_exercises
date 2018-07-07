package fpinscala.mutability

/**
  * @author caleb
  */
object Main {
  def swapAddOne(x: Int, y: Int) = new RunnableST[(Int, Int)] {
    override def apply[S]: ST[S, (Int, Int)] = for {
      r1 <- STRef(x)
      r2 <- STRef(y)
      x <- r1.read
      y <- r2.read
      _ <- r1.write(y + 1)
      _ <- r2.write(x + 1)
      a <- r1.read
      b <- r2.read
    } yield (a, b)
  }

  def incrementRead(x: Int): Int = ST.runST(new RunnableST[Int] {
    override def apply[S]: ST[S, Int] = for {
      xr <- STRef(x)
      _ <- xr.modify(_ + 1)
      y <- xr.read
    } yield y
  })

  def fillArray(values: Map[Int, Int]) = new RunnableST[List[Int]] {
    override def apply[S]: ST[S, List[Int]] = for {
      xs <- STArray(values.size, 0)
      _  <- xs.fill(values)
      ll <- xs.freeze
    } yield ll
  }

  def swap[A: Manifest](i: Int, j: Int, xs: List[A]): List[A] =
    ST.runST(new RunnableST[List[A]] {
      override def apply[S]: ST[S, List[A]] = for {
        xa <- STArray.fromList(xs)
        _ <- xa.swap(i, j)
        ys <- xa.freeze
      } yield ys
    })

  def partition[A : Manifest : Ordering](xs: List[A], n: Int, r: Int, pivot: Int): List[A] =
    ST.runST(new RunnableST[List[A]] {
      override def apply[S]: ST[S, List[A]] = for {
        aa <- STArray.fromList(xs)
        _ <- STArray.partition(aa, n, r, pivot)
        ys <- aa.freeze
      } yield ys
    })

  def main(args: Array[String]): Unit = {
    println(s"swapAddOne(1,2) = ${ST.runST(swapAddOne(1, 2))}")
    println(s"swapAddOne(5,0) = ${ST.runST(swapAddOne(5, 0))}")

    println(s"incrementRead(5) = ${incrementRead(5)}")

    val values = Map(0 -> 2, 1 -> 4)
    println(s"fillArray(${values}) = ${ST.runST(fillArray(values))}")

    val preSwapped = List(0, 1, 2, 3)
    println(s"swap(0, 3, ${preSwapped}) = ${swap(0, 3, preSwapped)}")

    val toPart = List(3, 2, 1, 0)
    println(s"partition(${toPart}, 0, 3, 2) = ${partition(toPart, 0, 3, 2)}")

    val toSort = List(4, 5, 8, 2, 1, 9, 4, 0)
    println(s"quicksort(${toSort}) = ${STArray.quicksort(toSort)}")
  }
}
