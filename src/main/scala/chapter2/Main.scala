package chapter2

object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc1: Int, acc2: Int): Int =
      if (n <= 0) acc2
      else go(n - 1, acc2, acc1 + acc2)

    n match {
      case 0 => 0
      case _ => go (n - 1, 0, 1)
    }
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Int): Boolean = {
    @annotation.tailrec
    def loop(n: Int, acc: Int): Boolean =
      if (n >= as.length) true
      else {
        val acc1 = ordered(as(n - 1), as(n))
        if (acc1 != acc) false
        else loop(n + 1, acc1)
      }

    if (as.length == 1) true
    else loop(1, ordered(as(0), as(1)))
  }

  def partial1[A,B,C](a: A, f: (A, B) => C): B => C =
    (b) => f(a, b)

  def curry1[A,B,C](f: (A, B) => C): A => B => C =
    (a) => partial1(a, f)

  def uncurry1[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a) => f(g(a))

  private def formatAbs(x: Int) = {
    formatResult("absolute value", x, abs)
  }

  private def formatFactorial(n: Int) = {
    formatResult("factorial", n, factorial)
  }

  private def formatFibonacci(n: Int) = {
    formatResult("Fibonacci", n, fibonacci)
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))

    @annotation.tailrec
    def goFac(n: Int, acc: Int): Unit = {
      println(formatFactorial(acc))
      if (acc < n) goFac(n, acc + 1)
    }

    goFac(7, 0)

    @annotation.tailrec
    def goFib(n: Int, acc: Int): Unit = {
      println(formatFibonacci(acc))
      if (acc < n) goFib(n, acc + 1)
    }

    goFib(9, 0)

    val words = Array("the", "quick", "brown", "fox")
    val index = findFirst(words, (x: String) => x == "fox")
    println("The index of %s in (\"the\", \"quick\", \"brown\", \"fox\") is %d".format("fox", index))

    val compare = (a: Int, b: Int) => a compare b
    val numbers = Array(1, 3, 3, 2)
    val indexN = findFirst(numbers, (x: Int) => x == 3)
    println("The index of %d in (1, 3, 3, 2) is %d".format(3, indexN))
    println("(1, 3, 3, 2) is sorted: %b".format(isSorted(numbers, compare)))

    val sortedNums = Array(0, 1, 2, 3)
    println("(0, 1, 2, 3) is sorted: %b".format(isSorted(sortedNums, compare)))

    val add2 = (a: Int, b: Int) => a + b
    println("add2(1, 2): %d".format(add2(1, 2)))
    val incr = partial1(1, add2)
    println("incr(2): %d".format(incr(2)))

    val incc = curry1(add2)
    val incp = incc(1)
    println(incp(3))

    val uncAdd = uncurry1(incc)
    println(uncAdd(5, 6))
  }
}