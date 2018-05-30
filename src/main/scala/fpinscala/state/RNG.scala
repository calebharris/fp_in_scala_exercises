package fpinscala.state

/**
  * @author caleb
  */
trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {

  import State._

  type Rand[A] = State[RNG, A]

  val int = State[RNG, Int](_.nextInt)

  val nonNegativeInt = int.map(i => if (i < 0) -(i + 1) else i)

  val nonNegativeEven = nonNegativeInt.map(i => i - i % 2)

  val double = nonNegativeInt.map(_ / (Int.MaxValue.toDouble + 1))

  val intDouble = both(int)(double)

  val doubleInt = both(double)(int)

  val double3 = for {
    a <- double
    b <- double
    c <- double
  } yield (a, b, c)

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] = nonNegativeInt.flatMap { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      unit(mod)
    else
      nonNegativeLessThan(n)
  }
}

object RNGOld {

  type Rand[+A] = RNG => (A, RNG)

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (n, rng) if n < 0 => (-(n + 1), rng)
    case (n, rng) => (n, rng)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  val int: Rand[Int] = _.nextInt

  val double: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def loop(c: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) =
      if (c <= 0) (acc, rng)
      else {
        val (i, r) = rng.nextInt
        loop(c - 1, r, i :: acc)
      }

    loop(count, rng, List())
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def mapOld[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2Old[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb) { (_, _) }

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequenceDumb[A](fs: List[Rand[A]]): Rand[List[A]] = {
    def loop(fs: List[Rand[A]], rng: RNG, acc: List[A]): (List[A], RNG) = fs match {
      case f :: fs => {
        val (a, r) = f(rng)
        loop(fs, r, a :: acc)
      }
      case _ => (acc, rng)
    }

    rng => loop(fs, rng, List())
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def intsSeq(count: Int)(rng: RNG): (List[Int], RNG) = sequence(List.fill(count)(int))(rng)

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      unit(mod)
    else
      nonNegativeLessThan(n)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      flatMap(rb) { b =>
        unit(f(a, b))
      }
    }
}
