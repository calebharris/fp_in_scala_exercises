package fpinscala.testing

import fpinscala.state.{RNG, SimpleRNG, State}
import fpinscala.testing.Prop.{FailedCase, MaxSize, SuccessCount, TestCases}

import scala.{Stream => _}
import fpinscala.laziness.Stream

import language.implicitConversions

/**
  * @author caleb
  */

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
    printReport(p.run(maxSize, testCases, rng))

  def printReport(result: Result) = result match {
    case Falsified(msg, n) =>
      println(s"! Falsified after $n passed tests:\n $msg")
    case Passed(n) =>
      println(s"+ OK, passed $n tests")
    case Proved =>
      println("+ OK, proved property.")
  }
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop(
    (max, n, r) => run(max, n, r) match {
      case fail: Falsified => fail
      case _ => p.run(max, n, r)
    }
  )

  def ||(p: Prop): Prop = Prop(
    (max, n, r) => run(max, n, r) match {
      case Falsified(_, _) => p.run(max, n, r)
      case other: Result => other
    }
  )
}

sealed trait Result {
  def isFalsified: Boolean
  def notFalsified: Boolean = !isFalsified
}

case object Proved extends Result {
  override def isFalsified = false
}

case class Passed(successes: SuccessCount) extends Result {
  override def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified = true
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val forSize2: Int => Gen[B] = n => {
      forSize(n) flatMap { f(_)(n) }
    }
    SGen(forSize2)
  }

  def map[B](f: A => B): SGen[B] = SGen { forSize(_) map f }

  def **[B](s2: SGen[B]): SGen[(A, B)] = SGen(n => apply(n) ** s2(n))
}

case class Gen[+A](sample: State[RNG, A]) {
  def **[B](gb: Gen[B]): Gen[(A, B)] = map2(gb)((_, _))

  def distinct = Gen.distinct(this)

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(
    sample.flatMap { a =>
      val x = f(a).sample
      x
    }
  )

  def map[B](f: A => B): Gen[B] = flatMap(a => Gen.unit(f(a)))

  def map2[B, C](gb: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(gb.sample)(f))

  def listOfN(n: Int) = Gen.listOfN(n, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap (n => listOfN(n))

  def optional: Gen[Option[A]] = flatMap (a => Gen.unit(Some(a)))

  def otherThan[B >: A](b: B) = Gen.otherThan(this, b)

  def tupled: Gen[(A, A)] = **(this)

  def unsized: SGen[A] = SGen(_ => this)

  def union[B >: A](that: Gen[B]): Gen[B] = Gen.union(this, that)
}

object Gen {
  import RNG._
  import State._

  private val A = 'A'.toInt
  private val a = 'a'.toInt
  private val z = 'z'.toInt

  implicit def charPairToGen(pair: (Char, Char)): Gen[Char] = charsInRange(pair._1, pair._2)

  def charsInRange(lo: Char, hi: Char): Gen[Char] = choose(lo, hi + 1).map(_.toChar)

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(nonNegativeLessThan(stopExclusive - start) map (_ + start))

  def double: Gen[Double] = Gen(RNG.double)

  def boolean: Gen[Boolean] = Gen(nonNegativeLessThan(2) map (_ == 1))

  def chars: Gen[Char] = ('!', '~')

  //0x30 - 0x39 (48 - 57)
  def digits: Gen[Char] = ('0', '9')

  //0x61 - 0x7a (97 - 122)
  def lowercaseLetters: Gen[Char] = ('a', 'z')

  //0x41 - 0x5a (65 - 90)
  def uppercaseLetters: Gen[Char] = ('A', 'Z')

  //0x21 - 0x2f (33 - 47, 15 chars)
  def punctuation1: Gen[Char] = ('!', '/')

  //0x3a - 0x40 (58 - 64, 7 chars)
  def punctuation2: Gen[Char] = (':', '@')

  //0x5b - 0x60 (91 - 96, 6 chars)
  def punctuation3: Gen[Char] = ('[', '`')

  //0x7b - 0x7e (123 - 126, 4 chars)
  def punctuation4: Gen[Char] = ('{', '~')

  def punctuation: Gen[Char] = weighted(
    punctuation1 → (15.0 / 32.0),
    punctuation2 → ( 7.0 / 32.0),
    punctuation3 → ( 6.0 / 32.0),
    punctuation4 → ( 4.0 / 32.0)
  )

  def letters: Gen[Char] = weighted(lowercaseLetters → 0.5, uppercaseLetters → 0.5)

  def lettersAndDigits: Gen[Char] = weighted(letters → 0.75, digits → 0.25)

  def string: SGen[String] = SGen(strings(_, chars))

  def strings(n: Int, gc: Gen[Char]): Gen[String] = gc.listOfN(n).map(_.mkString)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(sequence(List.fill(n)(g.sample)))

  def listOf[A](ga: Gen[A]): SGen[List[A]] = SGen { ga.listOfN }

  def listOf1[A](ga: Gen[A]): SGen[List[A]] = SGen { n => ga.listOfN(1 max n) }

//  def distinct[A](g: Gen[A])(a: A): Gen[A] = Gen(State { rng =>
//    @annotation.tailrec
//    def go(r: RNG): (A, RNG) = {
//      val (a1, r1) = g.sample.run(r)
//      if (a1 == a) go(r1)
//      else (a1, r1)
//    }
//    go(rng)
//  })

  def otherThan[A, B >: A](g: Gen[A], b: B): Gen[A] = Gen(g.sample until (_ != b))

  def distinct[A](g: Gen[A]): Gen[(A, A)] = g flatMap { a => unit(a) ** (g otherThan a) }

  def forAll[A](sg: SGen[A])(f: A => Boolean): Prop = forAll(sg(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => {
      val casesPerSize = (n + (max - 1)) / max
      println(s"casesPerSize: $casesPerSize")
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
    }
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (m, n, rng) => randomStream(as)(rng).zip(Stream.from(0).take(n)).map {
      case (a, i) => try {
        if (f(a)) Passed(1) else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed(n))
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(r => Some(g.sample.run(r)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def optionalOld[A](ga: Gen[A]): Gen[Option[A]] = Gen(State(s => {
    val (a1, s1) = ga.sample.run(s)
    (Some(a1), s1)
  }))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean flatMap (p => if (p) g1 else g2)

  implicit def unsized[A](ga: Gen[A]): SGen[A] = SGen(_ => ga)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    double flatMap (d => if (d < g1._2 / (g1._2 + g2._2)) g1._1 else g2._1)

  def weighted[A](gs: (Gen[A], Double)*): Gen[A] = {
    val total = gs.map(_._2).sum
    double flatMap { d =>
      gs
        .toStream
        .scanLeft(gs.head) {
          case ((_, acc), (g, wt)) => (g, acc + wt)
        }
        .find { case (g, cum) => d < cum / total }
        .get._1
    }
  }
}