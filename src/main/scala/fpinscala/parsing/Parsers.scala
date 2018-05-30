package fpinscala.parsing

import language.{higherKinds, implicitConversions}
import fpinscala.testing._
import Gen._
import fpinscala.parsing.Parsers.{Location, ParseError}

import scala.util.matching.Regex

/**
  * @author caleb
  */

trait Parsers[Parser[+_]] { self =>

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  implicit def regex(r: Regex): Parser[String]

  def attempt[A](p: Parser[A]): Parser[A]

  /** Returns a `Parser` that succeeds if there is no more input left to consume. */
  def end: Parser[String]

  /** In the event of an error, returns the error that occurred after consuming the most number of characters. */
  def furthest[A](p: Parser[A]): Parser[A]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def flatMapFailure[A](p: Parser[A])(f: ParseError => Parser[A]): Parser[A]

  def label[A](msg: String)(p: Parser[A]): Parser[A] = mapFailure(p) {
    case ParseError((loc, m) :: t, c) => ParseError((loc, msg) :: t, c)
  }

  /** In the event of an error, returns the error that occurred most recently. */
  def latest[A](p: Parser[A]): Parser[A]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  def char(c: Char): Parser[Char] = string(c.toString) map (_ charAt 0)

  def count[A](p: Parser[A]): Parser[Int] = slice(many(p)) map (_.size)

  /** C/Java style floating point literals, e.g .1, -1.0, 1e9, 1E-23, etc.
    * Result is left as a string to keep full precision
    */
  def doubleString: Parser[String] = token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  def double: Parser[Double] = doubleString map (_.toDouble)

  def escapedQuoted: Parser[String] = token(char('"') *> regex("""(?:[^"\\]|\\.)*""".r) *< char('"'))

  def keepLeft[A](pa: Parser[A], px: Parser[Any]): Parser[A] = pa ** px map { case (a, _) => a }

  def keepRight[A, B](pa: Parser[A], pb: Parser[B]): Parser[B] = pa ** pb map { case (_, b) => b}

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n > 0) map2(p, listOfN(n - 1, p))(_ :: _)
    else succeed(List())

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = flatMap(p)(a => unit(f(a)))

  def map2[A, B, C](pa: Parser[A], pb: => Parser[B])(f: (A, B) => C): Parser[C] =
    pa flatMap (a => pb map (b => f(a, b)))

  def mapFailure[A](p: Parser[A])(f: ParseError => ParseError): Parser[A] = flatMapFailure(p)(e => fail(f(e)))

  def product[A, B](pa: Parser[A], pb: => Parser[B]): Parser[(A, B)] =
    pa flatMap (a => pb map (b => (a, b)))

  def sep[A](pa: Parser[A], pb: Parser[Any]): Parser[List[A]] = sep1(pa, pb) or succeed(List())

  def sep1[A](pa: Parser[A], pb: Parser[Any]): Parser[List[A]] = map2(pa, many(pb *> pa))(_ :: _)

  def succeed[A](a: A): Parser[A] = unit(a)

  def surround[A](start: Parser[Any], stop: Parser[Any])(p: Parser[A]): Parser[A] =
    start *> p *< stop

  def token[A](p: Parser[A]): Parser[A] = p *< (attempt(whitespace) | attempt(end) | succeed(""))

  def unit[A](a: A): Parser[A]

  def fail[A](err: ParseError): Parser[A]

  def whitespace: Parser[String] = "\\s+".r

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A] = or _

    def *<(px: Parser[Any]) = self.keepLeft(p, px)

    def *>[B](pb: Parser[B]) = self.keepRight(p, pb)

    def **[B](pb: Parser[B]): Parser[(A, B)] = self.product(p, pb)

    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)

    def flatMap[B](f: A => Parser[B]) = self.flatMap(p)(f)

    def many: Parser[List[A]] = self.many(p)

    def many1: Parser[List[A]] = self.many1(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def or[B >: A](pb: => Parser[B]): Parser[B] = self.or(p, pb)

    def product[B](pb: Parser[B]): Parser[(A, B)] = self.product(p, pb)

    def scope(msg: String): Parser[A] = self.scope(msg)(p)

    def sep(pb: Parser[Any]): Parser[List[A]] = self.sep(p, pb)

    def slice: Parser[String] = self.slice(p)
  }

  def errorLocation(e: ParseError): Location = e.stack.head._1
  def errorMessage(e: ParseError): String = e.stack.head._2

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def succeedLaw[A](a: A)(in: Gen[String]): Prop =
      forAll(in)(s => run(succeed(a))(s) == Right(a))

    def associativeProductLaw[A, B, C](pa: Parser[A], pb: Parser[B], pc: Parser[C])(in: Gen[String]): Prop =
      equal((pa ** pb) ** pc map unbiasL, pa ** (pb ** pc) map unbiasR)(in)

    def productMapLaw[A, B, C, D](pa: Parser[A], pb: Parser[B])(f: A => C, g: B => D)(in: Gen[String]): Prop =
      equal(pa.map(f) ** pb.map(g), (pa ** pb) map { case (a, b) => (f(a), g(b)) })(in)

    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
      forAll(inputs ** Gen.string) { case (input, msg) =>
        run(label(msg)(p))(input) match {
          case Left(e) => errorMessage(e) == msg
          case _ => true
        }
      }

    def scopeLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
      forAll(inputs ** Gen.string) { case (input, msg) =>
        run(scope(msg)(p))(input) match {
          case (Left(e)) => errorMessage(e) == msg && e.stack.tail.nonEmpty
          case _ => true
        }
      }

    def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)

    def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)
  }
}

object Parsers { self =>
  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }

    def advance(n: Int): Location = copy(offset = offset + n)

    def remaining: String = input.slice(offset, input.size)
  }

  case class ParseError(stack: List[(Location, String)], committed: Boolean = true  ) {
    def report: String = {
      val (loc, msg) = stack.head
      s"$msg\nat L${loc.line}:${loc.col} near ${loc.remaining.slice(0, 10)}\n"
    }
  }

  object ParseError {
    def apply(context: (Location, String)): ParseError = new ParseError(List(context))
  }
}

