package fpinscala.parsing

import fpinscala.parsing.Parsers.{Location, ParseError}
import fpinscala.state.State

import scala.util.matching.Regex

import language.implicitConversions

/**
  * @author caleb
  */


trait Parser[+A] {
  def run(ls: List[Location]): (Either[ParseError, A], List[Location])
}

object StateParsers extends Parsers[Parser] {

  case class AttemptParser[A](p: Parser[A]) extends Parser[A] {
    override def run(ls: List[Location]): (Either[ParseError, A], List[Location]) = {
      p.run(ls) match {
        case (Left(ParseError(es, true)), ls1) => (Left(ParseError(es, committed = false)), ls1)
        case x => x
      }
    }
  }

  case class FailParser[A](err: ParseError) extends Parser[A] {
    override def run(ls: List[Location]): (Either[ParseError, A], List[Location]) = (Left(err), ls)
  }

  case class FlatMapFailureParser[A](p: Parser[A], f: ParseError => Parser[A]) extends Parser[A] {
    override def run(ls: List[Location]): (Either[ParseError, A], List[Location]) = {
      val r = p.run(ls)
      r match {
        case (Left(err), ls1) =>
          val e = f(err).run(ls1)
          e
        case r1@_ =>
          r1
      }
    }
  }

  case class FlatMapParser[A, B](p: Parser[A], f: A => Parser[B]) extends Parser[B] {
    override def run(ls: List[Location]): (Either[ParseError, B], List[Location]) = p.run(ls) match {
      case (Right(a), rem) => f(a).run(rem)
      case (Left(err), s) => (Left(err), s)
    }
  }

  case class FlatMapWithEntryPointParser[A, B](p: Parser[A], f: (A, Location) => Parser[B]) extends Parser[B] {
    override def run(ls: List[Location]): (Either[ParseError, B], List[Location]) = {
      flatMap(p)(f(_, ls.head)).run(ls)
    }
  }

  case class OrParser[A](s1: Parser[A], s2: () => Parser[A]) extends Parser[A] {
    lazy val _s2 = s2()

    override def run(ls: List[Location]): (Either[ParseError, A], List[Location]) = {
      println(s"OrParser.run $s1 ${_s2} ${ls.head}")
      s1 run ls match {
        case (Left(ParseError(_, false)), _) => _s2 run ls
        case ps => ps
      }
    }
  }

  //FIXME: partial regex match? (i.e. how do we know if we're committed or not?)
  case class RegExParser(r: Regex) extends Parser[String] {
    override def run(ls: List[Location]): (Either[ParseError, String], List[Location]) = {
      println(s"RegExParser.run $r ${ls.head}")
      val loc = ls.head
      r.findPrefixOf(loc.remaining) map { pfx =>
        success(pfx, loc.advance(pfx.size) :: ls)
      } getOrElse {
        failure(List((loc, s"""Expected string matching "$r" """)), true, ls)
      }
    }
  }

  case class SliceParser[A](p: Parser[A]) extends Parser[String] {
    override def run(ls: List[Location]): (Either[ParseError, String], List[Location]) =
      flatMapWithEntryPoint(p) { (_, loc) =>
        parser { endLocs =>
          (Right(loc.input.slice(loc.offset, endLocs.head.offset)), endLocs)
        }
      }.run(ls)
  }

  case class StringParser(s: String) extends Parser[String] {
    override def run(l: List[Location]): (Either[ParseError, String], List[Location]) = {
      val loc = l.head
      lazy val errHead = (loc, s"""Expected string "$s"""")

      val maybePfx = matchingPrefix(s, loc.remaining)
      maybePfx map { pfx =>
        if (pfx.size == s.size)
          success(pfx, loc.advance(pfx.size) :: l)
        else
          failure(List(errHead), true, l)
      } getOrElse {
        failure(List(errHead), false, l)
      }
    }
  }

  case class UnitParser[A](a: A) extends Parser[A] {
    override def run(ls: List[Location]): (Either[ParseError, A], List[Location]) = success(a, ls)
  }

  private def parser[A](f: List[Location] => (Either[ParseError, A], List[Location])): Parser[A] = {
    val state = State(f)
    (l: List[Location]) => state.run(l)
  }

  private def flatMapWithEntryPoint[A, B](p: Parser[A])(f: (A, Location) => Parser[B]): Parser[B] =
    FlatMapWithEntryPointParser(p, f)

  private def success[A](a: A, locs: List[Location]): (Either[ParseError, A], List[Location]) =
    (Right(a), locs)

  private def failure[A](errs: List[(Location, String)], committed: Boolean, locs: List[Location]): (Either[ParseError, A], List[Location]) =
    (Left(ParseError(errs, committed)), locs)

  private def matchingPrefix(s1: String, s2: String): Option[String] =
    ((s1, s2).zipped takeWhile { case (x, y) => x == y }).unzip._1 match {
      case x if x.isEmpty => None
      case x => Some(x.mkString)
    }

  override implicit def string(s: String): Parser[String] = StringParser(s)

  override implicit def regex(r: Regex): Parser[String] = RegExParser(r)

  override def attempt[A](p: Parser[A]): Parser[A] = AttemptParser(p)

  override def end: Parser[String] = parser[String] { ls =>
    if (ls.head.remaining == "")
      success("", ls)
    else
      failure(List((ls.head, "Expected end of input")), true, ls)
  }

  /** In the event of an error, returns the error that occurred after consuming the most number of characters. */
  override def furthest[A](p: Parser[A]): Parser[A] = ???

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]) = FlatMapParser(p, f)

  override def flatMapFailure[A](p: Parser[A])(f: ParseError => Parser[A]): Parser[A] = FlatMapFailureParser(p, f)

  /** In the event of an error, returns the error that occurred most recently. */
  override def latest[A](p: Parser[A]): Parser[A] = ???

  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = OrParser(s1, () => s2)

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = p.run(List(Location(input, 0)))._1

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = parser { ls =>
    val loc = ls.head
    mapFailure(p) {
      case ParseError(stack, committed) => ParseError((loc, msg) :: stack, committed)
    }.run(ls)
  }

  override def slice[A](p: Parser[A]): Parser[String] = SliceParser(p)

  override def unit[A](a: A): Parser[A] = UnitParser(a)

  override def fail[A](err: ParseError): Parser[A] = FailParser(err)

  override def errorLocation(e: ParseError): Location = e.stack.head._1

  override def errorMessage(e: ParseError): String = e.stack.head._2
}

