package fpinscala.parsing

import fpinscala.UnitSuite
import fpinscala.testing.Gen._
import fpinscala.testing.Prop._
import fpinscala.testing._

/**
  * @author caleb
  */
class ParsingSuite extends UnitSuite {

  private val parsers = StateParsers
  import parsers._

  test("Always true prop check works") {
    val r = check(true).run(1, 1, RNG)
    printReport(r)
    assert(!r.isFalsified)
  }

  test("Always false prop check works") {
    val r = check(false).run(1, 1, RNG)
    assert(r.isFalsified)
  }

  test("forAll reports correct n") {
    val intsDoubled = choose(0, 100).map(_ * 2)
    val r = forAll(intsDoubled)(_ % 2 == 0)
    r match {
      case Passed(n) => assert(n == 20)
      case _ => fail
    }
  }

  test("char parser recognizes its char") {
    val r = forAll(chars) { c =>
      parsers.run(parsers.char(c))(c.toString) == Right(c)
    }
    assert(!r.isFalsified)
  }

  test("char parser errors with any other char") {
    val r = forAll(chars.distinct) { case (c1, c2) =>
      val triedParse = parsers.run(parsers.char(c1))(c2.toString)
      triedParse.left.map(err => println(err.report))
      triedParse.isLeft
    }
    assert(!r.isFalsified)
  }

  test("string parser recognizes its string") {
    val r = forAll(strings1(chars)) { s =>
      parsers.run(s)(s) == Right(s)
    }
    assert(!r.isFalsified)
  }

  test("string parser fails with any other string") {
    val r = forAll(strings1(chars).distinct) { case (s1, s2) =>
      val triedParse = parsers.run(s1)(s2)
      triedParse.left.map(err => println(err.report))
      triedParse.isLeft
    }
    assert(!r.isFalsified)
  }

  test("string parser records useful location on failure") {
    val triedParse = parsers.run("abcdef")("abcabc")
    triedParse.left.map(err => println(err.report))
    assert(triedParse.isLeft)
  }

  test("product of string parsers works") {
    val r = forAll(strings1(chars).distinct) { case (s1, s2) =>
      val triedParse = parsers.run(s1 ** s2)(s1 + s2)
      if (triedParse.isLeft) println(triedParse.left.get.report)
      triedParse.isRight
    }
    assert(!r.isFalsified)
  }

  test("product of parsers reports useful location on failure") {
    val r = forAll(strings1(chars).distinct) { case (s1, s2) =>
      val triedParse = parsers.run(s1 ** s2)(s1 + s1)
      assert(triedParse.isLeft)
      val err = triedParse.left.get
      println(err.report)
      val loc = err.stack.head._1
      loc.line == 1 && loc.col == s1.length + 1
    }
    assert(!r.isFalsified)
  }

  test("slice returns matched part of input") {
    val r = forAll(strings1(chars).distinct) { case (s1, s2) =>
      val triedParse = parsers.run(slice(s1 ** s2))(s1 + s2 + s1)
      assert(triedParse.isRight)
      triedParse.right.get == s1 + s2
    }
    assert(!r.isFalsified)
  }

  test("slice passes through parse errors") {
    val r = forAll(strings1(chars).distinct) { case (s1, s2) =>
      val triedParse = parsers.run(slice(s1))(s2)
      assert(triedParse.isLeft)
      val err = triedParse.left.get
      println(err.report)
      val loc = err.stack.head._1
      loc.line == 1 && loc.col == 1
    }
    printReport(r)
    assert(!r.isFalsified)
  }

  test("count returns character count of matched part of input") {
    val r = forAll(strings1(chars).distinct) { case (s1, s2) =>
      val triedParse = parsers.run(count(s1 ** s2))(s1 + s2 + s1)
      if (triedParse.isLeft) println(triedParse.left.get.report)
      triedParse.exists(_ == s1.length + s2.length)
    }
    assert(!r.isFalsified)
  }

  test("many") {
    val triedParse = parsers.run(many(char('a')))("a")
    if (triedParse.isLeft) println(triedParse.left.get.report)
    assert(triedParse.isRight)
  }

  test("many1") {
    val triedParse = parsers.run(many1(char('a')))("a")
    if (triedParse.isLeft) println(triedParse.left.get.report)
    assert(triedParse.isRight)
  }

  test("or[String] fails when first branch fails but matches at least one char") {
    val r = forAll(strings1(chars).distinct) { case (s1, s2) =>
      val triedParse = parsers.run((s1 + s2) | (s1 + s1))(s1 + s1)
      triedParse.isLeft
    }
    assert(!r.isFalsified)
  }

  test("or[String] succeeds when first branch fails, matches at least one char, but is wrapped in attempt") {
    val r = forAll(strings1(chars).distinct) { case (s1, s2) =>
      val triedParse = parsers.run(attempt(s1 + s2) | (s1 + s1))(s1 + s1)
      triedParse.isRight
    }
    assert(!r.isFalsified)
  }

  test("or[String] succeeds when first branch does not match any chars and second branch matches") {
    val g = (chars.distinct ** strings1(chars).distinct) map { case ((c1, c2), (s1, s2)) => (c1 + s1, c2 + s2) }
    val r = forAll(g) { case (s1, s2) =>
      val triedParse = parsers.run(s1 | s2)(s2)
      triedParse.isRight
    }
    assert(!r.isFalsified)
  }

  test("double succeeds on all Double.toString values") {
    val r = forAll(Gen.double) { d =>
      val triedParse = parsers.run(parsers.double)(d.toString)
      triedParse.exists(_ == d)
    }
    assert(!r.isFalsified)
  }

  test("keepRight") {
    val r = forAll(strings1(chars).distinct) { case (s1, s2) =>
      val triedParse = parsers.run(s1 *> s2)(s1 + s2)
      triedParse.exists(_ == s2)
    }
    assert(!r.isFalsified)
  }

  test("keepLeft") {
    val r = forAll(strings1(chars).distinct) { case (s1, s2) =>
      val triedParse = parsers.run(s1 *< s2)(s1 + s2)
      triedParse.exists(_ == s1)
    }
    assert(!r.isFalsified)
  }

  test("escapedQuoted") {
    val triedParse = parsers.run(escapedQuoted)(""""val\"ue"""")
    assert(triedParse.exists(_ == """val\"ue"""))
  }

  test("label law holds for failed Double parsers") {
    val r = parsers.Laws.labelLaw(double, strings1(chars).unsized).run(100, 10, RNG)
    printReport(r)
    assert(!r.isFalsified)
  }

  test("scope law holds for failed Double parsers") {
    val r = parsers.Laws.scopeLaw(double, strings1(chars).unsized).run(100, 10, RNG)
    printReport(r)
    assert(!r.isFalsified)
  }
}
