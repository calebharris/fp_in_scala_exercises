package fpinscala.monoids

import fpinscala.UnitSuite
import MonoidInstances._
import Monoid.Laws._
import fpinscala.testing.Gen.{boolean, chars, choose}

/**
  * @author caleb
  */
class MonoidLaws extends UnitSuite {

  test("associativity under integer addition") {
    val r = run(associativity(IntAddition)(choose(-10000, 10000)))
    assert(r.notFalsified)
  }

  test("identity under integer addition") {
    val r = run(identity(IntAddition)(choose(-10000, 10000)))
    assert(r.notFalsified)
  }

  test("String.length is homomorphism between String concatenation and Integer addition") {
    val r = run(homomorphism(StringMonoid, IntAddition)(_.length)(strings(chars)))
    assert(r.notFalsified)
  }

  test("String and List[Char] concatenations are isomorphic") {
    val charLists = chars.listOfN(choose(0, 100))
    val r = run(
      isomorphism(StringMonoid, ListMonoid[Char])(_.toList, _.mkString)(strings(chars), charLists)
    )
    assert(r.notFalsified)
  }

  test("BooleanOr and BooleanAnd are isomorphic via negation") {
    val negate = (b: Boolean) => !b
    val r = run(
      isomorphism(BooleanOr, BooleanAnd)(negate, negate)(boolean, boolean)
    )
    assert(r.notFalsified)
  }
}
