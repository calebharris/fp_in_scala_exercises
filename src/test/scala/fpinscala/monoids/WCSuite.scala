package fpinscala.monoids

import fpinscala.UnitSuite
import fpinscala.testing.Gen
import Monoid.Laws._
import fpinscala.monoids.WC.WCMonoid
import Gen.{chars, union}
import fpinscala.monoids.MonoidInstances.StringMonoid

/**
  * @author caleb
  */
class WCSuite extends UnitSuite {

  private val parts = for {
    left <- strings(chars)
    words <- Gen.choose(0, 100)
    right <- strings(chars)
  } yield Part(left, words, right)

  private val stubs = for {
    chars <- strings(chars)
  } yield Stub(chars)

  private val wcs = union(parts, stubs)

  test("WCMonoid follows law of associativity") {
    val r = run(associativity(WCMonoid)(wcs))
    assert(r.notFalsified)
  }

  test("WCMonoid follows law of identity") {
    val r = run(identity(WCMonoid)(wcs))
    assert(r.notFalsified)
  }

  test("WC is homomorphism from String concatenation to WCMonoid") {
    val r = run(homomorphism(StringMonoid, WCMonoid)(s => WC(s))(strings(chars)))
    assert(r.notFalsified)
  }

}
