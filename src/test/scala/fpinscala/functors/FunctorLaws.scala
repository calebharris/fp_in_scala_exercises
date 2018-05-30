package fpinscala.functors

import fpinscala.UnitSuite
import fpinscala.testing.Gen.choose

/**
  * @author caleb
  */
class FunctorLaws extends UnitSuite {

  test("listFunctor obeys map identity law") {
    val r = run(Functor.Laws.mapIdentity(choose(-10000, 10000).listOfN(choose(0, 100))))
    assert(r.notFalsified)
  }
}
