package fpinscala.monads

import fpinscala.UnitSuite
import fpinscala.functors.Functor
import fpinscala.monads.Monad.optionMonad
import fpinscala.testing.Gen.{choose, union, unit}

/**
  * @author caleb
  */
class MonadLaws extends UnitSuite {

  test("Option monad obeys map identity law") {
    val g = union(choose(-10000, 10000).optional, unit(None: Option[Int]))
    val r = run(Functor.Laws.mapIdentity(g))
    assert(r.notFalsified)
  }

}
