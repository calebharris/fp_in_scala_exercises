package fpinscala.applicatives

import fpinscala.UnitSuite
import fpinscala.functors.Functor
import fpinscala.testing.Gen.{boolean, choose, double, union}
import Applicative._
import fpinscala.testing.Gen

/**
  * @author caleb
  */

class ApplicativeLaws extends UnitSuite {
  import Applicative.Laws

  private val failuresString = strings map (Failure(_))

  private def withStringFailures[A](g: Gen[A]): Gen[Validation[String, A]] =
    union(g map (Success(_)), failuresString)

  private val validationsStringInt = withStringFailures(choose(-10000, 10001))

  private val validationsStringDouble = withStringFailures(double)

  private val validationsStringBool = withStringFailures(boolean)

  test("validationApp obeys functor map identity law") {
    val r = run(Functor.Laws.mapIdentity(validationsStringInt))
    assert(r.notFalsified)
  }

  test("validationApp obeys left map identity law") {
    val r = run(Laws.leftMapIdentity(validationsStringInt))
    assert(r.notFalsified)
  }

  test("validationApp obeys right map identity law") {
    val r = run(Laws.rightMapIdentity(validationsStringInt))
    assert(r.notFalsified)
  }

  test("validationApp obeys associative law") {
    val r = run(Laws.associativity(validationsStringInt, validationsStringBool, validationsStringDouble))
    assert(r.notFalsified)
  }

  test("validationApp obeys naturality law") {
    val r = run(Laws.naturality(validationsStringInt, validationsStringBool)(_ % 2 == 0, _.toString))
    assert(r.notFalsified)
  }
}
