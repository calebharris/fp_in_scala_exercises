package fpinscala.parallel

import java.util.concurrent.Executors
import Executors._

import fpinscala.UnitSuite
import fpinscala.state.SimpleRNG
import fpinscala.testing.{Gen, Prop}
import Gen._
import Par._


/**
  * @author caleb
  */

object ** {
  def unapply[A, B](p: (A, B)) = Some(p)
}

class ParSuite extends UnitSuite {
  val ES = newCachedThreadPool()
  val S = weighted(
    choose(1, 4).map(newFixedThreadPool) -> .75,
    Gen.unit(newCachedThreadPool) -> .25
  )

  override val RNG = SimpleRNG(345265374)

  val pint2: Gen[Par[Int]] = choose(-100,100).listOfN(choose(0,20)).map(l =>
    l.foldLeft(Par.unit(0))((p,i) =>
      Par.fork { Par.map2(p, Par.unit(i))(_ + _) }))

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    Gen.forAll(S ** g) { case s ** a => Par.run(s)(f(a)).getOrElse(false) }

  test("Map with increment equals unit result") {
    val p2 = checkPar(Par.unit(1).map(_ + 1) equal Par.unit(2))
    assert(!p2.run(1, 1, RNG).isFalsified)
  }

  test("Mapping over identity has no effect") {
    val p = forAllPar(pint2)(n => equal(Par.map(n)(y => y), n))
    assert(!p.run(10, 10, RNG).isFalsified)
  }

  test("Forking computation does not change result") {
    val p = forAllPar(pint2)(n => equal(fork(n), n))
    assert(!p.run(10, 10, RNG).isFalsified)
  }
}
