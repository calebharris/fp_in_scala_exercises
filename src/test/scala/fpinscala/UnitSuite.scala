package fpinscala

import fpinscala.state.SimpleRNG
import fpinscala.testing.Gen.{chars, choose}
import fpinscala.testing.Prop.printReport
import fpinscala.testing.{Gen, Prop, Result}
import org.scalatest.FunSuite

/**
  * @author caleb
  */
abstract class UnitSuite extends FunSuite {

  protected val RNG = SimpleRNG(234234525)

  protected val strings: Gen[String] = strings(chars)

  protected def strings(g: Gen[Char]): Gen[String] = choose(0, 100) flatMap { n => Gen.strings(n, g) }

  protected def strings1(g: Gen[Char]): Gen[String] = choose(1, 100) flatMap { n => Gen.strings(n, g) }

  protected def strings2(g: Gen[Char]): Gen[(String, String)] = strings(g).tupled

  protected def forAll[A](g: Gen[A])(f: A => Boolean): Result = run(Gen.forAll(g)(f))

  protected def run(p: Prop): Result = {
    val r = p.run(100, 20, RNG)
    printReport(r)
    r
  }

}
