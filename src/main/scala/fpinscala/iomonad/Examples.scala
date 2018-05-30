package fpinscala.iomonad

import scala.io.StdIn._
import scala.language.postfixOps

/**
  * @author caleb
  */
object examples {
  def farenheitToCelsius(f: Double): Double = (f - 32) * 5.0/9.0

  object tailrec {
    import fpinscala.iomonad.tailrec.TailRec
    import TailRec._

    def converter: TailRec[Unit] = for {
      _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
      d <- ReadLine.map(_.toDouble)
      _ <- PrintLine(farenheitToCelsius(d).toString)
    } yield ()

    val helpstring =
      """
        | The Amazing Factorial REPL, v2.0
        | q - quit
        | <number> - compute the factorial of the given number
        | <anything else> - bomb with horrible error
      """.trim.stripMargin

    def factorial(n: Int): TailRec[Int] = for {
      acc <- ref(1)
      _ <- foreachM(1 to n toStream)(i => skip(acc.modify(_ * i)))
      result <- acc.get
    } yield result

    val factorialREPL: TailRec[Unit] = sequence_(
      TailRec {
        println(helpstring)
      },
      doWhile {
        TailRec {
          readLine
        }
      } { line =>
        val ok = line != "q"
        when(ok) {
          for {
            n <- factorial(line.toInt)
            _ <- TailRec {
              println("factorial: " + n)
            }
          } yield ()
        }
      }
    )
  }
}
