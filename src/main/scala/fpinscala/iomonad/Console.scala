package fpinscala.iomonad

import fpinscala.free._
import fpinscala.parallel.Par
import fpinscala.parallel.Par.Par

import scala.io.StdIn.readLine

import Free.runFree


/**
  * @author caleb
  */
object console {

  sealed trait Console[A] {
    def toPar: Par[A] = Par.lazyUnit(run)

    def toThunk: () => A = () => run

    def run: A
  }

  case object ReadLine extends Console[Option[String]] {
    def run: Option[String] =
      try {
        Some(readLine())
      }
      catch {
        case e: Exception => None
      }
  }

  case class PrintLine(line: String) extends Console[Unit] {
    def run: Unit = println(line)
  }

  private val consoleToFunction0 = new (Console ~> Function0) {
    override def apply[A](f: Console[A]): () => A = f.toThunk
  }

  private val consoleToPar = new (Console ~> Par.Par) {
    override def apply[A](f: Console[A]): Par[A] = f.toPar
  }

  object Console {
    type ConsoleIO[A] = Free[Console, A]

    def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)

    def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))

    def runConsoleFunction0[A](a: ConsoleIO[A]): () => A =
      runFree(a)(consoleToFunction0)

    def runConsolePar[A](a: ConsoleIO[A]): Par[A] =
      runFree(a)(consoleToPar)
  }

}
