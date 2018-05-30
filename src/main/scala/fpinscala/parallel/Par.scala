package fpinscala.parallel

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, TimeUnit}

import language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
  * @author caleb
  */

//case class Par[A](run: ExecutorService => Future[A]) {
//  import Par._
//
//  def map[B](f: A => B): Par[B] = flatMap(a => unit(f(a)))
//
//  def flatMap[B](f: A => Par[B]): Par[B] = Par[B](es => {
//    f(run(es).get()) run es
//  })
//}

sealed trait Future[+A] {
  private[parallel] def apply(k: Try[A] => Unit): Unit
}

object Par {

  private case class UnitFuture[A](get: A) extends java.util.concurrent.Future[A] {
    override def isDone = true
    override def get(timeout: Long, units: TimeUnit) = get
    override def isCancelled = false
    override def cancel(evenIfRunning: Boolean): Boolean = false
  }

  private case class Map2Future[A, B, C]
  (fa: java.util.concurrent.Future[A],
   fb: java.util.concurrent.Future[B],
   f: (A, B) => C)
    extends java.util.concurrent.Future[C] {

    @volatile var cache: Option[C] = None

    private def doTimed[T](t: => T): (Long, T) = {
      val start = System.currentTimeMillis()
      val t1 = t
      (System.currentTimeMillis() - start, t1)
    }

    override def isDone = cache.isDefined

    override def get = get(60, TimeUnit.SECONDS)

    override def get(timeout: Long, units: TimeUnit) = cache match {
      case Some(c) => c
      case None =>
        val (d, a) = doTimed(fa.get(timeout, units))
        val remaining = units.convert(d, TimeUnit.MILLISECONDS)
        val b = fb.get(remaining, units)
        val c = f(a, b)
        cache = Some(c)
        c
    }

    override def isCancelled: Boolean = fa.isCancelled || fb.isCancelled

    override def cancel(evenIfRunning: Boolean): Boolean = fa.cancel(evenIfRunning) || fb.cancel(evenIfRunning)
  }

  type Par[+A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(pa: Par[A]): Try[A] = {
    println("Inside run")
    val ref = new AtomicReference[Try[A]]
    val latch = new CountDownLatch(1)
    println("Just before pa(es)")
    pa(es).apply { a => println("Before ref.set(a)"); ref.set(a); latch.countDown; println("After latch.countDown") }
    latch.await
    ref.get
  }

  def unit[A](a: A): Par[A] = es => {
    new Future[A] {
      override private[parallel] def apply(k: Try[A] => Unit) = k(Success(a))
    }
  }

  def eval(es: ExecutorService)(r: => Unit): Unit = es.submit(
    new Callable[Unit] {
      override def call = { r }
    })

  def fork[A](pa: => Par[A]): Par[A] =
    es => {
      println("fork par called")
      new Future[A] {
        override private[parallel] def apply(k: Try[A] => Unit): Unit = {
          println("fork Future.apply called")
          eval(es) {
            println("Inside eval's r callback from fork")
            Try(pa(es)) match {
              case Success(fut) => fut(k)
              case Failure(e) => k(Failure(e))
            }
          }
        }
      }
    }


  def map2[A, B, C](ta: Try[A], tb: Try[B])(f: (A, B) => C): Try[C] =
    ta flatMap(a => tb map(b => f(a, b)))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    (pa map2 unit(()))((a, _) => f(a))

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C] {
      override private[parallel] def apply(k: Try[C] => Unit): Unit = {
        var ar: Option[Try[A]] = None
        var br: Option[Try[B]] = None

        val combiner = Actor[Either[Try[A], Try[B]]](es) {
          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) => eval(es)(k(map2(a, b)(f(_, _))))
          }
          case Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) => eval(es)(k(map2(a, b)(f(_, _))))
          }
        }

        pa(es)(a => combiner ! Left(a))
        pb(es)(b => combiner ! Right(b))
      }
    }

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] =
    map2(map2(pa, pb)(f.curried(_)(_)), pc)(_(_))

  def map4[A, B, C, D, E](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D])(f: (A, B, C, D) => E): Par[E] =
    map2(map3(pa, pb, pc)(f.curried(_)(_)(_)), pd)(_(_))

  def map5[A, B, C, D, E, F]
  (pa: Par[A],
   pb: Par[B],
   pc: Par[C],
   pd: Par[D],
   pe: Par[E])
  (f: (A, B, C, D, E) => F): Par[F] = map2(map4(pa, pb, pc, pd)(f.curried(_)(_)(_)(_)), pe)(_ (_))

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  def lazyUnit[A](a: => A) = { es: ExecutorService =>  println("lazyUnit par called"); fork(unit(a))(es) }

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = fork { sequence(as map asyncF(f)) }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = as map (asyncF((a: A) => if (f(a)) List(a) else List()))
    sequence(pars) map (_.flatten)
  }

  def parFoldBalanced[A, B](as: IndexedSeq[A])(i: Option[A] => B)(f: (B, B) => B): Par[B] = {
    def go(is: IndexedSeq[A]): Par[B] =
      if (is.size <= 1)
        unit(i(is.headOption))
      else {
        val (l, r) = is.splitAt(is.length / 2)
        (go(l) map2 go(r)) (f(_, _))
      }

    go(as)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List[A]()))((pa, pas) => (pa map2 pas)(_ :: _))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = parList map { _.sorted }

  def join[A](pa: Par[Par[A]]): Par[A] =
    es => new Future[A] {
      override def apply(k: Try[A] => Unit): Unit = {
        pa(es) {
          _.fold(
            err => k(Failure(err)),
            p2 => eval(es) { p2(es)(k) }
          )
        }
      }
    }

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = join(map(pa)(f))

  def choiceN[A](pn: Par[Int])(choices: List[Par[A]]): Par[A] = flatMap(pn)(choices(_))

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap(cond)(if (_) t else f)

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = map2(p, p2)(_ == _)

  implicit class ParOps[A](p: Par[A]) {
    def equal(p2: Par[A]) = Par.equal(p, p2)
    def flatMap[B](f: A => Par[B]) = Par.flatMap(p)(f)
    def map[B](f: A => B) = Par.map(p)(f)
    def map2[B, C](pb: Par[B])(f: (A, B) => C) = Par.map2(p, pb)(f)
    def run(s: ExecutorService) = Par.run(s)(p)
  }

  implicit class ListOps[A](l: List[A]) {
    def parFilter(f: A => Boolean) = Par.parFilter(l)(f)
    def parMap[B](f: A => B) = Par.parMap(l)(f)
  }

  implicit class IndexedOps[A](is: IndexedSeq[A]) {
    def parFoldBalanced[B](i: Option[A] => B)(f: (B, B) => B) = Par.parFoldBalanced(is)(i)(f)
  }

  implicit class ParListOps[A](l: List[Par[A]]) {
    def sequence = Par.sequence(l)
  }
}

object Examples {
  import Par.{Par, ParOps, IndexedOps, fork, unit, parFoldBalanced}

  def sum(ints: IndexedSeq[Int]): Par[Int] = ints.parFoldBalanced(_ getOrElse 0)(_ + _)

  def max(ints: IndexedSeq[Int]): Par[Int] = ints.parFoldBalanced(_ getOrElse Int.MinValue)(_ max _)
}
