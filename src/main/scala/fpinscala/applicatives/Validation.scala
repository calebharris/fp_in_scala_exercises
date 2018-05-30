package fpinscala.applicatives

/**
  * @author caleb
  */
sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Validation {
  def map2[E, A, B, C](va: Validation[E, A], vb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
    (va, vb) match {
      case (Success(a),      Success(b)     ) => Success(f(a, b))
      case (Success(_),      Failure(h, t)  ) => Failure(h, t)
      case (Failure(h, t),   Success(_)     ) => Failure(h, t)
      case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ (h2 +: t2))
    }
}