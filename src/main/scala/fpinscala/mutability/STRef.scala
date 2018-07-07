package fpinscala.mutability

/**
  * @author caleb
  */
sealed trait STRef[S, A] {
  protected var cell: A

  def read: ST[S, A] = ST(cell)

  def write(a: A): ST[S, Unit] = (s: S) => {
    cell = a
    ((), s)
  }

  def modify(f: A => A): ST[S, Unit] = for {
    a <- read
    _ <- write(f(a))
  } yield ()
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
    var cell: A = a
  })
}
