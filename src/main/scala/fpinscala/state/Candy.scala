package fpinscala.state

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

/**
  * @author caleb
  */
object Machine {
  import State._

  type MachineState = State[Machine, (Int, Int)]

  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(false, candies, _)) => s.copy(locked = true, candies = candies - 1)
      case (Coin, Machine(true, _, coins)) => s.copy(locked = false, coins = coins + 1)
    }


  def simulateMachine(inputs: List[Input]): MachineState =
    for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)
//  {
//    sequence(inputs.map(input => modify[Machine](machine => update(input)(machine))))
//      .flatMap(_ => get)
//        .map(s => (s.coins, s.candies))
//  }
}
