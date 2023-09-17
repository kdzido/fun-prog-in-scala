package funscala.ch6

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  /** [CHAP-6][EXERCISE-13] (hard) implement candy dispenser state machine */
  def transit(in: Input): Machine = (this, in) match {
    case (Machine(true, candies, coins), Coin) if candies > 0 => Machine(false, candies, coins + 1)
    case (Machine(false, candies, coins), Turn) if candies > 0 => Machine(true, candies - 1, coins)
    case (Machine(true, candies, coins), Turn) => Machine(true, candies, coins) // turning knob on locked machine does nothing
    case (Machine(false, candies, coins), Coin) => Machine(false, candies, coins) // inserting coin on unlocked machine does nothing
    case (Machine(locked, 0, coins), _) => Machine(locked, 0, coins) // out of candies, ignores all input
  }

}

object Machine {
  def simple(input: Input): State[Machine, Machine] = State(m => {
    val newState = m.transit(input)
    (newState, newState)
  })

  /** [CHAP-6][EXERCISE-13] (hard) implement candy dispenser state machine
   * @return number of coins in the candy machine
   */
  def simulateInputs(theInputs: List[Input]): State[Machine, Int] = State(machine => {
    val leftCoins = theInputs.map(input => Machine.simple(input).flatMap(m => State.unit(m.coins)))
    State.sequence(leftCoins).map(_.last).run(machine)
  })

}
