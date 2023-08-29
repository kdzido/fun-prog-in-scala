package funscala.ch1

case class Player(name: String, score: Int)

object Chapter1 {
  val Truth = true

  def printWinner(p: Player): Unit = println(p.name + " is the winner!")

  def winner(p1: Player, p2: Player): Player = if (p1.score > p2.score) p1 else p2

  def declareWinner(p1: Player, p2: Player): Unit = printWinner(winner(p1, p2))


  @main def main(): Unit = {
    println("Hello world!")

    val sue = Player("Sue", 7)
    val bob = Player("Bob", 8)
    val joe = Player("Joe", 4)

    declareWinner(sue, bob)

    val players = List(sue, bob, joe)
    val p = players.reduceLeft(winner)
    printWinner(p)
  }

}
