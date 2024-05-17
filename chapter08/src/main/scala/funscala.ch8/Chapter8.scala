package funscala.ch8

import funscala.ch6.{RNG, State}


trait Prop {
  def check: Boolean

  /** [CHAP-8][EXERCISE-3] implement && as method of Prop */
  def &&(p: Prop): Prop = new Prop {
    override def check: Boolean = and(Prop.this.check, p.check)
  }

  private def and(b1: Boolean, b2: Boolean): Boolean = b1 && b2
}

type Gen[A] = State[RNG, A]

object Gen {
  /** [CHAP-8][EXERCISE-4] implement Gen.choose */
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val delta = if (stopExclusive > start) stopExclusive - start else 1
     RNG.positiveMax(delta - 1)
  .map(s ⇒ s + start)
  }
}

case object AlwaysTrue extends Prop {
  override def check: Boolean = true
}

case object AlwaysFalse extends Prop {
  override def check: Boolean = false
}


object Chapter8 {

  @main def main(): Unit = {
    println("Chapter 8 - Property based testing")
    println("true && true = " + AlwaysTrue.&&(AlwaysTrue).check)
  }

}
