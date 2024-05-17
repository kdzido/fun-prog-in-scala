package funscala.ch8


trait Prop {
  def check: Boolean

  /** [CHAP-8][EXERCISE-3] implement && as method of Prop */
  def &&(p: Prop): Prop = new Prop {
    override def check: Boolean = and(Prop.this.check, p.check)
  }

  private def and(b1: Boolean, b2: Boolean): Boolean = b1 && b2
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
