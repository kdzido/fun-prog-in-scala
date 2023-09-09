package funscala.ch4

object Chapter5 {

  /** Book's example */
  def square(x: Double): Double = x * x

  /** Book's example.
   * Unevaluated form of expression is called a thunk, represented as Function0. */
  def if2[A](cond: Boolean, onTrue: ⇒ A, onFalse: ⇒ A): A =
    if (cond) onTrue else onFalse

  /** Book's example. */
  def pair(i: ⇒ Int) = (i, i)

  def pair2(i: ⇒ Int) = { lazy val j = i; (j,j)}

  // function is said to evaluate its argument by-name does evaluate it everytime such argument is referenced
  // function is said to evaluate its argument by-need does evaluate it only once and caches its value, also called functions's lazy

  def main(args: Array[String]): Unit = {
    println("Chapter 5 - Strictness and laziness")

    pair { println("pair's hi"); 1 + 41}
    pair2 { println("pair2's hi"); 1 + 41}
  }

}


