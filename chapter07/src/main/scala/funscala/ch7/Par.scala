package funscala.ch7

trait Par[A]

object Par {
  def unit[A](a: => A): Par[A] = ???

  def get[A](a: Par[A]): A = ???

  /** [CHAP-7][EXERCISE-01] provide map2 signature
   */
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = {
    val l = Par.get(a)
    val r = Par.get(b)
    Par.unit(f(l,r))
  }

  /** Book's example */
  def fork[A](a: ⇒ Par[A]): Par[A] = ???

  def cancel(eventIfRunning: Boolean): Boolean = ???
  def isDone(): Boolean = ???
  def isCancelled(): Boolean = ???
}
