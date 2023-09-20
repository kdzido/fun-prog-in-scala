package funscala.ch7

trait Par[A] {
  def get: A
}

object Par {
  def unit[A](a: => A): Par[A] = new Par[A] {
    lazy val value = a

    override def get: A = value
  }

  def get[A](a: Par[A]): A = a.get

  /** [CHAP-7][EXERCISE-01] provide map2 signature
   */
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = {
    val l = Par.get(a)
    val r = Par.get(b)
    Par.unit(f(l,r))
  }
}
