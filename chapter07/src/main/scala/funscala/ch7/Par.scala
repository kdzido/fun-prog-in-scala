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
}
