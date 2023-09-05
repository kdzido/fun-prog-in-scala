package funscala.ch4

object Chapter4 {

  /** Book's example */
  def failingFn(n: Int): Int = {
    val x: Int = throw new Exception("Fail!")
    try {
      val y = 42 + 5
      x + y
    } catch {
      case e: Exception ⇒ 43
    }
  }

  /** Book's example */
  def mean(xs: Seq[Double]): Double =
    if (xs.isEmpty)
      throw new ArithmeticException("mean of empty list!")
    else xs.sum / xs.length

  /** Book's example */
  def mean_1(xs: IndexedSeq[Double], onEmpty: Double): Double =
    if (xs.isEmpty) onEmpty
    else xs.sum / xs.length

  /** Book's example */
  def mean_2(xs: IndexedSeq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def main(args: Array[String]): Unit = {
    println("Chapter 4")
  }
}
