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
  def mean_2(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /** Book's example */
  def mean_3(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs.sum / xs.length)

  /** Book's example */
  def safeDiv(x: Double, y: Double): Either[Exception, Double] =
    try {
      Right(x / y)
    } catch {
      case e: Exception ⇒ Left(e)
    }


  /** [CHAP-4][EXERCISE-02] implement variance in terms of mean and flatMap */
  def variance(xs: Seq[Double]): Option[Double] = {
    mean_2(xs).flatMap(m ⇒ {
      val squaredDistances: Seq[Double] = xs.map(x ⇒ math.pow(x - m, 2))
      mean_2(squaredDistances)
    })
  }

  def main(args: Array[String]): Unit = {
    println("Chapter 4 - Error handling without exceptions")
  }
}


