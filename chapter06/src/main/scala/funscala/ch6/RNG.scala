package funscala.ch6

trait Random {
  def nextInt: Int
  def nextBoolean: Boolean
  def nextDouble: Double
}

/** Book's example */
trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  /** Book's example */
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) &
        ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int],
        simple(seed2))
    }
  }
}
