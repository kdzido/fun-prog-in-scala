package funscala.ch6

import scala.annotation.tailrec

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

  /** [CHAP-6][EXERCISE-01] implement random positiveInt */
  def positiveInt(rng: RNG): (Int, RNG) = {
    val (next, rng2) = rng.nextInt
    if (next == Int.MinValue) positiveInt(rng2) else (next.abs, rng2)
  }

  /** [CHAP-6][EXERCISE-02] implement random double */
  def double(rng: RNG): (Double, RNG) = {
    val (i1, rng2) = positiveInt(rng)
    (i1.toDouble * (1.0 / Int.MaxValue.toDouble), rng2)
  }

  /** [CHAP-6][EXERCISE-03] implement random pairs (Int,Double), (Double,Int), triple (Double,Double,Double) */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i1, rng2) = positiveInt(rng)
    val (d2, rng3) = double(rng2)
    ((i1,d2), rng3)
  }

  /** [CHAP-6][EXERCISE-03] implement random pairs (Int,Double), (Double,Int), triple (Double,Double,Double) */
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d1, rng2) = double(rng)
    val (i2, rng3) = positiveInt(rng2)
    ((d1, i2), rng3)
  }

  /** [CHAP-6][EXERCISE-03] implement random pairs (Int,Double), (Double,Int), triple (Double,Double,Double) */
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  /** [CHAP-6][EXERCISE-04] implement positiveInt */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(c: Int, acc: (List[Int], RNG), r: RNG): (List[Int], RNG) = {
      if (c <= 0) acc
      else {
        val (i, rng2) = positiveInt(r)
        go(c-1, (i :: acc._1, acc._2), rng2)
      }
    }
    val reversed = go(count, (List[Int](), rng), rng)
    (reversed._1.reverse, reversed._2)
  }

}
