package funscala.ch2

import scala.annotation.tailrec

object Chapter2 {
  /** Book's example */
  def abs(n: Int): Int = if (n < 0) -n else n

  /** Book's example */
  def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d."
    msg.format(x, abs(x))
  }

  /** Book's example */
  def formatResult(name: String, n: Int, f: Int ⇒ Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  /** Book's example */
  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)
    go(n, 1)
  }

  /** [CHAP-2][EXERCISE-01] - function returning nth Fibonacci's number, use tail-recursive function  */
  def fib(n: Int): Long = {
    @tailrec
    def go(i: Int, ub: Int, acc1: Long, acc2: Long): Long = {
      if (i == ub) acc1 + acc2
      else go(i+1, ub, acc2, acc1 + acc2)
    }
    go(0, n, 1, 0)
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
  }
}

