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

  /** Book's example */
  def lessThan = new Function2[Int, Int, Boolean] {
    override def apply(a: Int, b: Int): Boolean = a < b
  }

  /** Book's example */
  def binarySearch[A](as: Array[A], key: A, gt: (A, A) ⇒ Boolean): Int = {
    @tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val a = as(mid2)
        val greater = gt(a, key)
        if (!greater && !gt(key, a)) mid2
        else if (gt(a, key)) go(low, mid2, mid2-1)
        else go(mid2 + 1, mid2, high)
      }
    }
    go(0, 0, as.length - 1)
  }

  /** [CHAP-2][EXERCISE-02] - check if Array[A] is sorted according to given comparison function */
  def isSorted[A](as: Array[A], gt: (A, A) ⇒ Boolean): Boolean = {
    @tailrec
    def go(h: A, tail: Array[A]): Boolean = {
      if (tail.isEmpty) true
      else if (!gt(tail.head, h)) false
      else go(tail.head, tail.tail)
    }
    if (as.isEmpty) true else go(as.head, as.tail)
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
  }
}

