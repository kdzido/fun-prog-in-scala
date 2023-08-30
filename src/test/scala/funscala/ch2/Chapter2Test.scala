package funscala.ch2

import funscala.ch2.Chapter2.{abs, binarySearch, factorial, fib, formatAbs, formatResult, isSorted, lessThan}
import org.scalatest.funsuite.AnyFunSuiteLike

class Chapter2Test extends AnyFunSuiteLike {

  test("should get absolute value of given integer") {
    assert(abs(-1) == 1)
    assert(abs(0) == 0)
    assert(abs(1) == 1)

    assert(abs(Int.MaxValue) == Int.MaxValue)
    assert(abs(Int.MinValue) == -2147483648) // TODO abs not working in 1 edge case
  }

  test("should calculate factorial for n") {
    assert(factorial(0) == 1)
    assert(factorial(1) == 1)
    assert(factorial(2) == 2)
    assert(factorial(3) == 6)
    assert(factorial(4) == 24)
  }

  test("should calculate nth Fibonacci's number") {
    assert(fib(0) == 1)
    assert(fib(1) == 1)
    assert(fib(2) == 2)
    assert(fib(3) == 3)
    assert(fib(4) == 5)
    assert(fib(5) == 8)

    // would cause stack overflow if not tail-recursive implementation
    assert(fib(1500) == 1415338001064792265L)
  }

  test("should format abs as string") {
    assert(formatAbs(-42) == "The absolute value of -42 is 42.")
  }

  test("should format result as string") {
    assert(formatResult("absolute value", -42, abs) == "The absolute value of -42 is 42.")
    assert(formatResult("factorial", 7, factorial) == "The factorial of 7 is 5040.")
    assert(formatResult("increment", 7, (x: Int) ⇒ x + 1) == "The increment of 7 is 8.")
    assert(formatResult("increment2", 7, (x) ⇒ x + 1) == "The increment2 of 7 is 8.")
    assert(formatResult("increment3", 7, x ⇒ x + 1) == "The increment3 of 7 is 8.")
    assert(formatResult("increment4", 7, _ + 1) == "The increment4 of 7 is 8.")
    assert(formatResult("increment5", 7, x ⇒ { val r = x + 1; r}) == "The increment5 of 7 is 8.")
  }

  test("should check if less than n") {
    assert(lessThan.apply(1, 2) == true)
    assert(lessThan.apply(1, 1) == false)
    assert(lessThan.apply(1, 0) == false)
  }

  test("binary search should return -1 when element not found") {
    assert(binarySearch(Array[Double](), 3.14, _ > _) == -1)
    assert(binarySearch(Array(3.0), 3.14, _ > _) == -1)
    assert(binarySearch(Array(3.1), 3.14, _ > _) == -1)
    assert(binarySearch(Array(3.141), 3.14, _ > _) == -1)
    assert(binarySearch(Array(4.0), 3.14, _ > _) == -1)
  }

  test("binary search should index of found element") {
    assert(binarySearch(Array(0.0), 0.0, _ > _) == 0)
    assert(binarySearch(Array(0.0, 1.0, 2.0, 3.0), 3.0, _ > _) == 3)
  }

  test("isSorted should check if sorted as per fn") {
    val gt: (Int, Int) => Boolean = _ > _
    val lt: (Int, Int) => Boolean = _ < _
    val eq: (Int, Int) => Boolean = _ == _

    assert(isSorted(Array[Int](), gt) == true)
    assert(isSorted(Array[Int](), lt) == true)
    assert(isSorted(Array[Int](), eq) == true)

    assert(isSorted(Array(1), gt) == true)
    assert(isSorted(Array(1), lt) == true)
    assert(isSorted(Array(1), eq) == true)

    assert(isSorted(Array(1, 2), gt) == true)
    assert(isSorted(Array(1, 2), lt) == false)
    assert(isSorted(Array(1, 2), eq) == false)

    assert(isSorted(Array(1, 1), gt) == false)
    assert(isSorted(Array(1, 1), lt) == false)
    assert(isSorted(Array(1, 1), eq) == true)
    assert(isSorted(Array(1, 1, 1), eq) == true)

    assert(isSorted(Array(1, 2, 3, 5, 9), gt) == true)
    assert(isSorted(Array(2, 1, 3, 5, 9), gt) == false)
    assert(isSorted(Array(1, 2, 3, 9, 5), gt) == false)
  }

  test("should do partial application") {
    def sum: (Int, Int) ⇒ Int = _ + _
    def incrBy1 = Chapter2.partial1(1, sum)
    def incrBy2 = Chapter2.partial1(2, sum)

    assert(incrBy1(1) == 2)
    assert(incrBy1(2) == 3)

    assert(incrBy2(5) == 7)
    assert(incrBy2(6) == 8)
  }

}
