package funscala.ch2

import org.scalatest.funsuite.AnyFunSuiteLike

class Chapter2Test extends AnyFunSuiteLike {

  test("should get absolute value of given integer") {
    assert(Chapter2.abs(-1) == 1)
    assert(Chapter2.abs(0) == 0)
    assert(Chapter2.abs(1) == 1)

    assert(Chapter2.abs(Int.MaxValue) == Int.MaxValue)
    assert(Chapter2.abs(Int.MinValue) == -2147483648) // TODO abs not working in 1 edge case
  }

  test("should calculate factorial for n") {
    assert(Chapter2.factorial(0) == 1)
    assert(Chapter2.factorial(1) == 1)
    assert(Chapter2.factorial(2) == 2)
    assert(Chapter2.factorial(3) == 6)
    assert(Chapter2.factorial(4) == 24)
  }

  test("should calculate nth Fibonacci's number") {
    assert(Chapter2.fib(0) == 1)
    assert(Chapter2.fib(1) == 1)
    assert(Chapter2.fib(2) == 2)
    assert(Chapter2.fib(3) == 3)
    assert(Chapter2.fib(4) == 5)
    assert(Chapter2.fib(5) == 8)

    // would cause stack overflow if not tail-recursive implementation
    assert(Chapter2.fib(1500) == 1415338001064792265L)
  }

}
