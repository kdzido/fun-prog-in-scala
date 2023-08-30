package funscala.ch2

import funscala.ch2.Chapter2.{abs, factorial, fib, formatAbs, formatResult}
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



}
