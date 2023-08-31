package funscala.datastructures

import org.scalatest.flatspec.AnyFlatSpec

class ListTest extends AnyFlatSpec {

  "An empty immutable list" should "have always sum of 0" in {
    assert(List.sum(Nil) == 0)
  }

  it should "have always product of 1.0" in {
    assert(List.product(Nil) == 1.0)
  }

  "Non-empty immutable list" should "calculate sum of its elements" in {
    assert(List.sum(Cons(1, Nil)) == 1)
    assert(List.sum(Cons(1, Cons(2, Nil))) == 3)
  }

  it should "calculate product of its elements" in {
    assert(List.product(Cons(1.0, Nil)) == 1.0)
    assert(List.product(Cons(1.0, Cons(2.0, Cons(0.0, Nil)))) == 0.0)
    assert(List.product(Cons(1.0, Cons(2.0, Cons(3.0, Nil)))) == 6.0)
  }

  "Pure variadic apply" should "create empty immutable list" in {
    assert(List() == Nil)
    assert(List[Int]() == Nil)
  }

  it should "create non-empty immutable list" in {
    assert(List(1) == Cons(1, Nil))
    assert(List(1, 2, 3) == Cons(1, Cons(2, Cons(3, Nil))))
  }

  /** [CHAP-3][EXERCISE-01] pattern matching result */
  "The x pattern matching expr" should "return" in {
    assert(List.x == 3)
  }

}
