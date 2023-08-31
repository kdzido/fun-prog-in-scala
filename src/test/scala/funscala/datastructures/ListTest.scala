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

  /** [CHAP-3][EXERCISE-02] impl tail of List */
  "Tail on List" should "drop head" in {
    assert(List.tail(List()) == Nil)
    assert(List.tail(List(1)) == Nil)
    assert(List.tail(List(1,2,3)) == List(2,3))
  }

  /** [CHAP-3][EXERCISE-03] impl drop n elements of List */
  "Drop on List" should "drop n elements" in {
    assert(List.drop(List(), 0) == List())
    assert(List.drop(List(1), 0) == List(1))
    assert(List.drop(List(1,2,3), 0) == List(1,2,3))

    assert(List.drop(List(), -1) == List())
    assert(List.drop(List(1), -1) == List(1))
    assert(List.drop(List(1, 2, 3), -1) == List(1, 2, 3))

    assert(List.drop(List(), 1) == List())
    assert(List.drop(List(1), 1) == List())
    assert(List.drop(List(1), 2) == List())

    assert(List.drop(List(1,2,3), 1) == List(2,3))
    assert(List.drop(List(1,2,3), 2) == List(3))
    assert(List.drop(List(1,2,3), 3) == List())
    assert(List.drop(List(1,2,3), 4) == List())
  }

  /** [CHAP-3][EXERCISE-04] impl dropWhile on List */
  "Drop while on List" should "drop as long as precondition is met" in {
    assert(List.dropWhile(List[Int]())(_ <= 1) == List())
    assert(List.dropWhile(List(1,2,3))(_ <= 0) == List(1,2,3))
    assert(List.dropWhile(List(1,2,3))(_ <= 1) == List(2,3))
    assert(List.dropWhile(List(1,2,3))(_ <= 2) == List(3))
    assert(List.dropWhile(List(1,2,3))(_ <= 3) == List())
    assert(List.dropWhile(List(1,2,3))(_ <= 4) == List())
  }

  /** [CHAP-3][EXERCISE-05] impl setHead on List */
  "Set head on List" should "replace head element if present" in {
    assert(List.setHead(List[Int](), 5) == List())
    assert(List.setHead(List(1, 2, 3), 5) == List(5,2,3))
  }
}
