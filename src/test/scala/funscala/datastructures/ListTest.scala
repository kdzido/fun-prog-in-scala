package funscala.datastructures

import org.scalatest.flatspec.AnyFlatSpec

import java.util.concurrent.atomic.AtomicBoolean

class ListTest extends AnyFlatSpec {

  "An empty immutable list" should "have always sum of 0" in {
    assert(List.sum(Nil) == 0)
    assert(List.sum2(Nil) == 0)
  }

  it should "have always product of 1.0" in {
    assert(List.product(Nil) == 1.0)
    assert(List.product2(Nil) == 1.0)
  }

  "Non-empty immutable list" should "calculate sum of its elements" in {
    assert(List.sum(Cons(1, Nil)) == 1)
    assert(List.sum(Cons(1, Cons(2, Nil))) == 3)

    assert(List.sum2(Cons(1, Nil)) == 1)
    assert(List.sum2(Cons(1, Cons(2, Nil))) == 3)
  }

  it should "calculate product of its elements" in {
    assert(List.product(Cons(1.0, Nil)) == 1.0)
    assert(List.product(Cons(1.0, Cons(2.0, Cons(0.0, Nil)))) == 0.0)
    assert(List.product(Cons(1.0, Cons(2.0, Cons(3.0, Nil)))) == 6.0)

    assert(List.product2(Cons(1.0, Nil)) == 1.0)
    assert(List.product2(Cons(1.0, Cons(2.0, Cons(0.0, Nil)))) == 0.0)
    assert(List.product2(Cons(1.0, Cons(2.0, Cons(3.0, Nil)))) == 6.0)
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

  "Append of two Lists" should "return combined list" in {
    assert(List.append(List(), List()) == List())
    assert(List.append(List(), List(1)) == List(1))
    assert(List.append(List(), List(1,2)) == List(1,2))

    assert(List.append(List(1), List()) == List(1))
    assert(List.append(List(1,2), List()) == List(1,2))

    assert(List.append(List(1,2), List(3)) == List(1,2,3))
    assert(List.append(List(1,2), List(3,4)) == List(1,2,3,4))
  }

  // [CHAP-3][EXERCISE-14] impl append in terms of foldRight or foldLeft
  "Append of two Lists in terms of foldRight2" should "return combined list" in {
    assert(List.append2(List(), List()) == List())
    assert(List.append2(List(), List(1)) == List(1))
    assert(List.append2(List(), List(1,2)) == List(1,2))

    assert(List.append2(List(1), List()) == List(1))
    assert(List.append2(List(1,2), List()) == List(1,2))

    assert(List.append2(List(1,2), List(3)) == List(1,2,3))
    assert(List.append2(List(1,2), List(3,4)) == List(1,2,3,4))
  }

  "Init of List" should "return all except last element" in {
    assert(List.init(List()) == List())
    assert(List.init(List(1)) == List())

    assert(List.init(List(1,2)) == List(1))
    assert(List.init(List(1,2,3)) == List(1,2))
  }

  "Fold right of List" should "return sum of elements" in {
    val sum: (Int, Int) ⇒ Int = _ + _
    assert(List.foldRight(List[Int](), 0)(sum) == 0)
    assert(List.foldRight(List[Int](1), 0)(sum) == 1)
    assert(List.foldRight(List[Int](1,2,0), 0)(sum) == 3)
  }

  it should "return product of elements" in {
    val product: (Double, Double) ⇒ Double = _ * _
    assert(List.foldRight(List[Double](), 1.0)(product) == 1.0)
    assert(List.foldRight(List(1.0), 1.0)(product) == 1.0)
    assert(List.foldRight(List(1.0, 2.0, 3.0), 1.0)(product) == 6.0)
    assert(List.foldRight(List(1.0, 2.0, 0.0), 1.0)(product) == 0.0)
  }

  // [CHAP-3][EXERCISE-10] implement tail-recursive foldLeft
  "Fold left of List" should "return sum of elements" in {
    val sum: (Int, Int) ⇒ Int = _ + _
    assert(List.foldLeft(List[Int](), 0)(sum) == 0)
    assert(List.foldLeft(List[Int](1), 0)(sum) == 1)
    assert(List.foldLeft(List[Int](1, 2, 0), 0)(sum) == 3)
  }

  // [CHAP-3][EXERCISE-13] implement foldRight in terms of foldLeft (hard)
  "Right folded List" should "be implemented in terms of left fold of List" in {
    // original implementation, processing order impacts result
    assert(List.foldRight(List(3, 2, 1), 0)((a, b) ⇒ a - (b * b)) == 2)
    assert(List.foldRight(List(1, 2, 3), 0)((a, b) ⇒ a - (b * b)) == -48)

    // new implementation, processing order impacts result
    assert(List.foldRight2(List(3, 2, 1), 0)((a, b) ⇒ a - (b * b)) == 2)
    assert(List.foldRight2(List(1, 2, 3), 0)((a, b) ⇒ a - (b * b)) == -48)
  }

  // [CHAP-3][EXERCISE-10] implement tail-recursive foldLeft
  it should "return product of elements" in {
    val product: (Double, Double) ⇒ Double = _ * _
    assert(List.foldLeft(List[Double](), 1.0)(product) == 1.0)
    assert(List.foldLeft(List(1.0), 1.0)(product) == 1.0)
    assert(List.foldLeft(List(1.0, 2.0, 3.0), 1.0)(product) == 6.0)
    assert(List.foldLeft(List(1.0, 2.0, 0.0), 1.0)(product) == 0.0)
  }

  /** [CHAP-3][EXERCISE-07] Question:
   * Can product using foldRight immediately halt the recursion and
   * return 0.0 if it encounters 0.0 ? */
  "Product using foldRight" should "halt recursion when encounters 0.0" in {
    // given:
    val accessedA0 = AtomicBoolean(false)
    val accessedA1 = AtomicBoolean(false)
    val accessedA2 = AtomicBoolean(false)

    val a0: () ⇒ Double = {
      accessedA0.set(true)
      () ⇒ 0.0
    }
    val a1: () ⇒ Double = {
      accessedA1.set(true)
      () ⇒ 1.0
    }
    val a2: () ⇒ Double = {
      accessedA2.set(true)
      () ⇒ 2.0
    }

    // when:
    val inputs = List[() ⇒ Double](a1, a0, a2)
    val result = List.foldRight(inputs, 1.0)(_() * _)
    // then:
    assert(result == 0.0)
    // all list's elements evaluated
    assert(accessedA0.get() == true)
    assert(accessedA1.get() == true)
    assert(accessedA2.get() == true)
  }

  /** [CHAP-3][EXERCISE-08] Question:
   * See what happens when you pass Nil and Cons themselves to
   * foldRight like this:
   * foldRight(List(1,2,3) Nil:List[Int])(Cons(_,_)) */
  "Fold right List data constructors" should "return reversed list" in {
    // check what happens when
    assert(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) == Cons(1, Cons(2, Cons(3, Nil))))
    assert(Cons(1, Cons(2, Cons(3, Nil))) == List(1,2,3))

    // Answer:
    // New list can be constructed in one go only when starting from last element of input list.
    // Singly-linked list can be constructed from the end.
  }

  // [CHAP-3][EXERCISE-09] compute list's length using foldRight
  "Length of List" should "number of elements in list" in {
    assert(List.length(List[Int]()) == 0)
    assert(List.length(List(5)) == 1)
    assert(List.length(List(5,5,5)) == 3)
  }

}
