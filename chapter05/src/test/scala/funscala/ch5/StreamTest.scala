package funscala.ch5

import org.scalatest.matchers.should._
import org.scalatest.flatspec.AnyFlatSpec

class StreamTest extends AnyFlatSpec with Matchers {

  "Empty Stream" should "not have any elements" in {
    val emptyStream = Stream.empty[Int]
    assert(emptyStream.isEmpty == true)
    assert(emptyStream.uncons.isDefined == false)
  }

  "Stream created with cons" should "have elements" in {
    val stream = Stream.cons(1, Stream.cons(2, Stream.empty))
    assert(stream.uncons.get._1 == 1)
    assert(stream.uncons.get._2.uncons.get._1 == 2)
    assert(stream.uncons.get._2.uncons.get._2.isEmpty == true)
    assert(stream.toList == List(1,2))
  }

  "Stream created with apply" should "have elements" in {
    val stream = Stream(1,2)
    assert(stream.uncons.get._1 == 1)
    assert(stream.uncons.get._2.uncons.get._1 == 2)
    assert(stream.uncons.get._2.uncons.get._2.isEmpty == true)
    assert(stream.toList == List(1,2))
  }

  // [CHAP-5][EXERCISE-01] implement toList on Stream
  "Stream" should "be converted into List" in {
    assert(Stream.empty[Int].toList == List[Int]())
    assert(Stream(1).toList == List(1))
    assert(Stream(1, 2, 3).toList == List(1, 2, 3))
  }

  // [CHAP-5][EXERCISE-02] implement take on Stream
  it should "allow to take n elements" in {
    assert(Stream.empty[Int].take(-1).toList == List[Int]())
    assert(Stream.empty[Int].take(0).toList == List[Int]())
    assert(Stream.empty[Int].take(1).toList == List[Int]())

    assert(Stream(1).take(0).toList == List())
    assert(Stream(1).take(1).toList == List(1))
    assert(Stream(1).take(2).toList == List(1))

    assert(Stream(1,2,3).take(0).toList == List())
    assert(Stream(1,2,3).take(1).toList == List(1))
    assert(Stream(1,2,3).take(2).toList == List(1,2))
    assert(Stream(1,2,3).take(3).toList == List(1,2,3))
    assert(Stream(1,2,3).take(4).toList == List(1,2,3))
  }

  // [CHAP-5][EXERCISE-03] implement takeWhile on Stream
  it should "allow to takeWhile predicate holds" in {
    assert(Stream.empty[Int].takeWhile(_ <= 1).toList == List[Int]())
    assert(Stream.empty[Int].takeWhile(_ > 1).toList == List[Int]())

    assert(Stream(1).takeWhile(_ <= 1).toList == List(1))
    assert(Stream(1).takeWhile(_ > 1).toList == List())

    assert(Stream(1, 2, 3).takeWhile(_ <= 0).toList == List())
    assert(Stream(1, 2, 3).takeWhile(_ <= 1).toList == List(1))
    assert(Stream(1, 2, 3).takeWhile(_ <= 2).toList == List(1, 2))
    assert(Stream(1, 2, 3).takeWhile(_ <= 3).toList == List(1, 2, 3))
    assert(Stream(1, 2, 3).takeWhile(_ <= 4).toList == List(1, 2, 3))
  }

  // [CHAP-5][EXERCISE-05] implement takeWhile in terms of foldRight
  it should "allow to takeWhile_2 predicate holds" in {
    assert(Stream.empty[Int].takeWhile_2(_ <= 1).toList == List[Int]())
    assert(Stream.empty[Int].takeWhile_2(_ > 1).toList == List[Int]())

    assert(Stream(1).takeWhile_2(_ <= 1).toList == List(1))
    assert(Stream(1).takeWhile_2(_ > 1).toList == List())

    assert(Stream(1, 2, 3).takeWhile_2(_ <= 0).toList == List())
    assert(Stream(1, 2, 3).takeWhile_2(_ <= 1).toList == List(1))
    assert(Stream(1, 2, 3).takeWhile_2(_ <= 2).toList == List(1, 2))
    assert(Stream(1, 2, 3).takeWhile_2(_ <= 3).toList == List(1, 2, 3))
    assert(Stream(1, 2, 3).takeWhile_2(_ <= 4).toList == List(1, 2, 3))
  }


  it should "foldRight" in {
    assert(Stream[Int]().foldRight(0)(_ + _) == 0)
    assert(Stream(1).foldRight(0)(_ + _) == 1)
    assert(Stream(1,2,3).foldRight(0)(_ + _) == 6)
  }

  it should "check existence of element" in {
    assert(Stream[Int]().exists(_ == 1) == false)
    assert(Stream(1, 2, 3).exists(_ == 0) == false)
    assert(Stream(1, 2, 3).exists(_ == 1) == true)
    assert(Stream(1, 2, 3).exists(_ == 2) == true)
    assert(Stream(1, 2, 3).exists(_ == 3) == true)
    assert(Stream(1, 2, 3).exists(_ == 4) == false)
  }

  // [CHAP-5][EXERCISE-04] implement forAll on Stream
  it should "check predicate forAll" in {
    assert(Stream[Int]().forAll(_ == 0) == true)
    assert(Stream(1, 2, 3).forAll(_ >= 0) == true)
    assert(Stream(1, 2, 3).forAll(_ <= 3) == true)

    assert(Stream(1, 2, 3).forAll(_ <= 2) == false)
    assert(Stream(1, 2, 3).forAll(_ <= 1) == false)
    assert(Stream(1, 2, 3).forAll(_ <= 0) == false)
  }

  // [CHAP-5][EXERCISE-06] implement map, filter, append and flatMap in terms of foldRight
  it should "map using given function" in {
    assert(Stream[Int]().map(_.toString).toList == List[String]())
    assert(Stream(1).map(_.toString).toList == List("1"))
    assert(Stream(1,2,3).map(_.toString).toList == List("1","2","3"))
  }

  // [CHAP-5][EXERCISE-06] implement map, filter, append and flatMap in terms of foldRight
  it should "flatMap using given function" in {
    assert(Stream[Int]().flatMap((a) ⇒ Stream(a, a.toString)).toList == List[String]())
    assert(Stream(1).flatMap((a) ⇒ Stream(a.toString, a.toString)).toList == List("1","1"))
    assert(Stream(1,2,3).flatMap((a) ⇒ Stream(a.toString, a.toString)).toList == List("1","1","2","2","3","3"))
  }

  it should "filter using given predicate" in {
    assert(Stream[Int]().filter(_ == 0).toList == List[Int]())

    assert(Stream(1).filter(_ == 0).toList == List[Int]())
    assert(Stream(1).filter(_ == 1).toList == List[Int](1))

    assert(Stream(1, 2, 3).filter(_ <= 3).toList == List(1,2,3))
    assert(Stream(1, 2, 3).filter(_ <= 2).toList == List(1,2))
    assert(Stream(1, 2, 3).filter(_ <= 1).toList == List(1))
    assert(Stream(1, 2, 3).filter(_ <= 0).toList == List())
  }

  it should "append another stream" in {
    assert(Stream.append(Stream.empty[Int], Stream.empty[Int]).toList == List[Int]())
    assert(Stream.append(Stream(1,2,3), Stream.empty[Int]).toList == List(1,2,3))
    assert(Stream.append(Stream.empty[Int], Stream(4,5,6)).toList == List(4,5,6))
    assert(Stream.append(Stream(1,2,3), Stream(4,5,6)).toList == List(1,2,3,4,5,6))
  }

  "Infinite stream of ones" should "allow to take n elements" in {
    assert(Stream.ones.take(3).toList == List(1,1,1))
  }

  it should "check existence lazily with short circuiting" in {
    assert(Stream.ones.exists(_ % 2 != 0) == true)
  }

  it should "check forAll lazily with short circuiting" in {
    assert(Stream.ones.forAll(_ != 1) == false)
  }

  // [CHAP-5][EXERCISE-07] implement infinite Stream generator of constant
  "Infinite Stream of constant" should "generate constant values" in {
    assert(Stream.constant(2).take(3).toList == List(2,2,2))
    assert(Stream.constant(2).exists(_ == 2) == true)
    assert(Stream.constant(2).forAll(_ != 2) == false)
  }

  // [CHAP-5][EXERCISE-08] implement infinite incremental Stream generator starting from given n
  "Infinite incremental Stream from Int" should "generate consecutive Ints" in {
    assert(Stream.from(3).take(3).toList == List(3,4,5))
    assert(Stream.from(3).exists(_ == 3) == true)
    assert(Stream.from(3).forAll(_ < 3) == false)
  }

  // [CHAP-5][EXERCISE-9] implement infinite Stream of Fibonacci numbers
  "Infinite Stream of Fibonacci numbers" should "generate consecutive fibs" in {
    Stream.fibs.take(0).toList shouldBe List()
    Stream.fibs.take(1).toList shouldBe List(0)
    Stream.fibs.take(8).toList shouldBe List(0,1,1,2,3,5,8,13)
  }

  // [CHAP-5][EXERCISE-10] implement unfold on Stream
  "unfold" should "generate Stream" in {
    Stream.unfold(0)(s ⇒ Some((s+1, s+1))).take(0).toList shouldBe List()
    Stream.unfold(0)(s ⇒ Some((s+1, s+1))).take(1).toList shouldBe List(1)
    Stream.unfold(0)(s ⇒ Some((s+1, s+1))).take(3).toList shouldBe List(1,2,3)
  }

}
