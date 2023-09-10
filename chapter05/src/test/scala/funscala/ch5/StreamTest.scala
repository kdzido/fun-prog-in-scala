package funscala.ch5

import org.scalatest.flatspec.AnyFlatSpec

class StreamTest extends AnyFlatSpec {

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

}
