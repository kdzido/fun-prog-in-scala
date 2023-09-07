package funscala.ch4

import org.scalatest.flatspec.AnyFlatSpec

class EitherTest extends AnyFlatSpec {

  "Either data type" should "construct all types of values" in {
    val left = Left(new IllegalArgumentException("fail!"))
    val right = Right(1)

    assert(left.isInstanceOf[Left[_]])
    assert(left.value.getMessage == "fail!")

    assert(right.isInstanceOf[Right[_]])
    assert(right.value == 1)
  }

  // [CHAP-4][EXERCISE-07] implement Either trait methods
  it should "map" in {
    val left = Left(new IllegalArgumentException("fail!"))
    val right = Right(1)

    assertResult("fail!") {
      left.map(_.toString) match {
        case Left(a) ⇒ a.getMessage
      }
    }
    assertResult("1") {
      right.map(_.toString) match {
        case Right(b) ⇒ b
      }
    }
  }

  it should "flatMap" in {
    val left = Left(new IllegalArgumentException("fail!"))
    val right = Right(1)

    assertResult("fail!") {
      left.flatMap((a) ⇒ Right(a.toString)) match {
        case Left(a) ⇒ a.getMessage
      }
    }
    assertResult("failed") {
      right.flatMap((a) ⇒ Left("failed")) match {
        case Left(a) ⇒ a
      }
    }
    assertResult("1") {
      right.flatMap((a) ⇒ Right(a.toString)) match {
        case Right(b) ⇒ b
      }
    }
  }

  it should "orElse" in {
    val left = Left(new IllegalArgumentException("fail!"))
    val right = Right(1)

    assertResult("orElse failed") {
      left.orElse(Left("orElse failed")) match {
        case Left(a) ⇒ a
      }
    }
    assertResult(1) {
      right.orElse(Left("orElse failed")) match {
        case Right(a) ⇒ a
      }
    }
  }

  it should "map2" in {
    val left = Left(new IllegalArgumentException("fail!"))
    val left2 = Left(new IllegalArgumentException("fail2!"))
    val right = Right(1)
    val right2 = Right(2)

    assertResult("fail!") {
      left.map2(Right("right2"))((a,b) ⇒ a.toString) match {
        case Left(a) ⇒ a.getMessage
      }
    }
    assertResult("fail2!") {
      right.map2(left2)((a,b) ⇒ a.toString) match {
        case Left(a) ⇒ a.getMessage
      }
    }
    assertResult("3") {
      right.map2(right2)((a,b) ⇒ (a+b).toString) match {
        case Right(a) ⇒ a
      }
    }
  }


}
