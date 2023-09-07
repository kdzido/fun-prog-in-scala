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

}
