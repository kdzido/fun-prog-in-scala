package funscala.ch7

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Chapter7Test extends AnyFlatSpec with Matchers {

  it should "be always true" in {
    Chapter7.Truth shouldBe true
  }

  it should "sum elements of sequence" in {
    Chapter7.sum(Vector(1,2,3)) shouldBe 6
  }


}
