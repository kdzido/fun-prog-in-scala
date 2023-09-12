package funscala.ch6

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Chapter6Test extends AnyFlatSpec with Matchers {

  it should "be true" in {
    true shouldBe true
  }

}
