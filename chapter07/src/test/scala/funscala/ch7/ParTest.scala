package funscala.ch7

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParTest extends AnyFlatSpec with Matchers {

  it should "map2 two Pars" in {
    Par.get(Par.map2(Par.unit(1), Par.unit(2))(_ + _)) shouldBe 3
  }

}
