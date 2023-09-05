package funscala.ch4

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OptionTest extends AnyFlatSpec {

  "Option" should "construct None and Some value" in {
    assert(None.isInstanceOf[Option[_]])
    assert(Some(1).isInstanceOf[Option[Int]])
  }

}
