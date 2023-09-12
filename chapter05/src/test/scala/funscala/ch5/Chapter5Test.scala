package funscala.ch5

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Chapter5Test extends AnyFlatSpec with Matchers {

  "square of Double" should "always evaluates its arguments (strictly)" in {
    assert(Chapter5.square(2.0) == 4.0)
    assert(Chapter5.square(41.0 + 1.0) == 1764)
    assert(Chapter5.square(-1.0) == 1.0)

    the[RuntimeException] thrownBy {
      Chapter5.square(sys.error("failure"))
    } should have message ("failure")
  }

  "if control structure" should "choose to evaluate only one branch thunk based on condition" in {
    val resultOfIf1 = if (true) "OK" else sys.error("failure")
    val resultOfIf1_2 = Chapter5.if2 (true, "OK", sys.error("failure"))
    assert(resultOfIf1 == "OK")
    assert(resultOfIf1_2 == "OK")

    the[RuntimeException] thrownBy {
      if (false) "OK" else sys.error("failure")
    } should have message ("failure")

    the[RuntimeException] thrownBy {
      Chapter5.if2 (false, "OK" , sys.error("failure"))
    } should have message ("failure")
  }

}
