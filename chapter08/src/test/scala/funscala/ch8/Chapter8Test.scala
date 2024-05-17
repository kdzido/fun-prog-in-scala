package funscala.ch8

import com.google.common.util.concurrent.MoreExecutors
import funscala.ch8.{AlwaysFalse, AlwaysTrue}
import org.scalacheck.Prop.forAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Chapter8Test extends AnyFlatSpec with Matchers {

  /** [CHAP-8][EXERCISE-3] implement && as method of Prop */
  it should "join two Props" in {
    AlwaysTrue.&&(AlwaysTrue).check shouldBe true
    AlwaysTrue.&&(AlwaysFalse).check shouldBe false
    AlwaysFalse.&&(AlwaysTrue).check shouldBe false
    AlwaysFalse.&&(AlwaysFalse).check shouldBe false
  }

}
