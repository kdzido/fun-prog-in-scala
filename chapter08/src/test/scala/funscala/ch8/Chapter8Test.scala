package funscala.ch8

import com.google.common.util.concurrent.MoreExecutors
import funscala.ch6.RNG
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

  /** [CHAP-8][EXERCISE-4] implement Gen.choose */
  it should "choose between 1 and 1" in {
    val rng1 = RNG.simple(3)

    val p1 = Gen.choose(1, 1).run(rng1)
    val p2 = Gen.choose(1, 1).run(p1._2)
    val p3 = Gen.choose(1, 1).run(p2._2)
    val p4 = Gen.choose(1, 1).run(p3._2)

    p1._1 shouldBe 1
    p2._1 shouldBe 1
    p3._1 shouldBe 1
    p4._1 shouldBe 1
  }

  /** [CHAP-8][EXERCISE-4] implement Gen.choose */
  it should "choose between 1 and 2" in {
    val rng1 = RNG.simple(3)

    val p1 = Gen.choose(1, 2).run(rng1)
    val p2 = Gen.choose(1, 2).run(p1._2)
    val p3 = Gen.choose(1, 2).run(p2._2)
    val p4 = Gen.choose(1, 2).run(p3._2)

    p1._1 shouldBe 1
    p2._1 shouldBe 1
    p3._1 shouldBe 1
    p4._1 shouldBe 1
  }

  /** [CHAP-8][EXERCISE-4] implement Gen.choose */
  it should "choose between min and maxExcl" in {
    val rng1 = RNG.simple(3)

    val p1 = Gen.choose(1, 4).run(rng1)
    val p2 = Gen.choose(1, 4).run(p1._2)
    val p3 = Gen.choose(1, 4).run(p2._2)
    val p4 = Gen.choose(1, 4).run(p3._2)
    val p5 = Gen.choose(1, 4).run(p4._2)
    val p6 = Gen.choose(1, 4).run(p5._2)

    p1._1 shouldBe 1
    p2._1 shouldBe 2
    p3._1 shouldBe 3
    p4._1 shouldBe 2
    p5._1 shouldBe 3
    p6._1 shouldBe 1
  }
}
