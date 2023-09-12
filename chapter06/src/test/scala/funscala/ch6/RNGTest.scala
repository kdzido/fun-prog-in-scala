package funscala.ch6

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
class RNGTest extends AnyFlatSpec with Matchers {

  it should "generate 2 random ints from seed" in {
    val rng1 = RNG.simple(1)

    val (i1, rng2) = rng1.nextInt
    val (i2, _) = rng2.nextInt
    i1 shouldBe 384748
    i2 shouldBe -1151252339
  }

  // [CHAP-6][EXERCISE-01] implement random positiveInt
  it should "generate 2 random positive Ints" in {
    val rng1 = RNG.simple(1)
    val (_, rng2) = rng1.nextInt

    RNG.positiveInt(rng1) shouldBe 384748
    RNG.positiveInt(rng2) shouldBe 1151252339
  }

}
