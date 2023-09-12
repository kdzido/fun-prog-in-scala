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

  it should "normalize Int.MaxValue to Double <0.0, 1.0>" in {
    val normalizedRatio: Double = 1.0 / Int.MaxValue.toDouble

    (Int.MaxValue.toDouble * normalizedRatio) shouldBe 1.0
    (0 * normalizedRatio) shouldBe 0.0
  }

  // [CHAP-6][EXERCISE-02] implement random double
  it should "generate 2 random Doubles in range <0,1>" in {
    val rng1 = RNG.simple(1)
    val (_, rng2) = rng1.nextInt

    RNG.double(rng1) shouldBe 0.000179162249052507
    RNG.double(rng2) shouldBe 0.5360936464444239
  }

}
