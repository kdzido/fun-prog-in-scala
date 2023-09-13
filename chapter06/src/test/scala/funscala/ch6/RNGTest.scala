package funscala.ch6

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RNGTest extends AnyFlatSpec with Matchers {
  val rng1 = RNG.simple(1)

  it should "create int of Rand" in {
    RNG.int(rng1)._1 shouldBe 384748
  }

  it should "create unit of Rand that does not alter state" in {
    // given:
    val InitialRandomInt = 384748
    RNG.int(rng1)._1 shouldBe InitialRandomInt

    // when:
    val (i1, rng2) = RNG.unit(1)(rng1)
    val (i2, rng3) = RNG.unit(1)(rng2)
    // then:
    i1 shouldBe 1
    i2 shouldBe 1

    // expect: same value generated as initially, state not altered
    RNG.int(rng3)._1 shouldBe InitialRandomInt
  }

  it should "map Rand into another Rand" in {
    RNG.int(rng1)._1 shouldBe 384748

    val incr1 = RNG.map(RNG.int)(_ + 1)
    incr1(rng1)._1 shouldBe 384749
  }

  it should "map2 two Rands in another Rand" in {
    val (r1, rng2) = RNG.map2(RNG.positiveInt, RNG.unit(0))(_ + _)(rng1)
    val (r2, rng3) = RNG.map2(RNG.positiveInt, RNG.unit(0))(_ + _)(rng2)
    r1 shouldBe 384748
    r2 shouldBe 1151252339

    RNG.map2(RNG.positiveInt, RNG.positiveInt)(_ + _)(rng1)._1 shouldBe (r1 + r2)
  }

  it should "sequence Rands" in {
    RNG.sequence(List())(rng1)._1 shouldBe List()
    RNG.sequence(List(RNG.positiveInt))(rng1)._1 shouldBe List(384748)

    val seq3 = RNG.sequence(List(RNG.positiveInt, RNG.positiveInt, RNG.positiveInt))(rng1)
    seq3._1 shouldBe List(384748,1151252339,549383847)
    RNG.positiveInt(seq3._2)._1 shouldBe 1612966641 // valid next random int
  }

  it should "generate 2 random ints from seed" in {
    val (i1, rng2) = rng1.nextInt
    val (i2, _) = rng2.nextInt
    i1 shouldBe 384748
    i2 shouldBe -1151252339
  }

  // [CHAP-6][EXERCISE-01] implement random positiveInt
  it should "generate 2 random positive Ints" in {
    val (_, rng2) = rng1.nextInt

    RNG.positiveInt(rng1)._1 shouldBe 384748
    RNG.positiveInt(rng2)._1 shouldBe 1151252339
  }

  it should "generate positive int between 0 and n inclusive" in {
    val (i1, rng2) = RNG.positiveMax(1)(rng1)
    val (i2, rng3) = RNG.positiveMax(1)(rng2)
    val (i3, rng4) = RNG.positiveMax(1)(rng3)
    val (i4, rng5) = RNG.positiveMax(1)(rng4)
    val (i5, rng6) = RNG.positiveMax(1)(rng5)
    val (i6, rng7) = RNG.positiveMax(1)(rng6)
    i1 shouldBe 0
    i2 shouldBe 1
    i3 shouldBe 0
    i4 shouldBe 1
    i5 shouldBe 0
    i6 shouldBe 1

    val (j1, jrng2) = RNG.positiveMax(2)(rng1)
    val (j2, jrng3) = RNG.positiveMax(2)(jrng2)
    val (j3, jrng4) = RNG.positiveMax(2)(jrng3)
    val (j4, jrng5) = RNG.positiveMax(2)(jrng4)
    val (j5, jrng6) = RNG.positiveMax(2)(jrng5)
    val (j6, jrng7) = RNG.positiveMax(2)(jrng6)
    j1 shouldBe 0
    j2 shouldBe 1
    j3 shouldBe 0
    j4 shouldBe 2
    j5 shouldBe 1
    j6 shouldBe 2
  }

  it should "normalize Int.MaxValue to Double <0.0, 1.0>" in {
    val normalizedRatio: Double = 1.0 / Int.MaxValue.toDouble

    (Int.MaxValue.toDouble * normalizedRatio) shouldBe 1.0
    (0 * normalizedRatio) shouldBe 0.0
  }

  // [CHAP-6][EXERCISE-02] implement random double
  it should "generate 2 random Doubles in range <0,1>" in {
    val (_, rng2) = rng1.nextInt

    RNG.double(rng1)._1 shouldBe 0.000179162249052507
    RNG.double(rng2)._1 shouldBe 0.5360936464444239

    RNG.double_2(rng1)._1 shouldBe 0.000179162249052507
    RNG.double_2(rng2)._1 shouldBe 0.5360936464444239
  }

  it should "generate random pair (Int,Double)" in {
    RNG.intDouble(rng1)._1 shouldBe (384748, 0.5360936464444239)
  }

  it should "generate random pair (Double, Int)" in {
    RNG.doubleInt(rng1)._1 shouldBe (0.000179162249052507, 1151252339)
  }

  it should "generate random triple (Double, Double, Double)" in {
    RNG.double3(rng1)._1 shouldBe (0.000179162249052507, 0.5360936464444239, 0.2558267895392267)
  }

  it should "generate random List[Int]" in {
    RNG.ints(0)(rng1)._1 shouldBe List()
    RNG.ints(1)(rng1)._1 shouldBe List(384748)
    RNG.ints(5)(rng1)._1 shouldBe List(384748,1151252339,549383847,1612966641,883454042)
  }

}
