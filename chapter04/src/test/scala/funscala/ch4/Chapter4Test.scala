package funscala.ch4

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Chapter4Test extends AnyFlatSpec with Matchers {

  "Non-pure failing function" should "throw exception" in {
    the [Exception] thrownBy {
      Chapter4.failingFn(1)
    } should have message "Fail!"
  }

  "Partial mean fn of empty List" should "throw exception" in {
    the [ArithmeticException] thrownBy {
      Chapter4.mean(Seq())
    } should have message "mean of empty list!"
  }

  "Partial mean fn of List" should "return mean of all elements" in {
    assert(Chapter4.mean(Seq(1.0)) == 1.0)
    assert(Chapter4.mean(Seq(1.0,2.0)) == 1.5)
  }

  "Total mean_1 fn of empty List" should "return bogus value" in {
    assert(Chapter4.mean_1(IndexedSeq(), -1.0) == -1)
  }

  "Total mean_1 fn of List" should "return mean of all elements" in {
    assert(Chapter4.mean_1(IndexedSeq(1.0), -1.0) == 1.0)
    assert(Chapter4.mean_1(IndexedSeq(1.0, 2.0), -1.0) == 1.5)
  }

  "Total mean_2 fn of empty List" should "return bogus value" in {
    assert(Chapter4.mean_2(IndexedSeq()) == None)
  }

  "Total mean_2 fn of List" should "return mean of all elements" in {
    assert(Chapter4.mean_2(IndexedSeq(1.0)) == Some(1.0))
    assert(Chapter4.mean_2(IndexedSeq(1.0, 2.0)) == Some(1.5))
  }

  "Total mean_3 fn of empty List" should "return bogus value" in {
    assert(Chapter4.mean_3(IndexedSeq()) == Left(""))
  }

  "Total mean_3 fn of List" should "return mean of all elements" in {
    assert(Chapter4.mean_3(IndexedSeq(1.0)) == Right(1.0))
    assert(Chapter4.mean_3(IndexedSeq(1.0, 2.0)) == Right(1.5))
  }

  "Two doubles" should "be safely divided" in {
    assert(Chapter4.safeDiv(3.0, 2.0) == Right(1.5))
    assert(Chapter4.safeDiv(3.0, 0.0) == Right(Double.PositiveInfinity))  // not throwing
  }

  // [CHAP-4][EXERCISE-02] implement variance in terms of mean and flatMap
  "Variance of List" should "return calculated value" in {
    assert(Chapter4.variance(Seq()) == None)
    assert(Chapter4.variance(Seq(1.0)) == Some(0.0))
    assert(Chapter4.variance(Seq(1.0, 1.0)) == Some(0.0))
    assert(Chapter4.variance(Seq(1.0, 3.0)) == Some(1.0))
  }


}
