package funscala.ch8

import com.google.common.util.concurrent.MoreExecutors
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

object Chapter8Test extends Properties("intList") {

  val oneIntList = Gen.listOfN[Int](1, Gen.choose(0, 100))
  val sameElementsIntList = Gen.listOfN[Int](100, Gen.choose(1, 1))
  val intList = Gen.listOf[Int](Gen.choose(0, 100))

  /** [CHAP-8][EXERCISE-01] implement sum property test */
  property("sum") = forAll(oneIntList)(l ⇒ l.sum == l.headOption.get) &&
    forAll(sameElementsIntList)(l ⇒ l.sum == 100) &&
    forAll(intList)(l ⇒ l.sum == l.reverse.sum)

}
