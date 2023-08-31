package funscala.ch3

import org.scalatest.flatspec.AnyFlatSpec

class Chapter3Test extends AnyFlatSpec {

  "An empty list" should "have size 0" in {
    assert(List.empty.size == 0)
  }

  it should "be immutable" in {
    assert(List(1).isInstanceOf[scala.collection.immutable.List[Int]])
  }

  "An empty set" should "have size 0" in {
    assert(Set.empty.size == 0)
  }

  it should "be immutable" in {
    assert(Set(1).isInstanceOf[scala.collection.immutable.Set[Int]])
  }

}
