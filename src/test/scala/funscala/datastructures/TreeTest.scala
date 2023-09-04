package funscala.datastructures

import org.scalatest.flatspec.AnyFlatSpec

class TreeTest extends AnyFlatSpec {

  /** [CHAP-3][EXERCISE-25] implement size on Tree */
  "Non-empty Tree" should "have size" in {
    val t1 = Leaf(1)
    val t3 = Branch(Leaf(1), Leaf(2))
    val t5 = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    assert(Tree.size(t1) == 1)
    assert(Tree.size(t3) == 3)
    assert(Tree.size(t5) == 5)
  }


}
