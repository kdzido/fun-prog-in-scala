package funscala.datastructures

import org.scalatest.flatspec.AnyFlatSpec

class TreeTest extends AnyFlatSpec {

  // [CHAP-3][EXERCISE-25] implement size on Tree
  // [CHAP-3][EXERCISE-29] generalize size, maximum, depth, map
  "Non-empty Tree of Ints" should "have size" in {
    val t1 = Leaf(1)
    val t3 = Branch(Leaf(1), Leaf(2))
    val t5 = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    assert(Tree.size(t1) == 1)
    assert(Tree.size(t3) == 3)
    assert(Tree.size(t5) == 5)

    assert(Tree.size2(t1) == 1)
    assert(Tree.size2(t3) == 3)
    assert(Tree.size2(t5) == 5)
  }

  // [CHAP-3][EXERCISE-26] implement maximum on Tree
  // [CHAP-3][EXERCISE-29] generalize size, maximum, depth, map
  it should "have maximum element" in {
    val t1 = Leaf(1)
    val t3a = Branch(Leaf(1), Leaf(2))
    val t3b = Branch(Leaf(1), Leaf(2))

    val t5a = Branch(Leaf(1), Branch(Leaf(5), Leaf(3)))
    val t5b = Branch(Leaf(1), Branch(Leaf(3), Leaf(5)))
    val t5c = Branch(Leaf(5), Branch(Leaf(1), Leaf(3)))

    assert(Tree.maximum(t1) == 1)
    assert(Tree.maximum(t3a) == 2)
    assert(Tree.maximum(t3b) == 2)
    assert(Tree.maximum(t5a) == 5)
    assert(Tree.maximum(t5b) == 5)
    assert(Tree.maximum(t5c) == 5)

    assert(Tree.maximum2(t1) == 1)
    assert(Tree.maximum2(t3a) == 2)
    assert(Tree.maximum2(t3b) == 2)
    assert(Tree.maximum2(t5a) == 5)
    assert(Tree.maximum2(t5b) == 5)
    assert(Tree.maximum2(t5c) == 5)
  }

  // [CHAP-3][EXERCISE-27] implement depeth on Tree
  // [CHAP-3][EXERCISE-29] generalize size, maximum, depth, map
  it should "find max depth" in {
    val t1 = Leaf(1)
    val t2a = Branch(Leaf(1), Leaf(2))
    val t2b = Branch(Leaf(1), Leaf(2))
    val t3a = Branch(Leaf(1), Branch(Leaf(5), Leaf(3)))
    val t3b = Branch(Branch(Leaf(3), Leaf(5)), Leaf(1))
    val t4a = Branch(Branch(Leaf(3), Branch(Leaf(4), Leaf(5))), Leaf(1))

    assert(Tree.depth(t1) == 1)
    assert(Tree.depth(t2a) == 2)
    assert(Tree.depth(t2b) == 2)
    assert(Tree.depth(t3a) == 3)
    assert(Tree.depth(t3b) == 3)
    assert(Tree.depth(t4a) == 4)

    assert(Tree.depth2(t1) == 1)
    assert(Tree.depth2(t2a) == 2)
    assert(Tree.depth2(t2b) == 2)
    assert(Tree.depth2(t3a) == 3)
    assert(Tree.depth2(t3b) == 3)
    assert(Tree.depth2(t4a) == 4)
  }

  // [CHAP-3][EXERCISE-28] implement map on Tree
  // [CHAP-3][EXERCISE-29] generalize size, maximum, depth, map
  it should "be mapped by incrementing each leaf" in {
    val t1 = Leaf(1)
    val t2a = Branch(Leaf(1), Leaf(2))
    val t2b = Branch(Leaf(1), Leaf(2))
    val t3a = Branch(Leaf(1), Branch(Leaf(5), Leaf(3)))

    val incrByOne: Int ⇒ Int = _ + 1

    assert(Tree.map(t1)(incrByOne) == Leaf(2))
    assert(Tree.map(t2a)(incrByOne) == Branch(Leaf(2), Leaf(3)))
    assert(Tree.map(t2b)(incrByOne) == Branch(Leaf(2), Leaf(3)))
    assert(Tree.map(t3a)(incrByOne) == Branch(Leaf(2), Branch(Leaf(6), Leaf(4))))

    assert(Tree.map2(t1)(incrByOne) == Leaf(2))
    assert(Tree.map2(t2a)(incrByOne) == Branch(Leaf(2), Leaf(3)))
    assert(Tree.map2(t2b)(incrByOne) == Branch(Leaf(2), Leaf(3)))
    assert(Tree.map2(t3a)(incrByOne) == Branch(Leaf(2), Branch(Leaf(6), Leaf(4))))
  }

}
