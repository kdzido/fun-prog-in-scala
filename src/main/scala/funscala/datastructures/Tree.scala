package funscala.datastructures

/** Book's example */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /** [CHAP-3][EXERCISE-25] implement size on Tree */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) ⇒ 1
    case Branch(t1, t2) ⇒ 1 + size(t1) + size(t2)
  }

  /** [CHAP-3][EXERCISE-29] generalize size, maximum, depth, map */
  def size2[A](t: Tree[A]): Int = fold(t)(_ ⇒ 1)((ts1, ts2) ⇒ 1 + (ts1 + ts2))

  /** [CHAP-3][EXERCISE-26] implement maximum on Tree.
   * NOTE non-tail recursive, for balanced trees shall not be much of issue due to O(log n) */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) ⇒ x
    case Branch(t1, t2) ⇒ maximum(t1) max maximum(t2)
  }

  /** [CHAP-3][EXERCISE-29] generalize size, maximum, depth, map */
  def maximum2(t: Tree[Int]): Int = fold(t)(_.value)(_ max _)

  /** [CHAP-3][EXERCISE-27] implement depth on Tree.
   * NOTE non-tail recursive, for balanced trees shall not be much of issue due to O(log n) */
  def depth(t: Tree[Int]): Int = t match {
    case Leaf(x) ⇒ 1
    case Branch(t1, t2) ⇒ 1 + (depth(t1) max depth(t2))
  }

  /** [CHAP-3][EXERCISE-29] generalize size, maximum, depth, map */
  def depth2(t: Tree[Int]): Int = fold(t)(_ ⇒ 1)((td1, td2) ⇒ 1 + (td1 max td2))

  /** [CHAP-3][EXERCISE-28] implement map on Tree.
   * NOTE non-tail recursive, for balanced trees shall not be much of issue due to O(log n) */
  def map[A](t: Tree[A])(f: A => A): Tree[A] = t match {
    case Leaf(x) ⇒ Leaf(f(x))
    case Branch(t1, t2) ⇒ Branch(map(t1)(f), map(t2)(f))
  }

  /** [CHAP-3][EXERCISE-29] generalize size, maximum, depth, map */
  def map2[A](t: Tree[A])(f: A => A): Tree[A] = // ???
    fold(t)(l ⇒ Leaf(f(l.value)).asInstanceOf[Tree[A]])((mt1, mt2) ⇒ Branch(mt1, mt2))

  /** [CHAP-3][EXERCISE-29] generalize size, maximum, depth, map
   * NOTE non-tail recursive, for balanced trees shall not be much of issue due to O(log n) */
  def fold[A, B](t: Tree[A])(f: Leaf[A] ⇒ B)(g: (B, B) => B): B = t match {
    case ll: Leaf[A] ⇒ f(ll)
    case Branch(t1, t2) ⇒ g(fold(t1)(f)(g), fold(t2)(f)(g))
  }


}
