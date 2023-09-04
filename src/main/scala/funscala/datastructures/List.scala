package funscala.datastructures

import scala.annotation.tailrec

/** Book's example */
sealed trait List[+A]
/** Data constructor of List */
case object Nil extends List[Nothing]
/** Data constructor of List */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  /** Book's example */
  def sum(ints: List[Int]): Int = ints match {
    case Nil ⇒ 0
    case Cons(x, xs) ⇒ x + sum(xs)
  }

  /** Book's example */
  def product(ds: List[Double]): Double = ds match {
    case Nil ⇒ 1.0
//    case Cons(0.0, _) ⇒ 0.0   // 1) 0.0 cannot short-circuit as it break abstract types of function 2) checking eq of elems requires another function
    case Cons(x, xs) ⇒ x * product(xs)
  }

  /** Book's example. Generalization of sum and product functions.
   * NOTE: non tail-recursive
   */
  def foldRight[A,B](as: List[A], z: B)(f: (A,B) ⇒ B): B = as match {
    case Nil ⇒ z
    case Cons(x, xs) ⇒ f(x, foldRight(xs, z)(f))

    /* [CHAP-3][EXERCISE-07] Question:
     * Can product using foldRight immediately halt the recursion and
     * return 0.0 if it encounters 0.0 ? */
    /* Answer:
     * 1) Existing abstract foldRight function signature does not allow to implement
     * short-circuiting for a special value.
     * What is missing in comparison function, and special value to be returned.
     * 2) Currently foldRight evaluates all elements of List eagerly.
     */
  }

  /** [CHAP-3][EXERCISE-10] implement tail-recursive foldLeft */
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) ⇒ B): B = as match {
    case Nil ⇒ z
    case Cons(h, t) ⇒ foldLeft(t, f(z, h))(f)
  }

  def reverse[A](as: List[A]): List[A] = {
    List.foldLeft(as, Nil: List[A])((t, h) ⇒ Cons(h, t))
  }

  /** [CHAP-3][EXERCISE-13] implement foldRight in terms of foldLeft (hard).
   * NOTE: O(2n), using tail-recursive functions only */
  def foldRight2[A,B](as: List[A], z: B)(f: (A,B) ⇒ B): B = {
    foldLeft(reverse(as), z)((b, a) ⇒ f(a,b))
  }

/** [CHAP-3][EXERCISE-16] transform list of ints by adding 1 to each element.
 * [CHAP-3][EXERCISE-17] transform list of double to strings.
 * [CHAP-3][EXERCISE-18] generalize to function map */
  def map[A,B](as: List[A])(f: A ⇒ B): List[B] =
    reverse(foldLeft(as, Nil:List[B])((b,a) ⇒ Cons(f(a), b)))


  /** [CHAP-3][EXERCISE-19] implement filter function on List
   * [CHAP-3][EXERCISE-21] implement filter in terms of flatMap on List */
  def filter[A](as: List[A])(f: A ⇒ Boolean): List[A] =
//    reverse(foldLeft(as, Nil:List[A])((acc, a) ⇒ if (f(a)) Cons(a, acc) else acc))
    flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)

  /** Book's example.*/
  def sum2(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)

  /** Book's example.*/
  def product2(ds: List[Double]): Double = foldRight(ds, 1.0)(_ * _)

  /** [CHAP-3][EXERCISE-09] compute list's length using foldRight */
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, b) ⇒ b+1)

  /** [CHAP-3][EXERCISE-02] impl tail of List */
  def tail[A](as: List[A]): List[A] = as match {
    case Nil ⇒ Nil
    case Cons(h, t) ⇒ t
  }

  /** [CHAP-3][EXERCISE-03] impl drop n elements of List */
  @tailrec
  def drop[A](as: List[A], n: Int): List[A] = as match {
    case Nil ⇒ Nil
    case Cons(h, t) ⇒ if (n <= 0) as else drop(t, n-1)
  }

  /** [CHAP-3][EXERCISE-04] impl dropWhile on List */
  @tailrec
  def dropWhile[A](as: List[A])(f: A ⇒ Boolean): List[A] = as match {
    case Nil ⇒ Nil
    case Cons(h, t) ⇒ if (f(h)) dropWhile(t)(f) else as
  }

  /** [CHAP-3][EXERCISE-05] impl setHead on List */
  def setHead[A](as: List[A], newHead: A): List[A] = as match {
    case Nil ⇒ Nil
    case Cons(h, t) ⇒ Cons(newHead, t)
  }


  /** Book's example.
   * NOTE: non tail-recursive
   */
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil ⇒ a2
    case Cons(h1, t1) ⇒ Cons(h1, append(t1, a2))
  }


/** [CHAP-3][EXERCISE-14] impl append in terms of foldRight or foldLeft */
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight2(a1, a2)((e, acc) ⇒ Cons(e, acc))

  /** [CHAP-3][EXERCISE-15] impl concatenation of list of lists in terms of existing functions (hard).
   * Complexity should be linear. */
  def flatConcat[A](l: List[List[A]]): List[A] =
    foldRight2(l, Nil:List[A])((elem, acc) ⇒ append2(elem, acc))


  /** [CHAP-3][EXERCISE-20] impl flatMap on List */
  def flatMap[A, B](as: List[A])(f: A ⇒ List[B]): List[B] =
    foldRight2(as, Nil:List[B])((elem, acc) ⇒ append2(f(elem), acc))

  /** [CHAP-3][EXERCISE-22] sum elements of two lists */
  def zipAndSumElems(l1: List[Int], l2: List[Int]): List[Int] = {
    @tailrec
    def goSum(m1: List[Int], m2: List[Int], acc: List[Int]): List[Int] = (m1, m2) match {
      case (Nil, Nil) ⇒ acc
      case (Cons(h1,t1), Cons(h2,t2)) ⇒ goSum(t1, t2, Cons(h1+h2, acc))
    }
    reverse(goSum(l1, l2, Nil))
  }

  /** [CHAP-3][EXERCISE-23] zip lists */
  def zipCustom[A](l1: List[A], l2: List[A])(f: (A,A) ⇒ A): List[A] = {
    @tailrec
    def go(m1: List[A], m2: List[A], acc: List[A]): List[A] = (m1, m2) match {
      case (Nil, Nil) ⇒ acc
      case (Cons(h1, t1), Cons(h2, t2)) ⇒ go(t1, t2, Cons(f(h1, h2), acc))
    }
    reverse(go(l1, l2, Nil))
  }

  /** [CHAP-3][EXERCISE-24] implement hasSubsequence on List (hard) */
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    @tailrec
    def goMatchingAgainstBeginningOfList(xs: List[A], pat: List[A], matchingAcc: Boolean): Boolean = (xs, pat) match {
      case (Nil, Nil) ⇒ matchingAcc
      case (Cons(_,_), Nil) ⇒ matchingAcc
      case (Nil, Cons(_,_)) ⇒ false
      case (Cons(xsh, xst), Cons(ph, pt)) ⇒ goMatchingAgainstBeginningOfList(xst, pt, matchingAcc && (xsh == ph))
    }

    @tailrec
    def loop(ls: List[A], foundAcc: Boolean): Boolean = ls match {
      case Nil ⇒ foundAcc
      case Cons(h, t) ⇒ loop(t, foundAcc || goMatchingAgainstBeginningOfList(ls, sub, true))
    }

    loop(l, false)
  }

  /** [CHAP-3][EXERCISE-06] impl init on List.
   * NOTE: non tail-recursive
   */
  def init[A](as: List[A]): List[A] = as match {
    case Nil ⇒ Nil
    case Cons(h, Nil) ⇒ Nil
    case Cons(h, t) ⇒ Cons(h, init(t))
  }


  /** Book's example */
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  /** [CHAP-3][EXERCISE-01] pattern matching result */
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) ⇒ x
    case Nil ⇒ 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) ⇒ x + y
    case Cons(h, t) ⇒ h + sum(t)
    case _ ⇒ 101
  }

}
