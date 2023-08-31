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
    case Cons(0.0, _) ⇒ 0.0
    case Cons(x, xs) ⇒ x * product(xs)
  }

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
