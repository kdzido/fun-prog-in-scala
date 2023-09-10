package funscala.ch5

import funscala.ch5.Stream.{cons, empty}

import scala.annotation.tailrec

/** Book's example */
sealed trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]

  def isEmpty: Boolean = uncons.isEmpty

  /** [CHAP-5][EXERCISE-01] implement toList on Stream */
  def toList: List[A] = {
    @tailrec def go(s: Stream[A], acc: List[A]): List[A] = s.uncons match {
      case None ⇒ acc
      case Some((sh, st)) ⇒ go(st, sh :: acc)
    }

    go(this, List[A]()).reverse
  }

  /** [CHAP-5][EXERCISE-02] implement take on Stream */
  def take(n: Int): Stream[A] = {
    def go(nn: Int, left: ⇒ Stream[A]): Stream[A] =
      if (nn <= 0) empty[A]
      else {
        left.uncons match {
          case None ⇒ empty[A]
          case Some((h, ts)) ⇒ Stream.cons(h, go(nn-1, ts))
        }
    }
    go(n, this)
  }

  /** [CHAP-5][EXERCISE-03] implement takeWhile on Stream */
  def takeWhile(p: A ⇒ Boolean): Stream[A] = {
    def go(left: ⇒ Stream[A]): Stream[A] =
      left.uncons match {
        case None ⇒ empty[A]
        case Some((h, ts)) ⇒ if (p(h) == false) empty[A] else Stream.cons(h, go(ts))
      }
    go(this)
  }

  /** [CHAP-5][EXERCISE-05] implement takeWhile in terms of foldRight */
  def takeWhile_2(p: A ⇒ Boolean): Stream[A] = foldRight(empty[A])((a,b) ⇒ {
      if (p(a)) cons(a, b)
      else empty[A]
    })

  /** Book's example */
  def foldRight[B](z: ⇒ B)(f: (A, ⇒ B) ⇒ B): B = uncons match {
    case Some((h, t)) ⇒ f(h, t.foldRight(z)(f))
    case None ⇒ z
  }

  /** Book's example */
  def exists(p: A ⇒ Boolean): Boolean = foldRight(false)((a,b) ⇒ p(a) || b)

  /** [CHAP-5][EXERCISE-04] implement forAll on Stream */
  def forAll(p: A ⇒ Boolean): Boolean = foldRight(true)((a,b) ⇒ p(a) && b)

  /** [CHAP-5][EXERCISE-06] implement map, filter, append and flatMap in terms of foldRight */
  def map[B](f: A ⇒ B): Stream[B] = foldRight(empty[B]) {
    (a,b) ⇒ cons(f(a), b)
  }

  /** [CHAP-5][EXERCISE-06] implement map, filter, append and flatMap in terms of foldRight */
  def filter(f: A ⇒ Boolean): Stream[A] = foldRight(empty[A]) {
      (a,b) ⇒ if (f(a)) cons(a, b) else b
    }

  /** [CHAP-5][EXERCISE-06] implement map, filter, append and flatMap in terms of foldRight */
  def flatMap[B](f: A ⇒ Stream[B]): Stream[B] = foldRight(empty[B])((a,b) ⇒ Stream.append(f(a), b))

}

/** Book's example */
object Stream {
  def empty[A]: Stream[A] = new Stream[A] {
    override def uncons = None
  }
  def cons[A](hd: ⇒ A, tl: ⇒ Stream[A]): Stream[A] = new Stream[A] {
    lazy val uncons = Some((hd, tl))
  }
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty[A]
    else cons(as.head, apply(as.tail: _*))

  /** [CHAP-5][EXERCISE-06] implement map, filter, append and flatMap in terms of foldRight */
  def append[A](a1: Stream[A], a2: Stream[A]): Stream[A] = a2.uncons match {
    case None ⇒ a1
    case Some((h2, t2)) ⇒ a1.foldRight(a2)((a,b) ⇒ cons(a,b))
  }


  /** Book's example */
  val ones: Stream[Int] = Stream.cons(1,  ones)


  /** [CHAP-5][EXERCISE-07] implement infinite Stream generator of constant */
  def constant(n: Int): Stream[Int] = Stream.cons(n,  constant(n))
}

