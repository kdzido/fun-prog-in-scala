package funscala.ch5

import funscala.ch5.Stream.empty

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
}

