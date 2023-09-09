package funscala.ch5

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
}
/** Book's example */
object Stream {
  def empty[A]: Stream[A] = new Stream[A] {
    override def uncons = None
  }
  def cons[A](hd: ⇒ A, tl: ⇒ Stream[A]): Stream[A] = new Stream[A] {
    def uncons = Some((hd, tl))
  }
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty[A]
    else cons(as.head, apply(as.tail: _*))
}

