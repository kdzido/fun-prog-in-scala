package funscala.ch5

import scala.annotation.tailrec

/** Book's example */
sealed trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty
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

