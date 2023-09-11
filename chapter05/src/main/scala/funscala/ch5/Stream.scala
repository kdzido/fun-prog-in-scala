package funscala.ch5

import funscala.ch5.Stream.{cons, empty, unfold}

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

  /** [CHAP-5][EXERCISE-12] implement map, take, takeWhile, zip, zipAll in terms of unfold */
  def take_2(n: Int): Stream[A] = unfold((n, this))(s ⇒ {
    val (leftN, stream) = s
    if (leftN <= 0) None
    else {
      stream.uncons match {
        case None ⇒ None
        case Some(aa, ss) ⇒ Some((aa, (leftN-1, ss)))
      }
    }
  })

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

  /** [CHAP-5][EXERCISE-12] implement map, take, takeWhile, zip, zipAll in terms of unfold */
  def takeWhile_3(p: A ⇒ Boolean): Stream[A] = unfold(this)(s ⇒ {
    s.uncons match {
      case None ⇒ None
      case Some(aa, ss) ⇒ if (p(aa)) Some((aa, ss)) else None
    }
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

  /** [CHAP-5][EXERCISE-12] implement map, take, takeWhile, zip, zipAll in terms of unfold */
  def map_2[B](f: A ⇒ B): Stream[B] = unfold(this)(s ⇒ {
    s.uncons match {
      case None ⇒ None
      case Some(aa,ss) ⇒ Some((f(aa), ss))
    }
  })

  /** [CHAP-5][EXERCISE-06] implement map, filter, append and flatMap in terms of foldRight */
  def filter(f: A ⇒ Boolean): Stream[A] = foldRight(empty[A]) {
      (a,b) ⇒ if (f(a)) cons(a, b) else b
    }

  /** [CHAP-5][EXERCISE-06] implement map, filter, append and flatMap in terms of foldRight */
  def flatMap[B](f: A ⇒ Stream[B]): Stream[B] = foldRight(empty[B])((a,b) ⇒ Stream.append(f(a), b))

  /** [CHAP-5][EXERCISE-14] implement tails unsing unfold on Streams */
  def tails: Stream[Stream[A]] = unfold((this, true))(state ⇒ {
    val (ss, hasMore) = state
    (ss.uncons, hasMore) match {
      case (None, false) ⇒ None
      case (None, true) ⇒ Some((empty, (empty, false)))
      case (Some(a, t), true) ⇒ Some(cons(a, t), (t, true))
    }
  })
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

  /** [CHAP-5][EXERCISE-11] implement fibs, from, constant, ones in terms of unfold */
  val ones_2: Stream[Int] = unfold(1)(s ⇒ Some((s,s)))

  /** [CHAP-5][EXERCISE-07] implement infinite Stream generator of constant */
  def constant(n: Int): Stream[Int] = Stream.cons(n,  constant(n))

  /** [CHAP-5][EXERCISE-11] implement fibs, from, constant, ones in terms of unfold */
  def constant_2(n: Int): Stream[Int] = unfold(n)(s ⇒ Some((s,s)))

  /** [CHAP-5][EXERCISE-08] implement infinite incremental Stream generator starting from given n */
  def from(n: Int): Stream[Int] = Stream.cons(n,  from(n+1))

  /** [CHAP-5][EXERCISE-11] implement fibs, from, constant, ones in terms of unfold */
  def from_2(n: Int): Stream[Int] = unfold(n)(s ⇒ Some((s, s+1)))


  /** [CHAP-5][EXERCISE-9] implement infinite Stream of Fibonacci numbers */
  def fibs: Stream[Int] = {
    def go(acc1: Int, acc2: Int): Stream[Int] = {
      cons(acc1, go(acc2, acc1 + acc2))
    }
    go(0, 1)
  }

  /** [CHAP-5][EXERCISE-11] implement fibs, from, constant, ones in terms of unfold */
  def fibs_2: Stream[Int] = unfold((0, 1))(s ⇒ Some((s._1, (s._2, s._1 + s._2))))

  /** [CHAP-5][EXERCISE-10] implement unfold on Stream */
  def unfold[A,S](s: S)(f: S ⇒ Option[(A,S)]): Stream[A] = f(s) match {
    case None ⇒ empty[A]
    case Some((a,s)) ⇒ cons(a, unfold(s)(f))
  }

  /** [CHAP-5][EXERCISE-12] implement map, take, takeWhile, zip, zipAll in terms of unfold */
  def zip[A](s1: Stream[A], s2: Stream[A]): Stream[(A, A)] = unfold((s1.uncons,s2.uncons))(state ⇒ {
    state match {
      case (None, None) ⇒ None
      case (Some(_, _), None) ⇒ None
      case (None, Some(_, _)) ⇒ None
      case (Some(sa1, ss1), Some(sa2, ss2)) ⇒ Some((sa1, sa2), (ss1.uncons, ss2.uncons))
    }
  })

  /** [CHAP-5][EXERCISE-12] implement map, take, takeWhile, zip, zipAll in terms of unfold */
  def zipAll[A](s1: Stream[A], s2: Stream[A]): Stream[(Option[A], Option[A])] = unfold((s1.uncons,s2.uncons))(state ⇒ {
    state match {
      case (None, None) ⇒ None
      case (Some(sa1, ss1), None) ⇒ Some((Some(sa1), None), (ss1.uncons, None))
      case (None, Some(sa2, ss2)) ⇒ Some((None, Some(sa2)), (None, ss2.uncons))
      case (Some(sa1, ss1), Some(sa2, ss2)) ⇒ Some((Some(sa1), Some(sa2)), (ss1.uncons, ss2.uncons))
    }
  })

  /** [CHAP-5][EXERCISE-13] implement startsWith on Streams */
  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean = (s.uncons, s2.uncons) match {
    case (None, None) ⇒ true
    case (None, Some(_, _)) ⇒ false
    case (Some(_, _), None) ⇒ true
    case (Some(sh, st), Some(sh2, st2)) ⇒ if (sh != sh2) false else startsWith(st, st2)
  }

  /** Book's example */
  def hasSubsequence[A](s: Stream[A], s2: Stream[A]): Boolean =
    s.tails.exists(startsWith(_, s2))

}

