package funscala.ch4

import scala.annotation.tailrec

/** Book's example */
sealed trait Option[+A] {
  /** [CHAP-4][EXERCISE-01] implement Option trait */
  def map[B](f: A ⇒ B): Option[B] = this match {
    case None ⇒ None
    case Some(a) ⇒ Some(f(a))
  }

  /** [CHAP-4][EXERCISE-01] implement Option trait */
  def flatMap[B](f: A ⇒ Option[B]): Option[B] = this match {
    case None ⇒ None
    case Some(a) ⇒ f(a)
  }

  /** [CHAP-4][EXERCISE-01] implement Option trait */
  def getOrElse[B >: A](default: ⇒ B): B = this match {
    case None ⇒ default
    case Some(a) ⇒ a
  }

  /** [CHAP-4][EXERCISE-01] implement Option trait */
  def orElse[B >: A](ob: ⇒ Option[B]): Option[B] = this match {
    case None ⇒ ob
    case s: Some[A] ⇒ s
  }

  /** [CHAP-4][EXERCISE-01] implement Option trait */
  def filter(f: A ⇒ Boolean): Option[A] = this match {
    case None ⇒ None
    case Some(a) ⇒ if (f(a)) Some(a) else None
  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  import java.util.regex._

  /** Book's example */
  def lift[A,B](f: A ⇒ B): Option[A] ⇒ Option[B] = _ map f

  /** [CHAP-4][EXERCISE-03] implement map2 on Option */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) ⇒ C): Option[C] =
    for {
      av <- a
      bv <- b
    } yield f(av,bv)

  /** Book's example */
  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException ⇒ None
    }

  /** Book's example */
  def mkMatcher(pat: String): Option[String ⇒ Boolean] =
    pattern(pat) map(p ⇒ (s: String) ⇒ p.matcher(s).matches())

  /** Book's example */
  def mkMatcher_1(pat: String): Option[String ⇒ Boolean] =
    for {
      p <- pattern(pat)
    } yield ((s: String) ⇒ p.matcher(s).matches())

  /** Book's example */
  def doesMatch(pat: String, s: String): Option[Boolean] =
    for {
      p <- mkMatcher_1(pat)
    } yield p(s)

  /** Book's example */
  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
    for {
      f <- mkMatcher(pat)
      g <- mkMatcher(pat2)
    } yield f(s) && g(s)

  /** Book's example */
  def bothMatch_1(pat: String, pat2: String, s: String): Option[Boolean] =
    mkMatcher(pat).flatMap(f ⇒
    mkMatcher(pat2).map(g ⇒
      f(s) && g(s)))

  /** [CHAP-4][EXERCISE-04] re-implement bothMatch in terms of map2 */
  def bothMatch_2(pat: String, pat2: String, s: String): Option[Boolean] = Option.map2(mkMatcher(pat), mkMatcher(pat2))(
    (a, b) ⇒ a(s) && b(s))

  /** [CHAP-4][EXERCISE-05] implement sequence */
  def sequence[A](as: List[Option[A]]): Option[List[A]] = {
    @tailrec
    def go(l: List[Option[A]], acc: Option[List[A]]): Option[List[A]] = l match {
      case Nil ⇒ acc
      case None :: _ ⇒ None
      case Some(h) :: t ⇒ go(t, acc.flatMap(al ⇒ Some(h :: al)))
    }
    go(as, Some(List())).flatMap(al ⇒ Some(al.reverse))
  }

  def sequence_1[A](as: List[Option[A]]): Option[List[A]] = traverse(as)(ao ⇒  ao)

  /** [CHAP-4][EXERCISE-06] implement traverse */
  def traverse[A,B](as: List[A])(f: A ⇒ Option[B]): Option[List[B]] = {
    @tailrec
    def go(l: List[A], acc: Option[List[B]]): Option[List[B]] = l match {
      case Nil ⇒ acc
      case h :: t ⇒ go(t, for {al <- acc; fl <- f(h)} yield fl :: al)
    }
    go(as, Some(List())).flatMap(al ⇒ Some(al.reverse))
  }

}
