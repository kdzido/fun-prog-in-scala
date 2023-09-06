package funscala.ch4

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
}
