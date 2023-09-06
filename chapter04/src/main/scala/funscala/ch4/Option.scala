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

}
