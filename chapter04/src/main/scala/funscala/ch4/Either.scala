package funscala.ch4

import scala.annotation.tailrec

/** Book's example */
sealed trait Either[+E, +A] {
  /** [CHAP-4][EXERCISE-07] implement Either trait methods */
  def map[B](f: A ⇒ B): Either[E, B]
  def flatMap[EE >: E, B](f: A ⇒ Either[EE, B]): Either[EE, B]
  def orElse[EE >: E, B >: A](b: ⇒ Either[EE,B]): Either[EE,B]
  def map2[EE >: E, B, C](b: Either[EE,B])(f: (A,B) ⇒ C): Either[EE, C]
  /** [CHAP-4][EXERCISE-09] implement Either map2 so that returns both errors */
  def map2_1[EE >: E, B, C](b: Either[EE,B])(f: (A,B) ⇒ C): Either[List[EE], C]
}

/** [CHAP-4][EXERCISE-07] implement Either trait methods */
case class Left[+E](value: E) extends Either[E, Nothing] {
  override def map[B](f: Nothing ⇒ B): Either[E, B] = this
  override def flatMap[EE >: E, B](f: Nothing ⇒ Either[EE, B]): Either[EE, B] = this.asInstanceOf[Either[EE,B]]
  override def orElse[EE >: E, B >: Nothing](b: ⇒ Either[EE, B]): Either[EE, B] = b

  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) ⇒ C): Either[EE, C] = this

  /** [CHAP-4][EXERCISE-09] implement Either map2 so that returns both errors */
  override def map2_1[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) ⇒ C): Either[List[EE], C] = b match {
    case Left(bb) ⇒ Left(List(value, bb)).asInstanceOf[Either[List[EE], C]]
    case Right(bb) ⇒ Left(List(value)).asInstanceOf[Either[List[EE], C]]
  }
}

/** [CHAP-4][EXERCISE-07] implement Either trait methods */
case class Right[+A](value: A) extends Either[Nothing, A] {
  override def map[B](f: A ⇒ B): Either[Nothing, B] = Right(f(value))
  override def flatMap[EE >: Nothing, B](f: A ⇒ Either[EE, B]): Either[EE, B] = f(value)
  override def orElse[EE >: Nothing, B >: A](b: ⇒ Either[EE, B]): Either[EE, B] = this.asInstanceOf[Either[EE,B]]

  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) ⇒ C): Either[EE, C] = (this, b) match {
    case (_, Left(lb)) ⇒ Left(lb)
    case (Right(ra), Right(rb)) ⇒ Right(f(ra, rb))
  }

  /** [CHAP-4][EXERCISE-09] implement Either map2 so that returns both errors */
  override def map2_1[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) ⇒ C): Either[List[EE], C] = b match {
    case Left(bb) ⇒ Left(List(bb)).asInstanceOf[Either[List[EE], C]]
    case Right(bb) => Right(f(value, bb))
  }
}

object Either {

  /** [CHAP-4][EXERCISE-08] implement sequence and traverse for Either */
  def sequence[E,A](es: List[Either[E, A]]): Either[E,List[A]] = {
    @tailrec def go(es: List[Either[E,A]], acc: Either[E, List[A]]): Either[E, List[A]] = es match {
      case Nil ⇒ acc
      case Left(ee) :: ot ⇒ Left(ee)
      case Right(h) :: ot ⇒ go(ot, acc.map(l ⇒ h :: l))
    }

    @tailrec def reverseLoop(as: List[A], acc: List[A]): List[A] = as match {
      case Nil ⇒ acc
      case h :: t ⇒ reverseLoop(t, h :: acc)
    }

    val transformed = go(es, Right(List()))
    transformed.map(l ⇒ reverseLoop(l, Nil))
  }

  /** [CHAP-4][EXERCISE-08] implement sequence and traverse for Either */
  def traverse[E,A,B](as: List[A])(f: A => Either[E,B]): Either[E, List[B]] = {
    val listOfEithers = as.map(f)

    @tailrec def go(es: List[Either[E, B]], acc: Either[E, List[B]]): Either[E, List[B]] = es match {
      case Nil ⇒ acc
      case Left(ee) :: ot ⇒ Left(ee)
      case Right(h) :: ot ⇒ go(ot, acc.map(l ⇒ h :: l))
    }

    @tailrec def reverseLoop(as: List[B], acc: List[B]): List[B] = as match {
      case Nil ⇒ acc
      case h :: t ⇒ reverseLoop(t, h :: acc)
    }

    val transformed = go(listOfEithers, Right(List()))
    transformed.map(l ⇒ reverseLoop(l, Nil))
  }

}
