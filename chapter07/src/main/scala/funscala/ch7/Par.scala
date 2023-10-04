package funscala.ch7

import com.google.common.util.concurrent.MoreExecutors

import java.util.concurrent.{Callable, ExecutorService, Future}

type Par[A] = ExecutorService ⇒ Future[A]

object Par {
  /** Book's example */
  def sortPar(l: Par[List[Int]]): Par[List[Int]] =
    Par.map2(l, unit(()))((a, _) => a.sorted)

  /** Book's example */
  def map[A, B](a: Par[A])(f: A => B): Par[B] =
    Par.map2(a, unit(()))((a,_) => f(a))

  /** [CHAP-7][EXERCISE-05] (optional) implement product and map as primitives, define map2 in terms of them */
  def map_1[A, B](fa: Par[A])(f: A => B): Par[B] = e => {
    val a = fa(e).get
    val t = new Callable[B]():
      override def call(): B = f(a)
    e.submit(t)
  }

  /** [CHAP-7][EXERCISE-06] implement parMap */
  def parMap[A,B](l: List[A])(f: A => B): Par[List[B]] = e => {
    val c = new Callable[List[B]]():
      override def call(): List[B] = l.map(f).toList
    e.submit(c)
  }
  def parMap_1[A,B](l: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = l.map(asyncF(f))
    sequence(fbs)
  }

  /** [CHAP-7][EXERCISE-08] implement parFilter */
  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = e => {
    val c = new Callable[List[A]]():
      override def call(): List[A] = l.filter(f)
    e.submit(c)
  }

  /** [CHAP-7][EXERCISE-06] (hard) implement sequence */
  def sequence[A](l: List[Par[A]]): Par[List[A]] = e => {
    def go(left: List[Par[A]], acc: List[A]): List[A] = left match {
      case Nil => acc
      case h :: t => go(t, h(e).get() :: acc)
    }

    val c = new Callable[List[A]]():
      override def call(): List[A] = go(l, Nil).reverse
    e.submit(c)
  }

  /** [CHAP-7][EXERCISE-05] (optional) implement product and map as primitives, define map2 in terms of them */
  def product[A,B](fa: Par[A], fb: Par[B]): Par[(A,B)] = e => {
    val a = fa(e).get()
    val b = fb(e).get()
    val t = new Callable[(A,B)]():
      override def call(): (A,B) = (a,b)
    e.submit(t)
  }

  def unit[A](a: A): Par[A] = {
    val c = new Callable[A]():
      override def call(): A = a
    e ⇒ e.submit(c)
  }

  /** Book's example */
  def async[A](a: ⇒ A): Par[A] = fork(unit(a))

  /** [CHAP-7][EXERCISE-04] impl asyncF using async */
  def asyncF[A,B](f: A => B): A => Par[B] = a => async(f(a))

  def fork[A](a: ⇒ Par[A]): Par[A] = es2 ⇒ {
     a(es2)
  }

  /** [CHAP-7][EXERCISE-01] provide map2 signature
   */
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = es ⇒ {
    val l: Future[A] = Par.run(es)(a)
    val r: Future[B] = Par.run(es)(b)
    val c: C = f(l.get(), r.get())
    Par.unit(c)(es)
  }

  /** [CHAP-7][EXERCISE-05] (optional) implement product and map as primitives, define map2 in terms of them */
  def map2_1[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    map_1(product(a, b))((tup) => f(tup._1, tup._2))

  /** [CHAP-7][EXERCISE-03] impl Par representation */
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

}
