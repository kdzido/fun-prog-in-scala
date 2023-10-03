package funscala.ch7

import com.google.common.util.concurrent.MoreExecutors

import java.util.concurrent.{Callable, ExecutorService, Future}

type Par[A] = ExecutorService ⇒ Future[A]

object Par {
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

  /** [CHAP-7][EXERCISE-03] impl Par representation */
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

}
