package funscala.ch7


object Chapter7 {
  val Truth = true

  /** Book's example */
  def sum(as: IndexedSeq[Int]): Int =
    if (as.size <= 1) as.headOption getOrElse(0)
    else {
      val (l,r) = as.splitAt(as.length/2)
      sum(l) + sum(r)
    }

  @main def main(): Unit = {
    println("Chapter 7 - Purely functional parallelism")
    // create a library for creating and composing parallel and asynchronous computation
    // discover data type and primitive functions for the domain, and derive some useful combinators

  }

}
