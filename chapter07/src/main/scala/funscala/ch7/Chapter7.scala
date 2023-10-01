package funscala.ch7


object Chapter7 {
  val Truth = true

  def sum(as: IndexedSeq[Int]): Par[Int] =
    if (as.size <= 1) Par.unit(as.headOption getOrElse(0))
    else {
      val (l,r) = as.splitAt(as.length/2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

  @main def main(): Unit = {
    println("Chapter 7 - Purely functional parallelism")
    // create a library for creating and composing parallel and asynchronous computation
    // discover data type and primitive functions for the domain, and derive some useful combinators

  }

}
