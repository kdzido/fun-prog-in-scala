package funscala.ch6


object Chapter6 {

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1,i2), rng3)
  }

  def main(args: Array[String]): Unit = {
    println("Chapter 6 - Purely functional state")

    import java.util.Random
    val rng = new Random
    println("rng.nextDouble: " + rng.nextDouble())
    println("rng.nextDouble: " + rng.nextDouble())

    println("RNG.nextInt: " + RNG.simple(1).nextInt._1)
    println("RNG.nextInt: " + RNG.simple(2).nextInt._1)

    val seedState = RNG.simple(1)
    println("randomPair: " + randomPair(seedState)._1)
  }

}


