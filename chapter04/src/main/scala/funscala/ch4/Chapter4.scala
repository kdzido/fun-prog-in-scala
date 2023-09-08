package funscala.ch4

object Chapter4 {

  /** Book's example */
  def failingFn(n: Int): Int = {
    val x: Int = throw new Exception("Fail!")
    try {
      val y = 42 + 5
      x + y
    } catch {
      case e: Exception ⇒ 43
    }
  }

  /** Book's example */
  def mean(xs: Seq[Double]): Double =
    if (xs.isEmpty)
      throw new ArithmeticException("mean of empty list!")
    else xs.sum / xs.length

  /** Book's example */
  def mean_1(xs: IndexedSeq[Double], onEmpty: Double): Double =
    if (xs.isEmpty) onEmpty
    else xs.sum / xs.length

  /** Book's example */
  def mean_2(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /** Book's example */
  def mean_3(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs.sum / xs.length)

  /** Book's example */
  def safeDiv(x: Double, y: Double): Either[Exception, Double] =
    try {
      Right(x / y)
    } catch {
      case e: Exception ⇒ Left(e)
    }


  /** [CHAP-4][EXERCISE-02] implement variance in terms of mean and flatMap */
  def variance(xs: Seq[Double]): Option[Double] = {
    mean_2(xs).flatMap(m ⇒ {
      val squaredDistances: Seq[Double] = xs.map(x ⇒ math.pow(x - m, 2))
      mean_2(squaredDistances)
    })
  }


  // Book's example
  sealed class Name(val value: String)
  sealed class Age(val value: Int)
  case class Person(name: Name, age: Age)

  /** Book's example */
  def mkName(name: String): Either[String, Name] =
    if (name == null || name == "") Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_,_))

  def main(args: Array[String]): Unit = {
    println("Chapter 4 - Error handling without exceptions")

    // Book's example
    case class Employee(age: Int, name: String, salary: Double)
    val emp: Either[String, Employee] = for {
      age <- Right(42)
      name <- Left("invalid name")
      salary <- Right(100000.0)
    } yield Employee(age, name, salary)
    println("Employee: " + emp)

    println("mkName: " + mkName("John"))
    println("mkName: " + mkName(""))
    println("mkAge: " + mkAge(12))
    println("mkAge: " + mkAge(-1))
    println("mkPerson: " + mkPerson("John", 12))
    println("mkPerson: " + mkPerson("", 12))
    println("mkPerson: " + mkPerson("John", -1))
  }

}


