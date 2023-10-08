package funscala.ch7

import com.google.common.util.concurrent.MoreExecutors
import funscala.ch7.Par.{map, unit}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.{ExecutorService, Executors}

class ParTest extends AnyFlatSpec with Matchers {

  val direct = MoreExecutors.newDirectExecutorService()
  val pool = Executors.newFixedThreadPool(3)

  it should "run unit" in {
    Par.run(direct)(unit(1)).get() shouldBe 1
    Par.run(pool)(unit(1)).get() shouldBe 1
  }

  it should "map2 two Pars" in {
    Par.run(direct)(Par.map2(unit(1), unit(2))(_ + _)).get() shouldBe 3
    Par.run(pool)(Par.map2(unit(1), unit(2))(_ + _)).get() shouldBe 3
  }

  it should "map2_1 two Pars" in {
    Par.run(direct)(Par.map2_1(unit(1), unit(2))(_ + _)).get() shouldBe 3
    Par.run(pool)(Par.map2_1(unit(1), unit(2))(_ + _)).get() shouldBe 3
  }

  it should "convert function into async function" in {
    Par.run(direct)(Par.asyncF((a: Int) => ("_" + a.toString))(2)).get() shouldBe "_2"
    Par.run(pool)(Par.asyncF((a: Int) => ("_" + a.toString))(2)).get() shouldBe "_2"
  }

  it should "sort list in Par" in {
    val lp = unit(List(2,1,5,4,3))
    Par.run(direct)(Par.sortPar(lp)).get() shouldBe List(1,2,3,4,5)
    Par.run(pool)(Par.sortPar(lp)).get() shouldBe List(1,2,3,4,5)
  }

  it should "map list into sorted list" in {
    val lp = unit(List(2,1,5,4,3))
    Par.run(direct)(map(lp)(_.sorted)).get() shouldBe List(1,2,3,4,5)
    Par.run(pool)(map(lp)(_.sorted)).get() shouldBe List(1,2,3,4,5)
  }

  it should "map_1 list into sorted list" in {
    val lp = unit(List(2,1,5,4,3))
    Par.run(direct)(Par.map_1(lp)(_.sorted)).get() shouldBe List(1,2,3,4,5)
    Par.run(pool)(Par.map_1(lp)(_.sorted)).get() shouldBe List(1,2,3,4,5)
  }

  it should "parMap list into sorted list" in {
    val lp = List(2,1,5,4,3)
    Par.run(direct)(Par.parMap(lp)(_.toString)).get() shouldBe List("2","1","5","4","3")
    Par.run(pool)(Par.parMap(lp)(_.toString)).get() shouldBe List("2","1","5","4","3")
  }

  it should "parMap_1 list into sorted list" in {
    val lp = List(2,1,5,4,3)
    Par.run(direct)(Par.parMap_1(lp)(_.toString)).get() shouldBe List("2","1","5","4","3")
    Par.run(pool)(Par.parMap_1(lp)(_.toString)).get() shouldBe List("2","1","5","4","3")
  }

  it should "parFilter list " in {
    val lp = List(2,1,5,4,3)
    Par.run(direct)(Par.parFilter(lp)(_ % 2 == 0)).get() shouldBe List(2,4)
    Par.run(pool)(Par.parFilter(lp)(_ % 2 == 0)).get() shouldBe List(2,4)
  }

  it should "sequence List of Pars" in {
    Par.run(direct)(Par.sequence(List())).get() shouldBe List()
    Par.run(direct)(Par.sequence(List(unit(1)))).get() shouldBe List(1)
    Par.run(direct)(Par.sequence(List(unit(1), unit(2), unit(3)))).get() shouldBe List(1,2,3)

    Par.run(pool)(Par.sequence(List())).get() shouldBe List()
    Par.run(pool)(Par.sequence(List(unit(1)))).get() shouldBe List(1)
    Par.run(pool)(Par.sequence(List(unit(1), unit(2), unit(3)))).get() shouldBe List(1,2,3)
  }

  it should "create product of two Pars" in {
    val pa = unit(1)
    val pb = unit(2)
    Par.run(direct)(Par.product(pa, pb)).get shouldBe (1,2)
    Par.run(pool)(Par.product(pa, pb)).get shouldBe (1,2)
  }

  // Book's example of identity law: map(unit(1))(_ + 1) == unit(2)
  "unit-map identity law" should "enforce equivalence of mapping over a unit with the resulting unit" in {
    isEquivalent(direct)(map(unit(1))(_ + 1), unit(2)) shouldBe true
  }

  // Book's example of identity law: map(unit(x))(f) == unit(f(x))
  it should "hold for any choice of x and f" in {
    val f: Int => Int = _ + 1
    val x = 1
    isEquivalent(direct)(map(unit(x))(f), unit(f(x))) shouldBe true
  }

  // Book's example
  "map identity law" should "be defined in terms of simpler laws" in {
    val f: Int => Int = _ + 1
    val x = 1
    val y = Par.async(2)

    isEquivalent(direct)(map(unit(x))(f), unit(f(x))) shouldBe true
    isEquivalent(direct)(map(unit(x))(id), unit(id(x))) shouldBe true
    isEquivalent(direct)(map(unit(x))(id), unit(x)) shouldBe true
    isEquivalent(direct)(map(y)(id), y) shouldBe true
  }


  // Helpers
  def id[A](a: A): A = a

  def isEquivalent[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get

}
