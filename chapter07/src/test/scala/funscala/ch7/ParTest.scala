package funscala.ch7

import com.google.common.util.concurrent.MoreExecutors
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.Executors

class ParTest extends AnyFlatSpec with Matchers {

  val direct = MoreExecutors.newDirectExecutorService()
  val pool = Executors.newFixedThreadPool(3)

  it should "run unit" in {
    Par.run(direct)(Par.unit(1)).get() shouldBe 1
    Par.run(pool)(Par.unit(1)).get() shouldBe 1
  }

  it should "map2 two Pars" in {
    Par.run(direct)(Par.map2(Par.unit(1), Par.unit(2))(_ + _)).get() shouldBe 3
    Par.run(pool)(Par.map2(Par.unit(1), Par.unit(2))(_ + _)).get() shouldBe 3
  }

  it should "map2_1 two Pars" in {
    Par.run(direct)(Par.map2_1(Par.unit(1), Par.unit(2))(_ + _)).get() shouldBe 3
    Par.run(pool)(Par.map2_1(Par.unit(1), Par.unit(2))(_ + _)).get() shouldBe 3
  }

  it should "convert function into async function" in {
    Par.run(direct)(Par.asyncF((a: Int) => ("_" + a.toString))(2)).get() shouldBe "_2"
    Par.run(pool)(Par.asyncF((a: Int) => ("_" + a.toString))(2)).get() shouldBe "_2"
  }

  it should "sort list in Par" in {
    val lp = Par.unit(List(2,1,5,4,3))
    Par.run(direct)(Par.sortPar(lp)).get() shouldBe List(1,2,3,4,5)
    Par.run(pool)(Par.sortPar(lp)).get() shouldBe List(1,2,3,4,5)
  }

  it should "map list into sorted list" in {
    val lp = Par.unit(List(2,1,5,4,3))
    Par.run(direct)(Par.map(lp)(_.sorted)).get() shouldBe List(1,2,3,4,5)
    Par.run(pool)(Par.map(lp)(_.sorted)).get() shouldBe List(1,2,3,4,5)
  }

  it should "map_1 list into sorted list" in {
    val lp = Par.unit(List(2,1,5,4,3))
    Par.run(direct)(Par.map_1(lp)(_.sorted)).get() shouldBe List(1,2,3,4,5)
    Par.run(pool)(Par.map_1(lp)(_.sorted)).get() shouldBe List(1,2,3,4,5)
  }

  it should "parMap list into sorted list" in {
    val lp = List(2,1,5,4,3)
    Par.run(direct)(Par.parMap(lp)(_.toString)).get() shouldBe List("2","1","5","4","3")
    Par.run(pool)(Par.parMap(lp)(_.toString)).get() shouldBe List("2","1","5","4","3")
  }

  it should "create product of two Pars" in {
    val pa = Par.unit(1)
    val pb = Par.unit(2)
    Par.run(direct)(Par.product(pa, pb)).get shouldBe (1,2)
    Par.run(pool)(Par.product(pa, pb)).get shouldBe (1,2)
  }

}
