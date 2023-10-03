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

  it should "convert function into async function" in {
    Par.run(direct)(Par.asyncF((a: Int) => ("_" + a.toString))(2)).get() shouldBe "_2"
    Par.run(pool)(Par.asyncF((a: Int) => ("_" + a.toString))(2)).get() shouldBe "_2"
  }

}
