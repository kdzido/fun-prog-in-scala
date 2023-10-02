package funscala.ch7

import com.google.common.util.concurrent.MoreExecutors
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.Executors

class Chapter7Test extends AnyFlatSpec with Matchers {

  val direct = MoreExecutors.newDirectExecutorService()
  val pool = Executors.newFixedThreadPool(3)

  it should "sum elements of sequence" in {
    Par.run(pool)(Chapter7.sum(Vector(1,2,3))).get() shouldBe 6
    Par.run(direct)(Chapter7.sum(Vector(1,2,3))).get() shouldBe 6
  }

}
