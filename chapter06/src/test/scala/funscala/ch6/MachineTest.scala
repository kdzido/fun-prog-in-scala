package funscala.ch6

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MachineTest extends AnyFlatSpec with Matchers {

  "Locked machine with candies" should "get unlocked when coin is inserted" in {
    Machine(true, 1, 0).transit(Coin) shouldBe Machine(false, 1, 1)
    Machine(true, 1, 1).transit(Coin) shouldBe Machine(false, 1, 2)
  }

  "Unlocked machine with candies" should "dispose candy when knob is turned" in {
    Machine(false, 1, 1).transit(Turn) shouldBe Machine(true, 0, 1)
    Machine(false, 2, 1).transit(Turn) shouldBe Machine(true, 1, 1)
  }

  "Locked machine" should "does nothing when turning knob" in {
    Machine(true, 1, 0).transit(Turn) shouldBe Machine(true, 1, 0)
    Machine(true, 1, 1).transit(Turn) shouldBe Machine(true, 1, 1)
    Machine(true, 0, 1).transit(Turn) shouldBe Machine(true, 0, 1)
    Machine(true, 0, 1).transit(Turn) shouldBe Machine(true, 0, 1)
  }

  "Unlocked machine" should "does nothing when inserting coin" in {
    Machine(false, 1, 0).transit(Coin) shouldBe Machine(false, 1, 0)
    Machine(false, 1, 1).transit(Coin) shouldBe Machine(false, 1, 1)
    Machine(false, 0, 0).transit(Coin) shouldBe Machine(false, 0, 0)
    Machine(false, 0, 1).transit(Coin) shouldBe Machine(false, 0, 1)
  }

  "Machine out of candies" should "ignore all inputs" in {
    Machine(false, 0, 0).transit(Coin) shouldBe Machine(false, 0, 0)
    Machine(false, 0, 0).transit(Turn) shouldBe Machine(false, 0, 0) // invalid state
    Machine(false, 0, 1).transit(Coin) shouldBe Machine(false, 0, 1)
    Machine(false, 0, 1).transit(Turn) shouldBe Machine(false, 0, 1)
    Machine(true, 0, 0).transit(Coin) shouldBe Machine(true, 0, 0)
    Machine(true, 0, 0).transit(Turn) shouldBe Machine(true, 0, 0)
    Machine(true, 0, 1).transit(Coin) shouldBe Machine(true, 0, 1)
    Machine(true, 0, 1).transit(Turn) shouldBe Machine(true, 0, 1)
  }

  "Machine" should "transit on Input stimuli" in {
    val lockedMachine = Machine.simple(Coin).flatMap(m => State.unit(m.coins))
    val (coinsInside2, newMachineState2) = lockedMachine.run(Machine(true, 1, 0))
    coinsInside2 shouldBe 1
    newMachineState2.locked shouldBe false
    newMachineState2.candies shouldBe 1
    newMachineState2.coins shouldBe 1

    val unlockedMachine = Machine.simple(Turn).flatMap(m => State.unit(m.coins))
    val (coinsInside3, newMachineState3) = unlockedMachine.run(Machine(false, 1, 1))
    coinsInside3 shouldBe 1
    newMachineState3.locked shouldBe true
    newMachineState3.candies shouldBe 0
    newMachineState3.coins shouldBe 1
  }

  "Candy machine" should "accept simulated inputs and return number of coins left inside of it at the end" in {
    Machine.simulateInputs(List(Turn)).run(Machine(false, 1, 1))._1 shouldBe 1
    Machine.simulateInputs(List(Coin, Turn, Coin, Turn)).run(Machine(true, 1, 0))._1 shouldBe 1 // no more candies

    Machine.simulateInputs(List(Coin, Turn)).run(Machine(true, 1, 0))._1 shouldBe 1
    Machine.simulateInputs(List(Coin, Turn, Coin, Turn)).run(Machine(true, 1, 0))._1 shouldBe 1 // no more candies

    Machine.simulateInputs(List(Coin, Turn, Coin, Turn)).run(Machine(false, 2, 0))._1 shouldBe 1
    Machine.simulateInputs(List(Coin, Turn, Coin, Turn)).run(Machine(true, 2, 0))._1 shouldBe 2

    Machine.simulateInputs(List(Coin, Coin, Coin, Coin)).run(Machine(false, 4, 10))._1 shouldBe 10
    Machine.simulateInputs(List(Coin, Coin, Coin, Coin)).run(Machine(true, 4, 10))._1 shouldBe 11
    Machine.simulateInputs(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin)).run(Machine(true, 4, 10))._1 shouldBe 14
    Machine.simulateInputs(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(true, 4, 10))._1 shouldBe 14
  }

}
