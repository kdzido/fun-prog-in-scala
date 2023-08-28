package funscala.ch1

import org.scalatest.funsuite.AnyFunSuiteLike

class Chapter1Test extends AnyFunSuiteLike {

  test("should be always true") {
    assert(Chapter1.Truth == true)
  }

}
