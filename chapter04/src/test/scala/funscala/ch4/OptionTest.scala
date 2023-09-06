package funscala.ch4

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OptionTest extends AnyFlatSpec {

  "Option" should "construct None and Some value" in {
    assert(None.isInstanceOf[Option[_]])
    assert(Some(1).isInstanceOf[Option[Int]])
  }

  "None Option" should "always flat map to None" in {
    assert(None.flatMap((a: Int) ⇒ Some(a.toString)) == None)
  }

  it should "always map to None" in {
    assert(None.map((a: Int) ⇒ Some(a.toString)) == None)
  }

  it should "return getOrElse's default " in {
    assert(None.getOrElse("<none>") == "<none>")
  }

  it should "return orElse's default " in {
    assert(None.orElse(Some("<none>")) == Some("<none>"))
    assert(None.orElse(None) == None)
  }

  it should "filter always to None" in {
    assert(None.filter(_ == true) == None)
  }

  "Some Option" should "flat map to Some or None" in {
    assert(Some(1).flatMap((a: Int) ⇒ Some(a.toString)) == Some("1"))
    assert(Some(1).flatMap((a: Int) ⇒ None) == None)
  }

  it should "always map to Some" in {
    assert(Some(1).map((a: Int) ⇒ a.toString) == Some("1"))
  }

  it should "never return getOrElse's default" in {
    assert(Some(1).getOrElse("<none>") == 1)
  }

  it should "never return orElse's default " in {
    assert(Some(1).orElse(Some("<none>")) == Some(1))
    assert(Some(1).orElse(None) == Some(1))
  }

  it should "filter as per predicate" in {
    assert(Some(1).filter(_ % 2 == 0) == None)
    assert(Some(1).filter(_ % 2 == 1) == Some(1))
  }

  "Function" should "be lifted" in {
    val incrFn: Int ⇒ Int = _ + 1
    val liftedIncrFn = Option.lift(incrFn)
    assert(liftedIncrFn(None) == None)
    assert(liftedIncrFn(Some(1)) == Some(2))
  }

  "Lifted pattern matching function" should "match strings" in {
    assert(Option.doesMatch("(1,2,3", "") == None)  // invalid pattern
    assert(Option.doesMatch("[1,2,3]{1,3}", "") == Some(false))
    assert(Option.doesMatch("[1,2,3]{1,3}", "123") == Some(true))
    assert(Option.doesMatch("[1,2,3]{1,3}", "1234") == Some(false))
  }

  // [CHAP-4][EXERCISE-03] implement map2 on Option
  "Two Options" should "be mapped to 3rd Option" in {
    val f: (Int, Int) ⇒ String = (a,b) ⇒ (a + b).toString

    assert(Option.map2(None, None)(f) == None)
    assert(Option.map2(Some(1), None)(f) == None)
    assert(Option.map2(None, Some(2))(f) == None)
    assert(Option.map2(Some(1), Some(2))(f) == Some("3"))
  }

  // [CHAP-4][EXERCISE-04] re-implement bothMatch in terms of map2
  "String" should "be matched against both patterns simultaneously" in {
    assert(Option.bothMatch("[1", "[2,3,4]+", "1") == None) // invalid pattern
    assert(Option.bothMatch("[1,2,3]+", "[2", "1") == None) // invalid pattern
    assert(Option.bothMatch("[1", "[2", "1") == None)       // invalid both patterns

    assert(Option.bothMatch("[1,2,3]+", "[2,3,4]+", "1") == Some(false))
    assert(Option.bothMatch("[1,2,3]+", "[2,3,4]+", "2") == Some(true))
    assert(Option.bothMatch("[1,2,3]+", "[2,3,4]+", "3") == Some(true))
    assert(Option.bothMatch("[1,2,3]+", "[2,3,4]+", "4") == Some(false))
  }
}
