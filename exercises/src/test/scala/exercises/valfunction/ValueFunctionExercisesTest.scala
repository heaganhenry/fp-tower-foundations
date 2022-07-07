package exercises.valfunction

import exercises.valfunction.ValueFunctionExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ValueFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  /////////////////////////////////////////////////////
  // Exercise 1: String API with higher-order functions
  /////////////////////////////////////////////////////

  // replace `ignore` by `test` to enable the test
  test("selectDigits examples") {
    assert(selectDigits("hello4world-80") == "480")
    assert(selectDigits("welcome") == "")
  }

  // replace `ignore` by `test` to enable the test
  test("selectDigits length is smaller") {
    forAll { (text: String) =>
      assert(selectDigits(text).length <= text.length)
    }
  }

  test("selectDigits chars are all digits") {
    forAll { (text: String) =>
      selectDigits(text).foreach(c => assert(c.isDigit))
    }
  }

  test("secret examples") {
    assert(secret("Welcome123") == "**********")
  }

  test("secret length is the same") {
    forAll { (text: String) =>
      assert(secret(text).length == text.length)
    }
  }

  test("secret chars are all asterisks") {
    forAll { (text: String) =>
      secret(text).foreach(c => assert(c == '*'))
    }
  }

  test("secret idempotence") {
    forAll { (text: String) =>
      val once = secret(text)
      val twice = secret(secret(text))
      assert(once == twice)
    }
  }

  test("isValidUsernameCharacter examples") {
    assert(isValidUsernameCharacter('3'))
    assert(isValidUsernameCharacter('a'))
    assert(!isValidUsernameCharacter('^'))
  }

  test("isValidUsername") {
    forAll { (username: String) =>
      assert(isValidUsername(username.reverse) == isValidUsername(username))
    }
  }

  ///////////////////////
  // Exercise 2: Point
  ///////////////////////

  test("Point isPositive max 0") {
    forAll { (x: Int, y: Int, z: Int) =>
      assert(Point(x.max(0), y.max(0), z.max(0)).isPositive)
    }
  }

  test("Point isEven product 2") {
    forAll { (x: Int, y: Int, z: Int) =>
      assert(Point(x * 2, y * 2, z * 2).isEven)
    }
  }

  test("Point forAll") {
    forAll { (x: Int, y: Int, z: Int, predicate: Int => Boolean) =>
      assert(Point(x,y,z).forAll(predicate) == List(x,y,z).forall(predicate))
    }
  }
}
