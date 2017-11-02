package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    val s6 = singletonSet(6)
    val s7 = singletonSet(7)
    val s8 = singletonSet(8)
    val s9 = singletonSet(9)
    val s10 = singletonSet(10)
    val s11 = singletonSet(-999)
    val s12 = singletonSet(-998)
    val s13 = singletonSet(-985)
    val s14 = singletonSet(-984)
    val s15 = singletonSet(984)
    val s16 = singletonSet(985)
    val s17 = singletonSet(998)
    val s18 = singletonSet(999)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton 1")
    }
  }

  test("singletonSet(2) contains 2") {
    new TestSets {
      assert(contains(s2, 2), "Singleton 2")
    }
  }

  test("singletonSet(3) contains 3") {
    new TestSets {
      assert(contains(s3, 3), "Singleton 3")
    }
  }

  test("singletonSet(3) does not contain 2") {
    new TestSets {
      assert(!contains(s3, 2), "Singleton 4")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains elements common to each set") {
    new TestSets {
      val sa = union(union(s1, s2), s3)
      val sb = union(union(s2, s3), s4)
      val sc = intersect(sa, sb)
      assert(!contains(sc, 1), "Intersect 1")
      assert(contains(sc, 2), "Intersect 2")
      assert(contains(sc, 3), "Intersect 3")
      assert(!contains(sc, 4), "Intersect 4")
    }
  }

  test("diff contains elements in first set but not in other") {
    new TestSets {
      val sa = union(union(s1, s2), s3)
      val sb = union(union(s2, s3), s4)
      val sc = diff(sa, sb)
      assert(contains(sc, 1), "Diff 1")
      assert(!contains(sc, 2), "Diff 2")
      assert(!contains(sc, 3), "Diff 3")
      assert(!contains(sc, 4), "Diff 4")
    }
  }

  test("set filtered on evens contains only even numbers") {
    new TestSets {
      val sa = union(union(union(union(union(union(union(union(union(s1, s2), s3), s4), s5), s6), s7), s8), s9), s10)
      val sb = filter(sa, _ % 2 == 0)
      assert(!contains(sb, 1), "Filter 1")
      assert(contains(sb, 2), "Filter 2")
      assert(!contains(sb, 5), "Filter 3")
      assert(contains(sb, 6), "Filter 4")
      assert(!contains(sb, 9), "Filter 5")
      assert(contains(sb, 10), "Filter 6")
    }
  }

  test("forall checking for evens returns true on set of even numbers") {
    new TestSets {
      val sa = union(union(union(union(union(union(union(union(s2, s4), s6), s8), s10), s12), s14), s15), s17)
      assert(forall(sa, _ % 2 == 0), "ForAll 1")
    }
  }

  test("forall checking for evens returns false if odd number in set") {
    new TestSets {
      val sa = union(union(union(union(union(union(union(union(union(s2, s4), s6), s8), s10), s12), s14), s15), s17), s18)
      assert(!forall(sa, _ % 2 == 0), "ForAll 2")
    }
  }

  test("exists checking for odds returns true if odd number in set") {
    new TestSets {
      val sa = union(union(union(union(union(union(union(union(union(s2, s4), s6), s8), s10), s12), s14), s15), s17), s18)
      assert(exists(sa, _ % 2 != 0), "Exists 1")
    }
  }

  test("exists checking for odds returns false on set of even numbers") {
    new TestSets {
      val sa = union(union(union(union(union(union(union(union(s2, s4), s6), s8), s10), s12), s14), s15), s17)
      assert(!exists(sa, _ % 2 != 0), "Exists 2")
    }
  }

  test("map returns set of square numbers when square function passed in") {
    new TestSets {
      val sa = union(union(union(union(union(union(union(union(union(s1, s2), s3), s4), s5), s6), s7), s8), s9), s10)
      val sb = map(sa, x => x * x)
      assert(contains(sb, 1), "Map 1")
      assert(contains(sb, 4), "Map 2")
      assert(contains(sb, 9), "Map 3")
      assert(contains(sb, 16), "Map 4")
      assert(contains(sb, 25), "Map 5")
      assert(contains(sb, 36), "Map 6")
      assert(contains(sb, 49), "Map 7")
      assert(contains(sb, 64), "Map 8")
      assert(contains(sb, 81), "Map 9")
      assert(contains(sb, 100), "Map 10")
    }
  }

}
