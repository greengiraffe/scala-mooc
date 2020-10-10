package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
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
    val s1 = singletonSet(1) //  x => x == 1
    val s2 = singletonSet(2)(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s1to4 = union(union(union(s1, s2), s3), s4)
    val sOnlyPositive = union(s1, s2)

    val s5 = allButThis(5)

    printSet(s5)
    printSet(s1to4)
    printSet(sOnlyPositive)
  }

  @Test def `allButThis Test`: Unit = {
    new TestSets {
      assert(!contains(s5, 5), "does not contain 5")
      assert(contains(s5, 1), "does contain 1")
    }
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * @Ignore annotation.
   */
  @Test def `singleton set one contains one`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `intersection contains equal elements of each set`: Unit = {
    new TestSets {
      val s_yes = intersect(s1, s1)
      val s_no = intersect(s1, s2)
      assert(contains(s_yes, 1), "Intersect 1&1 contains 1")
      assert(!contains(s_yes, 2), "Intersect 1&1 does not contain 2")
      assert(!contains(s_no, 1), "Intersect 1&2 does not contain 1")
      assert(!contains(s_no, 2), "Intersect 1&2 does not contain 2")
    }
  }

  @Test def `diff contains elements that are not shared in the sets`: Unit = {
    new TestSets {
      val s_yes = diff(s1, s2)
      val s_no = diff(s1, s1)
      assert(contains(s_yes, 1), "Diff 1&2 contains 1")
      assert(!contains(s_yes, 2), "Diff 1&2 does not contain 2")
      assert(!contains(s_no, 1), "Diff 1&1 does not contain 1")
    }
  }

  @Test def `filter returns a subset for which a predicate holds`: Unit = {
    new TestSets {
      val s = filter(s1to4, x => x % 2 == 0) // even numbers
      assert(contains(s, 2), "2 in set")
      assert(contains(s, 2), "4 in set")
      assert(!contains(s, 1), "1 not in set")
      assert(!contains(s, 3), "3 not in set")
      assert(!contains(s, 0), "0 not in set")
    }
  }

  @Test def `forall tests a predicate for each element of a set`: Unit = {
    new TestSets {
      val isPositive = forall(sOnlyPositive, x => x > 0)
      val isNegative = forall(sOnlyPositive, x => x < 0)
      assert(isPositive, "positive number set only contains positive numbers")
      assert(!isNegative, "positive number set does not contain negative numbers")
    }
  }

  @Test def `exists tests if some element is in a set for which a predicate holds`: Unit = {
    new TestSets {
      val exists2 = exists(union(s1,s2), x => x == 2)
      val exists3 = exists(union(s1,s2), x => x == 3)
      assert(exists2, "2 exists in set (1,2)")
      assert(!exists3, "3 does not exist in set (1,2)")
    }
  }

  @Test def `map returns a given set that is transformed by a given funtion p`: Unit = {
    new TestSets {
      val mapped = map(sOnlyPositive, x => -x)
      printSet(mapped)
      assert(contains(mapped, -1), "-1 exists in set")
      assert(!contains(mapped, 1), "+1 does not exist in set")
      assert(forall(mapped, x => x < 0), "all numbers in set should be negative")
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
