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
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remvoe the
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

  @Test def `diff contains only elements from first set and not the other`: Unit = {
    new TestSets {
      val su = union(union(s1, s2), s3)
      val s = diff(su, s3)
      assert(contains(s, 1), "Diff 1")
      assert(contains(s, 2), "Diff 2")
      assert(!contains(s, 3), "Diff 3")
    }
  }

  @Test def `diff contains only elements in both sets`: Unit = {
    new TestSets {
      val su = union(s1, s2)
      val s = intersect(s1, su)
      assert(contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  @Test def `filter contains only elements divisable by 3`: Unit = {
    new TestSets {
      val su = union(union(s1, s2), s3)
      val s = filter(su, x => x % 3 == 0)
      assert(!contains(s, 1), "Filter 1")
      assert(!contains(s, 2), "Filter 2")
      assert(contains(s, 3), "Filter 3")
    }
  }

  @Test def `forall checks if all elements are divisable by 1, 2 and 3`: Unit = {
    new TestSets {
      val su = union(union(s1, s2), s3)
      assert(forall(su, x => x % 1 == 0), "Forall 1")
      assert(!forall(su, x => x % 2 == 0), "Forall 2")
      assert(!forall(su, x => x % 3 == 0), "Forall 3")
    }
  }

  @Test def `exists checks if there is at least one element are divisable by 1, 2, 3 and 4`: Unit = {
    new TestSets {
      val su = union(union(s1, s2), s3)
      assert(exists(su, x => x % 1 == 0), "Forall 1")
      assert(exists(su, x => x % 2 == 0), "Forall 2")
      assert(exists(su, x => x % 3 == 0), "Forall 3")
      assert(!exists(su, x => x % 4 == 0), "Forall 4")
    }
  }

  @Test def `map transforms the set by multiplying elements by 2`: Unit = {
    new TestSets {
      val su = union(union(s1, s2), s3)
      assert(contains(map(su, x => 2 * x), 2), "Map 1")
      assert(contains(map(su, x => 2 * x), 4), "Map 2")
      assert(contains(map(su, x => 2 * x), 6), "Map 3")
      assert(!contains(map(su, x => 2 * x), 8), "Map 4")
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
