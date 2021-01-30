package patmat

import org.junit._
import org.junit.Assert.assertEquals

class HuffmanSuite {
  import Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }


  @Test def `weight of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(5, weight(t1))
    }


  @Test def `chars of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(List('a','b','d'), chars(t2))
    }

  @Test def `string2chars hello world`: Unit =
    assertEquals(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'), string2Chars("hello, world"))

  @Test def `times empty`: Unit =
    assertEquals(List(), times(string2Chars("")))

  @Test def `times one char`: Unit =
    assertEquals(List(('a', 1)), times(string2Chars("a")))

  @Test def `times two same chars`: Unit =
    assertEquals(List(('a', 2)), times(string2Chars("aa")))

  @Test def `times two different chars`: Unit =
    assertEquals(List(('a', 1), ('b', 1)), times(string2Chars("ab")))

  @Test def `times "aab" chars`: Unit =
    assertEquals(List(('a', 2), ('b', 1)), times(string2Chars("aab")))

  @Test def `times "aabb" chars`: Unit =
    assertEquals(List(('a', 2), ('b', 2)), times(string2Chars("aabb")))

  @Test def `times "aabba" chars`: Unit =
    assertEquals(List(('a', 3), ('b', 2)), times(string2Chars("aabba")))

  @Test def `times "aabbac" chars`: Unit =
    assertEquals(List(('a', 3), ('b', 2), ('c', 1)), times(string2Chars("aabbac")))

  @Test def `make ordered leaf list for some frequency table (15pts)`: Unit =
    assertEquals(List(Leaf('e',1), Leaf('t',2), Leaf('x',3)), makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))


  @Test def `combine of some leaf list (15pts)`: Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)), combine(leaflist))
  }

  @Test def `decode and encode a very short text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("ab".toList, decode(t1, encode(t1)("ab".toList)))
    }

  @Test def `decode and encode a text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("abbbdadddabbbbbaaaddadadada".toList, decode(t2, encode(t2)("abbbdadddabbbbbaaaddadadada".toList)))
    }

  @Test def `decode and quick encode a very short text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("ab".toList, decode(t1, quickEncode(t1)("ab".toList)))
    }

  @Test def `decode and quick encode a text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("abbbdadddabbbbbaaaddadadada".toList, decode(t2, quickEncode(t2)("abbbdadddabbbbbaaaddadadada".toList)))
    }

  @Test def `decode secret`: Unit =
    println("decodedSecret = " + decodedSecret)


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
