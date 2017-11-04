package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
      assert(weight(t2) === 9)
    }
  }

  test("weight of a smaller tree") {
    assert(weight(Leaf('a', 8)) === 8)
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
      assert(chars(t1) === List('a','b'))
    }
  }

  test("chars of a smaller tree") {
    assert(chars(Leaf('a',8)) === List('a'))
  }

  test("makeCodeTree on two larger trees") {
    new TestTrees {
      val t3 = makeCodeTree(t1, t2)
      assert(weight(t3) === weight(t1) + weight(t2))
      assert(chars(t3) === chars(t1) ::: chars(t2))
    }
  }

  test("makeCodeTree on two smaller trees") {
    val t3 = makeCodeTree(Leaf('a',8), Leaf('b',5))
    assert(weight(t3) === 13)
    assert(chars(t3) === List('a','b'))
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times calculates frequencies") {
    assert(times(List('a', 'b', 'a')) === List(('b', 1), ('a', 2)))
    assert(times(List('h', 'e', 'l', 'l', 'o', 'w', 'o', 'r', 'l', 'd')) === List(('e', 1), ('l', 3), ('h', 1), ('r', 1), ('w', 1), ('o', 2), ('d', 1)))
    assert(times(List()) === List())
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
    assert(makeOrderedLeafList(List()) === List())
  }

  test("singleton returns true when one tree in list") {
    new TestTrees {
      assert(singleton(List(t1)) === true)
    }
  }

  test("singleton returns false when more than one tree in list") {
    new TestTrees {
      assert(singleton(List(t1, t2)) === false)
    }
  }

  test("singleton returns false when zero trees in list") {
    new TestTrees {
      assert(singleton(List()) === false)
    }
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until combines leaf lists") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) === List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),Leaf('x',4),List('e', 't', 'x'),7)))
  }

  test("until returns unchanged single leaf list") {
    val leaflist = List(Leaf('e', 1))
    assert(until(singleton, combine)(leaflist) === List(Leaf('e', 1)))
  }

  test("createCodeTrees returns correct code tree from list of Chars") {
    val chars = List('t', 'x', 'x', 'e', 'x', 't', 'x')
    assert(createCodeTree(chars) === Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),Leaf('x',4),List('e', 't', 'x'),7))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

}
