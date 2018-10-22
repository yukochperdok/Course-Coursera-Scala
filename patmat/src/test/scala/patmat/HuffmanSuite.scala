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
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("makeOrderedLeafListOption2 for some frequency table") {
    assert(makeOrderedLeafListOption2(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("createCodeTree(\"hello, world\") not empty") {
    assert(createCodeTree(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))!== List.empty)
  }

  test("createCodeTree(\"hello, world\")") {
    val tree =
      Fork(
        Fork(
          Leaf('o',2),
          Fork(
            Leaf('h',1),
            Fork(
              Leaf(',',1),
              Leaf('e',1),
              List(',','e'),2),
            List('h',',','e'),3),
          List('o', 'h',',', 'e'),5),
        Fork(Leaf('l',3),
          Fork(
            Fork(
              Leaf('w',1),
              Leaf(' ',1),
              List('w',' '),2),
            Fork(
              Leaf('d',1),
              Leaf('r',1),
              List('d', 'r'),2),
            List('w',' ', 'd', 'r'),4),
          List('l', 'w', ' ', 'd', 'r'),7),
        List('o', 'h', ',', 'e', 'l', 'w', ' ', 'd', 'r'),12)

    assert(createCodeTree(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')) == tree)
  }

  test("decode a very short text should be right") {
    new TestTrees {
      assert(decode(t2,List(0,0,0,1)) === "ab".toList)
    }
  }

  test("decode a very short text with wrong bits should be right") {
    new TestTrees {
      assert(decode(t2,List(0,0,7,0,1)) === "ab".toList)
    }
  }

  test("decode a very short text with incomplete string of bits should fail") {
    new TestTrees {
      intercept[IllegalArgumentException](decode(t2,List(0,0,0)))
    }
  }

  test("decode a empty short text should be empty") {
    new TestTrees {
      assert(decode(t2,List.empty) === List.empty)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  trait TestCodeTables {
    val codes: CodeTable =
      List(
        ('a', List(0,0,0,1)),
        ('b', List(0,0,1,0)),
        ('c', List(0,1,0,0)),
        ('d', List(1,0,0,0))
      )
  }

  test("codeBits should codify any char which belong to code table") {
    new TestCodeTables {
      assert(codeBits(codes)('a')=== List(0,0,0,1))
    }
  }

  test("codeBits should return empty List with not included char") {
    new TestCodeTables {
      assert(codeBits(codes)('z')=== List.empty)
    }
  }

  test("convert should convert a CodeTree to CodeTable") {
    new TestCodeTables with TestTrees {
      assert(codeBits(convert(t2))('a') === List(0,0))
    }
  }

  test("quickSort should encode as same way decode function") {
    new TestCodeTables with TestTrees {
      assert(quickEncode(t2)("ab".toList) === encode(t2)("ab".toList))
    }
  }

}
