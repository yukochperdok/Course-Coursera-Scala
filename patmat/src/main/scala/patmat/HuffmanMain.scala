package patmat

import patmat.Huffman._

object HuffmanMain extends App{
  val sampleTree =
    makeCodeTree(
      makeCodeTree(
        Leaf('x', 1),
        Leaf('e', 1)),
      Leaf('t', 2)
    )

  println(sampleTree)

  val lTimes = times(List('a', 'b', 'a', 'c', 'd', 'b', 'b'))
  println(lTimes)
  println(makeOrderedLeafList(lTimes))


  println(singleton(makeOrderedLeafList(lTimes)))
  println(singleton(Leaf('a',3)::Nil))
  println(singleton(Nil))

  println(createCodeTree(List('a', 'b', 'a', 'c', 'd', 'b', 'b')))
  println(createCodeTree(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')))

  println(decodedSecret)

  val codes: CodeTable =
    List(
    ('a', List(0,0,0,1)),
    ('b', List(0,0,1,0)),
    ('c', List(0,1,0,0)),
    ('d', List(1,0,0,0))
    )
  val funcCodeBits: Char => List[Bit] = codeBits(codes)

  println(funcCodeBits('b'))
  println(funcCodeBits('d'))
  println(funcCodeBits('z'))

  val quickCodeHelloWord: List[Char] => List[Bit] =
    quickEncode(createCodeTree(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')))

  println(quickCodeHelloWord("o,".toList))


}
