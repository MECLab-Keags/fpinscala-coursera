import patmat.Huffman._

times(string2Chars("hello, world"))

val target1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
decode(target1, List(0,0,1,1))

val cd = makeCodeTree(Leaf('C',1), Leaf('D',1))
val ef = makeCodeTree(Leaf('E',1), Leaf('F',1))
val gh = makeCodeTree(Leaf('G',1), Leaf('H',1))

val bcd = makeCodeTree(Leaf('B',3), cd)
val efgh = makeCodeTree(ef,gh)

val bcdefgh = makeCodeTree(bcd,efgh)
val atoh = makeCodeTree(Leaf('A',8), bcdefgh)

decode(atoh, List(1,0,0,0,1,0,1,0))

frenchCode
val decoded = decode(frenchCode, secret).toString
decoded

convert(atoh)
val encoded = encode(atoh)(string2Chars("BAC"))
decode(atoh, encoded)