package fpis.dstrs

import Tree._
object TreeTests {
    val l1 = Leaf(1)
    val l2 = Leaf(2)
    val l3 = Leaf(3)
    val l4 = Leaf(4)
    val l5 = Leaf(5)
    val l6 = Leaf(16)
    val l7 = Leaf(7)
    val l8 = Leaf(8)

val b12 = Branch(l1,l2)
val b34 = Branch(l3,l4)
val b56 = Branch(l5,l6)
val b78 = Branch(l7,b12)

val b1234 = Branch(b12,Leaf(34))
val b5678 = Branch(b56,b78)

val t = Branch(b1234,b5678)

assert(max(t)==34)
assert(depth(t)==4)
assert(max(map(b34, (v:Int)=> v*v))==16)

assert(max2(t)==34)
assert(depth2(t)==4)
assert(max2(map2(b34, (v:Int)=> v*v))==16)
}