package fpis.dstrs

import List._

object Tests {
    //assert (List(1,2,3) == Cons(1,Cons(2,Cons(3,Nil))))
    assert(hasSubsequence(List(1,2,3),List(1,2,3)))
}