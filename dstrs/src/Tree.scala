package fpis.dstrs

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A],right:Tree[A]) extends Tree[A]

object Tree {

    def size[A](t:Tree[A]):Int = t match {
        case Leaf(_) => 1
        case Branch(l,r) => size(l) + size(r)
    }

    def max(t:Tree[Int]):Int = t match {
        case Branch(Leaf(l),Leaf(r)) => l max r
        case Branch(l,r) => {
            val maxl = max(l)
            val maxr = max(r)
            maxl max maxr
        }
        case Leaf(v) => v
    }

    def depth[a](t:Tree[a]):Int = t match {
        case Leaf(v) => 0
        case Branch(l,r) => (depth(l) max depth(r)) + 1
    }


    def map[a,b](t:Tree[a], f:a=>b):Tree[b] = t match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l,r) => Branch(map(l,f),map(r,f))
    }
    

    def fold[A,B](t:Tree[A], z:A=>B)(f:(B,B)=>B):B = t match {
        case Leaf(v) => z(v)
        case Branch(l,r) => f( fold(l,z)(f), fold(r,z)(f) )
    }

    def size2[A](t:Tree[A]):Int = fold( t, (x:A) => 1) ( (l,r)=> l + r )

    def depth2[A](t:Tree[A]):Int = fold(t, (x:A) => 0) ( (l,r)=> (l max r) + 1)

    def max2(t:Tree[Int]):Int = fold(t, (x:Int) => x) ((l,r) => l max r)

    def map2[A,B](t:Tree[A], f:A=>B):Tree[B] = fold( t, (x:A)=>Leaf(f(x)).asInstanceOf[Tree[B]] ) ( (b,c)=>Branch(b,c) )


}