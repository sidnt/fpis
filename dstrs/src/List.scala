package fpis.dstrs

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def apply[A](args: A*): List[A] = {
        if(args.isEmpty) Nil
        else Cons(args.head, apply(args.tail: _*))
    }

    def tail[A](xs:List[A]) = xs match {
        case Nil => throw new Error("tail of empty")
        case Cons(x,xs) => xs
    }

    def drop[A](n:Int, xs:List[A]):List[A] = 
        if(n > 0) drop(n-1, tail(xs))
        else xs
    
    def dropWhile[A](xs:List[A])(f: A => Boolean):List[A] = xs match {
        case Nil => Nil
        case Cons(x,xs1) => if(f(x)) dropWhile(xs1)(f) else xs
    }

    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
        case Nil => a2
        case Cons(h,t) => Cons(h, append(t, a2))
    }

    def setHead[A](x: A, xs:List[A]): List[A] = xs match {
        case Nil => Nil
        case Cons(y,ys) => Cons(x, ys)
    }

    def init[A](l: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(x,Nil) => Nil
        case Cons(x,xs) => Cons(x, init(xs))
    }

    def foldRight[A,B](as:List[A], z:B)(f: (A,B)=>B):B = as match {
        case Nil => z
        case Cons(x,xs) => f(x,foldRight(xs,z)(f))
    }
    def foldLeft[A,B]( as:List[A], z:B)(f: (B,A)=>B):B = as match {
        case Nil => z
        case Cons(x,xs) => foldLeft(xs, f(z,x))(f)
    }

    def sum(l:List[Int]):Int = foldLeft(l,0)(_ + _)
    def product(l:List[Double]):Double = foldLeft(l,1.0)(_*_)
    def length[A](l:List[A]):Int = foldLeft(l,0)((b,a)=> b + 1)

    def idL[A](as:List[A]):List[A] = foldLeft(as, Nil:List[A])((b,a)=>Cons(a,b))
    def idR[A](as:List[A]):List[A] = foldRight(as,Nil:List[A])(Cons(_,_))

    def rev1[A](as:List[A]):List[A] = as match {
        case Nil => Nil
        case Cons(x,xs) => append(rev1(xs), Cons(x,Nil))
    }

    def rev[A](as:List[A]):List[A] = idL(as) //can't you write reverse using foldRight?

    def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1,a2)(Cons(_,_))

    def concat[A](outer:List[List[A]]):List[A] = {
        
        def loop(o:List[List[A]], acc:List[A]):List[A] = o match {
            case Nil => rev(acc)
            case Cons(x,xs) => loop(xs, foldLeft(x, acc)((b,a)=>Cons(a,b)))
        }

        loop(outer, Nil)
    }

    def add1(as:List[Int]):List[Int] = as match {
        case Nil => Nil
        case Cons(x,xs) => Cons(x + 1, add1(xs))
    }

    def dts(as:List[Double]):List[String] = as match {
        case Nil => Nil
        case Cons(x,xs) => Cons(x.toString, dts(xs))
    }

    def map[A,B](l: List[A])(f: A => B): List[B] = l match {
        case Nil => Nil
        case Cons(x,xs) => Cons(f(x), map(xs)(f))
    }

    def filter[A](l:List[A])(f:A=>Boolean):List[A] = l match {
        case Nil => Nil
        case Cons(x,xs) => if(f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
    }
    def filter2[A](l:List[A])(f:A=>Boolean):List[A] = l 

    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))

    def addCorr(a:List[Int],b:List[Int]):List[Int] = (a,b) match {
        case (Cons(x,xs),Cons(y,ys)) => Cons(x+y, addCorr(xs,ys))
        case _ => Nil
    }

    def mapCorr[A](a1:List[A],a2:List[A])(f:(A,A)=>A):List[A] = (a1,a2) match {
        case (Cons(x,xs),Cons(y,ys)) => Cons(f(x,y), mapCorr(xs,ys)(f))
        case _ => Nil
    }

    def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {

        def loop[A](l:List[A], sub:List[A], found:Boolean):Boolean = (l,sub) match {

            case (Nil,Nil) => found
            case (Nil, Cons(_,_)) => false
            case (Cons(_,_), Nil) => found
            case (Cons(l,ls),Cons(s,ss)) => if(l==s) loop(ls,ss, found && true) else loop(ls,sub, true)
            
        }

        loop(l,sub,true)

    }

}
/*
import fpis.dstrs.List
*/