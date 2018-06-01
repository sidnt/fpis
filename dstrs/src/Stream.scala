package fpis.dstrs

trait Strm[+A] {
    def uncons: Option[(A, Strm[A])]
    def isEmpty = uncons.isEmpty

    def toList: List[A] = this.uncons match {
        case Some((a, sa)) => Cons(a, sa.toList)
        case None => Nil
    }

    def take(n:Int): Strm[A] = if(n > 0) this.uncons match {
        case Some((a,sa)) => Strm.cons(a, sa.take(n-1))
        case None => Strm.empty
    } else Strm.empty

    // def take(n:Int): Strm[A] = n match {
    //     case n:Int if n < 0 => Strm.empty
    //     case n:Int if n == 0 => this
    //     case n:Int if n > 0 => this.uncons match {
    //         case Some((a,sa)) => Strm.cons(a, sa.take(n-1))
    //         case None => Strm.empty
    //     }
    // }

    // def take(n:Int): Strm[A] = (n, this.uncons) match {
    //     case (n:Int if n > 0 , Some((a,sa)) ) => cons(a, sa.take(n-1))
    //     case (n:Int if n > 0, None) => empty
    //     case (n:Int if n == 0, _) => this
    //     case (n:Int if n < 0, _) => None
    // }

    def takeWhile(p: A => Boolean): Strm[A] = this.uncons match {
        case Some((a,sa)) if p(a) => Strm.cons(a, sa.takeWhile(p))
        case None => Strm.empty
    }
    
}

object Strm {
    def empty[A] = new Strm[A] {
        def uncons = None
    }

    def cons[A](hd: => A, tl: => Strm[A]) = new Strm[A] {
        def uncons = Some((hd, tl))
    }

    def  apply[A](as: A*): Strm[A] = {
        if(as.isEmpty) empty
        else cons(as.head, apply(as.tail: _*))
    }
}