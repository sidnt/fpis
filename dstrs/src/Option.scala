package fpis.dstrs

sealed trait Option[+A] {
    
    def map[B](f:A=>B):Option[B] = this match {
        case None => None
        case Some(a) => Some(f(a))
    }

    def flatMap[B](f:A=>Option[B]):Option[B] = this map f getOrElse None
    //flatMap can be implemented in terms of map.
    //How about other way round? Can map be implemented in terms of flatMap, if flatMap were implemented via patmat

    /*def flatMap[B](f:A=>Option[B]):Option[B] = this match {
        case None => None
        case Some(a) => f(a)
    }*/

    def getOrElse[B>:A](default: =>B):B = this match {
        case None => default
        case Some(a) => a
    }

    /*def orElse[B>:A](ob: =>Option[B]):Option[B] = this match {
        case None => ob
        case _ => this
    }*/

    def orElse[B>:A](ob: =>Option[B]):Option[B] = this.map(Some(_)).getOrElse(ob)
    //def orEls [B>:A](ob: =>Option[B]):Option[B] = can implement in terms of fmap?

    /*def filter(f:A=>Boolean):Option[A] = this match {
        case None => None
        case Some(a) => if(f(a)) this else None
    }*/

    def filter(f:A=>Boolean):Option[A] = this.flatMap( (a:A) => {if(f(a)) this else None:Option[A]} )
}

object Option {
    def map2[A,B,C](oa:Option[A], ob:Option[B], f: (A,B) => C): Option[C] = for {
        a <- oa
        b <- ob
    } yield f(a,b)

    def map2b[A,B,C](oa:Option[A], ob:Option[B], f: (A,B) => C): Option[C] = {
        oa flatMap { a =>
            ob map { b =>
                f(a,b)
            }
        }
    }
}

case object None extends Option[Nothing]
case class Some[+A](get:A) extends Option[A]