package fpis.dstrs

sealed trait Either[+E, +A] {
    def map[B](f:A=>B): Either[E,B] = this match {
        case Right(a) => Right(f(a))
        case Left(e) => Left(e) //saying `this` here throws type error because this is either[e,a] we want either[e,b]
    }

    def flatMap[EE >: E, B](f:A=>Either[EE, B]): Either[EE,B] = this match {
        case Right(a) => f(a)
        case Left(e) => Left(e)
        /** the important thing is that
        I conveniently thought that simply `this` would work 
        but we have to explicitly invoke the Left constructor
        .
        ask, why doesn't B need to be a supertype of A, as in orElse? 
        is it because here a mapping exists from A, while in orElse, it doesn't*/
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE,B] = this match {
        case Right(a) => this
        case Left(e) => b
        /** it is our requirement by design
        that we need an expression of
        choosing this either or another either, via the orElse method
        .
        and so, simultaneously, we don't have any control over what that another either might be
        so our type for another object is completely general
        .
        but our algebra should be available, no matter what occurs in a `orElse`
        and so the operations available should only be the intersection of operations avlbl on both
        and that is expressed via the type constraint expression*/
    }

    /** so contrary to the first case understanding
    that map mean a map from one thing to another
    a map, as shown here, could be from two things to another one */
    def map2[EE >: E, B, C](b: Either[EE,B])(f:(A,B)=>C): Either[EE,C] = (this,b) match {
        case (Right(a), Right(b)) => Right(f(a,b))
        case (Right(a), Left(lb)) => Left(lb) //try saying just `b` here
        case (Left(la), Right(b)) => Left(la)
        case (Left(la), Left(lb)) => Left(la) //does it matter, saying Left(la) or Left(lb) here?
        /** it doth not, atleast at typelevel, because at typelevel, the type constraint makes it fall in line
        because Left(la) is same type as Left(lb)
        at value level it makes a diffference as we are choosing one value over the other
        we could even collect both the left values, but we dont have a method for doing so
        .
        maybe another Either algebra could provision for the same*/
    }

}
case class Left[+E](value:E) extends Either[E, Nothing]
case class Right[+A](value:A) extends Either[Nothing, A]

