package fpis.dstrs

object EitherCalx {

    def mean(xs:IndexedSeq[Double]): Either[String, Double] = 
        if (xs.isEmpty) Left("mean of empty sequence")
        else Right(xs.sum/xs.length)

    def safeDiv(x:Double,y:Double):Either[Exception,Double] = 
        try   { Right(x/y) }
        catch { case e:Exception => Left(e) }

    def sequenceOld[E,A](a: List[Either[E,A]]): Either[E, List[A]] = {
        /** imagine a number of objects arranged in line
        they could be either left[e] or right[a]
        .
        we can't just say Right[A] for the result type
        because then it wouldn't include the case where we encounter a Left[E]
        . so we've to be complete by saying Either in the result type 
        
        another dilemma we face here is 
        which Left instance to choose, if they are more than one*/

        val z: Either[E,List[A]] = Right(Nil)
        def ff(ea: Either[E,A], ela: Either[E,List[A]]): Either[E,List[A]] = (ea,ela) match {
            case (Right(a), Right(Cons(h,t))) => Right(Cons(a,Cons(h,t)))
            case (Right(a), Right(Nil)) => Right(Cons(a,Nil))
            case (Right(a), Left(e) ) => Left(e)
            case (Left(e), _) => Left(e) // we can choose to make a selection about which left
        }

        List.foldRight(a,z)(ff)

    }

    def traverse[E, A, B](la: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {

        val z: Either[E,List[B]] = Right(Nil)

        def ff(a: A, elb: Either[E,List[B]]): Either[E,List[B]] = (f(a),elb) match {
            case (Right(b), Right(Cons(h,t))) => Right(Cons(b,Cons(h,t)))
            case (Right(b), Right(Nil)) => Right(Cons(b,Nil))
            case (Right(b), Left(e) ) => Left(e)
            case (Left(e), _) => Left(e) // we can choose to make a selection about which left
        }

        List.foldRight(la,z)(ff)

    }

    //def sequence[ A ](a: List[Option[ A ]]): Option[List[A]] = traverse(a)(e => e)
    def sequence[E,A](a: List[Either[E,A]]): Either[E, List[A]] = traverse(a)(e => e)
}