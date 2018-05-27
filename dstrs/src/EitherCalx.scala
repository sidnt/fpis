package fpis.dstrs

object EitherCalx {

    def mean(xs:IndexedSeq[Double]): Either[String, Double] = 
        if (xs.isEmpty) Left("mean of empty sequence")
        else Right(xs.sum/xs.length)

    def safeDiv(x:Double,y:Double):Either[Exception,Double] = 
        try   { Right(x/y) }
        catch { case e:Exception => Left(e) }

}