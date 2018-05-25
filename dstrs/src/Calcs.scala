package fpis.dstrs
import Option._
import java.util.regex._

object Calcs {

    def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

    //variance is the mean of (x-m)^2 for all x using mean and fmap
    def variance(xs: Seq[Double]): Option[Double] = {
        val oMean: Option[Double] = mean(xs)
        oMean flatMap {
            m => mean(xs.map(e => math.pow((e-m),2)))
        }
        
    }

    def pattern(pat:String): Option[Pattern] = {
        try {
            Some(Pattern.compile(pat))
        }
        catch {
            case e:PatternSyntaxException => None
        }
    }

    def mkMatcher_1(pat: String): Option[String => Boolean] =
        for {
            p <- pattern(pat)
        } yield p.matcher(_).matches

    def doesMatch(pat:String, s:String):Option[Boolean] = 
        for {
            m <- mkMatcher_1(pat)
        } yield m(s)

//  map2[A,B,C](oa:Option[A], ob:Option[B], f: (A,B) => C): Option[C]
//  [String=>Boolean, String=>Boolean , Boolean]
    def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = {
        def m1 = mkMatcher_1(pat1)
        def m2 = mkMatcher_1(pat2)
        def f(f1:String=>Boolean,f2:String=>Boolean): Boolean = f1(s) && f2(s)
        map2(m1,m2, f)//()  (m1(s) && m2(s)) )
    }

    def sequenceOld[A](a: List[Option[A]]): Option[List[A]] = {
        val z: Option[List[A]] = Some(Nil)
        def ff(oa: Option[A], ola: Option[List[A]]):Option[List[A]] = (oa,ola) match {
            case (Some(a), Some(Cons(h,t))) => Some(Cons(a,Cons(h,t)))
            case (Some(a), Some(Nil)) => Some(Cons(a,Nil))
            case (Some(a), None) => None
            case (None, _) => None
        }
        List.foldRight(a,z)(ff)
    }

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {

        val z: Option[List[B]] = Some(Nil)
        def ff(a:A, olb: Option[List[B]]):Option[List[B]] = (f(a), olb) match {
            case (Some(b), Some(Cons(h,t))) => Some(Cons(b,Cons(h,t)))
            case (Some(b), Some(Nil)) => Some(Cons(b,Nil))
            case (Some(b), None) => None
            case (None, _) => None
        }
        List.foldRight(a,z)(ff)
    }

    def sequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(e => e)

}