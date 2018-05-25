package fpis.dstrs
import scala.collection.immutable.{List => oList}
import Calcs._

object OptionTests extends App {
    val d = 2.3
    val od:Option[Double] = Some(d)
    val a = Cons(d,Nil)

    val tests = oList(
        (od map ((x:Double) => x.toInt)) == Some(2),
        (od.filter(_ < 0.0) == None),
        (od.filter(_ > 0.0) == od),
        sequence(Cons(od,Nil)) == Some(a)
    )

    tests foreach assert
    println(sequence(List(od)))
}