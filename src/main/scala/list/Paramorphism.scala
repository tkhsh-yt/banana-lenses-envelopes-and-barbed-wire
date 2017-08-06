package list

import Lazy._
import List._

object Paramophisms {

  def numParamorphism[B](n: Int)(b: B)(f: (Int, B) => B): B =
    n match {
      case 0 => b
      case n => f(n-1, numParamorphism(n-1)(b)(f))
    }

  def fac(n: Int): Int =
    numParamorphism(n)(1) { (n, m) => (1 + n) * m }

  def listParamorphism[A, B](list: List[A])(b: B)(f: (A, (List[A], B)) => B): B =
    list match {
      case Nil()       => b
      case Cons(a, as) => f(a, (eval(as), listParamorphism(eval(as))(b)(f)))
    }

  def tails[A](list: List[A]): List[List[A]] =
    listParamorphism(list)(cons[List[A]](nil, nil)) { (a, t) =>
      val (as, tls) = t
      cons(cons(a, as), tls)
    }
}
