package list

import Lazy._

// A* ::= Nil | Cons (A||A*)
sealed trait List[A] { self =>

  import List.{cons, nil}

  def take(n: Int): List[A] =
    n > 0 match {
      case false => nil
      case true  =>
        self match {
          case Nil()       => nil
          case Cons(a, as) => cons(a, eval(as).take(n-1))
        }
    }

  // Start Catamorphism
  def cata[B](b: => B)(f: (A, B) => B): B = self match {
    case Nil()       => b
    case Cons(a, as) => f(a, eval(as).cata(b)(f))
  }

  def length: Int =
    self.cata(0) { (a, n) => 1 + n }

  def filter(p: A => Boolean): List[A] =
    self.cata(nil[A]) { (a, as) =>
      p(a) match {
        case true  => cons(a, as)
        case false => as
      }
    }
  // End Catamorphism
  
  // Start Anamorphism
  def zip[B](bs: List[B]): List[(A, B)] = {
    def g(seed: (List[A], List[B])): ((A, B), (List[A], List[B])) = {
      val (Cons(a, as), Cons(b, bs)) = seed
      ((a, b), (eval(as), eval(bs)))
    }

    def p(seed: (List[A], List[B])): Boolean = {
      val (as, bs) = seed
      as == Nil() || bs == Nil()
    }

    List.ana((self, bs))(g)(p)
  }
  // // End Anamorphism
}
case class Nil[A]() extends List[A] {

  override def toString = "[]"
}
case class Cons[A](a: A, as: Thunk[List[A]]) extends List[A] {

  private def toString(as: Thunk[List[A]], acc: String): String = as() match {
    case Nil()       => acc
    case Cons(a, as) => toString(as, acc + ", " + a)
  }

  override def toString = "[" + toString(as, a.toString) + "]"
}

object List {

  def cons[A](hd: => A, tl: => List[A]): List[A] = {
    lazy val head = hd
    lazy val tail = tl

    Cons(head, thunk(tail))
  }

  def nil[A]: List[A] = Nil()
  
  // Start Anamorphism
  def ana[A, B](b: => B)(g: B => (A, B))(p: B => Boolean): List[A] =
    p(b) match {
      case true  => nil
      case false =>
        val (a, bb) = g(b)
        cons(a, ana(bb)(g)(p))
    }

  def iterate[A](a :A)(f: A => A): List[A] = {
    def g(seed: A): (A, A)  = (seed, f(seed))
    def p(seed: A): Boolean = false

    List.ana(a)(g)(p)
  }
  // End Anamorphism

  // Start Hylomorphism
  def hylo[A, B, C](c: => C, f: (B, C) => C)(a: => A, g: A => (B, A), p: A => Boolean): C =
    p(a) match {
      case true  => c
      case false =>
        val (b, aa) = g(a)
        f(b, hylo(c, f)(aa, g, p))
    }

  def fac(n: Int): Int = {
    def f(b: Int, c: Int): Int = b * c
    def g(n: Int): (Int, Int) = (n, n - 1)
    def p(n: Int): Boolean = n == 0

    hylo(1, f)(n, g, p)
  }
  // End Hylomorphism
}
