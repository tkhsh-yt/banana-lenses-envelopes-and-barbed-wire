import list._

import Lazy._

import List._

object Main extends App {
  val list_1   = cons(1, nil)
  val list_12  = cons(1, cons(2, nil))
  val list_123 = cons(1, cons(2, cons(3, nil)))
  println(nil)
  println(list_1)
  println(list_12)
  
  // Start Catamorphism
  println(list_12.cata(0)(_ + _))
  
  println(list_1.length)
  println(list_12.length)

  println(list_123.filter(_ % 2 == 0))
  println(list_123.filter(_ % 2 == 1))
  // End Catamorphism

  // Start Anamorphism
  println(list_123.zip(list_12))
  println(List.iterate((_: Int) + 1)(0).take(10))
  // // End Anamorphism
}
