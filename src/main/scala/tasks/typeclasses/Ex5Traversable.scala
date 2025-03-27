package u04lab

import u03.Sequences.*
import Sequence.*
import u03.Optionals.Optional
import u04lab.Ex5Traversable.{given, *}


/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:
  trait Traversable[T[_]]:
    extension [A](a: T[A]) def consumer(f: A => Unit): Unit

  given Traversable[Optional] with
    extension [A](a: Optional[A]) def consumer(f: A => Unit): Unit = a match
      case Optional.Just(v) => f(v)
      case _ =>

  given Traversable[Sequence] with
    extension [A](s:Sequence[A]) def consumer (f: A => Unit): Unit = s match
      case Sequence.Cons(h, t) => f(h); t.consumer(f)
      case Sequence.Nil() =>

  def log[A](a: A): Unit = println("The next element is: " + a)

@main def tryTraversables =
  val seq = Cons(10, Cons(20, Cons(30, Nil())))
  println:
    seq.consumer(log) // 10,20,30
    seq.consumer(println)

  val opt = Optional.Just(3)
  opt.consumer(log) //3
  opt.consumer(println)