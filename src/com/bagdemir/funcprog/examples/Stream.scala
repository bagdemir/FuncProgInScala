package com.bagdemir.funcprog.examples

/**
 * Functional Programming in Scala - Chapter 5 Answer Key.
 * ISBN-10: 1617290653
 * ISBN-13: 978-1617290657
 */
object Stream {

  import scala.reflect.runtime.{ universe => ru }

  case class SimpleAnnotation() extends scala.annotation.StaticAnnotation

  @SimpleAnnotation
  case class ClassWithAnnotation()

  val simpleClassSymbol = ru.typeOf[ClassWithAnnotation].typeSymbol.asClass
  def anno = simpleClassSymbol.annotations

  sealed trait Stream[+A] {

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    // Example 5.1 (Page 69)
    def toList(): List[A] = {
      @annotation.tailrec
      def toList(s: Stream[A], acc: List[A]): List[A] = s match {
        case Empty => acc
        case Cons(head, tail) => toList(tail(), head() :: acc)
      }
      toList(this, Nil) reverse
    }
    // Example 5.2 (Page 70)
    def take(n: Int): Stream[A] = this match {
      case Cons(head, tail) if n > 0 => Cons(() => head(), () => tail().take(n - 1))
      case _ => Empty
    }
    
    // Example 5.2 (Page 70)
    def drop(n: Int): Stream[A] = this match {
      case Cons(head, tail) if n <= 0 => Cons(() => head(), () => tail().drop(n - 1))
      case Cons(_, tail) => tail().drop(n - 1)
      case _ => Empty
    }

    // Example 5.3 (Page 70)
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(head, tail) if p(head()) => Cons(() => head(), () => tail().takeWhile(p))
      case Cons(_, tail) => tail().takeWhile(p)
      case _ => Empty
    }

    // Example 5.4 (Page 71)
    def forAll(p: (A) => Boolean): Boolean = foldRight(false)((a, b) => p(a))
    
    // Example 5.5 (Page 71)
    def takeWhile2(p: A => Boolean): Stream[A] = foldRight[Stream[A]](Empty)((a, b) => p(a) match {
      case true => Cons(() => a, () => b)
      case _ => b
    })
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd;
    lazy val tail = tl;
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }
}
