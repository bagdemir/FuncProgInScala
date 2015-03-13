package com.bagdemir.funcprog.examples

object StreamWorkbench {

  sealed trait CStream[+A] {

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def toList(): List[A] = {
      @annotation.tailrec
      def toList(s: CStream[A], acc: List[A]): List[A] = s match {
        case Empty => acc
        case Cons(head, tail) => toList(tail(), head() :: acc)
      }
      toList(this, Nil) reverse
    }

    def take(n: Int): CStream[A] = this match {
      case Cons(head, tail) if n > 0 => Cons(() => head(), () => tail().take(n - 1))
      case _ => Empty
    }

    def drop(n: Int): CStream[A] = this match {
      case Cons(head, tail) if n <= 0 => Cons(() => head(), () => tail().drop(n - 1))
      case Cons(_, tail) => tail().drop(n - 1)
      case _ => Empty
    }

    def takeWhile(p: A => Boolean): CStream[A] = this match {
      case Cons(head, tail) if p(head()) => Cons(() => head(), () => tail().takeWhile(p))
      case Cons(_, tail) => tail().takeWhile(p)
      case _ => Empty
    }

    def takeWhile2(p: A => Boolean): CStream[A] = foldRight[CStream[A]](Empty)((a, b) => p(a) match {
      case true => Cons(() => a, () => b)
      case _ => b
    })

    def forAll(p: (A) => Boolean): Boolean = foldRight(false)((a, b) => p(a))
  }

  case object Empty extends CStream[Nothing]
  case class Cons[+A](h: () => A, t: () => CStream[A]) extends CStream[A]

  object CStream {
    def cons[A](hd: => A, tl: => CStream[A]): CStream[A] = {
      lazy val head = hd;
      lazy val tail = tl;
      Cons(() => head, () => tail)
    }

    def empty[A]: CStream[A] = Empty

    def apply[A](as: A*): CStream[A] = {
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    }
  }

  CStream(1, 2, 3, 4).takeWhile2(_ % 2 == 0) toList
                                                  //> res0: List[Int] = List(2, 4)
}