package io.moo.exercises

object StraemWorkbench {

  sealed trait Stream[+A] {

    def toList(): List[A] = {
      @annotation.tailrec
      def toList(s: Stream[A], acc: List[A]): List[A] = s match {
        case Empty => acc
        case Cons(head, tail) => toList(tail(), head() :: acc)
      }
      toList(this, Nil) reverse
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(head, tail) if n > 0 => Cons(() => head(), () => tail().take(n - 1))
      case _ => Empty
    }
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
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

  Stream(1, 2, 3, 4).take(3) toList               //> res0: List[Int] = List(1, 2, 3)
}