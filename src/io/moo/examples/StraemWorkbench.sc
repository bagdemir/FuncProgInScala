package io.moo.exercises

object StraemWorkbench {
  sealed trait Stream[+A] {

    def toList(): List[A] = {
      @annotation.tailrec
      def toList(s: Stream[A], l: List[A]): List[A] = s match {
        case Empty => Nil
        case Cons(head, tail) => toList(tail(), head() :: l)
      }
      toList(this, Nil)
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
      println("apply")
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    }
  }
  
  Stream(1,2,3,4).toList                          //> apply
                                                  //| apply
                                                  //| apply
                                                  //| apply
                                                  //| apply
                                                  //| res0: List[Int] = List()
}