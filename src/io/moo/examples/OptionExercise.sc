package io.moo.exercises

object OptionExercise {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  sealed trait Stream[+A]
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

  Stream(1, 2, 3, 4)                              //> apply
                                                  //| res0: io.moo.exercises.OptionExercise.Stream[Int] = Cons(<function0>,<functi
                                                  //| on0>)

}