package com.bagdemir.funcprog.examples

object StreamWorkbench {

  Stream(1, 2, 3, 4).takeWhile2(_ % 2 == 0) toList//> res0: List[Int] = List(2, 4)
}