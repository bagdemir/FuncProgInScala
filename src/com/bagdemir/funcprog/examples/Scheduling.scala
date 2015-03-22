package com.bagdemir.funcprog.examples

import scala.io.Source

object Scheduling {
  case class Schedule(weight: Int, len: Int, score: Double)

  def scoreAlgo1(x: Int, y: Int): Double = (x - y).toDouble
  def scoreAlgo2(x: Int, y: Int): Double = x.toDouble / y.toDouble
  val fileName = "/Users/bagdemir/Desktop/jobs.txt"
  val sep = " "

  def data(algo: (Int, Int) => Double) = for (line <- Source.fromFile(fileName).getLines if line.contains(sep)) yield {
    val arr = line.split(sep)
    Schedule(arr(0).toInt, arr(1).toInt, algo(arr(0).toInt, arr(1).toInt))
  }

  def sumCompletionTimes(lengths: List[Schedule], currentComp: Long, result: Long): Long =
    lengths match {
      case head :: tail => sumCompletionTimes(tail, currentComp + head.len, ((currentComp + head.len) * head.weight) + result)
      case _ => result
    }

  def go(source: List[Schedule],
    acc1: List[Schedule], acc2: List[Schedule], score: Option[Double]): List[Schedule] = source match {
    case head :: tail => score match {
      case Some(x) if x == head.score => go(tail, head :: acc1, acc2, score)
      case Some(x) if x != head.score => go(tail, head :: Nil, acc1.sortBy(_.weight) ::: acc2, Some(head.score))
      case None => go(tail, head :: acc1, acc2, Some(head.score))
    }
    case _ => acc1.sortBy(_.weight) ::: acc2
  }

  def main(args: Array[String]) {

    val result1 = go(data(scoreAlgo1).toList.sortBy(_.score).reverse, Nil, Nil, None).reverse
    val result2 = go(data(scoreAlgo2).toList.sortBy(_.score).reverse, Nil, Nil, None).reverse

    println("difference:" + sumCompletionTimes(result1, 0, 0))
    println("ratio:" + sumCompletionTimes(result2, 0, 0))
  }
}