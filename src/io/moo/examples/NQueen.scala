package io.moo.examples

class NQueen(n: Int) {
  def isSafe(row: Int, solution: List[Int]): Boolean = {
    def isViolating(row: Int, solution: List[Int], backtracker: Int): Boolean = solution match {
      case head :: tail if head == row || (head + backtracker) == row || (head - backtracker) == row => true
      case head :: tail => isViolating(row, tail, backtracker + 1)
      case _ => false
    }
    (row < n && !isViolating(row, solution, 1))
  }

  def findNextSafe(row: Int, solution: List[Int]): Option[Integer] = 
    if (row < n) isSafe(row, solution) match {
	    case true => Some(row)
	    case _ => findNextSafe(row + 1, solution)
  } else None

  def solve(row: Int, solution: List[Int]): List[Int] = if (solution.length < n)
    findNextSafe(row, solution) match {
      case Some(x) => solve(0, x :: solution)
      case _ => solution match {
        case x :: y :: tail => solve(y + 1, tail)
        case List(x) => solve(x + 1, Nil)
      }
    }
  else solution

  def start() = solve(0, Nil)
}

object NQueen {
  def main(args: Array[String]) {
    println(new NQueen(5) start)
  }
}
