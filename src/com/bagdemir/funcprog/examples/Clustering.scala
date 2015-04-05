package com.bagdemir.funcprog.examples

import scala.io.Source

/**
 * Not the most efficient, but the declerative way to solve k-clustering problem.
 * The program calculates the max-spacing between the clusters. You can change the
 * number of the clusters by setting a new value to "kCluster". The input file
 * should have the following form:
 *
 * number-of-vertices
 * node-1 node-2 edge-cost
 * node-1 node-2 edge-cost
 * ...
 *
 * Example input file:
 *
 * 9
 * 1 2 1000
 * 1 3 1000
 * 1 4 1414
 * 1 5 5385
 * 1 6 5000
 * 1 7 7280
 * 1 8 8246
 * 1 9 8062
 * 2 3 1414
 * 2 4 1000
 * 2 5 5010
 * 2 6 4472
 * 2 7 6325
 * 2 8 7280
 * 2 9 7071
 * 3 4 1000
 * 3 5 4472
 * 3 6 4240
 * 3 7 7071
 * 3 8 8062
 * 3 9 8000
 * 4 5 4123
 * 4 6 3606
 * 4 7 6083
 * 4 8 7071
 * 4 9 7000
 * 5 6 1414
 * 5 7 5831
 * 5 8 6708
 * 5 9 7211
 * 6 7 4472
 * 6 8 5385
 * 6 9 8831
 * 7 8 1000
 * 7 9 1414
 * 8 9 1000
 *
 * Should give:
 *
 *  For 2 clusters: 4472
 *  For 3 clusters: 3606
 *  For 4 clusters: 1414
 *
 */
object Clustering {
  val fileName = "<your-file>"
  val seperator = " "
  val kCluster = 2
  val numberOfVertices = Source.fromFile(fileName).getLines.next().toInt

  /* Read the edges from the file */
  val edges = for (line <- Source.fromFile(fileName).getLines.drop(1)) yield {
    val arr = line.split(seperator)
    Edge(Vertex(arr(0).toInt), Vertex(arr(1).toInt), arr(2).toInt)
  }

  case class Vertex(id: Int)
  case class Edge(u: Vertex, v: Vertex, cost: Int)
  case class Cluster(vertices: List[Vertex]) {
    def add(v: Vertex) = new Cluster(v :: vertices)
    def contains(v: Vertex) = vertices.contains(v)
    def merge(other: Cluster): Cluster = new Cluster(other.vertices ::: vertices)
  }

  /* finds the cluster to which the vertex belongs to. */
  def getClusterWith(v: Vertex, clusters: List[Cluster]) = clusters.find(_.contains(v))

  /* merges two clusters */
  def mergeClusters(e: Edge, clusters: List[Cluster]): List[Cluster] = {
    def go(c1: Option[Cluster], c2: Option[Cluster], c: List[Cluster], acc: List[Cluster]): List[Cluster] = c match {
      case head :: tail if head.contains(e.u) && !head.contains(e.v) => go(Some(head), c2, tail, acc)
      case head :: tail if head.contains(e.v) && !head.contains(e.u) => go(c1, Some(head), tail, acc)
      case head :: tail if !head.contains(e.v) && !head.contains(e.u) => go(c1, c2, tail, head :: acc)
      case head :: tail if head.contains(e.v) && head.contains(e.u) => clusters
      case _ => c1.get.merge(c2.get) :: acc
    }
    go(None, None, clusters, Nil)
  }

  def findMaxSpacing(e: List[Edge], clusters: List[Cluster], minSpace: Int): Int = e match {
    case head :: tail if getClusterWith(head.u, clusters) != getClusterWith(head.v, clusters) => if (head.cost <= minSpace) findMaxSpacing(tail, clusters, head.cost) else findMaxSpacing(tail, clusters, minSpace)
    case head :: tail if getClusterWith(head.u, clusters) == getClusterWith(head.v, clusters) => findMaxSpacing(tail, clusters, minSpace)
    case _ => minSpace
  }

  def findSpacing(e: List[Edge], acc: List[Edge], k: Int, clusters: List[Cluster]): Int = e match {
    case head :: tail if k > kCluster => {
      val newCluster = mergeClusters(head, clusters);
      findSpacing(tail, head :: acc, newCluster.length, newCluster)
    }
    case head :: tail if k == kCluster => findMaxSpacing(head :: tail, clusters, Int.MaxValue)
    case _ => -1
  }

  def main(args: Array[String]) = {
    /* sort edges in ascending order of edge cost */
    val sorted = edges.toList.sortBy(a => a.cost)
    /* initial cluster consists of only vertices */
    val clusters = for (v <- 1 to numberOfVertices) yield new Cluster(List(Vertex(v)))

    /* solve the problem */
    println(findSpacing(sorted.toList, Nil, numberOfVertices, clusters.toList))
  }
}
