package de.ummels.dijkstra

import org.scalacheck.Gen

object Graphs {
  def chain(size: Int): Graph[Int] = (x: Int) => x match {
    case n if n >= 0 && n < size => Map((n + 1) -> 1, (n + 2) -> 3, (n - 1) -> 1)
    case _ => Map.empty
  }

  def tree(depth: Int): Graph[List[Boolean]] = (x: List[Boolean]) => x match {
    case l if l.length < depth => Map((true :: l) -> 1, (false :: l) -> 2)
    case l if l.length == depth => Map(Nil -> 1)
    case _ => Map.empty
  }

  def randomGraph(nodes: Int, outDegree: Int, maxCost: Int): SimpleGraph[Int] = {
    val genEdge = Gen.zip(Gen.choose(0, nodes - 1), Gen.choose(0, maxCost))
    val genNeighbours = Gen.mapOfN(outDegree, genEdge)
    val genGraph = Gen.sequence[Map[Int, Map[Int, Int]], (Int, Map[Int, Int])] {
      (0 until nodes) map (i => Gen.zip(i, genNeighbours))
    }
    SimpleGraph(genGraph.sample.get)
  }
}
