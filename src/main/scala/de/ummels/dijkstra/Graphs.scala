package de.ummels.dijkstra

import org.scalacheck.Gen

object Graphs {
  type Graph[N] = N => Map[N, Int]

  def chain(size: Int): Graph[Int] = {
    case n if n >= 0 && n < size => Map((n + 1) -> 1, (n + 2) -> 3, (n - 1) -> 1)
    case _ => Map.empty
  }

  def tree(depth: Int): Graph[List[Boolean]] = {
    case l if l.length < depth => Map((true :: l) -> 1, (false :: l) -> 2)
    case l if l.length == depth => Map(Nil -> 1)
    case _ => Map.empty
  }

  def randomGraph(nodes: Int, outDegree: Int, maxCost: Int) = {
    val genEdge = Gen.zip(Gen.choose(0, nodes - 1), Gen.choose(0, maxCost))
    val genNeighbours = Gen.mapOfN(outDegree, genEdge)
    val genGraph = Gen.sequence[Map[Int, Map[Int, Int]], (Int, Map[Int, Int])] {
      (0 until nodes) map (i => Gen.zip(i, genNeighbours))
    }
    genGraph.sample.get withDefaultValue Map.empty[Int, Int]
  }

  def bfs[N](g: Graph[N])(source: N): Set[N] = {
    def go(initial: Set[N], acc: Set[N]): Set[N] =
      if (initial.isEmpty) acc
      else {
        val explored = acc ++ initial
        val neighbours = for {
          node <- initial
          (n, _) <- g(node) if !explored(n)
        } yield n
        go(neighbours, explored)
      }

    go(Set(source), Set.empty)
  }
}
