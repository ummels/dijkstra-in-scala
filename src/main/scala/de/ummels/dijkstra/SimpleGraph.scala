package de.ummels.dijkstra

/** Simple implementation of graphs using a map. */
case class SimpleGraph[N](succs: Map[N, Map[N, Int]]) extends Graph[N] {
  def apply(n: N) = succs.getOrElse(n, Map.empty)
  
  def reversed: SimpleGraph[N] = {
    val edges = for {
      (n, nbs) <- succs.toSeq
      succ <- nbs.toSeq
    } yield n -> succ
    val preds = (Map.empty[N, Map[N, Int]] /: edges){ (g, e) =>
      val (n1, (n2, c)) = e
      val nbs = g.getOrElse(n2, Map.empty[N, Int])
      g.updated(n2, nbs + (n1 -> c))
    }
    SimpleGraph(preds)
  }
}
