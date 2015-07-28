package de.ummels.dijkstra

import scala.collection.mutable

/** Implementation of Dijkstra\s algorithm using mutable maps. */
object DijkstraMutable extends Dijkstra {
  def dijkstra[N](g: Graph[N])(source: N): (Map[N, Int], Map[N, N]) = {
    val active = mutable.Set(source)
    val res = mutable.Map(source -> 0)
    val pred = mutable.Map.empty[N, N]
    while (active.nonEmpty) {
      val node = active.minBy(res)
      active -= node
      val cost = res(node)
      for ((n, c) <- g(node)) {
        val cost1 = cost + c
        if (cost1 < res.getOrElse(n, Int.MaxValue)) {
          active += n
          res += (n -> cost1)
          pred += (n -> node)
        }
      }
    }
    (res.toMap, pred.toMap)
  }

  override def toString = "DijkstraMutable"
}
