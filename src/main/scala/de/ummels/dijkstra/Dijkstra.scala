package de.ummels.dijkstra

import de.ummels.dijkstra.Util._

/** A trait for Dijkstra's algorithm. */
trait Dijkstra {
  /** Runs Dijkstra's algorithm on the given graph from the given source node.
    *
    * Returns a map with distances from the source and another map that maps
    * each node reachable from the source to a predecessor that lies on the
    * shortest path from the source.
    *
    * If a node is reachable from the source, neither of the two returned maps
    * contains that node as a key.
    */
  def dijkstra[N](g: Graph[N])(source: N): (Map[N, Int], Map[N, N])

  /** Optionally computes a shortest path between two given nodes in a graph.
    *
    * Returns a `Some` with a shortest path from `source` to `target` if `target`
    * is reachable from `source`; returns `None` otherwise.
    */
  def shortestPath[N](g: Graph[N])(source: N, target: N): Option[List[N]] = {
    val pred = dijkstra(g)(source)._2
    if (pred.contains(target) || source == target) Some(iterateRight(target)(pred.get))
    else None
  }
}
