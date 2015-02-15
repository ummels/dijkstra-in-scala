package de.ummels.dijkstra

/** A trait for finding all simple paths in a graph. */
trait PathFinder {
  /** Returns a stream containing all paths from `source` to `target`,
    * ordered by their cost.
    */
  def simplePaths[N](g: Graph[N])(source: N, target: N): Stream[List[N]]
}
