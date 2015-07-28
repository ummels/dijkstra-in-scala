package de.ummels.dijkstra

import de.ummels.prioritymap.PriorityMap

/** Implementation of `PathFinder` using a priority map.
  *
  * This implementation is rather slow, but elegant and concise.
  */
object PathFinderPriority extends PathFinder {
  def simplePaths[N](g: Graph[N])(source: N, target: N): Stream[List[N]] = {
    def go(initial: PriorityMap[List[N], Int]): Stream[List[N]] = initial.headOption match {
      case None => Stream.empty
      case Some((p, _)) if p.head == target => p #:: go(initial.tail)
      case Some((p, c)) =>
        val neighbours = for {
          (n, cost) <- g(p.head) if !p.contains(n)
        } yield (n :: p, c + cost)
        go(initial.tail ++ neighbours)
    }

    go(PriorityMap(List(source) -> 0)) map (_.reverse)
  }

  override def toString = "PathFinderPriority"
}
