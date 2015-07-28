package de.ummels.dijkstra

/** Implementation of `PathFinder` using repeated calls to `dijkstra`. */
object PathFinderDijkstra extends PathFinder {

  def simplePaths[N](g: Graph[N])(source: N, target: N): Stream[List[N]] = {
    // Order paths by their accumulated cost
    implicit val ordPath: Ordering[(List[N],Int)] =
      Ordering.by[(List[N], Int), Int](_._2)

    val dijkstra = (g: Graph[N]) => DijkstraPriority.dijkstra(g)(source)

    /** Returns a sorted stream of "backward paths" in `g` starting in `n`
      * and ending in `source`, together with their price.
      */
    def go(g: Graph[N])(n: N): Stream[(List[N], Int)] = {
      def prepend(suffix: (List[N], Int)): (List[N], Int) = suffix match {
        case (p, c) => (n :: p, c + g(p.head)(n))
      }

      if (n == source) Stream(List(source) -> 0)
      else {
        val (_, pred) = dijkstra(g)
        pred.get(n) match {
          case None => Stream.empty
          case Some(n1) =>
            val paths1 = go(g - n)(n1) map prepend // all paths through n -> n1
            lazy val paths2 = go(g - (n1, n))(n) // all paths avoiding n -> n1
            paths1.head #:: Util.mergeStreams(paths1.tail, paths2)
        }
      }
    }

    go(g)(target) map { case (p, _) => p.reverse }
  }

  override def toString = "PathFinderDijkstra"
}
