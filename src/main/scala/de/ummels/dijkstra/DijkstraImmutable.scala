package de.ummels.dijkstra

/** Implementation of Dijkstra's algorithm using immutable maps. */
object DijkstraImmutable extends Dijkstra {
  def dijkstra[N](g: Graph[N])(source: N): (Map[N, Int], Map[N, N]) = {
    def go(active: Set[N], res: Map[N, Int], pred: Map[N, N]): (Map[N, Int], Map[N, N]) =
      if (active.isEmpty) (res, pred)
      else {
        val node = active.minBy(res)
        val cost = res(node)
        val neighbours = for {
          (n, c) <- g(node) if cost + c < res.getOrElse(n, Int.MaxValue)
        } yield n -> (cost + c)
        val preds = neighbours mapValues (_ => node)
        go(active - node ++ neighbours.keys, res ++ neighbours, pred ++ preds)
      }

    go(Set(source), Map(source -> 0), Map.empty)
  }

  override def toString = "DijkstraImmutable"
}
