package de.ummels.dijkstra

import de.ummels.prioritymap.PriorityMap

import scala.collection.mutable

object Dijkstra {
  import de.ummels.dijkstra.Graphs.Graph

  def iterateRight[N](x: N)(f: N => Option[N]): List[N] = {
    def go(x: N, acc: List[N]): List[N] = f(x) match {
      case None => x :: acc
      case Some(y) => go(y, x :: acc)
    }

    go(x, List.empty)
  }

  def dijkstra1[N](g: Graph[N])(source: N): (Map[N, Int], Map[N, N]) = {
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

  def shortestPath1[N](g: Graph[N])(source: N, target: N): Option[List[N]] = {
    val pred = dijkstra1(g)(source)._2
    if (pred.contains(target) || source == target) Some(iterateRight(target)(pred.get))
    else None
  }

  def dijkstra2[N](g: Graph[N])(source: N): (Map[N, Int], Map[N, N]) = {
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

  def shortestPath2[N](g: Graph[N])(source: N, target: N): Option[List[N]] = {
    val pred = dijkstra2(g)(source)._2
    if (pred.contains(target) || source == target) Some(iterateRight(target)(pred.get))
    else None
  }

  def dijkstra3[N](g: Graph[N])(source: N): (Map[N, Int], Map[N, N]) = {
    def go(active: PriorityMap[N, Int], acc: Map[N, Int], pred: Map[N, N]):
    (Map[N, Int], Map[N, N]) =
      if (active.isEmpty) (acc, pred)
      else {
        val (node, cost) = active.head
        val neighbours = for {
          (n, c) <- g(node) if !acc.contains(n) && cost + c < active.getOrElse(n, Int.MaxValue)
        } yield n -> (cost + c)
        val preds = neighbours mapValues (_ => node)
        go(active.tail ++ neighbours, acc + (node -> cost), pred ++ preds)
      }

    go(PriorityMap(source -> 0), Map.empty, Map.empty)
  }

  def shortestPath3[N](g: Graph[N])(source: N, target: N): Option[List[N]] = {
    val pred = dijkstra3(g)(source)._2
    if (pred.contains(target) || source == target) Some(iterateRight(target)(pred.get))
    else None
  }

  /** Merges a collection of streams into a single stream.
    *
    * Returns a sorted stream if all input streams are sorted.
    */
  def mergeStreams[A](streams: Traversable[Stream[A]])(implicit ord: Ordering[A]): Stream[A] = {
    val streams1 = streams.toList filterNot (_.isEmpty) sortBy (_.head)
    if (streams1.isEmpty)
      Stream.empty
    else {
      val first = streams1.head
      first.head #:: mergeStreams(first.tail :: streams1.tail)
    }
  }

  def simplePaths1[N](g: Graph[N])(source: N, target: N): Stream[List[N]] = {
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
}
