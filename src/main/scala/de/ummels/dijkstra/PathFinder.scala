package de.ummels.dijkstra

import scala.language.implicitConversions

/** A trait for finding all simple paths in a graph. */
trait PathFinder {
  /** Returns a stream containing all paths from `source` to `target`,
    * ordered by their cost.
    */
  def simplePaths[N](g: Graph[N])(source: N, target: N): Stream[List[N]]
}

/** Companion object for the PathFinder trait.
  *
  * Defines an implicit conversion which can be used to call `simplePaths`
  * as a method on `Graph` instances.
  *
  * To enable the implicit conversion, you can either mix in the trait
  * `PathFinder.Syntax` or import `PathFinder.syntax._`.
  */
object PathFinder {
  case class PathFinderOps[N](graph: Graph[N], instance: PathFinder) {
    def simplePaths(source: N, target: N): Stream[List[N]] =
      instance.simplePaths(graph)(source, target)
  }

  trait Syntax {
    implicit def toPathFinderOps[N](graph: Graph[N])(implicit pf: PathFinder): PathFinderOps[N] =
      PathFinderOps(graph, pf)
  }

  object syntax extends Syntax
}
