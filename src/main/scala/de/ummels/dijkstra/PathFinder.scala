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
  * as methods on `Graph` instance.
  *
  * To enable the implicit conversion, you can either mix in the trait
  * `ToPathFinderOps` or import the member `toPathFinderOps`.
  */
object PathFinder {
  trait Ops[N] {
    def self: Graph[N]

    def pathFinderInstance: PathFinder

    def simplePaths(source: N, target: N): Stream[List[N]] =
      pathFinderInstance.simplePaths(self)(source, target)
  }

  trait ToPathFinderOps {
    implicit def toPathFinderOps[N](graph: Graph[N])(implicit pf: PathFinder): Ops[N] = new Ops[N] {
      def self = graph
      def pathFinderInstance = pf
    }
  }

  implicit def toPathFinderOps[N](graph: Graph[N])(implicit pf: PathFinder): Ops[N] = new Ops[N] {
    def self = graph
    def pathFinderInstance = pf
  }
}
