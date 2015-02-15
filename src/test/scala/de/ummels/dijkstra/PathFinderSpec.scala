package de.ummels.dijkstra

trait PathFinderSpec extends PropertySpec {

  import PathFinderSpec._

  val P: PathFinder

  property("simplePaths should return a non-empty stream of simple paths if and only if target is reachable from source") {
    forAll(inputs) { case (graph, source, target) =>
      val ps = P.simplePaths(graph)(source, target).take(50) // cutoff at 50 paths
      if (graph.isReachable(source, target)) {
        ps should not be empty
        ps.forall(isPath(graph)(_, source, target)) shouldBe true
      } else {
        ps shouldBe empty
      }
    }
  }

  property("simplePaths should return a stream of distinct paths, sorted by cost") {
    forAll(inputs) { case (graph, source, target) =>
      val ps = P.simplePaths(graph)(source, target).take(50) // cutoff at 50 paths
      ps map (_.distinct) shouldBe ps
      val costs = ps map cost(graph)
      costs shouldBe costs.sorted
    }
  }

  property("simplePaths should return all possible simple paths") {
    // Since this property is hard to check universally, we use an example graph here.
    val graph = Graphs.chain(10)

    val table = Table(
      ("source", "target", "count"),
      (0, 0, 1),
      (0, 1, 2),
      (0, 2, 3),
      (0, 3, 6),
      (0, 4, 11),
      (0, 5, 20),
      (0, 6, 37),
      (0, 7, 68),
      (0, 8, 125),
      (0, 9, 149),
      (1, 2, 3))

    forAll(table) { (source, target, count) =>
      P.simplePaths(graph)(source, target) should have length count
    }
  }
}

object PathFinderSpec {
  def isPath[N](graph: Graph[N])(path: List[N], source: N, target: N): Boolean = {
    def go(node: N, path: List[N]): Boolean = path match {
      case Nil => node == target
      case n :: t => (graph(node) contains n) && go(n, t)
    }

    path match {
      case Nil => false
      case n :: t => n == source && go(n, t)
    }
  }

  def cost[N](graph: Graph[N])(path: List[N]): Int = {
    def go(path: List[N], acc: Int): Int = path match {
      case Nil | List(_) => acc
      case n1 :: n2 :: t => go(path.tail, acc + graph(n1)(n2))
    }

    go(path, 0)
  }
}
