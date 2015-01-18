package de.ummels.dijkstra

import de.ummels.dijkstra.Dijkstra._
import org.scalacheck.Gen

class DijkstraSpec extends PropertySpec {

  import DijkstraSpec._

  property("the result of dijkstra* should contain all nodes reachable from source") {
    forAll(inputs) { case (graph, source, _) =>
      val reach = Graphs.bfs(graph)(source)
      dijkstra1(graph)(source)._1.keys shouldBe reach
      dijkstra2(graph)(source)._1.keys shouldBe reach
      dijkstra3(graph)(source)._1.keys shouldBe reach
    }
  }

  property("the result of dijkstra* should be consistent") {
    forAll(inputs) { case (graph, source, _) =>
      val test = (isConsistent(graph)(source)_).tupled
      test(dijkstra1(graph)(source)) shouldBe true
      test(dijkstra2(graph)(source)) shouldBe true
      test(dijkstra3(graph)(source)) shouldBe true
    }
  }

  property("simplePaths1 should return a non-empty stream of simple paths if and only if target is reachable from source") {
    forAll(inputs) { case (graph, source, target) =>
      val ps = simplePaths1(graph)(source, target).take(50) // cutoff at 50 paths
      if (isReachable(graph)(source, target)) {
        ps should not be empty
        ps.forall(isValid(graph)(_, source, target)) shouldBe true
      } else {
        ps shouldBe empty
      }
    }
  }

  property("simplePaths1 should return a stream of distinct paths, sorted by cost") {
    forAll(inputs) { case (graph, source, target) =>
      val ps = simplePaths1(graph)(source, target).take(50) // cutoff at 50 paths
      ps map (_.distinct) shouldBe ps
      val costs = ps map cost(graph)
      costs shouldBe costs.sorted
    }
  }

  property("simplePaths1 should return all possible simple paths") {
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
      simplePaths1(graph)(source, target) should have length count
    }
  }
}

object DijkstraSpec {
  import Graphs.Graph

  type IntGraph = Map[Int, Map[Int, Int]]

  def reversed(graph: IntGraph): IntGraph = {
    val edges = for {
      (n1, nb) <- graph.toSeq
      succ <- nb.toSeq
    } yield n1 -> succ
    val empty = Map.empty[Int, Map[Int, Int]] withDefaultValue Map.empty[Int, Int]
    (empty /: edges){ (g, e) =>
      val (n1, (n2, c)) = e
      val nb = g(n2)
      g.updated(n2, nb + (n1 -> c))
    }
  }

  def isConsistent(g: IntGraph)(source: Int)(cost: Map[Int, Int], pred: Map[Int, Int]): Boolean = {
    val rg = reversed(g)
    pred.keys.forall { n =>
        val p = pred(n)
        cost(n) == cost(p) + rg(n)(p) &&
        cost(n) == (for ((n1, c) <- rg(n) if cost.contains(n1)) yield cost(n1) + c).min
    }
  }

  def isReachable(graph: Graph[Int])(source: Int, target: Int): Boolean = {
    Graphs.bfs(graph)(source) contains target
  }

  def isValid(graph: Graph[Int])(path: List[Int], source: Int, target: Int): Boolean = {
    def go(node: Int, path: List[Int]): Boolean = path match {
      case Nil => node == target
      case n :: t => (graph(node) contains n) && go(n, t)
    }

    path match {
      case Nil => false
      case n :: t => n == source && go(n, t)
    }
  }

  def cost(graph: Graph[Int])(path: List[Int]): Int = {
    def go(path: List[Int], acc: Int): Int = path match {
      case Nil | List(_) => acc
      case n1 :: n2 :: t => go(path.tail, acc + graph(n1)(n2))
    }

    go(path, 0)
  }

  def genGraph(nodes: Seq[Int]): Gen[IntGraph] = {
    val genEdge = Gen.zip(Gen.oneOf(nodes), Gen.choose(0, 10))
    val genNeighbours = Gen.mapOf(genEdge)
    val genGraph = Gen.sequence[IntGraph, (Int, Map[Int, Int])](nodes map (n => Gen.zip(n, genNeighbours)))
    genGraph.map(g => g.withDefaultValue(Map.empty[Int, Int]))
  }

  val inputs: Gen[(IntGraph, Int, Int)] = Gen.sized { size =>
    val n = math.sqrt(size).toInt max 1
    for {
      g <- genGraph(0 until n)
      source <- Gen.choose(0, n - 1)
      target <- Gen.choose(0, n - 1)
    } yield (g, source, target)
  }
}
