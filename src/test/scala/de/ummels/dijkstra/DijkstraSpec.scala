package de.ummels.dijkstra

trait DijkstraSpec extends PropertySpec with Dijkstra.ToDijkstraOps {

  import DijkstraSpec._

  implicit val dijkstraInstance: Dijkstra

  property("the result of dijkstra should contain all nodes reachable from source") {
    forAll(inputs) { case (graph, source, _) =>
      val reach = graph.bfs(source)
      graph.dijkstra(source)._1.keys shouldBe reach
    }
  }

  property("the result of dijkstra should be consistent") {
    forAll(inputs) { case (graph, source, _) =>
      val test = (isConsistent(graph)(source) _).tupled
      test(graph.dijkstra(source)) shouldBe true
    }
  }
}

object DijkstraSpec {
  def isConsistent[N](g: SimpleGraph[N])(source: N)(cost: Map[N, Int], pred: Map[N, N]): Boolean = {
    val rg = g.reversed
    pred.keys.forall { n =>
      val p = pred(n)
      cost(n) == cost(p) + rg(n)(p) &&
        cost(n) == (for ((n1, c) <- rg(n) if cost.contains(n1)) yield cost(n1) + c).min
    }
  }
}
