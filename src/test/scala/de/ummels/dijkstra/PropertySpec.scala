package de.ummels.dijkstra

import org.scalacheck.Gen
import org.scalatest.{Matchers, prop, PropSpec}

trait PropertySpec extends PropSpec with prop.PropertyChecks with Matchers {
  def genGraph[N](nodes: Seq[N]): Gen[SimpleGraph[N]] = {
    val genEdge = Gen.zip(Gen.oneOf(nodes), Gen.choose(0, 10))
    val genNeighbours = Gen.mapOf(genEdge)
    val genGraph =
      Gen.sequence[Map[N, Map[N, Int]], (N, Map[N, Int])](nodes map (n => Gen.zip(n, genNeighbours)))
    genGraph.map(g => SimpleGraph(g))
  }

  val inputs: Gen[(SimpleGraph[Int], Int, Int)] = Gen.sized { size =>
    val n = math.sqrt(size).toInt max 1
    for {
      g <- genGraph(0 until n)
      source <- Gen.choose(0, n - 1)
      target <- Gen.choose(0, n - 1)
    } yield (g, source, target)
  }
}
