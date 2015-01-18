package de.ummels.dijkstra

import de.ummels.dijkstra.Dijkstra._
import org.scalacheck.Shrink
import org.scalatest.{Matchers, prop, PropSpec}

trait PropertySpec extends PropSpec with prop.PropertyChecks with Matchers {

  import scala.language.implicitConversions

  type IntGraph = Map[Int, Map[Int, Int]]

  implicit def shrinkInput(implicit s: Shrink[IntGraph]): Shrink[(IntGraph, Int, Int)] =
    Shrink { case (g, source, target) =>
      for (g1 <- Shrink.shrink(g)) yield (g1.withDefaultValue(Map.empty), source, target)
    }
}
