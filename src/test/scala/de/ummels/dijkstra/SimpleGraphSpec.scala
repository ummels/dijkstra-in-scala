package de.ummels.dijkstra

class SimpleGraphSpec extends PropertySpec {
  property("reverse should reverse all edges") {
    forAll(inputs){ case (g, s, t) =>
      g(s).get(t) shouldBe g.reversed(t).get(s)
    }
  }
}
