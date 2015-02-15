package de.ummels.dijkstra

object Main extends App {
  def time(f: => Any): Any = {
    val startTime = System.currentTimeMillis
    val res = f
    val endTime = System.currentTimeMillis
    println("Function call took " + (endTime - startTime) + " ms.")
    res
  }

  println("Test")
}
