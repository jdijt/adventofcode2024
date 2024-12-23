package eu.derfniw.aoc2024.day23

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

import scala.collection.mutable

class Graph(adjacent: Map[String, Set[String]]):
  //This is a modified bron-kerbosh that collects
  // all cliques of a given size
  def findCliques(size: Int): Set[Set[String]] =
    def _findCliques(
        candidate: Set[String],
        toVisit: Set[String],
        toSkip: Set[String]
    ): Set[Set[String]] =
      if candidate.size == size then Set(candidate)
      else if toVisit.isEmpty then Set.empty
      else
        Set
          .unfold((toSkip, toVisit)) { (newToSkip, newToVisit) =>
            if newToVisit.isEmpty then None
            else
              val v   = newToVisit.head
              val nbs = adjacent(v)
              val cliques = _findCliques(
                candidate + v,
                newToVisit.intersect(nbs),
                newToSkip.intersect(nbs)
              )
              Some(cliques, (newToSkip + v, newToVisit - v))
          }
          .flatten
    _findCliques(Set.empty, adjacent.keySet, Set.empty)
  end findCliques

  //This is a modified bron-kerbosh that finds the maximum clique
  def findMaxClique: Set[String] =
    def _findMaxClique(
        candidate: Set[String],
        toVisit: Set[String],
        toSkip: Set[String]
    ): Set[String] =
      if toVisit.isEmpty && toSkip.isEmpty then candidate
      else if toVisit.isEmpty then Set.empty
      else
        Set
          .unfold((toSkip, toVisit)) { (newToSkip, newToVisit) =>
            if newToVisit.isEmpty then None
            else
              val v   = newToVisit.head
              val nbs = adjacent(v)
              val maxClique = _findMaxClique(
                candidate + v,
                newToVisit.intersect(nbs),
                newToSkip.intersect(nbs)
              )
              Some(maxClique, (newToSkip + v, newToVisit - v))
          }
          .maxBy(_.size)

    _findMaxClique(Set.empty, adjacent.keySet, Set.empty)
  end findMaxClique

end Graph

object Graph:
  def fromInput(input: Seq[String]): Graph =
    val adjacent = mutable.Map.empty[String, mutable.Set[String]]
    input
      .collect { case s"$from-$to" => (from, to) }
      .foreach { (from, to) =>
        adjacent.getOrElseUpdate(from, mutable.Set.empty) += to
        adjacent.getOrElseUpdate(to, mutable.Set.empty) += from
      }
    Graph(adjacent.view.mapValues(_.toSet).toMap)
end Graph

def part1(input: Seq[String]): Int =
  Graph.fromInput(input).findCliques(3).count(_.exists(_.startsWith("t")))

def part2(input: Seq[String]): String =
  Graph.fromInput(input).findMaxClique.toSeq.sorted.mkString(",")

object Input extends InputReader(23)

@main
def day23(): Unit =
  println(s"Part 1: \n${runBenchmarked(Input.mainInput, part1).pretty}")
  println(s"Part 2: \n${runBenchmarked(Input.mainInput, part2).pretty}")
