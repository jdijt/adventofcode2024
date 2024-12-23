package eu.derfniw.aoc2024.day23

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

import scala.collection.mutable

class Graph(adjacent: Map[String, Set[String]]):
  def findCliques(size: Int): Set[Set[String]] =
    adjacent.keySet.toIndexedSeq
      .combinations(size)
      .filter { case Seq(a, b, c) =>
        // This works because we always insert both directions in the adjacency set.
        adjacent(a).contains(b) && adjacent(b).contains(c) && adjacent(c).contains(a)
      }
      .map(_.toSet)
      .toSet

  def findMaxClique: Set[String] =
    def _findMaxClique(
        candidate: Set[String],
        toVisit: Set[String],
        toSkip: Set[String]
    ): Set[String] =
      if toVisit.isEmpty && toSkip.isEmpty then candidate
      else if toVisit.isEmpty then Set.empty
      else
        // Pivot is the vertex with the smallest number of neighbors in the toVisit set.
        Set
          .unfold((toSkip, toVisit)) { (newToSkip, newToVisit) =>
            if newToVisit.isEmpty then None
            else
              val v   = newToVisit.head
              val nbs = adjacent(v)
              val maxClique =
                _findMaxClique(candidate + v, newToVisit.intersect(nbs), newToSkip.intersect(nbs))
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
  println(s"Part 1: \n${runBenchmarked(Input.mainInput, part1, 0).pretty}")
  println(s"Part 2: \n${runBenchmarked(Input.mainInput, part2, 0).pretty}")
