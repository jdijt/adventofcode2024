package eu.derfniw.aoc2024.day21

import eu.derfniw.aoc2024.util.{InputReader, runBenchmarked}

import scala.collection.mutable

case class Point(x: Int, y: Int)

class KeyPad(keypad: String):
  private val keyMap: Map[Char, Point] = keypad
    .split("\n")
    .zipWithIndex
    .flatMap { (line, y) =>
      line.zipWithIndex.map { (c, x) =>
        c -> Point(x, y)
      }
    }
    .toMap

  // Calculates the route in either "vertical -> horizontal" or "horizontal -> vertical"
  // (more turns increases movements on successive layers of dirpads, so will not be optimal.).
  // Takes into account the forbidden tile (empty space).
  def routes(from: Char, to: Char): Set[List[Char]] =
    val fromPoint = keyMap(from)
    val toPoint   = keyMap(to)
    val dx        = toPoint.x - fromPoint.x
    val dy        = toPoint.y - fromPoint.y

    val ySteps = List.fill(math.abs(dy))(if dy < 0 then '^' else 'v')
    val xSteps = List.fill(math.abs(dx))(if dx < 0 then '<' else '>')
    val result = mutable.Set.empty[List[Char]]
    // Vertical is safe
    if keyMap(' ') != Point(fromPoint.x, toPoint.y) then
      result += ('A' +: xSteps ++: ySteps).reverse
    // Horizontal is safe
    if keyMap(' ') != Point(toPoint.x, fromPoint.y) then
      result += ('A' +: ySteps ++: xSteps).reverse
    result.toSet
  end routes
end KeyPad

object KeyPad:
  private val numericPad: KeyPad = KeyPad(
    """|789
       |456
       |123
       | 0A""".stripMargin
  )
  private val dirPad: KeyPad = KeyPad(
    """| ^A
       |<v>""".stripMargin
  )

  def keyPadsForLayers(layers: Int): IndexedSeq[KeyPad] =
    numericPad +: IndexedSeq.fill(layers)(dirPad)
end KeyPad

class Calculator(depth: Int):
  private val keyPads = KeyPad.keyPadsForLayers(depth)

  def getComplexity(input: List[Char]): Long =
    input.take(3).mkString.toInt * minPressesNeeded(input, keyPads)

  private def minPressesNeeded(code: List[Char], keyPads: IndexedSeq[KeyPad]): Long = keyPads match
    case Nil => code.length
    case kps =>
      // Codes end with A (see input & route generation above)
      // And we always start at A as well.
      ('A' :: code)
        .sliding(2)
        .collect { case from :: to :: _ =>
          minPressesNeededForMove(from, to, kps)
        }
        .sum
  end minPressesNeeded

  private val cache   = mutable.Map.empty[(Char, Char, Int), Long]
  private def minPressesNeededForMove(currentKey: Char, nextKey: Char, keyPads: IndexedSeq[KeyPad]): Long =
    cache.getOrElseUpdate(
      (currentKey, nextKey, keyPads.length),
      keyPads.head
        .routes(currentKey, nextKey)
        .map(route => minPressesNeeded(route, keyPads.tail))
        .min
    )
  end minPressesNeededForMove


def part1(input: Seq[String]): Long =
  val calculator = Calculator(2)
  input.map(_.toList).map(calculator.getComplexity).sum

def part2(input: Seq[String]): Long =
  val calculator = Calculator(25)
  input.map(_.toList).map(calculator.getComplexity).sum

object Input extends InputReader(21)

@main
def day21(): Unit =
  println(s"Part1: \n${runBenchmarked(Input.mainInput, part1).pretty}")
  println(s"Part2: \n${runBenchmarked(Input.mainInput, part2).pretty}")
