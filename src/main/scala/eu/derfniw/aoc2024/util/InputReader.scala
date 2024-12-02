package eu.derfniw.aoc2024.util

import scala.io.Source
import scala.util.{Try, Using}

trait InputReader(day: Int):
  private val paddedDay = day.toString.reverse.padTo(2, '0').reverse

  lazy val mainInput: Seq[String] = readInputFile("main.txt").get

  def readInputFile(name: String): Try[Seq[String]] =
    Using(Source.fromFile(s"./inputs/day$paddedDay/$name"))(_.getLines().toSeq)
