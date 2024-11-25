package eu.derfniw.aoc2024.utils

import scala.io.Source
import scala.util.Using

def readInputFile(file: String): Seq[String] =
  Using(Source.fromFile(s"inputs/$file")) { source =>
    source.getLines().toSeq
  }.get
