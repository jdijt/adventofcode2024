package eu.derfniw.aoc2024.util

import java.time.Duration

case class BenchMarkResult[O](runtime: Duration, result: O):
  def pretty = s"Runtime: ${runtime.toNanos / 1_000} microseconds\n Result: $result"

def runBenchmarked[I, O](input: I, task: I => O): BenchMarkResult[O] =
  for _ <- 1 to 10 do task(input)

  val start   = System.nanoTime()
  val result  = task(input)
  val end     = System.nanoTime()
  val runtime = Duration.ofNanos(end - start)

  BenchMarkResult(runtime, result)
end runBenchmarked
