package dotty.tools.cappyrepl

import scala.language.unsafeNulls

/** Headless profiling entry point: drives a few inputs through
 *  `CappyReplDriver` and prints the wall-clock for startup and each
 *  per-input `run()` call.
 *
 *  Run with:
 *  {{{
 *  sbt "cappy-repl/runMain dotty.tools.cappyrepl.Profile"
 *  }}}
 */
object Profile:
  private def msSince(t0: Long): Double = (System.nanoTime - t0) / 1e6

  def main(args: Array[String]): Unit =
    val driver = new CappyReplDriver()
    val tStart = System.nanoTime
    driver.resetToInitial()
    val resetMs = msSince(tStart)
    System.err.println(f"[profile] resetToInitial (subprocess + preload): ${resetMs}%.1f ms")

    var state = driver.initialState
    val inputs = List(
      "1 + 1",
      "List(1, 2, 3)",
      "List(1, 2, 3)",
      "List(4, 5, 6)",
      "List(1, 2, 3).map(_ * 2)",
      "None",
      "Some(5)",
    )
    for input <- inputs do
      val t = System.nanoTime
      val (out, next) = driver.run(input)(using state)
      val ms = msSince(t)
      state = next
      System.err.println(f"[profile] run(${input}%-30s) = ${ms}%6.1f ms  → ${out.replace("\n", " | ")}")
    driver.shutdown()
end Profile
