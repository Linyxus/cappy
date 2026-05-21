package dotty.tools.benchmarks.py.mathalg

/** Sieve of Eratosthenes over `[2, size]`. Re-runs the full sieve each
 *  invocation: per-element `_scpy_i32` wrapping on `p * p` / `m += p`,
 *  loop-invariant `size` reads, and a fresh `Array[Boolean]` allocation. */
class SieveBench:
  var size: Int = 0
  var sieve: Array[Boolean] = new Array[Boolean](0)

  def setup(size: Int): Unit =
    this.size = size
    sieve = new Array[Boolean](size + 1)

  val operations: Map[String, () => Any] = Map(
    "sieve" -> { () =>
      val composite = new Array[Boolean](size + 1)
      composite(0) = true
      composite(1) = true
      var p = 2
      while p * p <= size do
        if !composite(p) then
          var m = p * p
          while m <= size do
            composite(m) = true
            m += p
        p += 1
      var count = 0
      var i = 2
      while i <= size do
        if !composite(i) then count += 1
        i += 1
      count
    },
  )

@main def main(): Unit = ()
