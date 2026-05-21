package dotty.tools.benchmarks.py.mathalg

/** Square-and-multiply modular and plain integer exponentiation. Mixes
 *  `Long` and `Int` arithmetic in tight inner loops; bit shifts `>>=` /
 *  `>>>=` and modular multiplies stress the 32/64-bit truncation path. */
class ModPowBench:
  var size: Int = 0

  def setup(size: Int): Unit =
    this.size = size

  val operations: Map[String, () => Any] = Map(
    "modPow" -> { () =>
      val mod = 1000000007L
      var acc = 0L
      var k = 1
      while k <= size do
        var base = 2L
        var exp = k
        var result = 1L
        val m = mod
        while exp > 0 do
          if (exp & 1) == 1 then result = result * base % m
          base = base * base % m
          exp >>>= 1
        acc += result
        k += 1
      acc
    },
    "intPow" -> { () =>
      var acc = 0L
      var k = 1
      while k <= size do
        var b = (k % 7 + 2).toLong
        var e = 20
        var r = 1L
        while e > 0 do
          if (e & 1) == 1 then r *= b
          b *= b
          e >>= 1
        acc += r
        k += 1
      acc
    },
  )

@main def main(): Unit = ()
