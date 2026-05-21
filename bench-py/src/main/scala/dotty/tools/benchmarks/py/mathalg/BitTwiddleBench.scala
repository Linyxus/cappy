package dotty.tools.benchmarks.py.mathalg

/** Bit-twiddling kernels: Kernighan popcount, 32-bit reverse, and
 *  leading-zero count. Compound bitwise assignments (`&=`, `|=`, `^=`,
 *  `<<=`, `>>>=`) each wrap through `_scpy_i32`. */
class BitTwiddleBench:
  var size: Int = 0

  def setup(size: Int): Unit =
    this.size = size

  val operations: Map[String, () => Any] = Map(
    "popcount" -> { () =>
      var total = 0
      var n = 1
      while n <= size do
        var x = n
        var c = 0
        while x != 0 do
          x &= x - 1
          c += 1
        total += c
        n += 1
      total
    },
    "reverseBits" -> { () =>
      var acc = 0
      var n = 1
      while n <= size do
        var x = n
        var r = 0
        var bits = 32
        while bits > 0 do
          r = (r << 1) | (x & 1)
          x >>>= 1
          bits -= 1
        acc ^= r
        n += 1
      acc
    },
    "leadingZeros" -> { () =>
      var total = 0
      var n = 1
      while n <= size do
        var x = n
        var lz = 0
        var shift = 16
        while shift > 0 do
          if (x >>> shift) == 0 then
            lz += shift
            x <<= shift
          shift >>= 1
        total += lz
        n += 1
      total
    },
  )

@main def main(): Unit = ()
