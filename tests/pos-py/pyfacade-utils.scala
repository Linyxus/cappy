/*
 * Direct coverage of `java.lang.Utils.roundUpToPowerOfTwo`. Lives in
 * `package java.lang` to access the `private[java]` helper the same way
 * `javalib-lang-bounds-checks.scala` reaches into `BoundsChecks`.
 *
 * The Scala.js semantics being pinned:
 *   - Rounds to the next power of two ≥ the input.
 *   - Input 1 stays 1 (smallest power-of-two ≥ 1 is 1, not 2).
 *   - Input 2 stays 2 (already a power of two).
 *   - Any i > 2^30 saturates — returns i unchanged (next power would
 *     overflow Int).
 */
package java.lang

@main def pyfacadeUtils(): Unit =
  // Smallest positive inputs.
  println("r(1):" + Utils.roundUpToPowerOfTwo(1))
  println("r(2):" + Utils.roundUpToPowerOfTwo(2))
  println("r(3):" + Utils.roundUpToPowerOfTwo(3))
  println("r(4):" + Utils.roundUpToPowerOfTwo(4))
  println("r(5):" + Utils.roundUpToPowerOfTwo(5))

  // Mid-range.
  println("r(100):" + Utils.roundUpToPowerOfTwo(100))
  println("r(1000):" + Utils.roundUpToPowerOfTwo(1000))
  println("r(1024):" + Utils.roundUpToPowerOfTwo(1024))
  println("r(1025):" + Utils.roundUpToPowerOfTwo(1025))

  // Boundary: 2^30 is the largest power we can round up to without
  // overflowing Int. 2^30 + 1 saturates to its own value per the
  // Scala.js contract.
  println("r(2^30):" + Utils.roundUpToPowerOfTwo(1 << 30))
  println("r(2^30+1):" + Utils.roundUpToPowerOfTwo((1 << 30) + 1))
  println("r(MaxValue):" + Utils.roundUpToPowerOfTwo(Int.MaxValue))
