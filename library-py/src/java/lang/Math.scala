/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 *
 * Ported to the Python backend (scala3-py). `js.Math.*` calls are replaced
 * by calls into `scala.python.PyMath` (which wraps Python's stdlib
 * `math` module). Algorithms whose Python equivalents differ semantically
 * from Java's contract — notably `round`, `floorDiv`, `floorMod`, and the
 * *Exact overflow-checking helpers — keep the scala-js-sourced algorithms.
 *
 * Not ported in this unit (require the bit-level helpers from the L1.1
 * boxed primitives work):
 *   - `getExponent(Float/Double): Int`
 *   - `scalb(Float/Double, Int)`
 * These can be added once `Float.floatToRawIntBits` /
 * `Double.doubleToRawLongBits` are available.
 */

package java
package lang

import scala.python.PyMath
import scala.python.runtime.PyStruct

object Math:
  final val E = 2.718281828459045
  final val PI = 3.141592653589793
  final val TAU = 6.283185307179586

  // ---- abs -------------------------------------------------------

  @inline def abs(a: scala.Int): scala.Int =
    // Hacker's Delight, Section 2-4
    val sign = a >> 31
    (a ^ sign) - sign

  @inline def abs(a: scala.Long): scala.Long =
    val sign = a >> 63
    (a ^ sign) - sign

  @inline def abs(a: scala.Float): scala.Float = PyMath.fabs(a.toDouble).toFloat
  @inline def abs(a: scala.Double): scala.Double = PyMath.fabs(a)

  // ---- max / min -------------------------------------------------

  @inline def max(a: scala.Int, b: scala.Int): scala.Int = if a > b then a else b
  @inline def max(a: scala.Long, b: scala.Long): scala.Long = if a > b then a else b

  /* For Float/Double, Java's contract says `max(NaN, _) = NaN` and
   * `max(_, NaN) = NaN`. It also distinguishes +0.0 and -0.0 (preferring
   * +0.0 for max, -0.0 for min). The `x != x` idiom handles NaN; the
   * ±0.0 distinction is left at default comparison semantics until a
   * downstream caller needs the finer contract. */
  @inline def max(a: scala.Double, b: scala.Double): scala.Double =
    if a != a then a
    else if b != b then b
    else if a > b then a
    else b

  @inline def max(a: scala.Float, b: scala.Float): scala.Float =
    if a != a then a
    else if b != b then b
    else if a > b then a
    else b

  @inline def min(a: scala.Int, b: scala.Int): scala.Int = if a < b then a else b
  @inline def min(a: scala.Long, b: scala.Long): scala.Long = if a < b then a else b

  @inline def min(a: scala.Double, b: scala.Double): scala.Double =
    if a != a then a
    else if b != b then b
    else if a < b then a
    else b

  @inline def min(a: scala.Float, b: scala.Float): scala.Float =
    if a != a then a
    else if b != b then b
    else if a < b then a
    else b

  // ---- clamp -----------------------------------------------------

  @inline def clamp(value: scala.Long, min: scala.Int, max: scala.Int): scala.Int =
    if min > max then
      throw new IllegalArgumentException(min.toString + " > " + max.toString)
    if value < min.toLong then min
    else if value > max.toLong then max
    else value.toInt

  @inline def clamp(value: scala.Long, min: scala.Long, max: scala.Long): scala.Long =
    if min > max then
      throw new IllegalArgumentException(min.toString + " > " + max.toString)
    if value < min then min
    else if value > max then max
    else value

  @inline def clamp(value: scala.Double, min: scala.Double, max: scala.Double): scala.Double =
    if !(min < max) then validateClampSlowPath(min, max)
    this.max(min, this.min(max, value))

  @inline def clamp(value: scala.Float, min: scala.Float, max: scala.Float): scala.Float =
    if !(min < max) then validateClampSlowPath(min.toDouble, max.toDouble)
    this.max(min, this.min(max, value))

  private def validateClampSlowPath(min: scala.Double, max: scala.Double): Unit =
    if min != max || (bitsEqual(min, +0.0) && bitsEqual(max, -0.0)) then
      throw new IllegalArgumentException(clampMessage(min, max))

  // Helper: see notes/issue-labeled-expression-position.md for why we
  // factor the NaN-arm into a plain-if instead of an expression-level match.
  private def clampMessage(min: scala.Double, max: scala.Double): String =
    if PyMath.isnan(min) then "min is NaN"
    else if PyMath.isnan(max) then "max is NaN"
    else min.toString + " > " + max.toString

  /** `Double.equals`-style check that distinguishes +0.0 from -0.0 and
   *  treats `NaN == NaN`. We use `PyMath.copysign` to read out the sign
   *  bit of a zero, avoiding a dependency on `Double.doubleToLongBits`. */
  @inline private def bitsEqual(a: scala.Double, b: scala.Double): scala.Boolean =
    if PyMath.isnan(a) then PyMath.isnan(b)
    else if a == 0.0 && b == 0.0 then
      PyMath.copysign(1.0, a) == PyMath.copysign(1.0, b)
    else a == b

  // ---- ceil / floor / rint --------------------------------------
  //
  // Python's `math.ceil`/`math.floor` raise `ValueError`/`OverflowError`
  // for NaN and ±infinity (they convert to `int`). Java's spec returns
  // the argument unchanged for NaN, ±inf, and ±0.0. Guard those.

  def ceil(a: scala.Double): scala.Double =
    if PyMath.isnan(a) || PyMath.isinf(a) || a == 0.0 then a
    else PyMath.ceil(a).asInstanceOf[scala.Double]

  def floor(a: scala.Double): scala.Double =
    if PyMath.isnan(a) || PyMath.isinf(a) || a == 0.0 then a
    else PyMath.floor(a).asInstanceOf[scala.Double]

  /** Returns the `double` value that is closest in value to `a` and is
   *  equal to a mathematical integer. Ties round to even. */
  def rint(a: scala.Double): scala.Double =
    val C = 4503599627370496.0 // 2^52
    if a > 0 then
      if a >= C then a
      else (C + a) - C
    else if a < 0 then
      if a <= -C then a
      else -((C - a) - C)
    else
      a // 0.0, -0.0 and NaN

  // ---- round -----------------------------------------------------
  //
  // Java rounds half-up toward +infinity — NOT banker's rounding and
  // NOT Python's `round` default (which uses banker's rounding).
  // Spec:
  //   round(x) = floor(x + 0.5)
  //   round(NaN) = 0
  //   round(+inf) = Long.MaxValue
  //   round(-inf) = Long.MinValue
  //   round(x) for x > Long.MaxValue → Long.MaxValue
  //   round(x) for x < Long.MinValue → Long.MinValue

  def round(a: scala.Float): scala.Int =
    if a != a then 0
    else if a >= scala.Int.MaxValue.toFloat then scala.Int.MaxValue
    else if a <= scala.Int.MinValue.toFloat then scala.Int.MinValue
    else PyMath.floor((a + 0.5f).toDouble).asInstanceOf[scala.Double].toInt

  def round(a: scala.Double): scala.Long =
    if a != a then 0L
    else if a >= scala.Long.MaxValue.toDouble then scala.Long.MaxValue
    else if a <= scala.Long.MinValue.toDouble then scala.Long.MinValue
    else PyMath.floor(a + 0.5).asInstanceOf[scala.Double].toLong

  // ---- sqrt / pow / exp / log ------------------------------------

  @inline def sqrt(a: scala.Double): scala.Double = PyMath.sqrt(a)
  @inline def pow(a: scala.Double, b: scala.Double): scala.Double = PyMath.pow(a, b)
  @inline def exp(a: scala.Double): scala.Double = PyMath.exp(a)
  @inline def log(a: scala.Double): scala.Double = PyMath.log(a)
  @inline def log10(a: scala.Double): scala.Double = PyMath.log10(a)
  @inline def log1p(a: scala.Double): scala.Double = PyMath.log1p(a)

  // ---- Trigonometry ----------------------------------------------

  @inline def sin(a: scala.Double): scala.Double = PyMath.sin(a)
  @inline def cos(a: scala.Double): scala.Double = PyMath.cos(a)
  @inline def tan(a: scala.Double): scala.Double = PyMath.tan(a)
  @inline def asin(a: scala.Double): scala.Double = PyMath.asin(a)
  @inline def acos(a: scala.Double): scala.Double = PyMath.acos(a)
  @inline def atan(a: scala.Double): scala.Double = PyMath.atan(a)
  @inline def atan2(y: scala.Double, x: scala.Double): scala.Double = PyMath.atan2(y, x)

  // ---- Random (not implemented) ---------------------------------

  /** `Math.random()` — placeholder until `java.util.Random` lands. */
  def random(): scala.Double =
    throw new UnsupportedOperationException("Math.random requires java.util.Random")

  // ---- toDegrees / toRadians / signum ---------------------------

  @inline def toDegrees(a: scala.Double): scala.Double = a * (180.0 / PI)
  @inline def toRadians(a: scala.Double): scala.Double = a * (PI / 180.0)

  @inline def signum(a: scala.Double): scala.Double =
    if a > 0 then 1.0
    else if a < 0 then -1.0
    else a

  @inline def signum(a: scala.Float): scala.Float =
    if a > 0 then 1.0f
    else if a < 0 then -1.0f
    else a

  // ---- cbrt / hypot / expm1 / sinh / cosh / tanh -----------------

  @inline def cbrt(a: scala.Double): scala.Double = PyMath.cbrt(a)
  @inline def hypot(a: scala.Double, b: scala.Double): scala.Double = PyMath.hypot(a, b)
  @inline def expm1(a: scala.Double): scala.Double = PyMath.expm1(a)
  @inline def sinh(a: scala.Double): scala.Double = PyMath.sinh(a)
  @inline def cosh(a: scala.Double): scala.Double = PyMath.cosh(a)
  @inline def tanh(a: scala.Double): scala.Double = PyMath.tanh(a)

  // ---- copySign --------------------------------------------------

  @inline def copySign(magnitude: scala.Double, sign: scala.Double): scala.Double =
    PyMath.copysign(magnitude, sign)

  @inline def copySign(magnitude: scala.Float, sign: scala.Float): scala.Float =
    PyMath.copysign(magnitude.toDouble, sign.toDouble).toFloat

  // ---- nextUp / nextDown / nextAfter -----------------------------
  //
  // `Double` overloads delegate to `math.nextafter`. `Float` overloads
  // have to work at Float precision — `PyMath.nextafter` operates on
  // Doubles, and the Double delta between `x.toDouble` and its Double
  // successor is far smaller than any Float gap, so converting back
  // collapses to the same Float. We step the Float bit pattern directly
  // via `PyStruct.float_to_int32_bits` / `float_from_int32_bits`.

  /** The least representable `double` greater than `a`. */
  def nextUp(a: scala.Double): scala.Double =
    PyMath.nextafter(a, scala.Double.PositiveInfinity)

  def nextUp(a: scala.Float): scala.Float =
    floatBitStep(a, upward = true)

  def nextDown(a: scala.Double): scala.Double =
    PyMath.nextafter(a, scala.Double.NegativeInfinity)

  def nextDown(a: scala.Float): scala.Float =
    floatBitStep(a, upward = false)

  def nextAfter(a: scala.Double, b: scala.Double): scala.Double =
    PyMath.nextafter(a, b)

  def nextAfter(a: scala.Float, b: scala.Double): scala.Float =
    if PyMath.isnan(a.toDouble) || PyMath.isnan(b) then scala.Float.NaN
    else if a.toDouble == b then b.toFloat
    else if b > a.toDouble then nextUp(a)
    else nextDown(a)

  /** Step `a` by one Float ulp in the indicated direction. */
  private def floatBitStep(a: scala.Float, upward: scala.Boolean): scala.Float =
    if PyMath.isnan(a.toDouble) then scala.Float.NaN
    else if upward && a == scala.Float.PositiveInfinity then a
    else if !upward && a == scala.Float.NegativeInfinity then a
    else if a == 0.0f then
      if upward then scala.Float.MinPositiveValue
      else -scala.Float.MinPositiveValue
    else
      val bits = PyStruct.float_to_int32_bits(a)
      // Positive Float: bigger-magnitude has higher bits; stepping up
      // = bits + 1. Negative Float: bigger-magnitude has higher bits
      // too (sign bit set), so stepping up = bits - 1.
      val delta = if (a > 0.0f) == upward then 1 else -1
      PyStruct.float_from_int32_bits(bits + delta)

  // ---- ulp -------------------------------------------------------

  /** Unit in last place. Python's `math.ulp` matches Java's spec for
   *  `Double`: `ulp(±0.0) = Double.MIN_VALUE`, `ulp(inf) = inf`,
   *  `ulp(NaN) = NaN`, and for normal `x` the distance to the next
   *  representable `Double` of larger magnitude. */
  @inline def ulp(a: scala.Double): scala.Double = PyMath.ulp(a)

  /** Float ulp — computed as `nextUp(|a|) - |a|` via direct Float bit
   *  step. `PyMath.ulp` operates on `Double` and would give the
   *  Double-ulp of the widened value (2^-52) instead of the Float
   *  2^-23. */
  def ulp(a: scala.Float): scala.Float =
    if PyMath.isnan(a.toDouble) then scala.Float.NaN
    else if a == scala.Float.PositiveInfinity || a == scala.Float.NegativeInfinity then
      scala.Float.PositiveInfinity
    else if a == 0.0f then scala.Float.MinPositiveValue
    else
      val abs_a = if a < 0.0f then -a else a
      floatBitStep(abs_a, upward = true) - abs_a

  // ---- *Exact (overflow-checking) arithmetic --------------------

  private def intOverflow(): Nothing =
    throw new ArithmeticException("Integer overflow")

  private def longOverflow(): Nothing =
    throw new ArithmeticException("Long overflow")

  @inline def addExact(a: scala.Int, b: scala.Int): scala.Int =
    val res = a + b
    if ((res ^ a) & (res ^ b)) < 0 then intOverflow()
    res

  @inline def addExact(a: scala.Long, b: scala.Long): scala.Long =
    val res = a + b
    if ((res ^ a) & (res ^ b)) < 0L then longOverflow()
    res

  @inline def subtractExact(a: scala.Int, b: scala.Int): scala.Int =
    val res = a - b
    if ((a ^ b) & (res ^ a)) < 0 then intOverflow()
    res

  @inline def subtractExact(a: scala.Long, b: scala.Long): scala.Long =
    val res = a - b
    if ((a ^ b) & (res ^ a)) < 0L then longOverflow()
    res

  @inline def multiplyExact(a: scala.Int, b: scala.Int): scala.Int =
    val full = multiplyFull(a, b)
    val res = full.toInt
    if ((full >>> 32).toInt != (res >> 31)) then intOverflow()
    res

  @inline def multiplyExact(a: scala.Long, b: scala.Int): scala.Long =
    val bLong = b.toLong
    val res = a * bLong
    if a != 0 && res / a != bLong then longOverflow()
    res

  @inline def multiplyExact(a: scala.Long, b: scala.Long): scala.Long =
    val res = a * b
    if (a < 0 && b == scala.Long.MinValue) || (a != 0 && res / a != b) then longOverflow()
    res

  @inline def incrementExact(a: scala.Int): scala.Int =
    if a == scala.Int.MaxValue then intOverflow()
    a + 1

  @inline def incrementExact(a: scala.Long): scala.Long =
    if a == scala.Long.MaxValue then longOverflow()
    a + 1L

  @inline def decrementExact(a: scala.Int): scala.Int =
    if a == scala.Int.MinValue then intOverflow()
    a - 1

  @inline def decrementExact(a: scala.Long): scala.Long =
    if a == scala.Long.MinValue then longOverflow()
    a - 1L

  @inline def negateExact(a: scala.Int): scala.Int =
    if a == scala.Int.MinValue then intOverflow()
    -a

  @inline def negateExact(a: scala.Long): scala.Long =
    if a == scala.Long.MinValue then longOverflow()
    -a

  @inline def toIntExact(a: scala.Long): scala.Int =
    val res = a.toInt
    if res.toLong != a then intOverflow()
    res

  // ---- multiplyFull / multiplyHigh ------------------------------

  @inline def multiplyFull(x: scala.Int, y: scala.Int): scala.Long =
    x.toLong * y.toLong

  @inline def multiplyHigh(x: scala.Long, y: scala.Long): scala.Long =
    // Hacker's Delight, Section 8-2.
    val x0 = x & 0xffffffffL
    val x1 = x >> 32
    val y0 = y & 0xffffffffL
    val y1 = y >> 32
    val t = x1 * y0 + ((x0 * y0) >>> 32)
    x1 * y1 + (t >> 32) + (((t & 0xffffffffL) + x0 * y1) >> 32)

  @inline def unsignedMultiplyHigh(x: scala.Long, y: scala.Long): scala.Long =
    val x0 = x & 0xffffffffL
    val x1 = x >>> 32
    val y0 = y & 0xffffffffL
    val y1 = y >>> 32
    val t = x1 * y0 + ((x0 * y0) >>> 32)
    x1 * y1 + (t >>> 32) + (((t & 0xffffffffL) + x0 * y1) >>> 32)

  // ---- floorDiv / floorMod --------------------------------------
  //
  // Well-defined for negative operands: result is always congruent
  // with the classical mathematical definition (quotient rounded
  // toward -infinity, remainder non-negative when divisor is positive).
  //
  // Implementation note: the Python backend currently lowers Scala `/`
  // and `%` on integers to Python `//` and `%`, which already follow
  // the floor convention. Therefore `floorDiv` / `floorMod` can simply
  // use the native operators — no need for the scala-js post-correction
  // (which was needed because JVM `/` truncates toward zero). If the
  // backend later switches to Java-style truncation, these bodies must
  // be rewritten to the scala-js post-correction algorithm:
  //   val quot = a / b
  //   if ((a ^ b) >= 0 || quot * b == a) quot else quot - 1

  @inline def floorDiv(a: scala.Int, b: scala.Int): scala.Int = a / b
  @inline def floorDiv(a: scala.Long, b: scala.Int): scala.Long = a / b.toLong
  @inline def floorDiv(a: scala.Long, b: scala.Long): scala.Long = a / b

  @inline def floorMod(a: scala.Int, b: scala.Int): scala.Int = a % b
  @inline def floorMod(a: scala.Long, b: scala.Int): scala.Int = (a % b.toLong).toInt
  @inline def floorMod(a: scala.Long, b: scala.Long): scala.Long = a % b

  // ---- IEEEremainder --------------------------------------------

  /** IEEE 754 remainder: result has absolute value <= |f2|/2 and is
   *  exact (no rounding error). Python's `math.remainder` matches
   *  Java's `Math.IEEEremainder` — both are spec'd from IEEE 754
   *  Section 5.3.1. */
  @inline def IEEEremainder(f1: scala.Double, f2: scala.Double): scala.Double =
    PyMath.remainder(f1, f2)
