import java.lang.Math

@main def javalibLangMath(): Unit =
  // -------- constants -------------------------------------------------

  println("pi-gt:" + (Math.PI > 3.14159265 && Math.PI < 3.14159266))
  println("e-gt:" + (Math.E > 2.71828182 && Math.E < 2.71828183))
  println("tau-gt:" + (Math.TAU > 6.28318530 && Math.TAU < 6.28318531))
  println("pi-exact:" + (Math.PI == 3.141592653589793))
  println("e-exact:" + (Math.E == 2.718281828459045))
  println("tau-2pi:" + (Math.TAU == 2.0 * Math.PI))

  // -------- trig ------------------------------------------------------

  println("sin0:" + Math.sin(0.0))
  println("cos0:" + Math.cos(0.0))
  println("tan0:" + Math.tan(0.0))
  println("sin-pi-close:" + (Math.abs(Math.sin(Math.PI)) < 1e-10))
  println("cos-pi:" + (Math.abs(Math.cos(Math.PI) + 1.0) < 1e-10))
  println("sin-pi2-close:" + (Math.abs(Math.sin(Math.PI / 2.0) - 1.0) < 1e-10))
  println("asin0:" + Math.asin(0.0))
  println("acos1:" + Math.acos(1.0))
  println("atan0:" + Math.atan(0.0))
  println("atan2-01:" + Math.atan2(0.0, 1.0))
  println("atan2-10:" + (Math.abs(Math.atan2(1.0, 0.0) - Math.PI / 2.0) < 1e-10))
  val s = Math.sin(1.0)
  val c = Math.cos(1.0)
  println("trig-id:" + (Math.abs(s * s + c * c - 1.0) < 1e-10))

  // -------- hyperbolic -----------------------------------------------

  println("sinh0:" + Math.sinh(0.0))
  println("cosh0:" + Math.cosh(0.0))
  println("tanh0:" + Math.tanh(0.0))
  println("sinh1-close:" + (Math.abs(Math.sinh(1.0) - 1.1752011936438014) < 1e-12))
  println("cosh1-close:" + (Math.abs(Math.cosh(1.0) - 1.5430806348152437) < 1e-12))
  println("tanh1-close:" + (Math.abs(Math.tanh(1.0) - 0.7615941559557649) < 1e-12))
  println("sinh-neg1:" + (Math.sinh(-1.0) == -Math.sinh(1.0)))
  println("cosh-neg1:" + (Math.cosh(-1.0) == Math.cosh(1.0)))

  // -------- exp-log ---------------------------------------------------

  println("exp0:" + Math.exp(0.0))
  println("exp1-eq-e:" + (Math.abs(Math.exp(1.0) - Math.E) < 1e-14))
  println("log1:" + Math.log(1.0))
  println("log-e:" + (Math.abs(Math.log(Math.E) - 1.0) < 1e-14))
  println("log10-100:" + Math.log10(100.0))
  println("log10-1:" + Math.log10(1.0))
  println("log1p-0:" + Math.log1p(0.0))
  println("expm1-0:" + Math.expm1(0.0))
  println("pow-2-10:" + Math.pow(2.0, 10.0))
  println("pow-neg:" + Math.pow(2.0, -1.0))
  println("sqrt4:" + Math.sqrt(4.0))
  println("sqrt0:" + Math.sqrt(0.0))
  println("cbrt27:" + Math.cbrt(27.0))
  println("cbrt-neg8:" + Math.cbrt(-8.0))
  println("hypot-3-4:" + Math.hypot(3.0, 4.0))
  println("hypot-5-12:" + Math.hypot(5.0, 12.0))

  // -------- degrees/radians ------------------------------------------

  println("deg-pi:" + Math.toDegrees(Math.PI))
  println("rad-180:" + (Math.abs(Math.toRadians(180.0) - Math.PI) < 1e-14))

  // -------- round (Double → Long) ------------------------------------

  // Half-up toward +infinity: the critical Java contract.
  println("round-d-0.5:" + Math.round(0.5))
  println("round-d-1.5:" + Math.round(1.5))
  // 2.5 rounds to 3 under half-up (banker's rounding would give 2).
  println("round-d-2.5:" + Math.round(2.5))
  println("round-d-3.5:" + Math.round(3.5))
  // -0.5 rounds to 0 under half-up (NOT -1).
  println("round-d-neg0.5:" + Math.round(-0.5))
  println("round-d-neg1.5:" + Math.round(-1.5))
  println("round-d-neg2.5:" + Math.round(-2.5))
  println("round-d-0.499:" + Math.round(0.499))
  println("round-d-0.501:" + Math.round(0.501))
  println("round-d-neg0.501:" + Math.round(-0.501))
  println("round-d-neg0.499:" + Math.round(-0.499))
  println("round-d-0:" + Math.round(0.0))
  println("round-d-neg0:" + Math.round(-0.0))
  println("round-d-1:" + Math.round(1.0))
  println("round-d-neg1:" + Math.round(-1.0))
  // NaN and infinities.
  println("round-d-nan:" + Math.round(scala.Double.NaN))
  println("round-d-posinf:" + Math.round(scala.Double.PositiveInfinity))
  println("round-d-neginf:" + Math.round(scala.Double.NegativeInfinity))
  // Overflow clamp.
  println("round-d-huge:" + Math.round(1e20))
  println("round-d-huge-neg:" + Math.round(-1e20))

  // -------- round (Float → Int) --------------------------------------

  println("round-f-0.5:" + Math.round(0.5f))
  println("round-f-1.5:" + Math.round(1.5f))
  println("round-f-2.5:" + Math.round(2.5f))
  println("round-f-neg0.5:" + Math.round(-0.5f))
  println("round-f-neg1.5:" + Math.round(-1.5f))
  println("round-f-nan:" + Math.round(scala.Float.NaN))
  println("round-f-posinf:" + Math.round(scala.Float.PositiveInfinity))
  println("round-f-neginf:" + Math.round(scala.Float.NegativeInfinity))
  println("round-f-huge:" + Math.round(1e20f))

  // -------- rint ------------------------------------------------------

  println("rint-0.5:" + Math.rint(0.5))       // banker's: -> 0.0
  println("rint-1.5:" + Math.rint(1.5))       // banker's: -> 2.0
  println("rint-2.5:" + Math.rint(2.5))       // banker's: -> 2.0
  println("rint-neg0.5:" + Math.rint(-0.5))   // banker's: -> -0.0
  println("rint-neg1.5:" + Math.rint(-1.5))   // banker's: -> -2.0
  println("rint-3.7:" + Math.rint(3.7))

  // -------- floor / ceil ---------------------------------------------

  println("floor-3.7:" + Math.floor(3.7))
  println("floor-neg3.2:" + Math.floor(-3.2))
  println("floor-neg0.5:" + Math.floor(-0.5))
  println("floor-0:" + Math.floor(0.0))
  println("floor-nan-isnan:" + (Math.floor(scala.Double.NaN) != Math.floor(scala.Double.NaN)))
  println("ceil-3.2:" + Math.ceil(3.2))
  println("ceil-neg3.7:" + Math.ceil(-3.7))
  println("ceil-neg0.5:" + Math.ceil(-0.5))
  println("ceil-nan-isnan:" + (Math.ceil(scala.Double.NaN) != Math.ceil(scala.Double.NaN)))

  // -------- abs -------------------------------------------------------

  println("abs-i-pos:" + Math.abs(5))
  println("abs-i-neg:" + Math.abs(-5))
  println("abs-i-zero:" + Math.abs(0))
  // Java contract: Math.abs(Int.MinValue) == Int.MinValue (overflow).
  println("abs-i-minvalue:" + (Math.abs(scala.Int.MinValue) == scala.Int.MinValue))
  println("abs-l-pos:" + Math.abs(5L))
  println("abs-l-neg:" + Math.abs(-5L))
  println("abs-l-minvalue:" + (Math.abs(scala.Long.MinValue) == scala.Long.MinValue))
  println("abs-d-pos:" + Math.abs(3.5))
  println("abs-d-neg:" + Math.abs(-3.5))
  println("abs-f-pos:" + Math.abs(2.5f))
  println("abs-f-neg:" + Math.abs(-2.5f))

  // -------- floor-div / floor-mod ------------------------------------

  // Java's floorDiv/floorMod have well-defined behavior on negatives,
  // unlike plain / and % which truncate toward zero.
  println("floordiv-7-2:" + Math.floorDiv(7, 2))       // 3
  println("floordiv-neg7-2:" + Math.floorDiv(-7, 2))   // -4 (not -3)
  println("floordiv-7-neg2:" + Math.floorDiv(7, -2))   // -4
  println("floordiv-neg7-neg2:" + Math.floorDiv(-7, -2)) // 3
  println("floordiv-0-5:" + Math.floorDiv(0, 5))       // 0
  println("floordiv-6-3:" + Math.floorDiv(6, 3))       // 2

  println("floormod-7-2:" + Math.floorMod(7, 2))       // 1
  println("floormod-neg7-2:" + Math.floorMod(-7, 2))   // 1 (not -1)
  println("floormod-7-neg2:" + Math.floorMod(7, -2))   // -1
  println("floormod-neg7-neg2:" + Math.floorMod(-7, -2)) // -1
  println("floormod-0-5:" + Math.floorMod(0, 5))       // 0
  println("floormod-6-3:" + Math.floorMod(6, 3))       // 0

  // Long overloads.
  println("floordiv-l-neg7-2:" + Math.floorDiv(-7L, 2L))
  println("floormod-l-neg7-2:" + Math.floorMod(-7L, 2L))
  println("floordiv-li-neg7-2:" + Math.floorDiv(-7L, 2))
  println("floormod-li-neg7-2:" + Math.floorMod(-7L, 2))

  // -------- exact arithmetic -----------------------------------------

  // Happy paths.
  println("addexact-i:" + Math.addExact(1, 2))
  println("addexact-l:" + Math.addExact(1L, 2L))
  println("subexact-i:" + Math.subtractExact(5, 3))
  println("subexact-l:" + Math.subtractExact(5L, 3L))
  println("mulexact-i:" + Math.multiplyExact(3, 4))
  println("mulexact-l:" + Math.multiplyExact(3L, 4L))
  println("mulexact-li:" + Math.multiplyExact(3L, 4))
  println("incexact-i:" + Math.incrementExact(5))
  println("incexact-l:" + Math.incrementExact(5L))
  println("decexact-i:" + Math.decrementExact(5))
  println("decexact-l:" + Math.decrementExact(5L))
  println("negexact-i:" + Math.negateExact(5))
  println("negexact-l:" + Math.negateExact(5L))
  println("toint-l:" + Math.toIntExact(42L))

  // Overflow paths: each must throw ArithmeticException.
  println("addexact-i-ovf:" + catchArith(Math.addExact(scala.Int.MaxValue, 1)))
  println("addexact-l-ovf:" + catchArith(Math.addExact(scala.Long.MaxValue, 1L)))
  println("subexact-i-ovf:" + catchArith(Math.subtractExact(scala.Int.MinValue, 1)))
  println("subexact-l-ovf:" + catchArith(Math.subtractExact(scala.Long.MinValue, 1L)))
  println("mulexact-i-ovf:" + catchArith(Math.multiplyExact(scala.Int.MaxValue, 2)))
  println("mulexact-l-ovf:" + catchArith(Math.multiplyExact(scala.Long.MaxValue, 2L)))
  println("incexact-i-ovf:" + catchArith(Math.incrementExact(scala.Int.MaxValue)))
  println("incexact-l-ovf:" + catchArith(Math.incrementExact(scala.Long.MaxValue)))
  println("decexact-i-ovf:" + catchArith(Math.decrementExact(scala.Int.MinValue)))
  println("decexact-l-ovf:" + catchArith(Math.decrementExact(scala.Long.MinValue)))
  println("negexact-i-ovf:" + catchArith(Math.negateExact(scala.Int.MinValue)))
  println("negexact-l-ovf:" + catchArith(Math.negateExact(scala.Long.MinValue)))
  println("toint-l-ovf:" + catchArith(Math.toIntExact(scala.Long.MaxValue)))
  println("toint-l-neg-ovf:" + catchArith(Math.toIntExact(scala.Long.MinValue)))

  // -------- multiplyFull / multiplyHigh -----------------------------

  println("mulfull:" + Math.multiplyFull(scala.Int.MaxValue, 2))
  println("mulfull-neg:" + Math.multiplyFull(-3, 4))
  println("mulhigh-small:" + Math.multiplyHigh(1L, 1L)) // 0
  println("mulhigh-big:" + Math.multiplyHigh(scala.Long.MaxValue, 2L))
  println("umulhigh-small:" + Math.unsignedMultiplyHigh(1L, 1L))

  // -------- min / max -----------------------------------------------

  println("min-i:" + Math.min(3, 5))
  println("min-i-neg:" + Math.min(-3, -5))
  println("max-i:" + Math.max(3, 5))
  println("max-i-neg:" + Math.max(-3, -5))
  println("min-l:" + Math.min(3L, 5L))
  println("max-l:" + Math.max(3L, 5L))
  println("min-d:" + Math.min(3.2, 5.7))
  println("max-d:" + Math.max(3.2, 5.7))
  println("min-f:" + Math.min(3.2f, 5.7f))
  println("max-f:" + Math.max(3.2f, 5.7f))

  // NaN propagation.
  println("min-d-nan1:" + (Math.min(1.0, scala.Double.NaN) != Math.min(1.0, scala.Double.NaN)))
  println("min-d-nan2:" + (Math.min(scala.Double.NaN, 1.0) != Math.min(scala.Double.NaN, 1.0)))
  println("max-d-nan1:" + (Math.max(1.0, scala.Double.NaN) != Math.max(1.0, scala.Double.NaN)))
  println("max-d-nan2:" + (Math.max(scala.Double.NaN, 1.0) != Math.max(scala.Double.NaN, 1.0)))
  println("min-f-nan:" + (Math.min(1.0f, scala.Float.NaN) != Math.min(1.0f, scala.Float.NaN)))
  println("max-f-nan:" + (Math.max(1.0f, scala.Float.NaN) != Math.max(1.0f, scala.Float.NaN)))

  // -------- signum ---------------------------------------------------

  println("signum-d-pos:" + Math.signum(3.5))
  println("signum-d-neg:" + Math.signum(-3.5))
  println("signum-d-zero:" + Math.signum(0.0))
  println("signum-d-nan-isnan:" + (Math.signum(scala.Double.NaN) != Math.signum(scala.Double.NaN)))
  println("signum-f-pos:" + Math.signum(3.5f))
  println("signum-f-neg:" + Math.signum(-3.5f))
  println("signum-f-zero:" + Math.signum(0.0f))

  // -------- copySign -------------------------------------------------

  println("copysign-pos:" + Math.copySign(3.0, 1.0))
  println("copysign-neg:" + Math.copySign(3.0, -1.0))
  println("copysign-neg-from-pos:" + Math.copySign(-3.0, 1.0))
  println("copysign-neg-from-neg:" + Math.copySign(-3.0, -1.0))
  println("copysign-zero-sign:" + Math.copySign(5.0, -0.0))
  println("copysign-f-pos:" + Math.copySign(3.0f, 1.0f))
  println("copysign-f-neg:" + Math.copySign(3.0f, -1.0f))

  // -------- nextUp / nextDown / nextAfter ----------------------------

  // nextUp(1.0) is the smallest representable > 1.0.
  println("nextup-1-gt:" + (Math.nextUp(1.0) > 1.0))
  println("nextdown-1-lt:" + (Math.nextDown(1.0) < 1.0))
  println("nextafter-up:" + (Math.nextAfter(1.0, 2.0) > 1.0))
  println("nextafter-down:" + (Math.nextAfter(1.0, 0.0) < 1.0))
  println("nextafter-same:" + (Math.nextAfter(1.0, 1.0) == 1.0))
  // nextUp then nextDown round-trips.
  val x = 1.5
  println("nextup-down-rt:" + (Math.nextDown(Math.nextUp(x)) == x))
  // Float variants.
  println("nextup-f-gt:" + (Math.nextUp(1.0f) > 1.0f))
  println("nextdown-f-lt:" + (Math.nextDown(1.0f) < 1.0f))

  // -------- ulp -------------------------------------------------------

  // ulp(1.0) should be 2^-52 ≈ 2.22e-16.
  val ulp1 = Math.ulp(1.0)
  println("ulp-1-pos:" + (ulp1 > 0.0))
  println("ulp-1-tiny:" + (ulp1 < 1e-15))
  println("ulp-1-exact:" + (ulp1 == java.lang.Math.pow(2.0, -52.0)))
  // ulp(0.0) should be MIN_VALUE (smallest subnormal).
  val ulp0 = Math.ulp(0.0)
  println("ulp-0-pos:" + (ulp0 > 0.0))
  println("ulp-0-subnormal:" + (ulp0 < 1e-300))
  // ulp(inf) should be inf.
  println("ulp-inf:" + (Math.ulp(scala.Double.PositiveInfinity) == scala.Double.PositiveInfinity))

  // -------- IEEEremainder --------------------------------------------

  println("ieee-rem-7-4:" + Math.IEEEremainder(7.0, 4.0)) // -1.0
  println("ieee-rem-10-3:" + Math.IEEEremainder(10.0, 3.0)) // 1.0

  // -------- clamp ----------------------------------------------------

  println("clamp-l-in:" + Math.clamp(3L, 0, 10))
  println("clamp-l-lo:" + Math.clamp(-5L, 0, 10))
  println("clamp-l-hi:" + Math.clamp(15L, 0, 10))
  println("clamp-ll-in:" + Math.clamp(3L, 0L, 10L))
  println("clamp-d-in:" + Math.clamp(3.5, 0.0, 10.0))
  println("clamp-d-lo:" + Math.clamp(-1.5, 0.0, 10.0))
  println("clamp-d-hi:" + Math.clamp(15.5, 0.0, 10.0))

  // -------- done -----------------------------------------------------

  println("done:true")

private inline def catchArith(inline thunk: => Any): String =
  try
    thunk
    "no-throw"
  catch
    case _: ArithmeticException => "ok"
