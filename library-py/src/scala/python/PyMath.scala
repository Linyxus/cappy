package scala.python

import scala.language.dynamics

/** Public wrapper for Python's `math` module.
 *
 *  We intentionally keep this as a plain Scala object instead of a raw
 *  `@extern("math")` facade object: Python's API has overload-like
 *  optional parameters, keyword-only parameters, variadics, and tuple /
 *  arbitrary-precision-int returns that do not fit cleanly into the
 *  backend's current facade restrictions.
 */
object PyMath:
  @extern("math")
  private object math extends PyDynamic

  def pi: Double = math.pi.asInstanceOf[Double]
  def e: Double = math.e.asInstanceOf[Double]
  def tau: Double = math.tau.asInstanceOf[Double]
  def inf: Double = math.inf.asInstanceOf[Double]
  def nan: Double = math.nan.asInstanceOf[Double]

  def comb(n: Long, k: Long): PyAny =
    math.comb(n, k).asInstanceOf[PyAny]

  def factorial(n: Long): PyAny =
    math.factorial(n).asInstanceOf[PyAny]

  def gcd(): PyAny =
    math.gcd().asInstanceOf[PyAny]

  def gcd(a: Long): PyAny =
    math.gcd(a).asInstanceOf[PyAny]

  def gcd(a: Long, b: Long): PyAny =
    math.gcd(a, b).asInstanceOf[PyAny]

  def gcd(a: Long, b: Long, c: Long): PyAny =
    math.gcd(a, b, c).asInstanceOf[PyAny]

  def isqrt(n: Long): PyAny =
    math.isqrt(n).asInstanceOf[PyAny]

  def lcm(): PyAny =
    math.lcm().asInstanceOf[PyAny]

  def lcm(a: Long): PyAny =
    math.lcm(a).asInstanceOf[PyAny]

  def lcm(a: Long, b: Long): PyAny =
    math.lcm(a, b).asInstanceOf[PyAny]

  def lcm(a: Long, b: Long, c: Long): PyAny =
    math.lcm(a, b, c).asInstanceOf[PyAny]

  def perm(n: Long): PyAny =
    math.perm(n).asInstanceOf[PyAny]

  def perm(n: Long, k: Long): PyAny =
    math.perm(n, k).asInstanceOf[PyAny]

  def ceil(x: Double): PyAny =
    math.ceil(x).asInstanceOf[PyAny]

  def fabs(x: Double): Double =
    math.fabs(x).asInstanceOf[Double]

  def floor(x: Double): PyAny =
    math.floor(x).asInstanceOf[PyAny]

  def fma(x: Double, y: Double, z: Double): Double =
    math.fma(x, y, z).asInstanceOf[Double]

  def fmod(x: Double, y: Double): Double =
    math.fmod(x, y).asInstanceOf[Double]

  def modf(x: Double): PyAny =
    math.modf(x).asInstanceOf[PyAny]

  def remainder(x: Double, y: Double): Double =
    math.remainder(x, y).asInstanceOf[Double]

  def trunc(x: Double): PyAny =
    math.trunc(x).asInstanceOf[PyAny]

  def copysign(x: Double, y: Double): Double =
    math.copysign(x, y).asInstanceOf[Double]

  def frexp(x: Double): PyAny =
    math.frexp(x).asInstanceOf[PyAny]

  def isclose(a: Double, b: Double): Boolean =
    math.isclose(a, b).asInstanceOf[Boolean]

  def isclose(a: Double, b: Double, relTol: Double, absTol: Double): Boolean =
    math.isclose(a, b, rel_tol = relTol, abs_tol = absTol).asInstanceOf[Boolean]

  def isfinite(x: Double): Boolean =
    math.isfinite(x).asInstanceOf[Boolean]

  def isinf(x: Double): Boolean =
    math.isinf(x).asInstanceOf[Boolean]

  def isnan(x: Double): Boolean =
    math.isnan(x).asInstanceOf[Boolean]

  def ldexp(x: Double, i: Int): Double =
    math.ldexp(x, i).asInstanceOf[Double]

  def nextafter(x: Double, y: Double): Double =
    math.nextafter(x, y).asInstanceOf[Double]

  def nextafter(x: Double, y: Double, steps: Long): Double =
    math.nextafter(x, y, steps = steps).asInstanceOf[Double]

  def ulp(x: Double): Double =
    math.ulp(x).asInstanceOf[Double]

  def cbrt(x: Double): Double =
    math.cbrt(x).asInstanceOf[Double]

  def exp(x: Double): Double =
    math.exp(x).asInstanceOf[Double]

  def exp2(x: Double): Double =
    math.exp2(x).asInstanceOf[Double]

  def expm1(x: Double): Double =
    math.expm1(x).asInstanceOf[Double]

  def log(x: Double): Double =
    math.log(x).asInstanceOf[Double]

  def log(x: Double, base: Double): Double =
    math.log(x, base).asInstanceOf[Double]

  def log1p(x: Double): Double =
    math.log1p(x).asInstanceOf[Double]

  def log2(x: Double): Double =
    math.log2(x).asInstanceOf[Double]

  def log10(x: Double): Double =
    math.log10(x).asInstanceOf[Double]

  def pow(x: Double, y: Double): Double =
    math.pow(x, y).asInstanceOf[Double]

  def sqrt(x: Double): Double =
    math.sqrt(x).asInstanceOf[Double]

  def dist(p: Any, q: Any): Double =
    math.dist(p, q).asInstanceOf[Double]

  def fsum(iterable: Any): Double =
    math.fsum(iterable).asInstanceOf[Double]

  def hypot(): Double =
    math.hypot().asInstanceOf[Double]

  def hypot(x: Double): Double =
    math.hypot(x).asInstanceOf[Double]

  def hypot(x: Double, y: Double): Double =
    math.hypot(x, y).asInstanceOf[Double]

  def hypot(x: Double, y: Double, z: Double): Double =
    math.hypot(x, y, z).asInstanceOf[Double]

  def prod(iterable: Any): PyAny =
    math.prod(iterable).asInstanceOf[PyAny]

  def prod(iterable: Any, start: Any): PyAny =
    math.prod(iterable, start = start).asInstanceOf[PyAny]

  def sumprod(p: Any, q: Any): PyAny =
    math.sumprod(p, q).asInstanceOf[PyAny]

  def degrees(x: Double): Double =
    math.degrees(x).asInstanceOf[Double]

  def radians(x: Double): Double =
    math.radians(x).asInstanceOf[Double]

  def acos(x: Double): Double =
    math.acos(x).asInstanceOf[Double]

  def asin(x: Double): Double =
    math.asin(x).asInstanceOf[Double]

  def atan(x: Double): Double =
    math.atan(x).asInstanceOf[Double]

  def atan2(y: Double, x: Double): Double =
    math.atan2(y, x).asInstanceOf[Double]

  def cos(x: Double): Double =
    math.cos(x).asInstanceOf[Double]

  def sin(x: Double): Double =
    math.sin(x).asInstanceOf[Double]

  def tan(x: Double): Double =
    math.tan(x).asInstanceOf[Double]

  def acosh(x: Double): Double =
    math.acosh(x).asInstanceOf[Double]

  def asinh(x: Double): Double =
    math.asinh(x).asInstanceOf[Double]

  def atanh(x: Double): Double =
    math.atanh(x).asInstanceOf[Double]

  def cosh(x: Double): Double =
    math.cosh(x).asInstanceOf[Double]

  def sinh(x: Double): Double =
    math.sinh(x).asInstanceOf[Double]

  def tanh(x: Double): Double =
    math.tanh(x).asInstanceOf[Double]

  def erf(x: Double): Double =
    math.erf(x).asInstanceOf[Double]

  def erfc(x: Double): Double =
    math.erfc(x).asInstanceOf[Double]

  def gamma(x: Double): Double =
    math.gamma(x).asInstanceOf[Double]

  def lgamma(x: Double): Double =
    math.lgamma(x).asInstanceOf[Double]
