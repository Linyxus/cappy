import scala.python.runtime.PyMath

@main def pyfacadeMath(): Unit =
  // --- Constants ---------------------------------------------------
  println("pi-close:" + (PyMath.pi > 3.14159 && PyMath.pi < 3.14160))
  println("e-close:" + (PyMath.e > 2.71828 && PyMath.e < 2.71829))
  println("tau-close:" + (PyMath.tau > 6.28318 && PyMath.tau < 6.28319))
  println("inf-is-inf:" + PyMath.isinf(PyMath.inf))
  println("inf-is-positive:" + (PyMath.inf > 0.0))
  println("neg-inf:" + PyMath.isinf(-PyMath.inf))
  println("nan-is-nan:" + PyMath.isnan(PyMath.nan))

  // --- Sqrt / cbrt / pow / exp / log ------------------------------
  println("sqrt-0:" + PyMath.sqrt(0.0))
  println("sqrt-1:" + PyMath.sqrt(1.0))
  println("sqrt-4:" + PyMath.sqrt(4.0))
  println("sqrt-9:" + PyMath.sqrt(9.0))
  println("sqrt-16:" + PyMath.sqrt(16.0))
  println("cbrt-0:" + PyMath.cbrt(0.0))
  println("cbrt-1:" + PyMath.cbrt(1.0))
  println("cbrt-8:" + PyMath.cbrt(8.0))
  println("cbrt-27:" + PyMath.cbrt(27.0))
  println("cbrt-neg8:" + PyMath.cbrt(-8.0))

  println("pow-2-0:" + PyMath.pow(2.0, 0.0))
  println("pow-2-1:" + PyMath.pow(2.0, 1.0))
  println("pow-2-10:" + PyMath.pow(2.0, 10.0))
  println("pow-2-neg1:" + PyMath.pow(2.0, -1.0))
  println("pow-10-3:" + PyMath.pow(10.0, 3.0))

  println("exp-0:" + PyMath.exp(0.0))
  println("exp-1-close:" + (PyMath.exp(1.0) > 2.71828 && PyMath.exp(1.0) < 2.71829))
  println("expm1-0:" + PyMath.expm1(0.0))
  println("exp2-0:" + PyMath.exp2(0.0))
  println("exp2-1:" + PyMath.exp2(1.0))
  println("exp2-5:" + PyMath.exp2(5.0))
  println("exp2-10:" + PyMath.exp2(10.0))

  println("log-1:" + PyMath.log(1.0))
  println("log-e-close:" + (PyMath.log(PyMath.e) > 0.99999 && PyMath.log(PyMath.e) < 1.00001))
  println("log-base-2-8:" + PyMath.log(8.0, 2.0))
  println("log-base-10-1000:" + PyMath.log(1000.0, 10.0))
  println("log1p-0:" + PyMath.log1p(0.0))
  println("log2-1:" + PyMath.log2(1.0))
  println("log2-2:" + PyMath.log2(2.0))
  println("log2-1024:" + PyMath.log2(1024.0))
  println("log10-1:" + PyMath.log10(1.0))
  println("log10-10:" + PyMath.log10(10.0))
  println("log10-100:" + PyMath.log10(100.0))

  // --- Trig --------------------------------------------------------
  println("sin-0:" + PyMath.sin(0.0))
  println("cos-0:" + PyMath.cos(0.0))
  println("tan-0:" + PyMath.tan(0.0))
  println("asin-0:" + PyMath.asin(0.0))
  println("asin-1-close:" + (PyMath.asin(1.0) > 1.5707 && PyMath.asin(1.0) < 1.5708))
  println("acos-1:" + PyMath.acos(1.0))
  println("atan-0:" + PyMath.atan(0.0))
  println("atan2-0-1:" + PyMath.atan2(0.0, 1.0))
  println("atan2-1-0-close:" + (PyMath.atan2(1.0, 0.0) > 1.5707 && PyMath.atan2(1.0, 0.0) < 1.5708))
  println("degrees-pi:" + PyMath.degrees(PyMath.pi))
  println("radians-180-close:" + (PyMath.radians(180.0) > 3.14159 && PyMath.radians(180.0) < 3.14160))

  // --- Hyperbolic --------------------------------------------------
  println("sinh-0:" + PyMath.sinh(0.0))
  println("cosh-0:" + PyMath.cosh(0.0))
  println("tanh-0:" + PyMath.tanh(0.0))
  println("asinh-0:" + PyMath.asinh(0.0))
  println("acosh-1:" + PyMath.acosh(1.0))
  println("atanh-0:" + PyMath.atanh(0.0))

  // --- Rounding / classification ----------------------------------
  println("floor-3.7:" + PyMath.floor(3.7))
  println("floor-neg-3.7:" + PyMath.floor(-3.7))
  println("ceil-3.2:" + PyMath.ceil(3.2))
  println("ceil-neg-3.2:" + PyMath.ceil(-3.2))
  println("trunc-3.7:" + PyMath.trunc(3.7))
  println("trunc-neg-3.7:" + PyMath.trunc(-3.7))
  println("fabs-3.5:" + PyMath.fabs(3.5))
  println("fabs-neg-3.5:" + PyMath.fabs(-3.5))
  println("copysign-3-neg1:" + PyMath.copysign(3.0, -1.0))
  println("copysign-neg3-1:" + PyMath.copysign(-3.0, 1.0))
  println("fmod-7-3:" + PyMath.fmod(7.0, 3.0))
  println("fmod-neg-7-3:" + PyMath.fmod(-7.0, 3.0))
  println("remainder-7-3:" + PyMath.remainder(7.0, 3.0))

  println("isfinite-1:" + PyMath.isfinite(1.0))
  println("isfinite-inf:" + PyMath.isfinite(PyMath.inf))
  println("isfinite-nan:" + PyMath.isfinite(PyMath.nan))
  println("isinf-1:" + PyMath.isinf(1.0))
  println("isnan-1:" + PyMath.isnan(1.0))

  // --- Integer helpers --------------------------------------------
  println("factorial-0:" + PyMath.factorial(0))
  println("factorial-5:" + PyMath.factorial(5))
  println("factorial-10:" + PyMath.factorial(10))
  println("gcd-0:" + PyMath.gcd())
  println("gcd-1:" + PyMath.gcd(12))
  println("gcd-12-18:" + PyMath.gcd(12, 18))
  println("gcd-48-18-30:" + PyMath.gcd(48, 18, 30))
  println("lcm-0:" + PyMath.lcm())
  println("lcm-1:" + PyMath.lcm(6))
  println("lcm-4-6:" + PyMath.lcm(4, 6))
  println("lcm-4-6-10:" + PyMath.lcm(4, 6, 10))
  println("isqrt-0:" + PyMath.isqrt(0))
  println("isqrt-1:" + PyMath.isqrt(1))
  println("isqrt-9:" + PyMath.isqrt(9))
  println("isqrt-10:" + PyMath.isqrt(10))
  println("comb-5-2:" + PyMath.comb(5, 2))
  println("comb-10-3:" + PyMath.comb(10, 3))
  println("perm-5:" + PyMath.perm(5))
  println("perm-5-2:" + PyMath.perm(5, 2))

  // --- FMA ---------------------------------------------------------
  println("fma:" + PyMath.fma(2.0, 3.0, 4.0))
  println("fma-zero:" + PyMath.fma(0.0, 1e300, 1.0))

  // --- Hypot / dist -----------------------------------------------
  println("hypot-0:" + PyMath.hypot())
  println("hypot-1:" + PyMath.hypot(3.0))
  println("hypot-3-4:" + PyMath.hypot(3.0, 4.0))
  println("hypot-2-3-6:" + PyMath.hypot(2.0, 3.0, 6.0))
  println("dist-2d:" + PyMath.dist(Array(0.0, 0.0), Array(3.0, 4.0)))
  println("dist-3d:" + PyMath.dist(Array(0.0, 0.0, 0.0), Array(2.0, 3.0, 6.0)))

  // --- Aggregates --------------------------------------------------
  println("prod:" + PyMath.prod(Array(2, 3, 4)))
  println("prod-start:" + PyMath.prod(Array(2, 3, 4), start = 5))
  println("sumprod:" + PyMath.sumprod(Array(1, 2, 3), Array(4, 5, 6)))

  // --- isclose -----------------------------------------------------
  println("isclose-same:" + PyMath.isclose(1.0, 1.0))
  println("isclose-kw:" + PyMath.isclose(1.0, 1.1, relTol = 0.2, absTol = 0.0))
  println("isclose-tight:" + PyMath.isclose(1.0, 1.001, relTol = 1e-6, absTol = 0.0))

  // --- nextafter / ulp / frexp / ldexp / modf ---------------------
  println("nextafter-up:" + (PyMath.nextafter(1.0, 2.0) > 1.0))
  println("nextafter-down:" + (PyMath.nextafter(1.0, 0.0) < 1.0))
  println("nextafter-steps:" + (PyMath.nextafter(1.0, 2.0, steps = 2) > PyMath.nextafter(1.0, 2.0)))
  println("ulp-1-positive:" + (PyMath.ulp(1.0) > 0.0))
  println("frexp-8:" + PyMath.frexp(8.0))
  println("ldexp-0.5-4:" + PyMath.ldexp(0.5, 4))
  println("modf-2.5:" + PyMath.modf(2.5))

  // --- Gamma / erf -----------------------------------------------
  println("gamma-1:" + PyMath.gamma(1.0))
  println("gamma-5:" + PyMath.gamma(5.0))
  println("erf-0:" + PyMath.erf(0.0))
  println("erfc-0:" + PyMath.erfc(0.0))
