import java.math.{BigDecimal, BigInteger, MathContext, RoundingMode}

@main def javalibMathExtras(): Unit =
  // JDK caches BigInteger.valueOf for -16..16. After the cache-range
  // fix, `valueOf` returns the same instance on repeat calls in that
  // range. Scala `eq` maps to Python `is` so this exercises identity
  // semantics (the one path where `==`-equivalent reference compare
  // is actually desired).
  val cachedA = BigInteger.valueOf(5L)
  val cachedB = BigInteger.valueOf(5L)
  val edgeLow = BigInteger.valueOf(-16L)
  val edgeHigh = BigInteger.valueOf(16L)
  val outsideCache = BigInteger.valueOf(17L)
  val outsideCacheDup = BigInteger.valueOf(17L)
  println(
    "valueof-cache:" +
      (cachedA eq cachedB) + ":" +
      (BigInteger.valueOf(-16L) eq edgeLow) + ":" +
      (BigInteger.valueOf(16L) eq edgeHigh) + ":" +
      (outsideCache eq outsideCacheDup)
  )

  // MathContext constants return fresh instances on each access —
  // JVM's `public static final` identity can't be matched because
  // Scala 3's `lazy val` synthesis on an object references
  // JVM-only `java.lang.invoke.MethodHandles`, and a plain `val`
  // trips module-init ordering on the Python backend. `.equals`
  // yields value equality regardless. See
  // `notes/issue-module-init-ordering-module-dependency.md`.
  val customA = new MathContext(7, RoundingMode.HALF_EVEN)
  println(
    "mathcontext-equals:" +
      MathContext.DECIMAL32.equals(MathContext.DECIMAL32) + ":" +
      customA.equals(MathContext.DECIMAL32) + ":" +
      MathContext.UNLIMITED.equals(new MathContext(0, RoundingMode.HALF_UP))
  )

  // BigInteger.valueOf(Double) variants: NaN/Infinity rejected for
  // BigDecimal.valueOf(Double).
  val nanThrew =
    try
      BigDecimal.valueOf(java.lang.Double.NaN)
      "no"
    catch
      case _: NumberFormatException => "yes"
  val infThrew =
    try
      BigDecimal.valueOf(java.lang.Double.POSITIVE_INFINITY)
      "no"
    catch
      case _: NumberFormatException => "yes"
  println("double-sanity:" + nanThrew + ":" + infThrew)

  // BigInteger `.equals(AnyRef other)` — non-BigInteger argument
  // returns false (the Any-matched `case _ => false` branch).
  val bi = BigInteger.valueOf(42L)
  val notBigInt: AnyRef = "42"
  println("equals-noninteger:" + bi.equals(notBigInt) + ":" + bi.equals(null))

  // BigInteger negative-value `toString(radix)` formats with a
  // leading `-` sign, not two's-complement digits.
  println(
    "negative-radix:" +
      BigInteger.valueOf(-255L).toString(16) + ":" +
      BigInteger.valueOf(-10L).toString(2) + ":" +
      BigInteger.valueOf(-42L).toString(36)
  )

  // BigInteger zero handling in toByteArray.
  val zeroBytes = BigInteger.ZERO.toByteArray()
  println("zero-bytes:" + zeroBytes.length + ":" + zeroBytes(0).toInt)

  // BigInteger `.gcd(ZERO)` = abs(this). `.gcd` with negatives.
  println(
    "gcd-edge:" +
      BigInteger.valueOf(42L).gcd(BigInteger.ZERO).toString(10) + ":" +
      BigInteger.valueOf(-12L).gcd(BigInteger.valueOf(18L)).toString(10)
  )

  // Miller-Rabin on a sizeable prime: 2^31-1 (Mersenne prime M31 =
  // 2147483647). Also exercise the adversarial path (Carmichael
  // composite 561 = 3*11*17) — documented limit of deterministic
  // witnesses, but 561 is clearly composite via small-prime pre-check
  // so it's a safe smoke test.
  val m31 = BigInteger.valueOf(2147483647L)
  val carmichael561 = BigInteger.valueOf(561L)
  println("primality-large:" + m31.isProbablePrime(40) + ":" + carmichael561.isProbablePrime(40))

  // BigDecimal unary ops + ulp
  val bd = new BigDecimal("-12.345")
  println(
    "unary:" +
      bd.abs().toString() + ":" +
      bd.negate().toString() + ":" +
      bd.ulp().toString() + ":" +
      bd.signum()
  )

  // BigDecimal scaleByPowerOfTen / movePointLeft / movePointRight
  val bdm = new BigDecimal("1.23")
  println(
    "move:" +
      bdm.scaleByPowerOfTen(2).toString() + ":" +
      bdm.movePointLeft(3).toString() + ":" +
      bdm.movePointRight(1).toString()
  )

  // BigDecimal.equals requires scale match
  val scale1 = new BigDecimal("1.0")
  val scale2 = new BigDecimal("1.00")
  val same1 = new BigDecimal("1.0")
  println(
    "bd-equals:" +
      scale1.equals(scale2) + ":" +
      scale1.equals(same1) + ":" +
      scale1.compareTo(scale2)
  )

  // BigDecimal.divide(BigDecimal) — no MathContext — must throw on
  // non-terminating decimal.
  val nonTerminating =
    try
      new BigDecimal("1").divide(new BigDecimal("3"))
      "no"
    catch
      case _: ArithmeticException => "yes"
  val terminating = new BigDecimal("1").divide(new BigDecimal("4"))
  println("exact-divide:" + nonTerminating + ":" + terminating.toString())

  // BigDecimal divide with MathContext — bounded precision, rounds.
  val mc = new MathContext(7, RoundingMode.HALF_UP)
  println("context-divide:" + new BigDecimal("1").divide(new BigDecimal("3"), mc).toString())
