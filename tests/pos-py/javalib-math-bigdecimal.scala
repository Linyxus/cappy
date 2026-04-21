import java.math.{BigDecimal, BigInteger, MathContext, RoundingMode}

@main def javalibMathBigdecimal(): Unit =
  val a = new BigDecimal("123.4500")
  val b = new BigDecimal("0.55")
  println("arith:" + a.add(b) + ":" + a.subtract(b) + ":" + a.multiply(b))

  val unnecessary =
    try
      new BigDecimal("1").divide(new BigDecimal("3"), 2, RoundingMode.UNNECESSARY)
      "no"
    catch
      case _: ArithmeticException => "yes"
  println(
    "divide-rounding:" +
      new BigDecimal("1").divide(new BigDecimal("3"), 2, RoundingMode.HALF_UP) +
      ":" +
      new BigDecimal("1").divide(new BigDecimal("3"), 2, RoundingMode.HALF_EVEN) +
      ":" +
      new BigDecimal("-1").divide(new BigDecimal("3"), 2, RoundingMode.FLOOR) +
      ":" +
      new BigDecimal("-1").divide(new BigDecimal("3"), 2, RoundingMode.CEILING) +
      ":" +
      unnecessary
  )

  println(
    "scale:" +
      new BigDecimal("1.234").setScale(2, RoundingMode.HALF_UP) +
      ":" +
      new BigDecimal("1.235").setScale(2, RoundingMode.HALF_EVEN) +
      ":" +
      new BigDecimal("1200.00").stripTrailingZeros().toString()
  )

  val scientific = new BigDecimal(BigInteger.ONE, -6)
  println(
    "tostring:" +
      scientific.toString() +
      ":" +
      scientific.toPlainString() +
      ":" +
      new BigDecimal("12345E6").toEngineeringString()
  )

  println("unscaled:" + a.unscaledValue().toString(10) + ":" + a.scale())

  val rounded = new BigDecimal("123456789.987654321").round(MathContext.DECIMAL32)
  println("context:" + rounded.toString() + ":" + rounded.precision())

  val exactIntegral = new BigDecimal("12.0").toBigIntegerExact()
  val exactFailure =
    try
      new BigDecimal("12.5").toBigIntegerExact()
      "no"
    catch
      case _: ArithmeticException => "yes"
  println("exact:" + exactIntegral.toString(10) + ":" + exactFailure)

  val qr = new BigDecimal("7.5").divideAndRemainder(new BigDecimal("2"))
  println("quotrem:" + qr(0) + ":" + qr(1))
