package java.math

import scala.python.PyDynamic
import scala.python.runtime.PyDecimal

object BigDecimal:
  private final class FactorCount(val value: BigInteger, val count: Int)

  def ZERO: BigDecimal = new BigDecimal(0L)
  def ONE: BigDecimal = new BigDecimal(1L)
  def TEN: BigDecimal = new BigDecimal(10L)

  final val ROUND_UP = 0
  final val ROUND_DOWN = 1
  final val ROUND_CEILING = 2
  final val ROUND_FLOOR = 3
  final val ROUND_HALF_UP = 4
  final val ROUND_HALF_DOWN = 5
  final val ROUND_HALF_EVEN = 6
  final val ROUND_UNNECESSARY = 7

  private def Two: BigInteger = BigInteger.TWO
  private def Five: BigInteger = BigInteger.valueOf(5L)

  def valueOf(unscaledVal: Long, scale: Int): BigDecimal =
    new BigDecimal(BigInteger.valueOf(unscaledVal), scale)

  def valueOf(unscaledVal: Long): BigDecimal =
    new BigDecimal(unscaledVal)

  def valueOf(d: Double): BigDecimal =
    if java.lang.Double.isNaN(d) || java.lang.Double.isInfinite(d) then
      throw new NumberFormatException("Infinity or NaN: " + d)
    new BigDecimal(java.lang.Double.toString(d))

  private[math] def unsafeFromPy(value: PyDynamic): BigDecimal =
    new BigDecimal(value)

  private def applyContext(value: PyDynamic, mc: MathContext): PyDynamic =
    if mc.getPrecision() == 0 then value
    else PyDecimal.unaryWithContext("plus", value, mc.getPrecision(), mc.getRoundingMode().pythonName)

  private def parseString(text: String): PyDynamic =
    try PyDecimal.fromString(text)
    catch case _: Throwable => throw new NumberFormatException(s"""For input string: "$text"""")

  private def parseString(text: String, mc: MathContext): PyDynamic =
    applyContext(parseString(text), mc)

  private def fromDoubleExact(value: Double): PyDynamic =
    if java.lang.Double.isNaN(value) || java.lang.Double.isInfinite(value) then
      throw new NumberFormatException("Infinity or NaN: " + value)
    PyDecimal.fromDouble(value)

  private def fromDoubleExact(value: Double, mc: MathContext): PyDynamic =
    applyContext(fromDoubleExact(value), mc)

  private def fromUnscaled(unscaled: BigInteger, scale: Int): PyDynamic =
    PyDecimal.scaleByPower(PyDecimal.fromPyInt(unscaled.pyValue), -scale)

  private def fromUnscaled(unscaled: BigInteger, scale: Int, mc: MathContext): PyDynamic =
    applyContext(fromUnscaled(unscaled, scale), mc)

  private def exactAddPrecision(left: BigDecimal, right: BigDecimal): Int =
    java.lang.Math.max(
      1,
      java.lang.Math.max(left.integerDigits(), right.integerDigits()) +
        java.lang.Math.max(java.lang.Math.max(left.scale(), right.scale()), 0) +
        1
    )

  private def exactMultiplyPrecision(left: BigDecimal, right: BigDecimal): Int =
    java.lang.Math.max(1, left.precision() + right.precision() + 1)

  private def exactPowPrecision(value: BigDecimal, exponent: Int): Int =
    if exponent == 0 then 1
    else java.lang.Math.max(1, value.precision() * exponent + 1)

  private def scaledDividePrecision(left: BigDecimal, right: BigDecimal, scale: Int): Int =
    java.lang.Math.max(1, left.adjusted() - right.adjusted() + 1) + java.lang.Math.max(scale, 0) + 8

  private def integralDividePrecision(left: BigDecimal, right: BigDecimal): Int =
    java.lang.Math.max(1, left.adjusted() - right.adjusted() + 1) + 1

  private def contextPrecision(mc: MathContext, fallbackPrecision: Int): Int =
    val precision =
      if mc.getPrecision() > 0 then mc.getPrecision()
      else fallbackPrecision
    java.lang.Math.max(1, precision)

  private def exactBinary(op: String, left: BigDecimal, right: BigDecimal, precision: Int): PyDynamic =
    PyDecimal.binaryWithContext(
      op,
      left.pyValue,
      right.pyValue,
      java.lang.Math.max(1, precision),
      RoundingMode.HALF_UP.pythonName
    )

  private def binaryInContext(
      op: String,
      left: BigDecimal,
      right: BigDecimal,
      mc: MathContext,
      fallbackPrecision: Int
  ): PyDynamic =
    PyDecimal.binaryWithContext(
      op,
      left.pyValue,
      right.pyValue,
      contextPrecision(mc, fallbackPrecision),
      mc.getRoundingMode().pythonName
    )

  private def exactPow(value: BigDecimal, exponent: Int, precision: Int): PyDynamic =
    PyDecimal.binaryWithContext(
      "pow",
      value.pyValue,
      exponent,
      java.lang.Math.max(1, precision),
      RoundingMode.HALF_UP.pythonName
    )

  private def powInContext(value: BigDecimal, exponent: Int, mc: MathContext, fallbackPrecision: Int): PyDynamic =
    PyDecimal.binaryWithContext(
      "pow",
      value.pyValue,
      exponent,
      contextPrecision(mc, fallbackPrecision),
      mc.getRoundingMode().pythonName
    )

  private def unaryInContext(op: String, value: BigDecimal, mc: MathContext, fallbackPrecision: Int): PyDynamic =
    PyDecimal.unaryWithContext(
      op,
      value.pyValue,
      contextPrecision(mc, fallbackPrecision),
      mc.getRoundingMode().pythonName
    )

  private def countFactor(value: BigInteger, factor: BigInteger): FactorCount =
    var current = value
    var count = 0
    while current.remainder(factor).signum() == 0 do
      current = current.divide(factor)
      count += 1
    new FactorCount(current, count)

  private def exactDivide(dividend: BigDecimal, divisor: BigDecimal): BigDecimal =
    if divisor.signum() == 0 then
      throw new ArithmeticException("Division by zero")

    val preferredScale = dividend.scale() - divisor.scale()
    var numerator = dividend.unscaledValue()
    var denominator = divisor.unscaledValue()

    if numerator.signum() == 0 then
      return new BigDecimal(BigInteger.ZERO, preferredScale)

    val scaleDelta = divisor.scale() - dividend.scale()
    if scaleDelta > 0 then
      numerator = numerator.multiply(BigInteger.TEN.pow(scaleDelta))
    else if scaleDelta < 0 then
      denominator = denominator.multiply(BigInteger.TEN.pow(-scaleDelta))

    if denominator.signum() < 0 then
      numerator = numerator.negate()
      denominator = denominator.negate()

    val common = numerator.gcd(denominator)
    if !common.equals(BigInteger.ONE) then
      numerator = numerator.divide(common)
      denominator = denominator.divide(common)

    val twosCount = countFactor(denominator, Two)
    val fivesCount = countFactor(twosCount.value, Five)
    if !fivesCount.value.equals(BigInteger.ONE) then
      throw new ArithmeticException("Non-terminating decimal expansion; no exact representable decimal result.")

    val minimalScale = java.lang.Math.max(twosCount.count, fivesCount.count)
    var unscaled =
      numerator.multiply(BigInteger.TEN.pow(minimalScale)).divide(denominator)
    val resultScale = java.lang.Math.max(preferredScale, minimalScale)
    if resultScale > minimalScale then
      unscaled = unscaled.multiply(BigInteger.TEN.pow(resultScale - minimalScale))

    new BigDecimal(unscaled, resultScale)

final class BigDecimal private[math] (private[math] val pyValue: PyDynamic)
    extends Number
    with Comparable[BigDecimal]:

  def this(in: Array[Char], offset: Int, len: Int) =
    this(BigDecimal.parseString(new String(in, offset, len)))

  def this(in: Array[Char], offset: Int, len: Int, mc: MathContext) =
    this(BigDecimal.parseString(new String(in, offset, len), mc))

  def this(in: Array[Char]) =
    this(BigDecimal.parseString(new String(in)))

  def this(in: Array[Char], mc: MathContext) =
    this(BigDecimal.parseString(new String(in), mc))

  def this(text: String) =
    this(BigDecimal.parseString(text))

  def this(text: String, mc: MathContext) =
    this(BigDecimal.parseString(text, mc))

  def this(value: Double) =
    this(BigDecimal.fromDoubleExact(value))

  def this(value: Double, mc: MathContext) =
    this(BigDecimal.fromDoubleExact(value, mc))

  def this(unscaledVal: BigInteger, scale: Int) =
    this(BigDecimal.fromUnscaled(unscaledVal, scale))

  def this(unscaledVal: BigInteger, scale: Int, mc: MathContext) =
    this(BigDecimal.fromUnscaled(unscaledVal, scale, mc))

  def this(bi: BigInteger) =
    this(BigDecimal.fromUnscaled(bi, 0))

  def this(bi: BigInteger, mc: MathContext) =
    this(BigDecimal.fromUnscaled(bi, 0, mc))

  def this(iVal: Int) =
    this(BigInteger.valueOf(iVal.toLong), 0)

  def this(iVal: Int, mc: MathContext) =
    this(BigInteger.valueOf(iVal.toLong), 0, mc)

  def this(lVal: Long) =
    this(BigInteger.valueOf(lVal), 0)

  def this(lVal: Long, mc: MathContext) =
    this(BigInteger.valueOf(lVal), 0, mc)

  private def wrap(value: PyDynamic): BigDecimal =
    BigDecimal.unsafeFromPy(value)

  private def integerDigits(): Int =
    java.lang.Math.max(1, precision() - scale())

  private def adjusted(): Int =
    PyDecimal.adjusted(pyValue)

  def add(augend: BigDecimal): BigDecimal =
    wrap(BigDecimal.exactBinary("add", this, augend, BigDecimal.exactAddPrecision(this, augend)))

  def add(augend: BigDecimal, mc: MathContext): BigDecimal =
    if mc.getPrecision() == 0 then add(augend)
    else
      wrap(BigDecimal.binaryInContext("add", this, augend, mc, BigDecimal.exactAddPrecision(this, augend)))

  def subtract(subtrahend: BigDecimal): BigDecimal =
    wrap(BigDecimal.exactBinary("subtract", this, subtrahend, BigDecimal.exactAddPrecision(this, subtrahend)))

  def subtract(subtrahend: BigDecimal, mc: MathContext): BigDecimal =
    if mc.getPrecision() == 0 then subtract(subtrahend)
    else
      wrap(BigDecimal.binaryInContext("subtract", this, subtrahend, mc, BigDecimal.exactAddPrecision(this, subtrahend)))

  def multiply(multiplicand: BigDecimal): BigDecimal =
    wrap(BigDecimal.exactBinary("multiply", this, multiplicand, BigDecimal.exactMultiplyPrecision(this, multiplicand)))

  def multiply(multiplicand: BigDecimal, mc: MathContext): BigDecimal =
    if mc.getPrecision() == 0 then multiply(multiplicand)
    else
      wrap(BigDecimal.binaryInContext("multiply", this, multiplicand, mc, BigDecimal.exactMultiplyPrecision(this, multiplicand)))

  def divide(divisor: BigDecimal, scale: Int, roundingMode: Int): BigDecimal =
    divide(divisor, scale, RoundingMode.valueOf(roundingMode))

  def divide(divisor: BigDecimal, scale: Int, roundingMode: RoundingMode): BigDecimal =
    try
      val precision = BigDecimal.scaledDividePrecision(this, divisor, scale)
      val quotient = BigDecimal.exactBinary("divide", this, divisor, precision)
      wrap(PyDecimal.quantize(quotient, scale, roundingMode.pythonName))
    catch
      case _: Throwable =>
        throw new ArithmeticException("Division undefined")

  def divide(divisor: BigDecimal, roundingMode: Int): BigDecimal =
    divide(divisor, scale(), roundingMode)

  def divide(divisor: BigDecimal, roundingMode: RoundingMode): BigDecimal =
    divide(divisor, scale(), roundingMode)

  def divide(divisor: BigDecimal): BigDecimal =
    BigDecimal.exactDivide(this, divisor)

  def divide(divisor: BigDecimal, mc: MathContext): BigDecimal =
    if mc.getPrecision() == 0 then divide(divisor)
    else
      try
        wrap(BigDecimal.binaryInContext("divide", this, divisor, mc, BigDecimal.scaledDividePrecision(this, divisor, scale())))
      catch
        case _: Throwable =>
          throw new ArithmeticException("Division undefined")

  def divideToIntegralValue(divisor: BigDecimal): BigDecimal =
    try
      wrap(BigDecimal.exactBinary("divideToIntegral", this, divisor, BigDecimal.integralDividePrecision(this, divisor)))
    catch
      case _: Throwable =>
        throw new ArithmeticException("Division undefined")

  def divideToIntegralValue(divisor: BigDecimal, mc: MathContext): BigDecimal =
    try
      wrap(BigDecimal.binaryInContext("divideToIntegral", this, divisor, mc, BigDecimal.integralDividePrecision(this, divisor)))
    catch
      case _: Throwable =>
        throw new ArithmeticException("Division undefined")

  def remainder(divisor: BigDecimal): BigDecimal =
    try
      wrap(BigDecimal.exactBinary("remainder", this, divisor, BigDecimal.integralDividePrecision(this, divisor)))
    catch
      case _: Throwable =>
        throw new ArithmeticException("Division undefined")

  def remainder(divisor: BigDecimal, mc: MathContext): BigDecimal =
    try
      wrap(BigDecimal.binaryInContext("remainder", this, divisor, mc, BigDecimal.integralDividePrecision(this, divisor)))
    catch
      case _: Throwable =>
        throw new ArithmeticException("Division undefined")

  def divideAndRemainder(divisor: BigDecimal): Array[BigDecimal] =
    Array(divideToIntegralValue(divisor), remainder(divisor))

  def divideAndRemainder(divisor: BigDecimal, mc: MathContext): Array[BigDecimal] =
    Array(divideToIntegralValue(divisor, mc), remainder(divisor, mc))

  def pow(n: Int): BigDecimal =
    if n < 0 then
      throw new ArithmeticException("Negative exponent")
    wrap(BigDecimal.exactPow(this, n, BigDecimal.exactPowPrecision(this, n)))

  def pow(n: Int, mc: MathContext): BigDecimal =
    if mc.getPrecision() == 0 then pow(n)
    else
      if n < 0 then
        throw new ArithmeticException("Negative exponent")
      wrap(BigDecimal.powInContext(this, n, mc, BigDecimal.exactPowPrecision(this, n)))

  def plus(): BigDecimal =
    this

  def plus(mc: MathContext): BigDecimal =
    round(mc)

  def round(mc: MathContext): BigDecimal =
    if mc.getPrecision() == 0 then this
    else wrap(BigDecimal.applyContext(pyValue, mc))

  def negate(): BigDecimal =
    wrap(PyDecimal.negate(pyValue))

  def negate(mc: MathContext): BigDecimal =
    if mc.getPrecision() == 0 then negate()
    else
      wrap(BigDecimal.unaryInContext("negate", this, mc, precision()))

  def abs(): BigDecimal =
    if signum() < 0 then negate() else this

  def abs(mc: MathContext): BigDecimal =
    if signum() < 0 then negate(mc) else plus(mc)

  def setScale(newScale: Int, roundingMode: RoundingMode): BigDecimal =
    try wrap(PyDecimal.quantize(pyValue, newScale, roundingMode.pythonName))
    catch case _: Throwable => throw new ArithmeticException("Rounding necessary")

  def setScale(newScale: Int, roundingMode: Int): BigDecimal =
    setScale(newScale, RoundingMode.valueOf(roundingMode))

  def setScale(newScale: Int): BigDecimal =
    setScale(newScale, RoundingMode.UNNECESSARY)

  def movePointLeft(n: Int): BigDecimal =
    scaleByPowerOfTen(-n)

  def movePointRight(n: Int): BigDecimal =
    scaleByPowerOfTen(n)

  def scaleByPowerOfTen(n: Int): BigDecimal =
    wrap(PyDecimal.scaleByPower(pyValue, n))

  def stripTrailingZeros(): BigDecimal =
    if signum() == 0 then BigDecimal.ZERO
    else wrap(PyDecimal.normalize(pyValue))

  def compareTo(that: BigDecimal): Int =
    PyDecimal.compare(pyValue, that.pyValue)

  def min(that: BigDecimal): BigDecimal =
    if compareTo(that) <= 0 then this else that

  def max(that: BigDecimal): BigDecimal =
    if compareTo(that) >= 0 then this else that

  def precision(): Int =
    PyDecimal.precision(pyValue)

  def scale(): Int =
    PyDecimal.unscaledValueAndScale(pyValue).scale

  def signum(): Int =
    PyDecimal.signum(pyValue)

  def unscaledValue(): BigInteger =
    BigInteger.unsafeFromPy(PyDecimal.unscaledValueAndScale(pyValue).unscaled)

  def ulp(): BigDecimal =
    new BigDecimal(BigInteger.ONE, scale())

  def toBigInteger(): BigInteger =
    BigInteger.unsafeFromPy(PyDecimal.toPyInt(pyValue))

  def toBigIntegerExact(): BigInteger =
    val out = toBigInteger()
    if compareTo(new BigDecimal(out)) != 0 then
      throw new ArithmeticException("Rounding necessary")
    out

  override def intValue(): Int =
    toBigInteger().intValue()

  override def longValue(): Long =
    toBigInteger().longValue()

  override def floatValue(): Float =
    PyDecimal.toFloat(pyValue)

  override def doubleValue(): Double =
    PyDecimal.toDouble(pyValue)

  override def byteValue(): Byte =
    intValue().toByte

  override def shortValue(): Short =
    intValue().toShort

  def byteValueExact(): Byte =
    val value = intValueExact()
    if value < Byte.MinValue || value > Byte.MaxValue then
      throw new ArithmeticException("Out of byte range")
    value.toByte

  def shortValueExact(): Short =
    val value = intValueExact()
    if value < Short.MinValue || value > Short.MaxValue then
      throw new ArithmeticException("Out of short range")
    value.toShort

  def intValueExact(): Int =
    val out = toBigIntegerExact()
    out.intValueExact()

  def longValueExact(): Long =
    val out = toBigIntegerExact()
    out.longValueExact()

  override def equals(other: Any): Boolean =
    other match
      case that: BigDecimal =>
        scale() == that.scale() && compareTo(that) == 0
      case _ =>
        false

  override def hashCode(): Int =
    31 * unscaledValue().hashCode() + scale()

  override def toString(): String =
    PyDecimal.toString(pyValue)

  def toPlainString(): String =
    PyDecimal.toPlainString(pyValue)

  def toEngineeringString(): String =
    PyDecimal.toEngineeringString(pyValue)
