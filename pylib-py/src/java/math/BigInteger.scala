package java.math

import java.util.Random

import scala.python.PyDynamic
import scala.python.runtime.PyInt

object BigInteger:
  // JDK caches `{-16..16}` as singletons so `BigInteger.valueOf(5) eq
  // BigInteger.valueOf(5)` holds for small values. Match the range.
  //
  // Why a `var` + null guard and not `lazy val`: Scala 3's `lazy val`
  // in a top-level object synthesises double-checked init that
  // references `java.lang.invoke.MethodHandles` — JVM-only, doesn't
  // link on the Python backend. A plain `val` would fire at module
  // load and hit `PyInt` before its module is initialised (see
  // `notes/issue-module-init-ordering-module-dependency.md`). A
  // `var`-holder stays at `null` until the first `valueOf` call, by
  // which point `PyInt` has loaded.
  private val CacheMin = -16
  private val CacheMax = 16
  private var cacheArray: Array[BigInteger] | Null = null

  private def cache: Array[BigInteger] =
    if cacheArray == null then
      val out = new Array[BigInteger](CacheMax - CacheMin + 1)
      var i = 0
      while i < out.length do
        out(i) = new BigInteger(PyInt.fromLong((CacheMin + i).toLong))
        i += 1
      cacheArray = out
    cacheArray.asInstanceOf[Array[BigInteger]]

  def ZERO: BigInteger = cache(-CacheMin)
  def ONE: BigInteger = cache(-CacheMin + 1)
  def TWO: BigInteger = cache(-CacheMin + 2)
  def TEN: BigInteger = cache(-CacheMin + 10)

  def valueOf(value: Long): BigInteger =
    if value >= CacheMin && value <= CacheMax then
      cache((value - CacheMin).toInt)
    else
      unsafeFromPy(PyInt.fromLong(value))

  def probablePrime(bitLength: Int, rnd: Random): BigInteger =
    Primality.probablePrime(bitLength, 100, rnd)

  private[math] def unsafeFromPy(value: PyDynamic): BigInteger =
    new BigInteger(value)

  private def numberFormat(message: String): Nothing =
    throw new NumberFormatException(message)

  private def requireBitIndex(index: Int): Unit =
    if index < 0 then
      throw new ArithmeticException("Negative bit address")

  private def parseText(text: String, radix: Int): PyDynamic =
    if text == null then
      throw new NumberFormatException("null")
    if java.lang.Character.isRadixInvalid(radix) then
      numberFormat("Radix out of range")
    try PyInt.fromString(text, radix)
    catch case _: Throwable => numberFormat(s"""For input string: "$text"""")

  private def allZero(bytes: Array[Byte]): Boolean =
    var i = 0
    while i < bytes.length do
      if bytes(i) != 0 then
        return false
      i += 1
    true

  private def fromTwosComplement(bytes: Array[Byte]): PyDynamic =
    if bytes == null then
      throw new NullPointerException()
    if bytes.length == 0 then
      numberFormat("Zero length BigInteger")
    PyInt.fromSignedBytes(bytes)

  private def fromSignAndMagnitude(signum: Int, magnitude: Array[Byte]): PyDynamic =
    if magnitude == null then
      throw new NullPointerException()
    if signum < -1 || signum > 1 then
      numberFormat("Invalid signum value")
    if magnitude.length == 0 || allZero(magnitude) then
      if signum == 0 || magnitude.length == 0 then
        return ZERO.pyValue
      if signum != 0 && allZero(magnitude) then
        return ZERO.pyValue
    if signum == 0 then
      numberFormat("signum-magnitude mismatch")

    val positive = PyInt.fromUnsignedBytes(magnitude)
    if signum < 0 then positive.__neg__().asInstanceOf[PyDynamic]
    else positive

  private def randomBits(numBits: Int, rnd: Random): PyDynamic =
    if numBits < 0 then
      throw new IllegalArgumentException("numBits must be non-negative")
    if numBits == 0 then
      return ZERO.pyValue

    val bytes = new Array[Byte]((numBits + 7) / 8)
    rnd.nextBytes(bytes)
    val extraBits = bytes.length * 8 - numBits
    if extraBits > 0 then
      val mask = (1 << (8 - extraBits)) - 1
      bytes(0) = (bytes(0).toInt & mask).toByte
    PyInt.fromUnsignedBytes(bytes)

final class BigInteger private[math] (private[math] val pyValue: PyDynamic)
    extends Number
    with Comparable[BigInteger]:

  def this(bytes: Array[Byte]) =
    this(BigInteger.fromTwosComplement(bytes))

  def this(signum: Int, magnitude: Array[Byte]) =
    this(BigInteger.fromSignAndMagnitude(signum, magnitude))

  def this(bitLength: Int, certainty: Int, rnd: Random) =
    this(Primality.probablePrime(bitLength, certainty, rnd).pyValue)

  def this(numBits: Int, rnd: Random) =
    this(BigInteger.randomBits(numBits, rnd))

  def this(text: String, radix: Int) =
    this(BigInteger.parseText(text, radix))

  def this(text: String) =
    this(text, 10)

  private def wrap(value: PyDynamic): BigInteger =
    BigInteger.unsafeFromPy(value)

  private def arithmeticError(message: String): Nothing =
    throw new ArithmeticException(message)

  def abs(): BigInteger =
    if signum() < 0 then negate() else this

  def add(that: BigInteger): BigInteger =
    wrap(pyValue.__add__(that.pyValue).asInstanceOf[PyDynamic])

  def subtract(that: BigInteger): BigInteger =
    wrap(pyValue.__sub__(that.pyValue).asInstanceOf[PyDynamic])

  def multiply(that: BigInteger): BigInteger =
    wrap(pyValue.__mul__(that.pyValue).asInstanceOf[PyDynamic])

  def divide(that: BigInteger): BigInteger =
    try wrap(PyInt.truncDiv(pyValue, that.pyValue))
    catch case _: Throwable => arithmeticError("BigInteger divide by zero")

  def remainder(that: BigInteger): BigInteger =
    try
      val quot = divide(that)
      subtract(quot.multiply(that))
    catch
      case _: ArithmeticException => arithmeticError("BigInteger divide by zero")

  def divideAndRemainder(that: BigInteger): Array[BigInteger] =
    val quot = divide(that)
    Array(quot, subtract(quot.multiply(that)))

  def mod(that: BigInteger): BigInteger =
    if that.signum() <= 0 then
      arithmeticError("BigInteger: modulus not positive")
    wrap(pyValue.__mod__(that.pyValue).asInstanceOf[PyDynamic])

  def pow(exponent: Int): BigInteger =
    if exponent < 0 then
      arithmeticError("Negative exponent")
    wrap(pyValue.__pow__(exponent).asInstanceOf[PyDynamic])

  def modPow(exponent: BigInteger, modulus: BigInteger): BigInteger =
    if modulus.signum() <= 0 then
      arithmeticError("BigInteger: modulus not positive")
    try wrap(PyInt.modPow(pyValue, exponent.pyValue, modulus.pyValue))
    catch case _: Throwable => arithmeticError("BigInteger modPow failed")

  def modInverse(modulus: BigInteger): BigInteger =
    if modulus.signum() <= 0 then
      arithmeticError("BigInteger: modulus not positive")
    try wrap(PyInt.modInverse(pyValue, modulus.pyValue))
    catch case _: Throwable => arithmeticError("BigInteger not invertible")

  def gcd(that: BigInteger): BigInteger =
    wrap(PyInt.gcd(pyValue, that.pyValue))

  def and(that: BigInteger): BigInteger =
    wrap(pyValue.__and__(that.pyValue).asInstanceOf[PyDynamic])

  def or(that: BigInteger): BigInteger =
    wrap(pyValue.__or__(that.pyValue).asInstanceOf[PyDynamic])

  def xor(that: BigInteger): BigInteger =
    wrap(pyValue.__xor__(that.pyValue).asInstanceOf[PyDynamic])

  def andNot(that: BigInteger): BigInteger =
    wrap(pyValue.__and__(that.pyValue.__invert__()).asInstanceOf[PyDynamic])

  def not(): BigInteger =
    wrap(pyValue.__invert__().asInstanceOf[PyDynamic])

  def shiftLeft(n: Int): BigInteger =
    if n >= 0 then wrap(pyValue.__lshift__(n).asInstanceOf[PyDynamic])
    else shiftRight(-n)

  def shiftRight(n: Int): BigInteger =
    if n >= 0 then wrap(pyValue.__rshift__(n).asInstanceOf[PyDynamic])
    else shiftLeft(-n)

  def flipBit(index: Int): BigInteger =
    BigInteger.requireBitIndex(index)
    wrap(pyValue.__xor__(BigInteger.ONE.shiftLeft(index).pyValue).asInstanceOf[PyDynamic])

  def setBit(index: Int): BigInteger =
    BigInteger.requireBitIndex(index)
    wrap(pyValue.__or__(BigInteger.ONE.shiftLeft(index).pyValue).asInstanceOf[PyDynamic])

  def clearBit(index: Int): BigInteger =
    BigInteger.requireBitIndex(index)
    wrap(pyValue.__and__(BigInteger.ONE.shiftLeft(index).not().pyValue).asInstanceOf[PyDynamic])

  def testBit(index: Int): Boolean =
    BigInteger.requireBitIndex(index)
    PyInt.testBit(pyValue, index)

  def bitCount(): Int =
    PyInt.bitCount(pyValue)

  def bitLength(): Int =
    PyInt.bitLength(pyValue)

  def getLowestSetBit(): Int =
    PyInt.lowestSetBit(pyValue)

  def compareTo(that: BigInteger): Int =
    PyInt.compare(pyValue, that.pyValue)

  override def equals(other: Any): Boolean =
    other match
      case that: BigInteger => compareTo(that) == 0
      case _                => false

  override def hashCode(): Int =
    PyInt.hashCode(pyValue)

  def signum(): Int =
    PyInt.signum(pyValue)

  def min(that: BigInteger): BigInteger =
    if compareTo(that) <= 0 then this else that

  def max(that: BigInteger): BigInteger =
    if compareTo(that) >= 0 then this else that

  def negate(): BigInteger =
    wrap(pyValue.__neg__().asInstanceOf[PyDynamic])

  def isProbablePrime(certainty: Int): Boolean =
    Primality.isProbablePrime(this, certainty)

  def nextProbablePrime(): BigInteger =
    Primality.nextProbablePrime(this)

  override def intValue(): Int =
    PyInt.toInt(pyValue)

  override def longValue(): Long =
    PyInt.toLong(pyValue)

  override def floatValue(): Float =
    PyInt.toFloat(pyValue)

  override def doubleValue(): Double =
    PyInt.toDouble(pyValue)

  def intValueExact(): Int =
    val out = intValue()
    if !equals(BigInteger.valueOf(out.toLong)) then
      arithmeticError("BigInteger out of int range")
    out

  def longValueExact(): Long =
    val out = longValue()
    if !equals(BigInteger.valueOf(out)) then
      arithmeticError("BigInteger out of long range")
    out

  override def toString(): String =
    PyInt.toString(pyValue, 10)

  def toString(radix: Int): String =
    if java.lang.Character.isRadixInvalid(radix) then toString()
    else PyInt.toString(pyValue, radix)

  def toByteArray(): Array[Byte] =
    PyInt.toSignedBytes(pyValue)
