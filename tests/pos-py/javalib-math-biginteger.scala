import java.math.BigInteger

@main def javalibMathBiginteger(): Unit =
  def render(bytes: Array[Byte]): String =
    val out = new java.lang.StringBuilder()
    var i = 0
    while i < bytes.length do
      if i != 0 then out.append(',')
      out.append(bytes(i).toInt)
      i += 1
    out.toString()

  val a = new BigInteger("123456789abcdef12345", 16)
  val b = new BigInteger("fedcba98765", 16)
  println(
    "arith:" +
      a.add(b).toString(16) +
      ":" +
      a.subtract(b).toString(16) +
      ":" +
      a.multiply(b).toString(16) +
      ":" +
      a.divide(b).toString(16) +
      ":" +
      a.remainder(b).toString(16)
  )

  println(
    "bitwise:" +
      a.shiftLeft(5).toString(16) +
      ":" +
      a.shiftRight(9).toString(16) +
      ":" +
      a.testBit(12) +
      ":" +
      a.flipBit(7).toString(16) +
      ":" +
      a.bitCount() +
      ":" +
      a.bitLength()
  )

  println(
    "modular:" +
      BigInteger.valueOf(5).modPow(BigInteger.valueOf(117), BigInteger.valueOf(19)).toString(10) +
      ":" +
      BigInteger.valueOf(42).modInverse(BigInteger.valueOf(2017)).toString(10) +
      ":" +
      BigInteger.valueOf(462).gcd(BigInteger.valueOf(1071)).toString(10)
  )

  val bytePos = new BigInteger(Array[Byte](0, -128, 1))
  val byteNeg = new BigInteger(Array[Byte](-1, 0, 1))
  val signMag = new BigInteger(1, Array[Byte](-128, 0))
  println(
    "bytes:" +
      render(bytePos.toByteArray()) +
      ":" +
      render(byteNeg.toByteArray()) +
      ":" +
      signMag.toString(16)
  )

  println(
    "radix:" +
      new BigInteger("ff", 16).toString(16) +
      ":" +
      new BigInteger("101010", 2).toString() +
      ":" +
      a.toString(36)
  )

  println(
    "primality:" +
      new BigInteger("101").isProbablePrime(40) +
      ":" +
      new BigInteger("221").isProbablePrime(40) +
      ":" +
      new BigInteger("100").nextProbablePrime().toString(10)
  )

  val exactOverflow =
    try
      BigInteger.valueOf(Int.MaxValue.toLong).add(BigInteger.ONE).intValueExact()
      "no"
    catch
      case _: ArithmeticException => "yes"
  println("exact:" + BigInteger.valueOf(123456789L).longValueExact() + ":" + exactOverflow)

  val c1 = new BigInteger("123456789abcdef", 16)
  val c2 = new BigInteger("123456789abcdef", 16)
  println("compare:" + (c1 == c2) + ":" + (c1.hashCode() == c2.hashCode()) + ":" + c1.compareTo(c2))
  println("equals:" + c1.equals(c2) + ":" + c1.equals(new BigInteger("0")))
