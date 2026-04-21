import scala.python.runtime.PyInt

@main def javalibPyint(): Unit =
  def render(bytes: Array[Byte]): String =
    val out = new java.lang.StringBuilder()
    var i = 0
    while i < bytes.length do
      if i != 0 then out.append(',')
      out.append(bytes(i).toInt)
      i += 1
    out.toString()

  val parsed = PyInt.fromString("ff", 16)
  val negative = PyInt.fromString("-101", 2)
  println("radix:" + PyInt.toString(parsed, 16) + ":" + PyInt.toString(negative, 10))

  val modPow = PyInt.modPow(PyInt.fromLong(5), PyInt.fromLong(117), PyInt.fromLong(19))
  val inverse = PyInt.modInverse(PyInt.fromLong(42), PyInt.fromLong(2017))
  println("modular:" + PyInt.toString(modPow, 10) + ":" + PyInt.toString(inverse, 10))

  val bytes = PyInt.toSignedBytes(PyInt.fromLong(-1025))
  val roundTrip = PyInt.fromSignedBytes(bytes)
  println("bytes:" + render(bytes) + ":" + PyInt.toString(roundTrip, 10))

  val big = PyInt.fromString("123456789abcdef", 16)
  println("bits:" + PyInt.bitLength(big) + ":" + PyInt.bitCount(big) + ":" + PyInt.lowestSetBit(big))
