import scala.python.runtime.{PyDecimal, PyInt}

@main def javalibPydecimal(): Unit =
  val parsed = PyDecimal.fromString("123.4500")
  val fromInt = PyDecimal.fromPyInt(PyInt.fromString("12345678901234567890", 10))
  val rounded =
    PyDecimal.withContext(6, "ROUND_HALF_UP") {
      PyDecimal.divide(fromInt, PyDecimal.fromString("1000"))
    }
  val parts = PyDecimal.unscaledValueAndScale(parsed)
  val quantized = PyDecimal.quantize(parsed, 2, "ROUND_DOWN")

  println("plain:" + PyDecimal.toPlainString(parsed) + ":" + PyDecimal.toEngineeringString(parsed))
  println("context:" + PyDecimal.toString(rounded) + ":" + PyDecimal.precision(rounded))
  println("tuple:" + PyInt.toString(parts.unscaled, 10) + ":" + parts.scale + ":" + PyDecimal.toString(quantized))
