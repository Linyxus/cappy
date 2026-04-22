package scala.python.runtime

import scala.python.{PyAny, PyDynamic, extern, name, native}

object PyDecimal:
  final class UnscaledAndScale(val unscaled: PyDynamic, val scale: Int)

  @extern("decimal", "Decimal")
  final class Handle extends PyAny:
    def adjusted(): Int = native
    def as_tuple(): PyDynamic = native
    def is_zero(): Boolean = native
    def normalize(): Handle = native
    def quantize(exp: Any, rounding: String): Handle = native
    def scaleb(power: Any): Handle = native
    def to_eng_string(): String = native
    def __add__(other: Any): Handle = native
    def __sub__(other: Any): Handle = native
    def __mul__(other: Any): Handle = native
    def __truediv__(other: Any): Handle = native
    def __floordiv__(other: Any): Handle = native
    def __mod__(other: Any): Handle = native
    def __pow__(other: Any): Handle = native
    def __neg__(): Handle = native
    def __pos__(): Handle = native
    def __abs__(): Handle = native

  @extern("decimal")
  private object decimalModule extends PyAny:
    @name("Decimal")
    def fromValue(value: Any): Handle = native

  @extern("decimal", "Decimal")
  private object decimalClass extends PyAny:
    @name("from_float")
    def fromFloat(value: Double): Handle = native

  @extern("builtins")
  private object builtins extends PyAny:
    @name("str")
    def toStringValue(value: Any): String = native

  @extern("operator")
  private object operator extends PyAny:
    @name("getitem")
    def getItem(container: Any, index: Int): Any = native

  @extern("__main__")
  private object runtime extends PyAny:
    @name("_scpy_f32")
    def toFloat32(value: Double): Float = native

    @name("_scpy_decimal_plain_string")
    def plainString(value: Any): String = native

    @name("_scpy_decimal_with_context")
    def withContext(precision: Int, rounding: String, thunk: Any): Any = native

    @name("_scpy_decimal_unary_with_context")
    def unaryWithContext(op: String, value: Any, precision: Int, rounding: String): PyDynamic = native

    @name("_scpy_decimal_binary_with_context")
    def binaryWithContext(op: String, lhs: Any, rhs: Any, precision: Int, rounding: String): PyDynamic = native

    @name("_scpy_decimal_unscaled_and_scale")
    def unscaledAndScale(value: Any): PyDynamic = native

    @name("_scpy_decimal_precision")
    def precisionOf(value: Any): Int = native

    @name("_scpy_decimal_quantize")
    def quantizeScale(value: Any, scale: Int, rounding: String): PyDynamic = native

    @name("_scpy_decimal_to_pyint")
    def toPyInt(value: Any): PyDynamic = native

    @name("_scpy_decimal_compare")
    def compare(lhs: Any, rhs: Any): Int = native

    @name("_scpy_decimal_signum")
    def signum(value: Any): Int = native

    @name("_scpy_decimal_to_double")
    def toDoubleValue(value: Any): Double = native

  def fromString(text: String): PyDynamic =
    decimalModule.fromValue(text).asInstanceOf[PyDynamic]

  def fromPyInt(value: PyDynamic): PyDynamic =
    decimalModule.fromValue(value).asInstanceOf[PyDynamic]

  def fromDouble(value: Double): PyDynamic =
    decimalClass.fromFloat(value).asInstanceOf[PyDynamic]

  def add(lhs: PyDynamic, rhs: PyDynamic): PyDynamic =
    lhs.asInstanceOf[Handle].__add__(rhs).asInstanceOf[PyDynamic]

  def subtract(lhs: PyDynamic, rhs: PyDynamic): PyDynamic =
    lhs.asInstanceOf[Handle].__sub__(rhs).asInstanceOf[PyDynamic]

  def multiply(lhs: PyDynamic, rhs: PyDynamic): PyDynamic =
    lhs.asInstanceOf[Handle].__mul__(rhs).asInstanceOf[PyDynamic]

  def divide(lhs: PyDynamic, rhs: PyDynamic): PyDynamic =
    lhs.asInstanceOf[Handle].__truediv__(rhs).asInstanceOf[PyDynamic]

  def divideToIntegral(lhs: PyDynamic, rhs: PyDynamic): PyDynamic =
    lhs.asInstanceOf[Handle].__floordiv__(rhs).asInstanceOf[PyDynamic]

  def remainder(lhs: PyDynamic, rhs: PyDynamic): PyDynamic =
    lhs.asInstanceOf[Handle].__mod__(rhs).asInstanceOf[PyDynamic]

  def pow(lhs: PyDynamic, exponent: Int): PyDynamic =
    lhs.asInstanceOf[Handle].__pow__(exponent).asInstanceOf[PyDynamic]

  def negate(value: PyDynamic): PyDynamic =
    value.asInstanceOf[Handle].__neg__().asInstanceOf[PyDynamic]

  def plus(value: PyDynamic): PyDynamic =
    value.asInstanceOf[Handle].__pos__().asInstanceOf[PyDynamic]

  def abs(value: PyDynamic): PyDynamic =
    value.asInstanceOf[Handle].__abs__().asInstanceOf[PyDynamic]

  def normalize(value: PyDynamic): PyDynamic =
    value.asInstanceOf[Handle].normalize().asInstanceOf[PyDynamic]

  def scaleByPower(value: PyDynamic, power: Int): PyDynamic =
    value.asInstanceOf[Handle].scaleb(power).asInstanceOf[PyDynamic]

  def adjusted(value: PyDynamic): Int =
    value.asInstanceOf[Handle].adjusted()

  def unscaledValueAndScale(value: PyDynamic): UnscaledAndScale =
    val raw = runtime.unscaledAndScale(value)
    new UnscaledAndScale(
      operator.getItem(raw, 0).asInstanceOf[PyDynamic],
      operator.getItem(raw, 1).asInstanceOf[Int]
    )

  def precision(value: PyDynamic): Int =
    runtime.precisionOf(value)

  def quantize(value: PyDynamic, scale: Int, rounding: String): PyDynamic =
    runtime.quantizeScale(value, scale, rounding)

  def toPlainString(value: PyDynamic): String =
    runtime.plainString(value)

  def toString(value: PyDynamic): String =
    builtins.toStringValue(value)

  def toEngineeringString(value: PyDynamic): String =
    value.asInstanceOf[Handle].to_eng_string()

  def withContext[T](precision: Int, rounding: String)(body: => T): T =
    runtime.withContext(precision, rounding, () => body).asInstanceOf[T]

  def unaryWithContext(op: String, value: PyDynamic, precision: Int, rounding: String): PyDynamic =
    runtime.unaryWithContext(op, value, precision, rounding)

  def binaryWithContext(op: String, lhs: PyDynamic, rhs: Any, precision: Int, rounding: String): PyDynamic =
    runtime.binaryWithContext(op, lhs, rhs, precision, rounding)

  def toPyInt(value: PyDynamic): PyDynamic =
    runtime.toPyInt(value)

  def compare(lhs: PyDynamic, rhs: PyDynamic): Int =
    runtime.compare(lhs, rhs)

  def signum(value: PyDynamic): Int =
    runtime.signum(value)

  def isZero(value: PyDynamic): Boolean =
    runtime.signum(value) == 0

  def toDouble(value: PyDynamic): Double =
    runtime.toDoubleValue(value)

  def toFloat(value: PyDynamic): Float =
    runtime.toFloat32(toDouble(value))
