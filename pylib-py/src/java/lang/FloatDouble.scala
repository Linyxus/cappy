/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package java.lang

import scala.python.runtime.PyBuiltins

/** Common algorithms between `Float` and `Double`. */
private[lang] object FloatDouble {

  @inline
  def toHexString(x: scala.Float): String =
    normalizeHex(PyBuiltins.float_hex(x.toDouble))

  @inline
  def toHexString(x: scala.Double): String =
    normalizeHex(PyBuiltins.float_hex(x))

  private def normalizeHex(raw: String): String = {
    if (raw == "nan") "NaN"
    else if (raw == "inf") "Infinity"
    else if (raw == "-inf") "-Infinity"
    else {
      val pIndex = raw.indexOf("p")
      val significand = raw.substring(0, pIndex)
      val exponent = raw.substring(pIndex + 1)
      val dotIndex = significand.indexOf(".")
      val intPart = significand.substring(0, dotIndex)
      val fracPart = trimTrailingZeros(significand.substring(dotIndex + 1))
      intPart + "." + fracPart + "p" + normalizeExponent(exponent)
    }
  }

  private def trimTrailingZeros(fracPart: String): String = {
    var end = fracPart.length()
    while (end > 1 && fracPart.charAt(end - 1) == '0') do
      end -= 1
    fracPart.substring(0, end)
  }

  private def normalizeExponent(exponent: String): String = {
    if (exponent.startsWith("+")) exponent.substring(1)
    else exponent
  }

}
