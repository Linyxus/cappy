package scala.runtime

/** Minimal ScalaRunTime for the Python backend.
 *
 *  Case-class synthesis and other compiler-generated code calls
 *  `ScalaRunTime._toString`, `_hashCode`, `_equals`.
 */
object ScalaRunTime:
  def _toString(x: Any): String =
    if x == null then "null" else x.toString

  def _hashCode(x: Any): Int =
    if x == null then 0 else x.hashCode

  def _equals(x: Any, y: Any): Boolean =
    if x == null then y == null
    else x.equals(y)

  def hash(x: Any): Int =
    if x == null then 0 else x.hashCode
