package scala.runtime

import scala.python.runtime.PyBuiltins

/** Scala rewrite of `scala.runtime.BoxesRunTime` (Java-defined upstream).
 *
 *  Python is unboxed, so all box/unbox operations are identity casts.
 *  Adapted from `library-js/src/scala/runtime/BoxesRunTime.scala`.
 */
final class BoxesRunTime

object BoxesRunTime:
  def boxToBoolean(b: Boolean): java.lang.Boolean = b.asInstanceOf[java.lang.Boolean]
  def boxToCharacter(c: Char): java.lang.Character = c.asInstanceOf[java.lang.Character]
  def boxToByte(b: Byte): java.lang.Byte = b.asInstanceOf[java.lang.Byte]
  def boxToShort(s: Short): java.lang.Short = s.asInstanceOf[java.lang.Short]
  def boxToInteger(i: Int): java.lang.Integer = i.asInstanceOf[java.lang.Integer]
  def boxToLong(l: Long): java.lang.Long = l.asInstanceOf[java.lang.Long]
  def boxToFloat(f: Float): java.lang.Float = f.asInstanceOf[java.lang.Float]
  def boxToDouble(d: Double): java.lang.Double = d.asInstanceOf[java.lang.Double]

  def unboxToBoolean(b: Any): Boolean = b.asInstanceOf[Boolean]
  def unboxToChar(c: Any): Char = c.asInstanceOf[Char]
  def unboxToByte(b: Any): Byte = b.asInstanceOf[Byte]
  def unboxToShort(s: Any): Short = s.asInstanceOf[Short]
  def unboxToInt(i: Any): Int = i.asInstanceOf[Int]
  def unboxToLong(l: Any): Long = l.asInstanceOf[Long]
  def unboxToFloat(f: Any): Float = f.asInstanceOf[Float]
  def unboxToDouble(d: Any): Double = d.asInstanceOf[Double]

  def equals(x: Object, y: Object): Boolean =
    // The `eq` shortcut is unsound for boxed floating-point values: in
    // Python, primitives are unboxed identities, so two `boxToDouble(d)`
    // calls produce the same Python float, whereas on the JVM each
    // `Double.valueOf(d)` returns a fresh box. The JVM's
    // `BoxesRunTime.equals` relies on the fresh-box invariant — when the
    // identity shortcut fails it dispatches to `equalsNumNum`, which
    // does `xn.doubleValue() == yn.doubleValue()` and so honours
    // IEEE-754 `NaN != NaN`. Skip the `eq` shortcut whenever either
    // operand is a Python float (which covers `boxToDouble` /
    // `boxToFloat` since both erase to a Python `float` at runtime),
    // so `equals(NaN, NaN)` returns `false` per IEEE-754 instead of
    // `true` per Python `is`.
    if PyBuiltins.is_float(x) || PyBuiltins.is_float(y) then
      if (x eq null) then y eq null
      else PyBuiltins.equal(x, y)
    else if (x eq y) then true
    else if (x eq null) then y eq null
    else PyBuiltins.equal(x, y)

  def hashFromLong(n: java.lang.Long): Int = Statics.longHash(n.asInstanceOf[Long])
  def hashFromDouble(n: java.lang.Double): Int = Statics.doubleHash(n.asInstanceOf[Double])
  def hashFromFloat(n: java.lang.Float): Int = Statics.floatHash(n.asInstanceOf[Float])
  def hashFromNumber(n: java.lang.Number): Int = Statics.anyHash(n)
  def hashFromObject(a: Object): Int = Statics.anyHash(a)
