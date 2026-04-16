package scala.runtime

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
    if (x eq y) then true
    else if x == null then y == null
    else x.equals(y)

  def hashFromLong(n: java.lang.Long): Int = Statics.longHash(n.asInstanceOf[Long])
  def hashFromDouble(n: java.lang.Double): Int = Statics.doubleHash(n.asInstanceOf[Double])
  def hashFromFloat(n: java.lang.Float): Int = Statics.floatHash(n.asInstanceOf[Float])
  def hashFromNumber(n: java.lang.Number): Int = Statics.anyHash(n)
  def hashFromObject(a: Object): Int = Statics.anyHash(a)
