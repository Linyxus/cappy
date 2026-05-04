package scala.math

/** A marker class for Number types introduced by Scala. Mirrors the
 *  upstream `scala.math.ScalaNumber.java` shape — abstract on both
 *  `isWhole` and `underlying`, extends `java.lang.Number`. The empty
 *  runtime stub previously provided by `PyIRRuntime` silently broke
 *  any dispatch through `ScalaNumber.{isWhole,underlying}` (BigInt /
 *  BigDecimal).
 */
abstract class ScalaNumber extends java.lang.Number:
  protected def isWhole(): Boolean
  def underlying(): AnyRef
