// JVM-bit-exact parity check for `scala.runtime.Statics`, MurmurHash3,
// and `java.lang.{Float,Double}` hash helpers. Catches future drift in
// `library-py/src/scala/runtime/Statics.scala` or the boxed-primitive
// hashCode implementations.
//
// Reference values were captured from JDK 25 + Scala 3.8.2 with
// `scala-cli run`, so they reflect what the JVM backend produces today.

import scala.runtime.Statics

object Test {
  private var failures = 0
  def expect(label: String, expected: Int, actual: Int): Unit =
    if expected != actual then
      failures += 1
      println(s"FAIL $label: expected=$expected actual=$actual")
    else
      println(s"OK   $label: $actual")

  case class P2(x: Int, y: Int)
  case class P3(a: Int, b: String, c: Long)
  case class P0()
  case class PMixed(a: Boolean, b: Byte, c: Short, d: Char, e: Int)

  def main(args: Array[String]): Unit = {
    // -- Statics primitives. Pin the underlying mix/finalize bit
    //    patterns so future drift fails loudly rather than silently
    //    hashing things differently from the JVM.
    expect("Statics.mix(0, 0)", -430675100, Statics.mix(0, 0))
    expect("Statics.mix(0xcafebabe, 1)", 451039597, Statics.mix(0xcafebabe, 1))
    expect("Statics.finalizeHash(0, 0)", 0, Statics.finalizeHash(0, 0))

    // -- Tuple / case-class via MurmurHash3.productHash
    //    (`MurmurHash3.scala` lives in the upstream stdlib, but uses
    //    the same algorithm as Statics; pin a few results to catch
    //    drift in library/src too.)
    expect("(1, 2).hashCode", 1316541600, (1, 2).hashCode)
    expect("(1, 2, 3).hashCode", 2016703621, (1, 2, 3).hashCode)
    expect("(\"a\", \"b\").hashCode", 961399813, ("a", "b").hashCode)
    expect(
      "MurmurHash3.caseClassHash((1, 2))",
      1316541600,
      scala.util.hashing.MurmurHash3.caseClassHash((1, 2)),
    )

    // -- Synthesized `caseHashCodeBody` path: case classes with
    //    primitive accessors call `Statics.mix`/`Statics.finalizeHash`
    //    directly. Three product shapes per acceptance criterion:
    //    (Pair, Triple-with-mixed-types, fully primitive).
    expect("P2(1, 2).hashCode", 692587590, P2(1, 2).hashCode)
    expect("P3(7, hello, 42L).hashCode", -1575658265, P3(7, "hello", 42L).hashCode)
    expect(
      "PMixed(true, 1.toByte, 2.toShort, 'c', 3).hashCode",
      -419584550,
      PMixed(true, 1.toByte, 2.toShort, 'c', 3).hashCode,
    )
    // Empty case class.
    expect("P0().hashCode", "P0".hashCode, P0().hashCode)

    // -- Long collapse: `1L.##` should equal `1.##`.
    expect("Statics.longHash(1L)", 1, Statics.longHash(1L))
    expect("Statics.longHash(-1L)", -1, Statics.longHash(-1L))

    // Long values outside Int's range fall back to `Long.hashCode`.
    val bigLong: Long = (1L << 40)
    expect(
      "Statics.longHash(1L << 40) == Long.hashCode(1L << 40)",
      java.lang.Long.hashCode(bigLong),
      Statics.longHash(bigLong),
    )

    // -- Double / Float bit patterns (acceptance: ±0, NaN, ±Infinity,
    //    integer-valued).
    expect("Float.hashCode(0.0f)", 0, java.lang.Float.hashCode(0.0f))
    expect("Float.hashCode(-0.0f)", Int.MinValue, java.lang.Float.hashCode(-0.0f))
    expect("Float.hashCode(NaN)", 2143289344, java.lang.Float.hashCode(Float.NaN))
    expect(
      "Float.hashCode(+Inf)",
      2139095040,
      java.lang.Float.hashCode(Float.PositiveInfinity),
    )
    expect(
      "Float.hashCode(-Inf)",
      -8388608,
      java.lang.Float.hashCode(Float.NegativeInfinity),
    )

    expect("Double.hashCode(0.0)", 0, java.lang.Double.hashCode(0.0))
    expect("Double.hashCode(-0.0)", Int.MinValue, java.lang.Double.hashCode(-0.0))
    expect("Double.hashCode(NaN)", 2146959360, java.lang.Double.hashCode(Double.NaN))
    expect("Double.hashCode(1.5)", 1073217536, java.lang.Double.hashCode(1.5))
    expect("Double.hashCode(1e100)", 1898351312, java.lang.Double.hashCode(1e100))

    // -- Cross-type numeric `.##` parity (`equality.scala` shape).
    val ai: Any = 5
    val al: Any = 5L
    val af: Any = 5.0f
    val ad: Any = 5.0d
    expect("5.## == 5L.##", ai.##, al.##)
    expect("5.## == 5.0f.##", ai.##, af.##)
    expect("5.## == 5.0d.##", ai.##, ad.##)
    expect("5.0f.## == 5", 5, af.##)
    expect("5.0d.## == 5", 5, ad.##)

    // -- String-hash JVM parity (used by case-class name mixing).
    expect("\"Hello, World!\".hashCode", 1498789909, "Hello, World!".hashCode)
    expect(
      "MurmurHash3.stringHash(Hello, World!)",
      236314546,
      scala.util.hashing.MurmurHash3.stringHash("Hello, World!"),
    )

    if failures > 0 then
      println(s"$failures FAILURE(S)")
      throw new RuntimeException(s"$failures FAILURE(S)")
    else
      println("ALL OK")
  }
}
