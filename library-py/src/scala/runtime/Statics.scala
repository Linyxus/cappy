package scala.runtime

import java.lang.Integer.{rotateLeft => rotl}

/** Scala rewrite of `scala.runtime.Statics` (Java-defined upstream).
 *
 *  Case-class `hashCode` synthesis calls `Statics.mix(acc, field)` for
 *  each field, then `finalizeHash(acc, productArity)`. To stay
 *  bit-compatible with the JVM (so `caseClassHash`, `productHash`,
 *  case-class `hashCode`, `equality`/`hashhash`/`hashCodeDistribution`
 *  fixtures observe identical Int values), this port mirrors the
 *  MurmurHash3-based algorithm used by the JVM's `Statics.java` exactly.
 *
 *  All Int arithmetic gets mechanical `_scpy_i32` wrapping from the
 *  Python emitter, so the C-level signed 32-bit overflow semantics that
 *  Murmur3 depends on are preserved.
 */
object Statics:
  /** Mix in a block of data into an intermediate hash value. */
  def mix(hash: Int, data: Int): Int =
    var h = mixLast(hash, data)
    h = rotl(h, 13)
    h * 5 + 0xe6546b64

  /** May optionally be used as the last mixing step. Slightly faster
   *  than `mix` because it does no further mixing — the hash is
   *  thoroughly mixed during finalization anyway.
   */
  def mixLast(hash: Int, data: Int): Int =
    var k = data
    k *= 0xcc9e2d51
    k = rotl(k, 15)
    k *= 0x1b873593
    hash ^ k

  /** Finalize a hash to incorporate the length and avalanche all bits. */
  def finalizeHash(hash: Int, length: Int): Int =
    avalanche(hash ^ length)

  /** Force all bits of the hash to avalanche. */
  def avalanche(hash: Int): Int =
    var h = hash
    h ^= h >>> 16
    h *= 0x85ebca6b
    h ^= h >>> 13
    h *= 0xc2b2ae35
    h ^= h >>> 16
    h

  /** `anyHash` mirrors the JVM contract: numeric values share a hash
   *  with their Int counterparts when they round-trip; otherwise the
   *  type-specific `hashCode` is used. The boxed numeric pattern
   *  matches dispatch through the runtime class-of-instance table:
   *  Python `int` reports as `java.lang.Integer` (Int/Long both share
   *  the integer slot), Python `float` reports as `java.lang.Double`
   *  (so `5.0f` boxed as `Any` reaches the Double branch). The
   *  `asInstanceOf[Long/Double/Float]` casts are identity on Python
   *  (no boxing exists), so we use them rather than the
   *  `doubleValue()` accessor that would assume a real boxed receiver.
   *
   *  String is handled explicitly: virtual dispatch on a Python `str`
   *  hits Python's salted, length-dependent `__hash__`, which is not
   *  bit-compatible with `java.lang.String.hashCode`. The JVM ports of
   *  `String.hashCode` live as the `_scpy_str_hash_code` runtime helper
   *  invoked by static-typed `String.hashCode()` calls; route the
   *  boxed-as-`Any` path through the same algorithm via
   *  `java.util.internal.MurmurHash3` is wrong (different algorithm) —
   *  use a manual JVM-faithful loop.
   */
  def anyHash(x: Any): Int =
    if x == null then 0
    else
      x match
        // Boolean check first: Python `bool` is a subclass of `int`, so
        // pattern-matching on `java.lang.Long` (which the runtime maps
        // through `java.lang.Integer`) would otherwise swallow `true`/
        // `false` before they reach the Boolean-specific branch.
        case _: java.lang.Boolean => if x.asInstanceOf[Boolean] then 1231 else 1237
        case _: java.lang.Long =>
          // The Python class-of-instance mapping reports plain `int` as
          // `java.lang.Integer`, NOT `Long` — so this branch only fires
          // when a Scala source wrapped the value as an explicit
          // `java.lang.Long`. Both `Int.hashCode` and `Long.hashCode`
          // collapse to `intValue` for in-range values, so it's safe to
          // route either through `longHash` here.
          longHash(x.asInstanceOf[Long])
        case _: java.lang.Double => doubleHash(x.asInstanceOf[Double])
        case _: java.lang.Float  => floatHash(x.asInstanceOf[Float])
        case s: String           => stringHashCode(s)
        // Catch-all for boxed Int/Short/Byte/Char and user types. The
        // numeric primitives unify on Python `int`, whose `__hash__` is
        // NOT bit-identical to `Integer.hashCode` in two specific cases:
        // `hash(-1) == -2` (CPython collapses -1 because -1 is the
        // sentinel error value), and `hash(very_large_int)` reduces
        // mod `sys.hash_info.modulus`. JVM's contract is
        // `Integer.hashCode == intValue`, so we route ints through
        // `_scpy_i32` (signed 32-bit truncation) here. `Char` arrives as
        // a `_scpy_Char` with its own `hashCode__I` returning the
        // codepoint, so a regular `__hash__` dispatch is correct for it.
        case _: java.lang.Integer => x.asInstanceOf[Int]
        case _                    => x.hashCode

  /** JVM-faithful `String.hashCode`. The pylib has `_scpy_str_hash_code`
   *  for static-typed `String.hashCode()` calls, but `Any.hashCode` in
   *  the Python-emitted bundle dispatches to `__hash__`, which on
   *  Python strings is salted. Replicate the algorithm here so the
   *  `anyHash` route stays JVM-bit-exact for String-typed fields. */
  private def stringHashCode(s: String): Int =
    var h = 0
    var i = 0
    val n = s.length
    while i < n do
      h = h * 31 + s.charAt(i).toInt
      i += 1
    h

  /** `Long` hashCode that collapses to Int's hashCode on values that
   *  fit in Int — required so equal numeric values across `Long`/`Int`
   *  observe equal hash codes (see `equality.scala`). */
  def longHash(lv: Long): Int =
    val iv = lv.toInt
    if iv.toLong == lv then iv else java.lang.Long.hashCode(lv)

  /** `Double` hashCode collapsing to Int / Long / Float hashes for
   *  values that round-trip — required for cross-type numeric
   *  hash equality. */
  def doubleHash(dv: Double): Int =
    val iv = dv.toInt
    if iv.toDouble == dv then iv
    else
      val lv = dv.toLong
      if lv.toDouble == dv then java.lang.Long.hashCode(lv)
      else
        val fv = dv.toFloat
        if fv.toDouble == dv then java.lang.Float.hashCode(fv)
        else java.lang.Double.hashCode(dv)

  /** `Float` hashCode collapsing to Int / Long hashes for values that
   *  round-trip — same rationale as `doubleHash`. */
  def floatHash(fv: Float): Int =
    val iv = fv.toInt
    if iv.toFloat == fv then iv
    else
      val lv = fv.toLong
      if lv.toFloat == fv then java.lang.Long.hashCode(lv)
      else java.lang.Float.hashCode(fv)

  def ioobe[T](n: Int): T =
    throw new IndexOutOfBoundsException(n.toString)

  /** Sentinel value for partial-function fall-through inside the stdlib
   *  collections. Identity-compared via `eq`, so just needs to be a fresh
   *  unique reference.
   */
  val pfMarker: AnyRef = new Object()

  /** No-op on Python — the JVM uses `VarHandle.releaseFence()` for the
   *  publication of immutable collection internals; Python's single-thread
   *  / GIL semantics make it unnecessary.
   */
  def releaseFence(): Unit = ()
