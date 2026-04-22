package scala.runtime

/** Scala rewrite of `scala.runtime.Statics` (Java-defined upstream).
 *
 *  Case-class `hashCode` synthesis calls `Statics.mix(acc, field)` for
 *  each field. All `Int` arithmetic gets mechanical `_scpy_i32` wrapping
 *  from the Python emitter.
 *
 *  Simplified hash algorithm: `acc * 31 + data` instead of MurmurHash3.
 *  Matches the runtime shim's behavior.
 */
object Statics:
  def mix(hash: Int, data: Int): Int =
    hash * 31 + data

  def mixLast(hash: Int, data: Int): Int =
    hash * 31 + data

  def finalizeHash(hash: Int, length: Int): Int =
    hash ^ length

  def anyHash(x: Any): Int =
    if x == null then 0 else x.hashCode

  def longHash(lv: Long): Int =
    val iv = lv.toInt
    if iv.toLong == lv then iv else lv.hashCode

  def doubleHash(dv: Double): Int =
    val iv = dv.toInt
    if iv.toDouble == dv then iv else dv.hashCode

  def floatHash(fv: Float): Int =
    val iv = fv.toInt
    if iv.toFloat == fv then iv else fv.hashCode

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
