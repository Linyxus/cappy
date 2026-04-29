package java.io

import scala.python.{extern, native}

/** Minimal `java.io.ObjectOutputStream` port for the Python backend.
 *
 *  The JDK class is `extends OutputStream implements ObjectOutput,
 *  ObjectStreamConstants`. For the pylib port we only model the surface
 *  exercised by the affected fixtures (defaults-serizaliable-with-forwarders,
 *  enums-serialization-compat, i16390, t3038d, t5590):
 *
 *    constructor(out: OutputStream)
 *    writeInt, writeBoolean, writeUTF, flush, close
 *    writeObject (pickle-backed, length-prefixed)
 *    defaultWriteObject (no-op; mirrors defaultReadObject)
 *
 *  Wire format is a simple length-prefixed binary stream — primitives use
 *  big-endian fixed-width writes (matching `DataOutputStream`); objects are
 *  serialized via Python's `pickle` module, prefixed with a 4-byte big-endian
 *  payload length.
 *
 *  Note: the read-side counterpart `ObjectInputStream` is not yet ported, so
 *  fixtures that round-trip through `readObject` will not fully pass until
 *  that lands. The constructor-level failure (which was the original bug
 *  here) is resolved by this file.
 */
class ObjectOutputStream(out: OutputStream) extends OutputStream {

  // --- OutputStream surface ----------------------------------------------

  def write(b: Int): Unit =
    out.write(b)

  override def write(b: Array[Byte]): Unit =
    out.write(b, 0, b.length)

  override def write(b: Array[Byte], off: Int, len: Int): Unit =
    out.write(b, off, len)

  override def flush(): Unit =
    out.flush()

  override def close(): Unit =
    out.close()

  // --- Primitive writes (mirrors DataOutputStream big-endian layout) -----

  final def writeBoolean(v: Boolean): Unit =
    write(if (v) 1 else 0)

  final def writeByte(v: Int): Unit =
    write(v)

  final def writeShort(v: Int): Unit = {
    write(v >> 8)
    write(v)
  }

  final def writeChar(v: Int): Unit = {
    write(v >> 8)
    write(v)
  }

  final def writeInt(v: Int): Unit = {
    write(v >> 24)
    write(v >> 16)
    write(v >> 8)
    write(v)
  }

  final def writeLong(v: Long): Unit = {
    writeInt((v >>> 32).toInt)
    writeInt(v.toInt)
  }

  final def writeFloat(v: Float): Unit =
    writeInt(java.lang.Float.floatToIntBits(v))

  final def writeDouble(v: Double): Unit =
    writeLong(java.lang.Double.doubleToLongBits(v))

  final def writeUTF(s: String): Unit = {
    // Modified-UTF-8 with a 2-byte big-endian length prefix, identical to
    // DataOutputStream.writeUTF.
    val buffer = new Array[Byte](2 + 3 * s.length)
    var idx = 2
    var i = 0
    while i < s.length() do {
      val c = s.charAt(i)
      if (c <= 0x7f && c >= 0x01) {
        buffer(idx) = c.toByte
        idx += 1
      } else if (c < 0x0800) {
        buffer(idx) = ((c >> 6) | 0xc0).toByte
        buffer(idx + 1) = ((c & 0x3f) | 0x80).toByte
        idx += 2
      } else {
        buffer(idx) = ((c >> 12) | 0xe0).toByte
        buffer(idx + 1) = (((c >> 6) & 0x3f) | 0x80).toByte
        buffer(idx + 2) = ((c & 0x3f) | 0x80).toByte
        idx += 3
      }
      i += 1
    }
    val encodedLength = idx - 2
    if (encodedLength >= 0x10000)
      throw new UTFDataFormatException(s"encoded string too long: $encodedLength bytes")
    buffer(0) = (encodedLength >> 8).toByte
    buffer(1) = encodedLength.toByte
    write(buffer, 0, idx)
  }

  // --- Object writes ------------------------------------------------------

  /** Serialize `obj` via Python's `pickle.dumps`, then emit a 4-byte
   *  big-endian length prefix followed by the payload bytes. The matching
   *  reader (`ObjectInputStream.readObject`) is not yet ported, so this
   *  produces a self-consistent stream but cannot yet round-trip.
   */
  def writeObject(obj: AnyRef): Unit = {
    val payload = ObjectOutputStream.pickleDumps(obj)
    val len = ObjectOutputStream.bytesLen(payload).asInstanceOf[Int]
    writeInt(len)
    var i = 0
    while i < len do {
      val byte = ObjectOutputStream.bytesGet(payload, i).asInstanceOf[Int]
      write(byte)
      i += 1
    }
  }

  /** No-op. Real JDK behavior writes the non-transient fields of the
   *  current object via reflection; for our minimal port we rely on
   *  callers (typically inside `writeObject(out)` overrides) to also write
   *  the explicit fields they care about, which is what t3038d does.
   */
  def defaultWriteObject(): Unit = ()

  /** Reset is a no-op here — we don't maintain a handle table. */
  def reset(): Unit = ()
}

object ObjectOutputStream {
  // Minimal `pickle` / `bytes` interop. Kept private to this object so
  // callers stay typed in pure Scala terms.

  @extern("pickle", "dumps")
  private def pickleDumps(obj: Any): Any = native

  @extern("builtins", "len")
  private def bytesLen(b: Any): Any = native

  @extern("operator", "getitem")
  private def bytesGet(b: Any, i: Int): Any = native
}
