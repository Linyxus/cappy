package java.io

import scala.python.{extern, native}

/** Minimal `java.io.ObjectInputStream` port for the Python backend.
 *
 *  The JDK class is `extends InputStream implements ObjectInput,
 *  ObjectStreamConstants`. For the pylib port we model only the surface
 *  exercised by the Layer 3.2 round-trip fixtures (case-class-serializable,
 *  defaults-serizaliable-{no,with}-forwarders, enums-serialization-compat,
 *  i8033, i9881, serialize{,-stream}, t3038d, t5590, etc.):
 *
 *    constructor(in: InputStream)
 *    readInt, readBoolean, readByte, readShort, readChar, readLong,
 *    readFloat, readDouble, readUTF, close
 *    readObject (pickle-backed, length-prefixed — symmetric to
 *    `ObjectOutputStream.writeObject`)
 *    defaultReadObject (no-op; mirrors `defaultWriteObject`)
 *
 *  Wire format mirrors `ObjectOutputStream`: primitives are big-endian
 *  fixed-width values; `readObject` consumes a 4-byte big-endian length
 *  prefix followed by the raw `pickle` payload that the writer emitted.
 *
 *  Limitations (intentional, scoped to the failing fixture set):
 *    - No JVM `serialVersionUID` checks, no handle table, no
 *      `writeReplace` / `readResolve` rewiring beyond what `pickle`
 *      already does for module-level classes. Singleton-identity
 *      fixtures that check `roundTrip(Foo) eq Foo` will not pass with
 *      this minimal port — pickle reconstructs a new instance.
 *    - Object graphs containing closures / anonymous classes that are
 *      not picklable will surface a `pickle.PicklingError` at write time
 *      rather than read time.
 */
class ObjectInputStream(in: InputStream) extends InputStream {

  // --- InputStream surface ------------------------------------------------

  def read(): Int =
    in.read()

  override def read(b: Array[Byte]): Int =
    in.read(b, 0, b.length)

  override def read(b: Array[Byte], off: Int, len: Int): Int =
    in.read(b, off, len)

  override def available(): Int =
    in.available()

  override def close(): Unit =
    in.close()

  // --- Helpers ------------------------------------------------------------

  /** Read a single byte, throwing `EOFException` on end-of-stream. The
   *  underlying `InputStream.read` returns `-1` at EOF; we surface that
   *  as the standard `java.io.EOFException` so callers (typically the
   *  primitive-read methods below) get the expected JVM semantics.
   */
  private def readByteUnsigned(): Int = {
    val b = in.read()
    if (b < 0) throw new EOFException()
    b & 0xff
  }

  /** Read exactly `n` bytes into a fresh array, throwing
   *  `EOFException` on truncation. Used by the object-payload path.
   */
  private def readFully(n: Int): Array[Byte] = {
    val buf = new Array[Byte](n)
    var off = 0
    while (off < n) {
      val r = in.read(buf, off, n - off)
      if (r < 0) throw new EOFException()
      off += r
    }
    buf
  }

  // --- Primitive reads (mirrors DataInputStream big-endian layout) -------

  final def readBoolean(): Boolean =
    readByteUnsigned() != 0

  final def readByte(): Byte =
    readByteUnsigned().toByte

  final def readUnsignedByte(): Int =
    readByteUnsigned()

  final def readShort(): Short = {
    val b1 = readByteUnsigned()
    val b2 = readByteUnsigned()
    ((b1 << 8) | b2).toShort
  }

  final def readUnsignedShort(): Int = {
    val b1 = readByteUnsigned()
    val b2 = readByteUnsigned()
    (b1 << 8) | b2
  }

  final def readChar(): Char = {
    val b1 = readByteUnsigned()
    val b2 = readByteUnsigned()
    ((b1 << 8) | b2).toChar
  }

  final def readInt(): Int = {
    val b1 = readByteUnsigned()
    val b2 = readByteUnsigned()
    val b3 = readByteUnsigned()
    val b4 = readByteUnsigned()
    (b1 << 24) | (b2 << 16) | (b3 << 8) | b4
  }

  final def readLong(): Long = {
    val hi = readInt().toLong & 0xffffffffL
    val lo = readInt().toLong & 0xffffffffL
    (hi << 32) | lo
  }

  final def readFloat(): Float =
    java.lang.Float.intBitsToFloat(readInt())

  final def readDouble(): Double =
    java.lang.Double.longBitsToDouble(readLong())

  final def readUTF(): String = {
    // Modified-UTF-8 with a 2-byte big-endian length prefix; identical to
    // `DataInputStream.readUTF`. The decoded char count is bounded by the
    // byte length, so allocating a same-size `Array[Char]` is safe.
    val length = readUnsignedShort()
    val bytes = readFully(length)
    val chars = new Array[Char](length)
    var i = 0
    var j = 0
    while (i < length) {
      val b = bytes(i) & 0xff
      if (b < 0x80) {
        chars(j) = b.toChar
        i += 1
      } else if ((b & 0xe0) == 0xc0) {
        if (i + 1 >= length)
          throw new UTFDataFormatException("partial 2-byte sequence")
        val b2 = bytes(i + 1) & 0xff
        chars(j) = (((b & 0x1f) << 6) | (b2 & 0x3f)).toChar
        i += 2
      } else if ((b & 0xf0) == 0xe0) {
        if (i + 2 >= length)
          throw new UTFDataFormatException("partial 3-byte sequence")
        val b2 = bytes(i + 1) & 0xff
        val b3 = bytes(i + 2) & 0xff
        chars(j) = (((b & 0x0f) << 12) | ((b2 & 0x3f) << 6) | (b3 & 0x3f)).toChar
        i += 3
      } else {
        throw new UTFDataFormatException(s"invalid UTF byte: $b")
      }
      j += 1
    }
    new String(chars, 0, j)
  }

  // --- Object reads -------------------------------------------------------

  /** Inverse of `ObjectOutputStream.writeObject`: reads a 4-byte
   *  big-endian length prefix, slurps that many bytes off the underlying
   *  stream, and feeds them to `pickle.loads`.
   *
   *  `Array[Byte]` lowers to a Python list of signed ints (-128..127).
   *  Python's `bytes(list)` rejects negative values, so we widen each
   *  byte to its unsigned form before constructing the `bytes` payload.
   */
  def readObject(): AnyRef = {
    val len = readInt()
    val payload = readFully(len)
    val unsigned = new Array[Int](len)
    var i = 0
    while (i < len) {
      unsigned(i) = payload(i) & 0xff
      i += 1
    }
    val pyBytes = ObjectInputStream.bytesFromIterable(unsigned)
    ObjectInputStream.pickleLoads(pyBytes).asInstanceOf[AnyRef]
  }

  /** No-op. Real JDK behavior reads the non-transient fields of the
   *  current object via reflection; we leave field initialization to
   *  the explicit primitive/object reads the caller performs after.
   *  This mirrors `ObjectOutputStream.defaultWriteObject`.
   */
  def defaultReadObject(): Unit = ()

  /** Stub to mirror the JVM API; we don't track validation callbacks. */
  def registerValidation(obj: AnyRef, prio: Int): Unit = ()
}

object ObjectInputStream {
  // Minimal `pickle` / `bytes` interop. Kept private so callers stay
  // typed in pure Scala terms, mirroring the writer side.

  @extern("pickle", "loads")
  private def pickleLoads(b: Any): Any = native

  @extern("builtins", "bytes")
  private def bytesFromIterable(a: Any): Any = native
}
