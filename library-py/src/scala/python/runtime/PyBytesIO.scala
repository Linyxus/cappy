package scala.python.runtime

import scala.python.{PyAny, PyDynamic, extern, name, native}

/** Thin facade over Python's `io.BytesIO`. */
object PyBytesIO:
  @extern("io", "BytesIO")
  final class Handle extends PyAny:
    def write(bytes: Any): Int = native
    def read(size: Int = -1): PyDynamic = native
    def getvalue(): PyDynamic = native
    def seek(offset: Int, whence: Int = 0): Int = native
    def tell(): Int = native
    def close(): Unit = native

  def create(): Handle =
    new Handle()

