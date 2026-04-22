package scala.python.runtime

import scala.python.{PyAny, extern, native}

/** Thin facade over Python's `io.StringIO`. */
object PyStringIO:
  @extern("io", "StringIO")
  final class Handle extends PyAny:
    def write(text: String): Int = native
    def read(size: Int = -1): String = native
    def getvalue(): String = native
    def seek(offset: Int, whence: Int = 0): Int = native
    def tell(): Int = native
    def close(): Unit = native

  def create(): Handle =
    new Handle()

