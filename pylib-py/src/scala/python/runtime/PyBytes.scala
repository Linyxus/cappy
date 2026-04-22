package scala.python.runtime

import scala.python.{PyAny, PyDynamic, extern, name, native}

/** Bridges Scala signed-byte arrays to Python `bytes` objects and back. */
object PyBytes:
  @extern("builtins")
  private object builtins extends PyAny:
    @name("bytes")
    def newBytes(iterable: Any): PyDynamic = native
    @name("len")
    def lengthOf(container: Any): Int = native

  @extern("operator")
  private object operator extends PyAny:
    @name("getitem")
    def getItem(container: Any, index: Int): Any = native

  def toPyBytes(src: Array[Byte]): PyDynamic =
    val unsigned = new Array[Int](src.length)
    var i = 0
    while i < src.length do
      val b = src(i).toInt
      unsigned(i) = if b < 0 then b + 256 else b
      i += 1
    builtins.newBytes(unsigned)

  def fromPyBytes(src: PyDynamic): Array[Byte] =
    val len = builtins.lengthOf(src)
    val out = new Array[Byte](len)
    var i = 0
    while i < len do
      val u = operator.getItem(src, i).asInstanceOf[Int]
      out(i) = (if u >= 128 then u - 256 else u).toByte
      i += 1
    out

