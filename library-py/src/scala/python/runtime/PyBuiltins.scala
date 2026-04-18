package scala.python.runtime

import scala.python.{extern, native}

object PyBuiltins:
  @extern("builtins", "int")
  private def pyInt(text: String, base: Int): Any = native

  def int_parse(text: String, base: Int): Long =
    pyInt(text, base).asInstanceOf[Long]
