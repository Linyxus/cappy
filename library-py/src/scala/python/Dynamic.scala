package scala.python

object Dynamic:
  def module(name: String): PyDynamic = native
  def attr(path: String): PyDynamic = native
