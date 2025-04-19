package cavia.core

object Fresh:
  private var counter = 0
  def freshName(prefix: String): String =
    val name = s"$prefix$$$counter"
    counter += 1
    name
