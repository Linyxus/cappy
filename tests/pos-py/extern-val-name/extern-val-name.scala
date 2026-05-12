// `@name` on a `val` member of an `@extern` facade must redirect the
// Scala-side getter to the Python attribute named by the annotation.
// Dotty's `Fields` phase splits `val axisX = ...` into a private field
// plus a getter accessor; `@name` lands on the *field*, so the codegen
// has to consult the backing field, not just the accessor, to find the
// annotation.

import scala.python.*

@extern("extern_val_name", "Point")
class Point(x: Int, y: Int):
  @name("my_x") val axisX: Int = native
  @name("my_y") val axisY: Int = native

@main def externValName(): Unit =
  val p = Point(7, 11)
  println("p.axisX=" + p.axisX)
  println("p.axisY=" + p.axisY)
