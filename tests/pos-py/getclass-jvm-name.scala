// Verifies that `Class.getName()` reports the JVM-shaped dotted name
// (with `$` separators, anonymous-class auto-suffixes, module-class
// trailing `$`) rather than the Python-encoded form that the codegen
// uses internally for Python identifiers (where `$` becomes `_` and
// trailing user `$` becomes `_scpy_d`). See Wave 5 item 12 — class-name
// leakage bucket.

class Outer:
  class Inner

  def anon: AnyRef = new Object {}

object Top:
  class Nested

@main def getclassJvmName(): Unit =
  // Module class trailing `$`.
  println(Top.getClass.getName)

  // Inner class: `Outer$Inner`, NOT `Outer_Inner`.
  val outer = new Outer
  println(new outer.Inner().getClass.getName)

  // Anonymous class: `Outer$$anon$1`.
  println(outer.anon.getClass.getName)

  // Top-level nested object's class: `Top$Nested`.
  println(new Top.Nested().getClass.getName)

  // toString shape: `class <jvmName>` for `Class.toString`.
  println(outer.anon.getClass.toString)
