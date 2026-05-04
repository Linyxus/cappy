// Phase 4 of Category A migration in notes/shrink-runtime.md.
// `scala.math.ScalaNumber` ports as a real abstract class with two
// abstract methods (`isWhole`, `underlying`) extending `java.lang.Number`.
// The empty runtime stub silently broke any dispatch through these
// methods; the .scala port restores correctness.

@main def runtimeScalaNumber(): Unit =
  class N(val v: Int) extends scala.math.ScalaNumber:
    protected def isWhole(): Boolean = true
    def underlying(): AnyRef = java.lang.Integer.valueOf(v)
    def intValue(): Int = v
    def longValue(): Long = v.toLong
    def floatValue(): Float = v.toFloat
    def doubleValue(): Double = v.toDouble

  val n = new N(42)
  // Polymorphic dispatch through the abstract base.
  val sn: scala.math.ScalaNumber = n
  println(sn.intValue())     // 42
  println(sn.doubleValue())  // 42.0
  println(sn.underlying())   // 42

  // Verify subclass relationship is real (used by anyHash and similar
  // pattern matches in `Statics.anyHash`).
  println(n.isInstanceOf[scala.math.ScalaNumber])  // true
  println(n.isInstanceOf[java.lang.Number])         // true
