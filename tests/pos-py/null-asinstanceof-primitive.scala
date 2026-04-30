// Layer 5.1.e regression guard.
// `null.asInstanceOf[Primitive]` must unbox to the JVM default
// (0 / 0L / 0.0 / false), not propagate as `None`. Without the
// `_scpy_unbox_or_default` lowering in `PyAsInstanceOf`, an erased
// generic that receives a null and casts to a primitive prints `null`
// or returns `None` instead of the primitive default.
def gen[A]: A = null.asInstanceOf[A]

@main def run(): Unit =
  val i: Int    = gen[Int]
  val l: Long   = gen[Long]
  val s: Short  = gen[Short]
  val b: Byte   = gen[Byte]
  val f: Float  = gen[Float]
  val d: Double = gen[Double]
  val z: Boolean = gen[Boolean]
  println(i)
  println(l)
  println(s)
  println(b)
  println(f)
  println(d)
  println(z)
