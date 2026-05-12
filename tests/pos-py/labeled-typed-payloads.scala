// Each `match` below is in expression position so the value-position
// rewrite fires: the labelled escape carries a value of the arm's
// type into a temp. We exercise each scalar / reference payload
// shape to verify the temp's typing and Python-side boxing path
// work for all of them.

final case class Point(x: Int, y: Int)

def asInt(n: Int): Int = n match
  case 0 => 7
  case _ => 11

def asLong(n: Int): Long = n match
  case 0 => 7L
  case _ => 9_000_000_000L

def asDouble(n: Int): Double = n match
  case 0 => 1.5
  case _ => -2.25

def asBoolean(n: Int): Boolean = n match
  case 0 => true
  case _ => false

def asString(n: Int): String = n match
  case 0 => "zero"
  case _ => "other"

def asNullable(n: Int): String | Null = n match
  case 0 => null
  case _ => "non-null"

def asCaseClass(n: Int): Point = n match
  case 0 => Point(0, 0)
  case _ => Point(1, 2)

@main def labeledTypedPayloads(): Unit =
  println("int(0):" + asInt(0))
  println("int(9):" + asInt(9))
  println("long(0):" + asLong(0))
  println("long(9):" + asLong(9))
  println("double(0):" + asDouble(0))
  println("double(9):" + asDouble(9))
  println("bool(0):" + asBoolean(0))
  println("bool(9):" + asBoolean(9))
  println("str(0):" + asString(0))
  println("str(9):" + asString(9))
  // Avoid `"…" + null` — Scala-on-JVM stringifies null in concat, but
  // this backend currently surfaces null as Python's `None` and the
  // `str + None` concat raises. That's a separate concern from the
  // label-escape behavior we're testing here, so we check the
  // payload's identity directly.
  println("nul(0)-is-null:" + (asNullable(0) == null))
  println("nul(9)-is-null:" + (asNullable(9) == null))
  println("nul(9):" + asNullable(9))
  val p0 = asCaseClass(0)
  val p9 = asCaseClass(9)
  println("cc(0):" + p0.x + "," + p0.y)
  println("cc(9):" + p9.x + "," + p9.y)
