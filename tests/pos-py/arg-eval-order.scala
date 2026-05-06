// Regression: Scala promises left-to-right argument evaluation. The
// Python codegen used to violate that whenever a *later* argument
// needed hoisting (multi-stmt if/match/try body lifted into a temp
// via `pendingLocalDefs`), because the hoisted statements would be
// flushed BEFORE the call expression — and any earlier argument that
// stayed inline at the call site evaluated AFTER those hoists.
// Manifested as parse errors in `tests/run/Course-2002-13.scala` and
// other right-side-of-call tokens-then-condition idioms.

object Test:
  var counter = 0
  def step(label: String): String =
    counter += 1
    s"$label@$counter"

  def two(a: String, b: String): String = s"a=$a b=$b"
  def three(a: String, b: String, c: String): String = s"a=$a b=$b c=$c"

  def main(args: Array[String]): Unit =
    // First arg inline + second arg requires hoisting (multi-stmt
    // then-branch). After fix: counter advances first → 1; if-cond
    // sees counter==1 (false) so else-branch runs → counter=2.
    counter = 0
    val r1 = two(
      step("first"),
      if counter == 0 then
        val intermediate = step("intermediate")
        step("after-then")
      else step("after-else")
    )
    println(r1)

    // Three-arg with hoist in middle. First and last must both lift.
    counter = 0
    val r2 = three(
      step("a1"),
      try step("a2-try")
      catch case _: RuntimeException => step("a2-catch"),
      step("a3")
    )
    println(r2)

    // Two hoists separated by a pure-leaf arg.
    counter = 0
    val r3 = three(
      step("b1"),
      "literal",
      if counter == 0 then
        val tmp = step("b3-then-pre")
        step("b3-then")
      else step("b3-else")
    )
    println(r3)
