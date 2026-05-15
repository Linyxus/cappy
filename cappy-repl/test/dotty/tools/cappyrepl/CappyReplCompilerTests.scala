package dotty.tools.cappyrepl

import scala.language.unsafeNulls

import org.junit.Assert.*
import org.junit.Test

/** End-to-end Cappy REPL tests. Each test spawns its own `uv run python`
 *  subprocess (~200ms startup) via [[CappyReplTest.setUp]] and tears it
 *  down via [[CappyReplTest.tearDown]].
 *
 *  These tests are slow by JVM-unit-test standards but cheap compared to
 *  manual smoke testing the TUI. Keep the suite small; bigger coverage
 *  belongs in `pyCompilerTests` proper.
 */
class CappyReplCompilerTests extends CappyReplTest:

  @Test def testArithmetic(): Unit =
    val out = feed("1 + 1")
    assertTrue(s"expected `res0: Int = 2`, got: $out", out.contains("res0: Int = 2"))

  @Test def testValBinding(): Unit =
    val a = feed("val x = 5")
    val b = feed("x + 10")
    assertTrue(s"expected `val x: Int = 5`, got: $a", a.contains("x: Int = 5"))
    assertTrue(s"expected `res0: Int = 15`, got: $b", b.contains("res0: Int = 15"))

  @Test def testDef(): Unit =
    val a = feed("def foo(n: Int) = n * 2")
    val b = feed("foo(21)")
    assertTrue(s"expected `def foo`, got: $a", a.contains("def foo"))
    assertTrue(s"expected `res0: Int = 42`, got: $b", b.contains("res0: Int = 42"))

  @Test def testPrintln(): Unit =
    val out = feed("println(\"hello\")")
    assertTrue(s"expected `hello`, got: $out", out.contains("hello"))

  @Test def testCompileError(): Unit =
    val err = feed("val x: Int = \"oops\"")
    assertTrue(s"expected an error message, got: $err",
      err.nonEmpty && (err.contains("error") || err.contains("Found")))
    // REPL still alive after the error.
    val ok = feed("1 + 1")
    assertTrue(s"expected `res0: Int = 2` after recovery, got: $ok",
      ok.contains("res0: Int = 2"))
end CappyReplCompilerTests
