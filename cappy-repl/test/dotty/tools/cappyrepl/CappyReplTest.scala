package dotty.tools.cappyrepl

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets
import scala.compiletime.uninitialized
import scala.language.unsafeNulls

import org.junit.{After, Before}

/** JUnit harness for Cappy REPL script-style tests.
 *
 *  Mirrors `dotty.tools.repl.ReplTest`: each test method spawns a fresh
 *  driver + Python subprocess in `@Before`, drives it through one or
 *  more inputs, and asserts on the captured output.
 */
class CappyReplTest:

  protected val out: ByteArrayOutputStream = new ByteArrayOutputStream()
  protected var driver: CappyReplDriver = uninitialized

  /** Stored output since the last reset. */
  protected def storedOutput(): String =
    val s = out.toString(StandardCharsets.UTF_8.name)
    out.reset()
    s

  @Before def setUp(): Unit =
    out.reset()
    driver = new CappyReplDriver(
      out = new PrintStream(out, /* autoFlush = */ true, StandardCharsets.UTF_8.name)
    )
    driver.resetToInitial()

  @After def tearDown(): Unit =
    if driver != null then driver.shutdown()

  /** Feed one input and return the captured chunk of output (no trailing
   *  newline). The REPL state is threaded through `currentState`. */
  private var currentState: CappyReplState = uninitialized

  protected def state: CappyReplState =
    if currentState == null then
      currentState = driver.initialState
    currentState

  protected def feed(input: String): String =
    val (output, next) = driver.run(input)(using state)
    currentState = next
    output

  /** Run a multi-line script: each line starting with `cappy>` is an
   *  input; lines in between are the expected output. Returns the
   *  actual output joined by newlines for easy comparison.
   *
   *  This mirrors `ReplTest.testScript` shape but returns the captured
   *  string so individual tests can do their own assertions.
   */
  protected def runScript(script: String): String =
    val Prompt = "cappy>"
    val inputs = script.linesIterator
      .filter(_.startsWith(Prompt))
      .map(_.stripPrefix(Prompt).stripLeading())
      .toList
    val outputs = inputs.map(feed)
    outputs.mkString("\n").stripLineEnd
end CappyReplTest
