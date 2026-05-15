package dotty.tools.cappyrepl

import java.io.{ByteArrayOutputStream, File => JFile, PrintStream}
import java.lang.System.{lineSeparator => EOL}
import java.nio.charset.StandardCharsets
import scala.compiletime.uninitialized
import scala.io.Source
import scala.language.unsafeNulls
import scala.util.Using

import org.junit.{After, Before}
import org.junit.Assert.fail

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
    val inputs = script.linesIterator
      .filter(_.startsWith(CappyReplTest.Prompt))
      .map(_.stripPrefix(CappyReplTest.Prompt).stripLeading())
      .toList
    val outputs = inputs.map(feed)
    outputs.mkString("\n").stripLineEnd

  // -- File-based script tests -----------------------------------------
  //
  // Mirrors scala3-repl's `ReplTest.testFiles` / `testScript`: each
  // script in `test-resources/cappy-repl/` is its own expected
  // transcript. Lines prefixed `cappy> ` are inputs; anything else is
  // expected output. We feed each input through the driver and compare
  // the resulting (input + EOL + capturedOutput) sequence line-by-line
  // against the file's content.
  //
  // Selective runs: pass `-Dcappy.tests.filter=foo,bar` to keep only
  // scripts whose path contains `foo` or `bar`. Matches the
  // `dotty.tests.filter` convention used elsewhere in the repo.

  protected def testFiles(dir: String): Unit =
    testFiles(scriptFiles(dir))

  protected def testFiles(files: Array[JFile]): Unit =
    val errors = files.iterator.flatMap(testFile).toList
    if errors.nonEmpty then fail(errors.mkString(EOL))

  private def scriptFiles(dir: String): Array[JFile] =
    val root = new JFile(dir)
    assert(root.exists && root.isDirectory, s"Couldn't load scripts dir: $dir")
    val filter = sys.props.get("cappy.tests.filter")
      .map(_.split(',').toList).getOrElse(Nil)
    root.listFiles.filter: f =>
      if !f.isFile then false
      else filter.isEmpty || filter.exists(f.getPath.contains)

  /** Returns `Some(error)` if the script's actual transcript doesn't
   *  match its content; `None` otherwise. */
  private def testFile(scriptFile: JFile): Option[String] =
    val lines = Using.resource(Source.fromFile(scriptFile, "UTF-8"))(_.getLines().toList)
    testScript(scriptFile.toString, lines, Some(scriptFile))

  protected def testScript(
      name: => String,
      lines: List[String],
      scriptFile: Option[JFile] = None
  ): Option[String] =
    // Reset driver state to a fresh session so each script is hermetic.
    if driver == null then setUp() else { driver.resetToInitial(); currentState = null.asInstanceOf[CappyReplState]; out.reset() }

    val expected = lines.iterator.filter(nonBlank).toList
    val inputLines = lines.filter(_.startsWith(CappyReplTest.Prompt))
    if inputLines.isEmpty then
      Some(s"Script $name has no `${CappyReplTest.Prompt}` input lines")
    else
      val buf = scala.collection.mutable.ListBuffer.empty[String]
      for input <- inputLines do
        val codeOnly = input.stripPrefix(CappyReplTest.Prompt).stripLeading()
        val output = try feed(codeOnly)
                     catch case ex: Throwable =>
                       s"<exception during evaluation: ${ex.getClass.getName}: ${ex.getMessage}>"
        buf += input
        for line <- output.linesIterator do buf += line
      val actual = buf.iterator.filter(nonBlank).toList
      if actual == expected then None
      else Some(formatDiff(name, expected, actual))

  private def nonBlank(s: String): Boolean =
    s.exists(c => !Character.isWhitespace(c))

  private def formatDiff(name: String, expected: List[String], actual: List[String]): String =
    val sb = new StringBuilder
    sb.append(s"Script $name failed: transcript did not match.").append(EOL)
    sb.append("--- expected ---").append(EOL)
    expected.foreach(l => { sb.append(l); sb.append(EOL) })
    sb.append("--- actual ---").append(EOL)
    actual.foreach(l => { sb.append(l); sb.append(EOL) })
    sb.append("----------------").append(EOL)
    sb.toString.stripLineEnd
end CappyReplTest

object CappyReplTest:
  inline val Prompt = "cappy>"
