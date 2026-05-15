package dotty.tools.cappyrepl

import java.io.{BufferedReader, IOException, InputStreamReader, OutputStreamWriter, PrintWriter}
import java.nio.charset.StandardCharsets
import java.util.UUID
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

/** Long-lived `uv run python` subprocess driving the Cappy REPL.
 *
 *  We don't use `python -u -` directly — that mode reads ALL of stdin to
 *  EOF before compiling/executing, which makes incremental evaluation
 *  impossible. Instead, the JVM launches Python with a tiny bootstrap
 *  loop (passed via `-c`) that:
 *
 *    1. Reads lines from stdin into a buffer until a chunk-delimiter
 *       line appears.
 *    2. `exec`s the accumulated buffer in a shared namespace.
 *    3. Prints the chunk's end-marker (sentinel) and flushes.
 *    4. Loops.
 *
 *  The JVM side wraps each `send(code)` as: `<code>\n<delim>\n` and then
 *  reads stdout until the end-marker appears, returning everything in
 *  between. Both delimiters are UUID-suffixed at construction so user
 *  code can't print a string that confuses the framing.
 */
final class PythonProcess private (
    repoRoot:      String,
    val endMarker: String,
    chunkDelim:    String
):

  private val bootstrap: String =
    // We run the loop inside a function so its locals (chunks, buffers)
    // don't pollute `__main__.__dict__`. User chunks themselves are
    // exec'd into `__main__.__dict__` — the Python backend lowers
    // `@extern("__main__")` facades (used by pylib for
    // `_scpy_codec_lookup`, codec step helpers, etc.) as
    // `from __main__ import X`, which requires the runtime helpers
    // defined by `PyIRRuntime.content` to live in the actual `__main__`
    // module.
    s"""|import sys, traceback
        |def _cappy_loop():
        |    _delim = ${pyStr(chunkDelim)}
        |    _end   = ${pyStr(endMarker)}
        |    _ns    = sys.modules['__main__'].__dict__
        |    while True:
        |        _buf = []
        |        while True:
        |            _line = sys.stdin.readline()
        |            if not _line:
        |                return
        |            if _line.rstrip('\\n') == _delim:
        |                break
        |            _buf.append(_line)
        |        _code = ''.join(_buf)
        |        try:
        |            exec(compile(_code, '<cappy>', 'exec'), _ns)
        |        except SystemExit:
        |            return
        |        except BaseException:
        |            traceback.print_exc()
        |        print(_end, flush=True)
        |_cappy_loop()
        |""".stripMargin

  private val process: Process =
    val pb = new ProcessBuilder(
      "uv", "run",
      "--project", repoRoot,
      "--no-sync",
      "python", "-u", "-c", bootstrap
    )
    pb.redirectErrorStream(true)
    pb.environment().put("PYTHONIOENCODING", "utf-8")
    pb.start()

  private val stdin: PrintWriter =
    new PrintWriter(
      new OutputStreamWriter(process.getOutputStream, StandardCharsets.UTF_8),
      /* autoFlush = */ false
    )

  // EOF token used by the reader thread to signal end-of-stream.
  private val EOF: String = " __CAPPY_PYTHON_EOF__ "

  private val lines = new LinkedBlockingQueue[String]()

  private val reader: Thread =
    val t = new Thread(() => readLoop(), "cappy-python-reader")
    t.setDaemon(true)
    t.start()
    t

  private def readLoop(): Unit =
    val br = new BufferedReader(
      new InputStreamReader(process.getInputStream, StandardCharsets.UTF_8)
    )
    try
      var line: String | Null = br.readLine()
      while line != null do
        lines.put(line)
        line = br.readLine()
    catch case _: IOException => ()
    finally
      try br.close() catch case _: IOException => ()
      lines.put(EOF)

  /** Send a code block. The block runs in the persistent namespace and
   *  any stdout/stderr (printed before the end-marker appears) is
   *  returned. Throws [[PythonProcessTerminated]] if Python dies first.
   */
  def send(code: String): String =
    if !process.isAlive then
      throw new PythonProcessTerminated("Python subprocess is not running")

    stdin.print(code)
    if !code.endsWith("\n") then stdin.print('\n')
    stdin.print(chunkDelim)
    stdin.print('\n')
    stdin.flush()

    val buf = new StringBuilder
    var done = false
    while !done do
      val line = lines.take()
      if line eq EOF then
        throw new PythonProcessTerminated(
          s"Python subprocess exited mid-evaluation. Captured output:\n${buf.toString}"
        )
      else if line == endMarker then
        done = true
      else
        if buf.nonEmpty then buf.append('\n')
        buf.append(line)
    buf.toString

  def isAlive: Boolean = process.isAlive

  def shutdown(): Unit =
    try
      stdin.close()
    catch case _: IOException => ()
    if !process.waitFor(2, TimeUnit.SECONDS) then
      process.destroy()
      if !process.waitFor(1, TimeUnit.SECONDS) then
        process.destroyForcibly()

  // Mirrors CappyRendering.pyStr — local copy to avoid a cross-package dep.
  private def pyStr(s: String): String =
    val buf = new StringBuilder(s.length + 2)
    buf += '"'
    for c <- s do
      c match
        case '\\' => buf ++= "\\\\"
        case '"'  => buf ++= "\\\""
        case '\n' => buf ++= "\\n"
        case '\r' => buf ++= "\\r"
        case '\t' => buf ++= "\\t"
        case c if c < 0x20 => buf ++= f"\\x${c.toInt}%02x"
        case c    => buf += c
    buf += '"'
    buf.toString
end PythonProcess

object PythonProcess:

  def start(repoRoot: String): PythonProcess =
    val u = UUID.randomUUID().toString.replace('-', '_')
    val endMarker  = s"__SCPY_REPL_END_${u}__"
    val chunkDelim = s"__SCPY_REPL_DELIM_${u}__"
    new PythonProcess(repoRoot, endMarker, chunkDelim)

final class PythonProcessTerminated(msg: String) extends RuntimeException(msg)
