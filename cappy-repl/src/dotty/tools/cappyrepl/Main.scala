package dotty.tools.cappyrepl

import layoutz._

object Main extends LayoutzApp[Main.State, Main.Msg]:

  case class Entry(input: String, output: String)

  /** `historyIdx = Some(i)` means the buffer mirrors `history(i).input` (we're
   *  navigating). `draft` holds whatever the user was typing before they
   *  started navigating, so Down past the most recent entry restores it.
   *
   *  `cursorPos` is the index in `buffer` where the next char goes (and
   *  where the visible cursor is rendered). Always satisfies
   *  `0 <= cursorPos <= buffer.length`. Multi-line buffers carry literal
   *  `\n`; the renderer wraps to subsequent gutter lines.
   *
   *  `spinnerFrame` advances on every Tick while `busy` or `initializing`
   *  is true; the layoutz Spinner element renders one of N animation
   *  frames per tick.
   *
   *  `initializing` is true while the driver is preloading the support
   *  classpath into Python at startup (~2 s). All input is blocked
   *  during this window and the view shows a "Compiling library
   *  classes…" spinner.
   */
  case class State(
    history: Vector[Entry],
    buffer: String,
    cursorPos: Int,
    busy: Boolean,
    inFlight: Option[String],
    spinnerFrame: Int,
    historyIdx: Option[Int],
    draft: String,
    initializing: Boolean,
  )

  sealed trait Msg
  /** Replace the buffer + cursor; also exits history navigation. */
  case class EditBuffer(buffer: String, cursorPos: Int) extends Msg
  /** Move cursor only — preserves history navigation. */
  case class MoveCursor(newPos: Int) extends Msg
  case object Submit extends Msg
  case object InsertNewline extends Msg
  /** Delete from cursor to end of current line (Ctrl-K). If the cursor
   *  is already at end-of-line, joins with the next line by deleting
   *  the trailing newline. */
  case object KillToLineEnd extends Msg
  /** Delete from start of current line to cursor (Ctrl-U). */
  case object KillToLineStart extends Msg
  /** Delete the word before the cursor (Ctrl-W). */
  case object KillWordBack extends Msg
  case class Done(input: String, output: String) extends Msg
  case object Quit extends Msg
  case object HistoryPrev extends Msg
  case object HistoryNext extends Msg
  case object Tick extends Msg
  case object Initialized extends Msg
  case class InitFailed(err: String) extends Msg

  def init: (State, Cmd[Msg]) =
    val s0 = State(
      history      = Vector.empty,
      buffer       = "",
      cursorPos    = 0,
      busy         = false,
      inFlight     = None,
      spinnerFrame = 0,
      historyIdx   = None,
      draft        = "",
      initializing = true,
    )
    // Compile + load the support classpath into Python up front so
    // the first user input doesn't pay the ~2 s preload cost.
    val initTask = Cmd.task(driver.resetToInitial()) {
      case Right(_)  => Initialized
      case Left(err) => InitFailed(err.toString)
    }
    (s0, initTask)

  def update(msg: Msg, s: State): (State, Cmd[Msg]) = msg match
    case Tick if s.busy || s.initializing =>
      (s.copy(spinnerFrame = s.spinnerFrame + 1), Cmd.none)
    case Tick =>
      (s, Cmd.none)

    case Initialized =>
      (s.copy(initializing = false, spinnerFrame = 0), Cmd.none)

    case InitFailed(err) =>
      val entry = Entry("<startup>", s"initialization failed: $err")
      (s.copy(initializing = false, history = s.history :+ entry, spinnerFrame = 0),
        Cmd.none)

    // While initializing, swallow all other input — the user
    // shouldn't be able to edit, submit, or navigate history before
    // the support classes finish loading. Tick/Initialized/InitFailed
    // are matched above and have already executed.
    case _ if s.initializing =>
      (s, Cmd.none)

    case EditBuffer(_, _) if s.busy =>
      (s, Cmd.none)
    case EditBuffer(b, pos) =>
      // Editing exits history navigation; the new buffer becomes the draft.
      (s.copy(
         buffer = b,
         cursorPos = pos.max(0).min(b.length),
         historyIdx = None,
         draft = "",
       ), Cmd.none)

    case MoveCursor(_) if s.busy =>
      (s, Cmd.none)
    case MoveCursor(pos) =>
      (s.copy(cursorPos = pos.max(0).min(s.buffer.length)), Cmd.none)

    case Submit if s.buffer.trim == ":quit" =>
      (s, Cmd.exit)
    case Submit if s.busy || s.buffer.isEmpty =>
      (s, Cmd.none)
    case Submit =>
      val line = s.buffer
      (s.copy(
         buffer = "", cursorPos = 0,
         busy = true, inFlight = Some(line),
         spinnerFrame = 0, historyIdx = None, draft = "",
       ),
        Cmd.task(evalOne(line)) {
          case Right(out) => Done(line, out)
          case Left(err)  => Done(line, s"error: $err")
        })

    case InsertNewline if s.busy => (s, Cmd.none)
    case InsertNewline =>
      val (before, after) = s.buffer.splitAt(s.cursorPos)
      val newBuf = before + "\n" + after
      (s.copy(
         buffer = newBuf,
         cursorPos = s.cursorPos + 1,
         historyIdx = None,
         draft = "",
       ), Cmd.none)

    case KillToLineEnd if s.busy => (s, Cmd.none)
    case KillToLineEnd =>
      val (_, lineEnd) = lineBoundsAt(s.buffer, s.cursorPos)
      // If we're already at end-of-line and there's more buffer
      // afterwards, eat the newline too (standard readline behavior).
      val cut = if s.cursorPos == lineEnd && lineEnd < s.buffer.length then lineEnd + 1
                else lineEnd
      val newBuf = s.buffer.substring(0, s.cursorPos) + s.buffer.substring(cut)
      (s.copy(buffer = newBuf, historyIdx = None, draft = ""), Cmd.none)

    case KillToLineStart if s.busy => (s, Cmd.none)
    case KillToLineStart =>
      val (lineStart, _) = lineBoundsAt(s.buffer, s.cursorPos)
      val newBuf = s.buffer.substring(0, lineStart) + s.buffer.substring(s.cursorPos)
      (s.copy(
         buffer = newBuf,
         cursorPos = lineStart,
         historyIdx = None,
         draft = "",
       ), Cmd.none)

    case KillWordBack if s.busy => (s, Cmd.none)
    case KillWordBack =>
      // Skip trailing whitespace before the cursor, then delete back to
      // the next whitespace or buffer start. Mirrors bash's Ctrl-W.
      val buf = s.buffer
      var i = s.cursorPos
      while i > 0 && buf.charAt(i - 1).isWhitespace do i -= 1
      while i > 0 && !buf.charAt(i - 1).isWhitespace do i -= 1
      val newBuf = buf.substring(0, i) + buf.substring(s.cursorPos)
      (s.copy(buffer = newBuf, cursorPos = i, historyIdx = None, draft = ""), Cmd.none)

    case Done(in, out) =>
      val trimmed = out.stripTrailing()
      (s.copy(history = s.history :+ Entry(in, trimmed), busy = false, inFlight = None),
        Cmd.none)

    case Quit =>
      (s, Cmd.exit)

    case HistoryPrev if s.busy || s.history.isEmpty =>
      (s, Cmd.none)
    case HistoryPrev =>
      val newIdx = s.historyIdx match
        case None    => s.history.size - 1
        case Some(i) => (i - 1).max(0)
      val newDraft = if s.historyIdx.isEmpty then s.buffer else s.draft
      val newBuf = s.history(newIdx).input
      (s.copy(
         buffer = newBuf,
         cursorPos = newBuf.length,
         historyIdx = Some(newIdx),
         draft = newDraft,
       ), Cmd.none)

    case HistoryNext if s.busy =>
      (s, Cmd.none)
    case HistoryNext =>
      s.historyIdx match
        case None => (s, Cmd.none)
        case Some(i) if i < s.history.size - 1 =>
          val newIdx = i + 1
          val newBuf = s.history(newIdx).input
          (s.copy(
             buffer = newBuf,
             cursorPos = newBuf.length,
             historyIdx = Some(newIdx),
           ), Cmd.none)
        case Some(_) =>
          // Past the most recent entry — return to the saved draft.
          (s.copy(
             buffer = s.draft,
             cursorPos = s.draft.length,
             historyIdx = None,
             draft = "",
           ), Cmd.none)

  // -- Buffer edit helpers --------------------------------------------

  /** Find the boundaries of the line containing `pos`. Returns the
   *  start index (inclusive) and the end index (exclusive of the
   *  trailing newline if any). */
  private def lineBoundsAt(buf: String, pos: Int): (Int, Int) =
    val start = buf.lastIndexOf('\n', pos - 1) + 1
    val nextNl = buf.indexOf('\n', pos)
    val end = if nextNl < 0 then buf.length else nextNl
    (start, end)

  /** Characters that we accept as printable input. Mirrors the set
   *  layoutz's `input.handle` used to allow; broadened to also accept
   *  Unicode letters/digits so users can type non-ASCII identifiers. */
  private def isPrintable(c: Char): Boolean =
    !c.isControl && c != '\n' && c != '\r' && c != '\t'

  def subscriptions(s: State): Sub[Msg] =
    val keys = Sub.onKeyPress { k =>
      // History nav must be checked BEFORE any text-editing dispatch,
      // otherwise Ctrl-P / Ctrl-N could be swallowed by editing.
      val navMsg: Option[Msg] = k match
        case Key.Up   | Key.Ctrl('P') => Some(HistoryPrev)
        case Key.Down | Key.Ctrl('N') => Some(HistoryNext)
        case _                        => None

      def insertAt(c: Char): EditBuffer =
        val (before, after) = s.buffer.splitAt(s.cursorPos)
        EditBuffer(before + c + after, s.cursorPos + 1)

      def deleteBack: Option[Msg] =
        if s.cursorPos == 0 then None
        else
          val (before, after) = s.buffer.splitAt(s.cursorPos)
          Some(EditBuffer(before.dropRight(1) + after, s.cursorPos - 1))

      def deleteForward: Option[Msg] =
        if s.cursorPos >= s.buffer.length then None
        else
          val (before, after) = s.buffer.splitAt(s.cursorPos)
          Some(EditBuffer(before + after.drop(1), s.cursorPos))

      def cursorMsg: PartialFunction[Key, Msg] =
        case Key.Left  | Key.Ctrl('B') => MoveCursor(s.cursorPos - 1)
        case Key.Right | Key.Ctrl('F') => MoveCursor(s.cursorPos + 1)
        case Key.Home  | Key.Ctrl('A') =>
          val (lineStart, _) = lineBoundsAt(s.buffer, s.cursorPos)
          MoveCursor(lineStart)
        case Key.End | Key.Ctrl('E') =>
          val (_, lineEnd) = lineBoundsAt(s.buffer, s.cursorPos)
          MoveCursor(lineEnd)

      def editMsg: PartialFunction[Key, Msg] =
        case Key.Char(c) if isPrintable(c) => insertAt(c)

      // layoutz 0.7.0's KeyParser swallows the byte after ESC unless
      // it's `[` (CSI). That means Alt-Enter (ESC + \r) collapses to
      // a bare Key.Escape — we never see the Enter. Plain Key.Enter
      // is already taken for Submit, so Escape is the only single
      // key left to bind "insert newline" to. This is the documented
      // multiline shortcut; in a REPL Escape has no other natural
      // meaning.
      //
      // Ctrl-Enter is *not* distinguishable from plain Enter: the
      // KeyParser maps both `\n` (0x0a) and `\r` (0x0d) to Key.Enter
      // and the terminal does not emit a separate sequence for
      // Ctrl-Enter without kitty / modifyOtherKeys protocol support.
      val terminalMsg: Option[Msg] = k match
        case Key.Enter     => Some(Submit)
        case Key.Ctrl('D') => Some(Quit)
        case Key.Escape    => Some(InsertNewline)
        case Key.Ctrl('K') => Some(KillToLineEnd)
        case Key.Ctrl('U') => Some(KillToLineStart)
        case Key.Ctrl('W') => Some(KillWordBack)
        case _             => None

      navMsg
        .orElse(cursorMsg.lift(k))
        .orElse(editMsg.lift(k))
        .orElse(deleteForward.filter(_ => k == Key.Delete))
        .orElse(deleteBack.filter(_ => k == Key.Backspace))
        .orElse(terminalMsg)
    }
    // Only tick the spinner while something's in flight (either the
    // startup preload or a per-input compile); an idle REPL shouldn't
    // be re-rendering 12x/sec for no reason.
    if s.busy || s.initializing then Sub.batch(keys, Sub.time.everyMs(80L, Tick))
    else keys

  def view(s: State) =
    val prompt = "cappy>".color(Color.Cyan).style(Style.Bold).render
    val cont   = "     …".color(Color.Cyan).style(Style.Dim).render
    val gutter = "│".color(Color.Cyan).style(Style.Dim).render

    // Output lines get a thin vertical bar in the left gutter to set them
    // apart from prompt lines. Blank output lines stay blank — we don't
    // want a bare gutter floating on its own line.
    def outputLines(out: String): Seq[Element] =
      if out.isEmpty then Seq.empty
      else out.split("\n", -1).iterator.map { ln =>
        val rendered: Element = if ln.isEmpty then "" else s"$gutter $ln"
        rendered
      }.toSeq

    /** Render a stored input (no live cursor). Prefixes the first line
     *  with `cappy>` and subsequent lines with a dim continuation. */
    def renderEntryInput(input: String): Seq[Element] =
      input.split("\n", -1).iterator.zipWithIndex.map { (ln, i) =>
        val pfx = if i == 0 then prompt else cont
        (s"$pfx $ln": Element)
      }.toSeq

    /** Render the active buffer with a block cursor at `cursorPos`.
     *
     *  We avoid drawing a one-cell vertical-bar cursor between
     *  characters because (a) different fonts render those glyphs
     *  inconsistently (▏ shows as a blank cell in many terminals,
     *  making "Left arrow" look like it inserted a space), and (b) it
     *  fights the terminal's own cursor cell. Instead we highlight
     *  the cell AT cursorPos with reverse video. If the cursor is at
     *  end-of-line/end-of-buffer, we paint a reverse-video space so
     *  the block stays visible.
     */
    def renderActiveBuffer(buffer: String, cursorPos: Int): Seq[Element] =
      def block(s: String): String = s.style(Style.Reverse).render
      // Split the buffer into lines, tracking the global index of
      // each line's start so we know whether/where the cursor lands.
      val lines = buffer.split("\n", -1)
      val starts = lines.scanLeft(0)(_ + _.length + 1).dropRight(1)
      lines.iterator.zip(starts.iterator).zipWithIndex.map { case ((ln, lineStart), i) =>
        val pfx = if i == 0 then prompt else cont
        val lineEnd = lineStart + ln.length
        val withCursor =
          if cursorPos < lineStart || cursorPos > lineEnd then ln
          else if cursorPos == lineEnd then
            // Cursor at end-of-line — paint a reverse-video space
            // after the line content so the block stays visible.
            ln + block(" ")
          else
            // Cursor on a real character; highlight that one cell.
            val rel = cursorPos - lineStart
            ln.substring(0, rel) + block(ln.charAt(rel).toString) + ln.substring(rel + 1)
        (s"$pfx $withCursor": Element)
      }.toSeq

    val transcriptLines: Seq[Element] =
      s.history.iterator.flatMap { e =>
        renderEntryInput(e.input).iterator ++ outputLines(e.output).iterator
      }.toSeq

    val currentBlock: Seq[Element] =
      if s.initializing then
        // Startup preload: support classpath is being compiled + sent
        // to the Python subprocess. No prompt yet — input is blocked
        // until this finishes.
        val label = "Compiling library classes…".color(Color.Yellow).render
        Seq(Spinner(label, s.spinnerFrame, SpinnerStyle.Dots): Element)
      else if s.busy then
        // Show the in-flight prompt with a spinner underneath so the user
        // still has context for what's compiling.
        val recent = s.inFlight.getOrElse("")
        val submittedLines = renderEntryInput(recent)
        val label = "Compiling…".color(Color.Yellow).render
        val spin: Element = Spinner(label, s.spinnerFrame, SpinnerStyle.Dots)
        submittedLines :+ spin
      else
        renderActiveBuffer(s.buffer, s.cursorPos)

    val header = "─── Cappy REPL ───".color(Color.Cyan).style(Style.Bold).render
    val hint   = "Enter to evaluate  ·  Esc / Alt-Enter for newline  ·  ↑/↓ history  ·  Ctrl-A/E line · Ctrl-K/U/W kill  ·  :quit / Ctrl-D"
                   .style(Style.Dim).render

    val body: Seq[Element] = transcriptLines ++ currentBlock
    layout(((Seq[Element](header, hint, br)) ++ body)*)

  // Driver + persistent REPL state live outside the layoutz Model so the
  // case class doesn't need to thread a mutable compiler Context. Each
  // Submit grabs `replState`, drives one input, and writes back the
  // updated CappyReplState.
  private lazy val driver: CappyReplDriver = new CappyReplDriver()
  @volatile private var replState: Option[CappyReplState] = None

  private def evalOne(line: String): String =
    val s0 = replState.getOrElse(driver.initialState)
    val (output, next) = driver.run(line)(using s0)
    replState = Some(next)
    output

  def main(args: Array[String]): Unit =
    try run
    finally
      if driver != null then driver.shutdown()
