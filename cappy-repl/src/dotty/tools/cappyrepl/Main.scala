package dotty.tools.cappyrepl

import layoutz._

object Main extends LayoutzApp[Main.State, Main.Msg]:

  case class Entry(input: String, output: String)

  /** `historyIdx = Some(i)` means the buffer mirrors `history(i).input` (we're
   *  navigating). `draft` holds whatever the user was typing before they
   *  started navigating, so Down past the most recent entry restores it.
   */
  case class State(
    history: Vector[Entry],
    buffer: String,
    busy: Boolean,
    historyIdx: Option[Int],
    draft: String,
  )

  sealed trait Msg
  case class Edit(b: String) extends Msg
  case object Submit extends Msg
  case class Done(input: String, output: String) extends Msg
  case object Quit extends Msg
  case object HistoryPrev extends Msg
  case object HistoryNext extends Msg

  def init: (State, Cmd[Msg]) =
    (State(Vector.empty, "", false, None, ""), Cmd.none)

  def update(msg: Msg, s: State): (State, Cmd[Msg]) = msg match
    case Edit(_) if s.busy =>
      (s, Cmd.none)
    case Edit(b) =>
      // Editing exits history navigation; the new buffer becomes the draft.
      (s.copy(buffer = b, historyIdx = None, draft = ""), Cmd.none)

    case Submit if s.buffer.trim == ":quit" =>
      (s, Cmd.exit)
    case Submit if s.busy || s.buffer.isEmpty =>
      (s, Cmd.none)
    case Submit =>
      val line = s.buffer
      (s.copy(buffer = "", busy = true, historyIdx = None, draft = ""),
        Cmd.task(evalOne(line)) {
          case Right(out) => Done(line, out)
          case Left(err)  => Done(line, s"error: $err")
        })

    case Done(in, out) =>
      (s.copy(history = s.history :+ Entry(in, out), busy = false), Cmd.none)

    case Quit =>
      (s, Cmd.exit)

    case HistoryPrev if s.busy || s.history.isEmpty =>
      (s, Cmd.none)
    case HistoryPrev =>
      val newIdx = s.historyIdx match
        case None    => s.history.size - 1
        case Some(i) => (i - 1).max(0)
      val newDraft = if s.historyIdx.isEmpty then s.buffer else s.draft
      (s.copy(buffer = s.history(newIdx).input, historyIdx = Some(newIdx), draft = newDraft),
        Cmd.none)

    case HistoryNext if s.busy =>
      (s, Cmd.none)
    case HistoryNext =>
      s.historyIdx match
        case None => (s, Cmd.none)
        case Some(i) if i < s.history.size - 1 =>
          val newIdx = i + 1
          (s.copy(buffer = s.history(newIdx).input, historyIdx = Some(newIdx)), Cmd.none)
        case Some(_) =>
          // Past the most recent entry — return to the saved draft.
          (s.copy(buffer = s.draft, historyIdx = None, draft = ""), Cmd.none)

  def subscriptions(s: State): Sub[Msg] = Sub.onKeyPress { k =>
    // History navigation must be checked BEFORE input.handle, otherwise
    // Ctrl-P / Ctrl-N could be swallowed by the text-editing helper.
    val navMsg: Option[Msg] = k match
      case Key.Up   | Key.Ctrl('P') => Some(HistoryPrev)
      case Key.Down | Key.Ctrl('N') => Some(HistoryNext)
      case _                        => None
    navMsg
      .orElse(input.handle(k, 0, 0, s.buffer).map(Edit(_)))
      .orElse(k match
        case Key.Enter     => Some(Submit)
        case Key.Ctrl('D') => Some(Quit)
        case _             => None)
  }

  def view(s: State) =
    val hint = "Enter to evaluate  ·  :quit or Ctrl-D to exit".style(Style.Dim).render

    val promptMark = "cappy>".color(Color.Cyan).render
    val arrow      = "→".color(Color.Green).style(Style.Dim).render

    val transcript: Element =
      if s.history.isEmpty then empty
      else
        layout(s.history.flatMap { e =>
          val dimOut = e.output.style(Style.Dim).render
          Seq[Element](
            s"$promptMark ${e.input}",
            s"  $arrow $dimOut",
            br
          )
        }*)

    val activePrompt = "cappy>".color(Color.Cyan).style(Style.Bold).render
    val busyPrompt   = "…".color(Color.Yellow).style(Style.Bold).render
    val cursor       = "▏".color(Color.Cyan).render
    val inputLine: Element =
      if s.busy then s"$busyPrompt ${s.buffer}"
      else s"$activePrompt ${s.buffer}$cursor"

    layout(
      hint,
      br,
      transcript,
      inputLine
    )

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
