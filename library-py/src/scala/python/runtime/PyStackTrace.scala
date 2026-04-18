package scala.python.runtime

import scala.python.{extern, native}

object PyStackTrace:
  type Captured = Any

  @extern("traceback", "walk_stack")
  private def pyWalkStack(frame: Any): Any = native

  @extern("builtins", "list")
  private def pyList(iterable: Any): Any = native

  @extern("builtins", "reversed")
  private def pyReversed(iterable: Any): Any = native

  @extern("sys", "_getframe")
  private def pyGetFrame(): Any = native

  @extern("builtins", "len")
  private def pyLen(value: Any): Int = native

  @extern("operator", "getitem")
  private def pyGetItem(value: Any, index: Int): Any = native

  @extern("builtins", "getattr")
  private def pyGetAttr(value: Any, name: String): Any = native

  /** Capture the active stack as a list of `(frame, lineno)` tuples.
   *
   *  Unlike `traceback.extract_stack()` (which returns `FrameSummary`
   *  objects), this preserves the raw frame so we can reach
   *  `frame.f_code.co_qualname` — needed to disambiguate internal
   *  scaffolding from user frames that share a common method name
   *  like `__init__` or `capture`.
   *
   *  Returned outermost-first (matching `traceback.extract_stack()`);
   *  `java.lang.StackTrace.extract` then reverses into innermost-first
   *  order.
   */
  def capture(): Captured =
    // `_getframe()` points at this method; `.f_back` starts walking from
    // the caller, so we don't appear in our own capture. `walk_stack`
    // yields innermost-first — reverse via a list so downstream indexing
    // matches the old FrameSummary ordering.
    val startFrame = pyGetAttr(pyGetFrame(), "f_back")
    pyList(pyReversed(pyList(pyWalkStack(startFrame))))

  def length(captured: Captured): Int =
    pyLen(captured)

  /** Returns the raw Python frame object at `index`.
   *
   *  Each element of `captured` is a `(frame, lineno)` tuple, so we
   *  unpack the frame for callers that want attribute access.
   */
  def frameAt(captured: Captured, index: Int): Any =
    val pair = pyGetItem(captured, index)
    pyGetItem(pair, 0)

  def filename(frame: Any): String =
    pyGetAttr(pyGetAttr(frame, "f_code"), "co_filename").asInstanceOf[String]

  /** Per-frame current line number. Prefer `linenoAt(captured, i)` when
   *  the traceback was captured eagerly; this is here for completeness.
   */
  def lineno(frame: Any): Int =
    pyGetAttr(frame, "f_lineno").asInstanceOf[Int]

  def name(frame: Any): String =
    pyGetAttr(pyGetAttr(frame, "f_code"), "co_name").asInstanceOf[String]

  /** Qualified name (e.g. `StackTrace_.capturePyError__...`) — the key
   *  discriminator between internal scaffolding and user frames that
   *  happen to share a plain method name.
   */
  def qualname(frame: Any): String =
    pyGetAttr(pyGetAttr(frame, "f_code"), "co_qualname").asInstanceOf[String]
