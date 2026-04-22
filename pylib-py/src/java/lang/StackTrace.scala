package java.lang

import scala.python.runtime.PyStackTrace

/** Python stack-trace capture for the ScalaPy javalib. */
private[lang] object StackTrace:
  private def decodeMethodName(name: String): String =
    val signatureStart = name.indexOf("__")
    if signatureStart <= 0 then name else name.substring(0, signatureStart)

  private def fileNameOnly(path: String): String =
    val slashIndex = path.lastIndexOf("/")
    val backslashIndex = path.lastIndexOf("\\")
    val splitIndex =
      if slashIndex > backslashIndex then slashIndex else backslashIndex
    if splitIndex < 0 then path else path.substring(splitIndex + 1)

  /** Everything before the final `.` in `co_qualname` — e.g. the
   *  owning class or top-level wrapper. An empty string means the
   *  frame is module-level (qualname has no dot).
   */
  private def qualnameOwner(qualname: String): String =
    val dotIndex = qualname.lastIndexOf(".")
    if dotIndex < 0 then "" else qualname.substring(0, dotIndex)

  /** Frame-classification strategy.
   *
   *  Pure filename-based filtering (per the L0.3 audit C4 plan) does
   *  not work for this backend: javalib code (the Throwable ctor,
   *  `fillInStackTrace`, `StackTrace.capturePyError`) and user code
   *  are both compiled into the SAME bundled `.py` file, so every
   *  frame Python reports has the same `co_filename`. That leaves the
   *  qualname (owner + method) as the only source of truth.
   *
   *  Filename-based pruning is still done as a defensive first pass:
   *  synthetic frames (`<string>`, `<exec>`, `<frozen ...>`) are
   *  skipped if the capture ever surfaces one. Everything else falls
   *  through to the scaffold tuple check below.
   *
   *  A frame counts as internal scaffolding when:
   *    - its filename is a Python synthetic (`<...>`), i.e. it is not
   *      a real source file;
   *    - its raw (un-demangled) method name is a compiler-invented
   *      prefix (`_scpy_ctor_*`, `_init$arity*`) — these can never
   *      come from user code, so no qualname check is needed;
   *    - its fully-qualified (owner, demangled-method) pair matches
   *      one of the fixed capture/Throwable scaffold frames — so a
   *      user class that happens to own a plain `capture` or
   *      `fillInStackTrace` method is NOT stripped;
   *    - it is an `__init__` dispatcher whose owning class also has a
   *      `_scpy_ctor_*` helper elsewhere on the captured stack (that
   *      pairing only arises for Python classes the backend emitted
   *      itself; a hand-written user `__init__` has no such sibling).
   */
  private def isSyntheticFilename(filename: String): scala.Boolean =
    // Python uses angle-bracket names for frames that have no real
    // source file: `<module>` is NOT one of these (it's the top of a
    // real `.py`), but `<string>` / `<exec>` / `<frozen ...>` are.
    filename.length >= 2 && filename.charAt(0) == '<' &&
      filename.charAt(filename.length - 1) == '>' &&
      filename != "<module>"

  private def isCompilerInventedName(methodName: String): scala.Boolean =
    methodName.startsWith("_scpy_ctor_") ||
      methodName.startsWith("_init$arity")

  /** Is `(owner, methodName)` one of the fixed capture-scaffold frames?
   *
   *  Matched by fully-qualified owner + demangled method name so user
   *  classes that happen to define a plain `capture` or
   *  `fillInStackTrace` method are not misidentified as internal.
   */
  private def isCaptureScaffold(owner: String, methodName: String): scala.Boolean =
    // Match the Python class identifiers the emitter generates. The
    // FQN-mangling change (see `PyIREmitter.classIdentifier`) makes
    // user-emitted classes use mangled FQNs like `java_lang_StackTrace_`,
    // while a few entries in `PyIREmitter.PythonReservedShortNames`
    // remap names that would clash with Python builtins —
    // `java.lang.Throwable -> _scpy_java_Throwable`, etc. The Python
    // qualname surfaces whichever identifier was used at definition
    // time, so the matcher accepts all known shapes.
    val capturePyError = methodName == "capturePyError"
    val getCurrentStackTrace = methodName == "getCurrentStackTrace"
    val fillInStackTrace = methodName == "fillInStackTrace"
    (owner == "PyStackTrace_" && methodName == "capture") ||
      (owner == "scala_python_runtime_PyStackTrace_" && methodName == "capture") ||
      ((owner == "StackTrace_" || owner == "java_lang_StackTrace_")
        && (capturePyError || getCurrentStackTrace)) ||
      ((owner == "Throwable"
          || owner == "java_lang_Throwable"
          || owner == "_scpy_java_Throwable")
        && fillInStackTrace)

  def getCurrentStackTrace(): Array[StackTraceElement] =
    extract(PyStackTrace.capture())

  def capturePyError(): Any =
    PyStackTrace.capture()

  def extract(captured: Any): Array[StackTraceElement] =
    if captured == null then
      new Array[StackTraceElement](0)
    else
      val frames = captured.asInstanceOf[PyStackTrace.Captured]
      val length = PyStackTrace.length(frames)
      // Build `reversed` innermost-first, each element carrying the
      // demangled method name, the qualname (for scaffold detection),
      // and the StackTraceElement to eventually return.
      val reversed = new Array[StackTraceElement](length)
      val qualnames = new Array[String](length)
      val rawNames = new Array[String](length)
      val filenames = new Array[String](length)
      var i = 0
      while i < length do
        val frame = PyStackTrace.frameAt(frames, length - i - 1)
        val rawName = PyStackTrace.name(frame)
        val methodName = decodeMethodName(rawName)
        val fullFilename = PyStackTrace.filename(frame)
        qualnames(i) = PyStackTrace.qualname(frame)
        rawNames(i) = rawName
        filenames(i) = fullFilename
        reversed(i) =
          new StackTraceElement(
            "<pycode>",
            methodName,
            fileNameOnly(fullFilename),
            PyStackTrace.lineno(frame)
          )
        i += 1

      val start = stripInternalPrefix(reversed, qualnames, rawNames, filenames)
      if start == 0 then
        reversed
      else
        val result = new Array[StackTraceElement](reversed.length - start)
        i = 0
        while i < result.length do
          result(i) = reversed(start + i)
          i += 1
        result

  /** Count the leading contiguous run of internal scaffolding frames
   *  (innermost-first). Stops at the first user frame.
   *
   *  A frame is internal iff:
   *    (a) its filename is a Python synthetic like `<string>` or
   *        `<exec>` (never a real source file); OR
   *    (b) its raw (un-demangled) name begins with `_scpy_ctor_` or
   *        `_init$arity` — compiler-invented, cannot be user code; OR
   *    (c) its fully-qualified name matches a known capture helper
   *        (`StackTrace.capturePyError`, `Throwable.fillInStackTrace`,
   *        etc.); OR
   *    (d) it is an `__init__` dispatcher paired with an adjacent
   *        `_scpy_ctor_*` helper in the owning class — only then is
   *        the `__init__` the emitted dispatcher and not user code.
   */
  private def stripInternalPrefix(
      reversed: Array[StackTraceElement],
      qualnames: Array[String],
      rawNames: Array[String],
      filenames: Array[String]
  ): Int =
    var start = 0
    var done = false
    while !done && start < reversed.length do
      val methodName = reversed(start).getMethodName()
      val rawName = rawNames(start)
      val qualname = qualnames(start)
      val filename = filenames(start)
      val owner = qualnameOwner(qualname)
      val isInternal =
        isSyntheticFilename(filename) ||
          isCompilerInventedName(rawName) ||
          isCaptureScaffold(owner, methodName) ||
          (methodName == "__init__" && ownerHasCtorHelper(owner, qualnames, rawNames))
      if isInternal then start += 1
      else done = true
    start

  /** Is the class named `owner` also the owner of some `_scpy_ctor_*`
   *  frame currently on the captured stack? Used to confirm that an
   *  `__init__` frame is an emitted dispatcher (paired with a ctor
   *  helper) rather than user code.
   */
  private def ownerHasCtorHelper(
      owner: String,
      qualnames: Array[String],
      rawNames: Array[String]
  ): scala.Boolean =
    if owner.length == 0 then false
    else
      var found = false
      var j = 0
      while !found && j < qualnames.length do
        if rawNames(j).startsWith("_scpy_ctor_") &&
            qualnameOwner(qualnames(j)) == owner
        then found = true
        else j += 1
      found
