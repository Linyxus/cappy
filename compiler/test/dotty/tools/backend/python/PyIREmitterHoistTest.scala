package dotty.tools.backend.python

import dotty.tools.backend.python.ir.pyir.*

import org.junit.Assert.*
import org.junit.Test

/** Tests for the method-scope hoisting of `_scpy_lbl_<n>` label-class
 *  declarations introduced to remove the per-iteration class-redefinition
 *  cost in tail-recursive `while True:` lowerings.
 *
 *  Construct PyIR trees directly, run `PyIREmitter.emitToString`, and
 *  inspect the emitted Python source. These tests assert the *shape*
 *  of the emitted code, not its runtime behaviour — behavioural
 *  regression coverage lives in `tests/pos-py/labeled-*.scala` and
 *  the new `tests/pos-py/labeled-{tailrec-hot-loop,sequential-blocks}`
 *  fixtures.
 */
class PyIREmitterHoistTest:

  private val NoPos = PyPosition.NoPosition

  // ----------------------------------------------------------------
  // Helpers
  // ----------------------------------------------------------------

  private def className(name: String): PyClassName = PyClassName(name)

  private def methodName(
      simple:    String,
      paramRefs: List[PyTypeRef] = Nil,
      resultRef: PyTypeRef       = PyPrimRef.VoidRef
  ): PyMethodName =
    PyMethodName(PySimpleMethodName(simple), paramRefs, resultRef)

  private def ctorName(): PyMethodName =
    PyMethodName(PySimpleMethodName.Constructor, Nil, PyPrimRef.VoidRef)

  private def method(
      name:       PyMethodName,
      body:       PyTree,
      resultType: PyType            = PyVoidType,
      namespace:  PyMemberNamespace = PyMemberNamespace.Public
  ): PyMethodDef =
    PyMethodDef(
      flags        = PyMemberFlags.empty.withNamespace(namespace),
      name         = name,
      originalName = PyOriginalName.NoOriginalName,
      args         = Nil,
      resultType   = resultType,
      body         = Some(body),
      pos          = NoPos
    )

  private def classDef(
      name:    PyClassName,
      methods: List[PyMethodDef]
  ): PyClassDef =
    PyClassDef(
      name         = name,
      originalName = PyOriginalName.NoOriginalName,
      kind         = PyClassKind.Class,
      superClass   = None,
      interfaces   = Nil,
      fields       = Nil,
      methods      = methods,
      pos          = NoPos
    )

  /** Emit a single class+method pair and return the emitted source. */
  private def emitMethod(method: PyMethodDef): String =
    val cls = classDef(className("Test"), List(method))
    PyIREmitter.emitToString(List(cls), None)

  /** Extract the body of `method` from a full bundle source by locating
   *  the matching `def <simple>__<sig>(...)` line and slicing through
   *  its indented body until indentation drops back to method-level
   *  or below. Required because the runtime prelude emits its own
   *  `class ...:` and `try:` blocks that shouldn't pollute body-scope
   *  assertions. The simple name is followed by `__` per
   *  `PyMethodName.encoded` (the param/result signature suffix). */
  private def methodBody(source: String, methodSimpleName: String): String =
    val lines = source.linesIterator.toIndexedSeq
    val defPrefix = s"def ${methodSimpleName}__"
    val defIdx = lines.indexWhere(_.trim.startsWith(defPrefix))
    assertTrue(s"could not locate `def ${methodSimpleName}__...(...)` in source:\n$source",
               defIdx >= 0)
    // The `def` is at method indent (4 spaces inside a class). Body
    // is everything indented strictly deeper than the `def` line.
    val defIndent = lines(defIdx).takeWhile(_ == ' ').length
    val tail = lines.drop(defIdx + 1)
    val bodyLines = tail.takeWhile { l =>
      l.trim.isEmpty || l.takeWhile(_ == ' ').length > defIndent
    }
    bodyLines.mkString("\n")

  /** Find the line index (0-based) of the unique line in `source` whose
   *  trimmed content equals `needle`. Fails the test if not exactly one
   *  match. */
  private def lineIndexOf(source: String, needle: String): Int =
    val lines = source.linesIterator.zipWithIndex.toList
    val matches = lines.filter { case (l, _) => l.trim == needle }
    assertEquals(s"expected exactly one '$needle' line in:\n$source",
                 1, matches.length)
    matches.head._2

  /** First line index whose trimmed content equals `needle`. Fails if
   *  no match. */
  private def firstLineIndexOf(source: String, needle: String): Int =
    val idx = source.linesIterator.zipWithIndex
      .find { case (l, _) => l.trim == needle }.map(_._2)
    assertTrue(s"expected at least one '$needle' line in:\n$source", idx.isDefined)
    idx.get

  /** Count occurrences of an exact-trimmed line. */
  private def countLines(source: String, needle: String): Int =
    source.linesIterator.count(_.trim == needle)

  /** True iff some line whose trimmed content matches `needle` occurs
   *  inside an indentation level deeper than the `while True:` line.
   *  Used to assert "label class is NOT inside the while body". */
  private def lineIsNestedDeeperThan(
      source:    String,
      needle:    String,
      outerLine: String
  ): Boolean =
    val lines = source.linesIterator.toIndexedSeq
    val outerIdx = lines.indexWhere(_.trim == outerLine)
    if outerIdx < 0 then return false
    val outerIndent = lines(outerIdx).takeWhile(_ == ' ').length
    lines.zipWithIndex.exists { case (l, i) =>
      i > outerIdx && l.trim == needle &&
      l.takeWhile(_ == ' ').length > outerIndent
    }

  // ----------------------------------------------------------------
  // Tests
  // ----------------------------------------------------------------

  /** 1. Single label inside a `PyWhile`: class declared at method scope,
   *  before the `while True:`, and NOT inside the loop body. */
  @Test def singleLabelInsideWhileIsHoisted(): Unit =
    val L = PyLabelName("L")
    val unit = PyUnitLit()(NoPos)
    val body =
      PyLabeled(
        L,
        PyWhile(
          PyBooleanLit(true)(NoPos),
          PyLabelReturn(L, unit)(NoPos)
        )(NoPos)
      )(PyVoidType, NoPos)

    val full = emitMethod(method(methodName("loop"), body))
    val mbody = methodBody(full, "loop")

    assertEquals("expected exactly one `class _scpy_lbl_1(BaseException):`",
                 1, countLines(mbody, "class _scpy_lbl_1(BaseException):"))
    val classLine = lineIndexOf(mbody, "class _scpy_lbl_1(BaseException):")
    val whileLine = lineIndexOf(mbody, "while True:")
    assertTrue(s"`class _scpy_lbl_1` (line $classLine) must precede `while True:` (line $whileLine):\n$mbody",
               classLine < whileLine)
    assertFalse("`class _scpy_lbl_1` must NOT be nested inside `while True:`",
                lineIsNestedDeeperThan(mbody,
                  "class _scpy_lbl_1(BaseException):", "while True:"))

  /** 2. Two sequential `PyLabeled`s in one method body get distinct
   *  counter values, both declared at method top before any `try:`. */
  @Test def twoSequentialLabeledBlocksGetDistinctNames(): Unit =
    val A = PyLabelName("A")
    val B = PyLabelName("B")
    val unit = PyUnitLit()(NoPos)
    val labeledA =
      PyLabeled(A, PyLabelReturn(A, unit)(NoPos))(PyVoidType, NoPos)
    val labeledB =
      PyLabeled(B, PyLabelReturn(B, unit)(NoPos))(PyVoidType, NoPos)
    val body = PyBlock(List(labeledA, labeledB), unit)(NoPos)

    val full = emitMethod(method(methodName("twoLabels"), body))
    val mbody = methodBody(full, "twoLabels")

    assertEquals(1, countLines(mbody, "class _scpy_lbl_1(BaseException):"))
    assertEquals(1, countLines(mbody, "class _scpy_lbl_2(BaseException):"))
    val cls1 = lineIndexOf(mbody, "class _scpy_lbl_1(BaseException):")
    val cls2 = lineIndexOf(mbody, "class _scpy_lbl_2(BaseException):")
    assertTrue("counter must be source-order: A→1, B→2", cls1 < cls2)
    val firstTry = firstLineIndexOf(mbody, "try:")
    assertTrue(s"both class decls must precede first `try:` (cls1=$cls1, cls2=$cls2, try=$firstTry)",
               cls1 < firstTry && cls2 < firstTry)

  /** 3. Nested `PyLabeled` (Outer wrapping Inner) — both declared at
   *  method top in pre-order. */
  @Test def nestedLabeledBlocksGetDistinctNames(): Unit =
    val Outer = PyLabelName("Outer")
    val Inner = PyLabelName("Inner")
    val unit = PyUnitLit()(NoPos)
    val innerBody =
      PyLabeled(Inner, PyLabelReturn(Inner, unit)(NoPos))(PyVoidType, NoPos)
    // Outer body must reference Outer so it isn't dead-pruned.
    val outerBody =
      PyBlock(
        List(innerBody),
        PyLabelReturn(Outer, unit)(NoPos)
      )(NoPos)
    val body = PyLabeled(Outer, outerBody)(PyVoidType, NoPos)

    val full = emitMethod(method(methodName("nested"), body))
    val mbody = methodBody(full, "nested")

    assertEquals(1, countLines(mbody, "class _scpy_lbl_1(BaseException):"))
    assertEquals(1, countLines(mbody, "class _scpy_lbl_2(BaseException):"))
    val cls1 = lineIndexOf(mbody, "class _scpy_lbl_1(BaseException):")
    val cls2 = lineIndexOf(mbody, "class _scpy_lbl_2(BaseException):")
    val firstTry = firstLineIndexOf(mbody, "try:")
    assertTrue("Outer mints _scpy_lbl_1 (pre-order)", cls1 < cls2)
    assertTrue("both class decls precede first try", cls2 < firstTry)

  /** 4. Dead label (no `PyLabelReturn` inside body) → no class decl,
   *  no try/except wrapper. */
  @Test def deadLabelIsPrunedAndEmitsNoClass(): Unit =
    val L = PyLabelName("Dead")
    val unit = PyUnitLit()(NoPos)
    val body = PyLabeled(L, unit)(PyVoidType, NoPos)

    val full = emitMethod(method(methodName("dead"), body))
    val mbody = methodBody(full, "dead")

    assertFalse(s"no `_scpy_lbl_*` class should be emitted for a dead label, body:\n$mbody",
                mbody.linesIterator.exists(_.trim.startsWith("class _scpy_lbl_")))
    assertFalse(s"no `try:` wrapper for a dead label, body:\n$mbody",
                mbody.linesIterator.exists(_.trim == "try:"))

  /** 5. Counter resets between methods: each method's first label is
   *  `_scpy_lbl_1`. */
  @Test def counterResetsAcrossMethods(): Unit =
    val A = PyLabelName("A")
    val B = PyLabelName("B")
    val unit = PyUnitLit()(NoPos)
    val mA = method(methodName("methA"),
      PyLabeled(A, PyLabelReturn(A, unit)(NoPos))(PyVoidType, NoPos))
    val mB = method(methodName("methB"),
      PyLabeled(B, PyLabelReturn(B, unit)(NoPos))(PyVoidType, NoPos))
    val cls = classDef(className("Test"), List(mA, mB))
    val full = PyIREmitter.emitToString(List(cls), None)
    val mbodyA = methodBody(full, "methA")
    val mbodyB = methodBody(full, "methB")

    // Each method should have a `_scpy_lbl_1` declaration; no `_scpy_lbl_2`
    // since neither method contains more than one label.
    assertEquals("methA gets its own `_scpy_lbl_1`",
                 1, countLines(mbodyA, "class _scpy_lbl_1(BaseException):"))
    assertEquals("methB gets its own `_scpy_lbl_1`",
                 1, countLines(mbodyB, "class _scpy_lbl_1(BaseException):"))
    assertEquals("no `_scpy_lbl_2` in methA",
                 0, countLines(mbodyA, "class _scpy_lbl_2(BaseException):"))
    assertEquals("no `_scpy_lbl_2` in methB",
                 0, countLines(mbodyB, "class _scpy_lbl_2(BaseException):"))

  /** 6. Method-tail peephole: a `PyLabeled` at method tail whose body
   *  is `PyLabelReturn(label, value)` directly is collapsed by
   *  `wrapLastReturn` into a plain `return <value>`. The pre-pass
   *  must NOT hoist a class for it (would be dead). */
  @Test def methodTailPeepholeProducesNoClass(): Unit =
    val L = PyLabelName("L")
    // Method body is PyLabeled at the tail with a single LabelReturn ⇒
    // isTailTargeted returns true ⇒ wrapLastReturn collapses it. Use a
    // value-returning method (Int result) so `returnLast` is true.
    val body =
      PyLabeled(
        L,
        PyLabelReturn(L, PyIntLit(42)(NoPos))(NoPos)
      )(PyIntType, NoPos)
    val mDef = method(
      name       = methodName("peep", resultRef = PyPrimRef.IntRef),
      body       = body,
      resultType = PyIntType
    )
    val full = emitMethod(mDef)
    val mbody = methodBody(full, "peep")

    assertFalse(s"no `class _scpy_lbl_` should be emitted when peephole fires, body:\n$mbody",
                mbody.linesIterator.exists(_.trim.startsWith("class _scpy_lbl_")))
    assertFalse(s"no `try:` wrapper when peephole fires, body:\n$mbody",
                mbody.linesIterator.exists(_.trim == "try:"))
    // Body should reduce to a plain `return 42`.
    assertTrue(s"peepholed body should emit `return 42`, body:\n$mbody",
               mbody.linesIterator.exists(_.trim == "return 42"))
