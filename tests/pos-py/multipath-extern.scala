import scala.python.*

// Multi-path `@extern` coverage. Two orthogonal code paths are exercised:
//
//   1. Top-level `@extern(module, seg0, seg1)` defs produce
//      `PyExternalRef(module, [seg0, seg1])` in `genExternCall`, and the
//      emitter canonicalises that to `FromImport(module, seg0)` with
//      `rest = [seg1]` walked at each use site. Two different multi-path
//      bindings sharing `seg0` share the same alias.
//
//   2. Chained `scala.Dynamic` selects on a `PyExternalRef` receiver hit
//      `genDynamicSelect`'s path-extension arm repeatedly, accumulating
//      multiple path segments on ONE `PyExternalRef` ([] -> ["path"] ->
//      ["path", "extsep"]). Breaking the extension branch would drop the
//      accumulated segments.

@extern("os", "path", "join")
def pyJoin(a: String, b: String): String = native

@extern("os", "path", "basename")
def pyBasename(path: String): String = native

@extern("builtins", "print")
def pyPrint(x: Any): Unit = native

@main def multipathExtern(): Unit =
  val full = pyJoin("foo", "bar.txt")
  pyPrint(full)
  pyPrint(pyBasename(full))

  // `os.path.extsep` is `.` on every supported platform (POSIX, Windows,
  // Darwin) so the check stays stable regardless of runner OS. The chain
  // `Dynamic.module("os").path.extsep` forces two successive PyExternalRef
  // path extensions inside `genDynamicSelect`.
  pyPrint(Dynamic.module("os").path.extsep)
