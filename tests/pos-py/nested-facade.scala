import scala.python.*

// Chained facade objects. A nested `@extern` object inside an enclosing
// `@extern` facade gets its OWN binding applied (genFacadeSelect rebinds
// to the inner's externBinding instead of extending the outer's path).
// This matters when the nested object's Python module differs from the
// simple extension of its container — here, `os.path` is a distinct
// Python submodule, and we access it both inline (`pyos.path.join(...)`)
// and via a Scala val (`val p = pyos.path`, then `p.join(...)`).

@extern("os")
object pyos extends PyAny:
  @extern("os.path")
  object path extends PyAny:
    def join(a: String, b: String): String = native
    val sep: String = native

@extern("builtins", "print")
def pyPrint(x: Any): Unit = native

@main def nestedFacade(): Unit =
  // Chained inline access - exercises the rebind-on-facade-select path.
  pyPrint(pyos.path.join("foo", "bar"))

  // Store the rebound reference in a Scala val, then select through it.
  // The val holds a PyExternalRef that then loses its structural identity
  // (becomes a PyVarRef), so subsequent selects fall back to PyAttrAccess.
  val p = pyos.path
  pyPrint(p.join("baz", "qux"))
  pyPrint(p.sep)
