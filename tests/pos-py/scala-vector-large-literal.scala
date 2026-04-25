// Regression test: `Vector(elems*)` with ≥16 literal arguments hits
// the builder fallback path (`Vector.from(IterableOnce)`) which uses
// labeled blocks AND transitively constructs a `ClassTag`, forcing
// initialization of `scala.runtime.ClassValueCompat`. The latter calls
// `Class.forName("java.lang.ClassValue", false, loader)`. Two bugs
// were fixed together: PyLabeled hoist (so the builder result is
// bound) and `_scpy_mod_java_lang_Class_` registration (so the
// `Class.forName` call resolves).
//
// See notes/issue-list-vector-large-literal-unbound-locals.md
// and notes/issue-java-lang-class-module-not-registered.md.

@main def scalaVectorLargeLiteral(): Unit =
  val xs = Vector(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
  println("len:" + xs.length)
  println("head:" + xs.head)
  println("at8:" + xs(8))
  println("last:" + xs.last)
