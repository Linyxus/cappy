package dotty.tools.backend.python.ir.pyir

import org.junit.Assert.*
import org.junit.Test

/** Regression tests for audit finding #16: distinct Scala class FQNs
 *  whose segment names contain `_` used to flatten to the same
 *  `PyClassRef.encoded` / `PyMethodName.encoded` string because the
 *  encoder naively joined segments with `_`. The current encoding
 *  escapes literal `_` as `_u` and segment breaks as `_d`, so
 *  collision-prone pairs now produce distinct identifiers.
 */
class PyEncodingInjectivityTest:

  // ---------------------------------------------------------------
  //  PyClassRef
  // ---------------------------------------------------------------

  @Test def classRefDistinguishesUnderscoreVsDotInLeftSegment(): Unit =
    val a = PyClassRef(PyClassName("a_b.c.D"))
    val b = PyClassRef(PyClassName("a.b_c.D"))
    assertNotEquals(
      "encodings of a_b.c.D and a.b_c.D must differ",
      a.encoded, b.encoded
    )

  @Test def classRefDistinguishesUnderscoreVsDotInRightSegment(): Unit =
    val a = PyClassRef(PyClassName("a.b.c_D"))
    val b = PyClassRef(PyClassName("a.b.c.D"))
    assertNotEquals(a.encoded, b.encoded)

  @Test def classRefHandlesAdjacentUnderscoresAndDots(): Unit =
    // After sanitization, a Scala module class can produce a segment
    // with a trailing underscore (originally `Foo$`). Make sure that
    // still doesn't alias with a different class name.
    val a = PyClassRef(PyClassName("pkg.Foo_.member"))
    val b = PyClassRef(PyClassName("pkg.Foo._member"))
    assertNotEquals(a.encoded, b.encoded)

  @Test def classRefStartsWithLPrefix(): Unit =
    val r = PyClassRef(PyClassName("a.b.C"))
    assertTrue(s"expected leading L-tag, got ${r.encoded}", r.encoded.startsWith("L"))

  @Test def classRefIsValidPythonIdentifier(): Unit =
    // Inputs in this list assume `$` has already been sanitized by
    // `PyEncoding.encodeClassName` (the production caller path) — the
    // FQN-encoder itself does not strip `$`.
    val tricky = List(
      "a_b.c.D",
      "a.b_c.D",
      "pkg.Foo_.member",
      "pkg.Foo._member",
      "java.lang.Object",
      "scala.collection.immutable.List_",
      "Outer_.Inner__Anon"
    )
    val pyIdent = "[A-Za-z_][A-Za-z0-9_]*".r
    for fqn <- tricky do
      val enc = PyClassRef(PyClassName(fqn)).encoded
      assertTrue(
        s"$enc (from $fqn) is not a valid Python identifier",
        pyIdent.matches(enc)
      )

  @Test def classRefEncodingIsInjectiveOverManyNames(): Unit =
    // Generate every length-3 string over a small alphabet that mixes
    // separators and escape-marker characters, treat each as a
    // potential FQN, encode it, and demand no two distinct inputs
    // collide. Catches regressions far more aggressively than the
    // hand-picked pairs above.
    val alphabet = List('a', 'b', '_', '.', 'd', 'u')
    val seen = scala.collection.mutable.Map.empty[String, String]
    for c1 <- alphabet; c2 <- alphabet; c3 <- alphabet do
      val raw = s"$c1$c2$c3"
      // PyClassName forbids empty segments, but we are exercising the
      // FQN encoder directly here so any non-empty string is OK.
      if raw.nonEmpty && !raw.startsWith(".") && !raw.endsWith(".") && !raw.contains("..") then
        val enc = PyClassRef.encodeFqn(raw)
        seen.get(enc) match
          case Some(prev) if prev != raw =>
            fail(s"collision: '$prev' and '$raw' both encode to '$enc'")
          case _ => seen(enc) = raw

  // ---------------------------------------------------------------
  //  PyMethodName
  // ---------------------------------------------------------------

  @Test def methodNameDistinguishesParamClassesWithUnderscoreVsDot(): Unit =
    // Same simple name and result type, but the single param's class
    // FQN differs only in where the `_` vs `.` lands.
    val simple = PySimpleMethodName("doStuff")
    val res    = PyPrimRef.VoidRef
    val m1 = PyMethodName(simple, List(PyClassRef(PyClassName("a_b.c.D"))), res)
    val m2 = PyMethodName(simple, List(PyClassRef(PyClassName("a.b_c.D"))), res)
    assertNotEquals(m1.encoded, m2.encoded)

  @Test def methodNameDistinguishesAdjacentClassRefsFromOneClassRef(): Unit =
    // Two-arg `(a.b.C, D.E)` vs one-arg `(a.b.C_D.E)` could in principle
    // alias if the inter-ref separator and the intra-ref separator are
    // both `_`. Verify they don't.
    val simple = PySimpleMethodName("foo")
    val res    = PyPrimRef.VoidRef
    val twoArgs = PyMethodName(
      simple,
      List(PyClassRef(PyClassName("a.b.C")), PyClassRef(PyClassName("D.E"))),
      res
    )
    val oneArg = PyMethodName(
      simple,
      List(PyClassRef(PyClassName("a.b.C_D.E"))),
      res
    )
    assertNotEquals(twoArgs.encoded, oneArg.encoded)

  @Test def methodNameDistinguishesResultClassWithUnderscore(): Unit =
    val simple = PySimpleMethodName("foo")
    val m1 = PyMethodName(simple, Nil, PyClassRef(PyClassName("a_b.C")))
    val m2 = PyMethodName(simple, Nil, PyClassRef(PyClassName("a.b_C")))
    assertNotEquals(m1.encoded, m2.encoded)

end PyEncodingInjectivityTest
