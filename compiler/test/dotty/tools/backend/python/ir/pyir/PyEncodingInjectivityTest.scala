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

  // ---------------------------------------------------------------
  //  PyMethodName: Python __name mangling guard
  // ---------------------------------------------------------------

  @Test def methodNameWithUnderscoreSimpleDoesNotStartWithDoubleUnderscore(): Unit =
    // A Scala member literally named `$` sanitizes to `_`, then the
    // encoded form prefixes `__<sig>` and used to produce `___<sig>`.
    // Inside a Python class body Python rewrites `__name` to
    // `_<ClassName>__name`, breaking attribute access. Guard at
    // `PyMethodName.encoded` ensures the emitted form never starts
    // with `__`.
    val m = PyMethodName(
      PySimpleMethodName("_"),
      Nil,
      PyClassRef(PyClassName("C"))
    )
    assertFalse(
      s"encoded form ${m.encoded} must not start with `__` (Python private-name mangling)",
      m.encoded.startsWith("__")
    )
    assertEquals("_scpy_m___LC", m.encoded)

  @Test def methodNameWithDoubleUnderscoreSimpleIsGuarded(): Unit =
    // User-written method named literally `__foo` (not a dunder — no
    // trailing `__`) would otherwise emit `__foo__V` and trigger
    // Python mangling. The guard prepends `_scpy_m`.
    val m = PyMethodName(
      PySimpleMethodName("__foo"),
      Nil,
      PyPrimRef.VoidRef
    )
    assertFalse(m.encoded.startsWith("__"))
    assertEquals("_scpy_m__foo__V", m.encoded)

  @Test def methodNameDundersAreNotGuarded(): Unit =
    // Standard Python dunders (start AND end with __, length >= 5) are
    // exempt from Python mangling and must round-trip unchanged.
    val hash = PyMethodName(
      PySimpleMethodName("__hash__"),
      Nil,
      PyPrimRef.IntRef
    )
    assertEquals("__hash__", hash.encoded)

    val eq = PyMethodName(
      PySimpleMethodName("__eq__"),
      List(PyClassRef(PyClassName("java.lang.Object"))),
      PyPrimRef.BooleanRef
    )
    assertEquals("__eq__", eq.encoded)

  @Test def methodNameConstructorsUnchanged(): Unit =
    val ctor = PyMethodName(
      PySimpleMethodName.Constructor,
      Nil,
      PyPrimRef.VoidRef
    )
    assertEquals("__init__", ctor.encoded)

    val clinit = PyMethodName(
      PySimpleMethodName.StaticInit,
      Nil,
      PyPrimRef.VoidRef
    )
    assertEquals("_scpy_clinit", clinit.encoded)

  @Test def methodNameWithSingleUnderscoreSimpleIsNotGuarded(): Unit =
    // A simple name like `_foo` (single leading underscore) produces
    // `_foo__V` — only one leading underscore, NOT subject to Python
    // mangling. The guard must NOT churn this case.
    val m = PyMethodName(
      PySimpleMethodName("_foo"),
      Nil,
      PyPrimRef.VoidRef
    )
    assertEquals("_foo__V", m.encoded)
    assertFalse(m.encoded.startsWith("__"))

  // ---------------------------------------------------------------
  //  PyFieldName: owner-aware mangling
  // ---------------------------------------------------------------

  @Test def fieldNamePrivateDistinguishesOwnersWithSameSimpleName(): Unit =
    // Two PRIVATE fields named `msg` declared in different classes must
    // encode to distinct Python attribute names. Otherwise a subclass
    // `val msg` and a parent's private `msg` (e.g. Throwable.msg vs
    // CommandLineParser.ParseError.msg) alias to the same `self.msg`
    // slot, and the parent ctor clobbers the subclass-set value.
    val parent = PyFieldName(
      PyClassName("java.lang.Throwable"),
      PySimpleFieldName("msg"),
      isPrivate = true
    )
    val child = PyFieldName(
      PyClassName("scala.util.CommandLineParser.ParseError"),
      PySimpleFieldName("msg"),
      isPrivate = true
    )
    assertNotEquals(parent.encoded, child.encoded)
    assertEquals("_scpy_f_java_dlang_dThrowable__msg", parent.encoded)
    assertEquals(
      "_scpy_f_scala_dutil_dCommandLineParser_dParseError__msg",
      child.encoded
    )

  @Test def fieldNamePublicKeepsSimpleName(): Unit =
    // PUBLIC / PROTECTED fields keep their simple name so hand-written
    // runtime classes (BoxedUnit.UNIT, IntRef.elem) match codegen by
    // spelling. Two public fields with the same simple name in
    // different owners DO collide on the same Python slot, but JVM
    // semantics for public fields don't require per-owner storage —
    // user code goes through accessor methods, not direct field reads.
    val a = PyFieldName(PyClassName("p.A"), PySimpleFieldName("x"))
    val b = PyFieldName(PyClassName("p.B"), PySimpleFieldName("x"))
    assertEquals("x", a.encoded)
    assertEquals("x", b.encoded)
    assertEquals(a.encoded, b.encoded)

  @Test def fieldNameSameOwnerAndSimpleEncodesIdentically(): Unit =
    val a = PyFieldName(PyClassName("p.A"), PySimpleFieldName("x"), isPrivate = true)
    val b = PyFieldName(PyClassName("p.A"), PySimpleFieldName("x"), isPrivate = true)
    assertEquals(a.encoded, b.encoded)

  @Test def fieldNamePrivateNeverStartsWithDoubleUnderscore(): Unit =
    // For private (mangled) fields the leading single `_` is deliberate:
    // the encoded form must NOT begin with `__`, otherwise Python's
    // compile-time private-name mangling rewrites it inside class
    // bodies. Spot-check a few owners including pathological ones.
    val tricky = List(
      "a.b.C",
      "_a.b.C",     // leading underscore in segment
      "a._b.C",
      "java.lang.Object",
      "Outer_.Inner",
      "a_b.C_d.E"
    )
    for fqn <- tricky do
      val f = PyFieldName(
        PyClassName(fqn),
        PySimpleFieldName("x"),
        isPrivate = true
      )
      assertFalse(
        s"$fqn encodes to ${f.encoded}, must not start with `__`",
        f.encoded.startsWith("__")
      )

  // ---------------------------------------------------------------
  //  Digit-leading segment sanitization (audit #8 / fixtures
  //  16405.scala, 9416.scala — file basenames that happen to be
  //  numeric land in synthetic class names like `16405$package$`).
  //  Python identifiers cannot start with a digit, so the encoder
  //  prepends a reserved `_scpy_n` prefix at the segment level.
  //
  //  These tests pin down the Python-validity guarantee plus
  //  injectivity: a digit-leading Scala segment must never collide
  //  with any legal Scala identifier.
  // ---------------------------------------------------------------

  @Test def classRefDigitLeadingSegmentEncodesAsValidIdentifier(): Unit =
    // The end-to-end scenario: a top-level def in `16405.scala`
    // generates the JVM class `16405$package$`, which `encodeClassName`
    // turns into a `PyClassName` whose segment is `_scpy_n16405_package_`.
    // The encoded `PyClassRef` must be a legal Python identifier
    // (i.e. `class _scpy_n16405_package_(...):` parses).
    val cls = PyClassName("_scpy_n16405_package_")
    val pyIdent = "[A-Za-z_][A-Za-z0-9_]*".r
    assertTrue(
      s"${cls.simpleName} should be a valid Python identifier",
      pyIdent.matches(cls.simpleName)
    )
    // The PyClassRef encoding must also be a valid Python identifier
    // (used for type-erased class references in encoded method signatures).
    val ref = PyClassRef(cls)
    assertTrue(
      s"${ref.encoded} should be a valid Python identifier",
      pyIdent.matches(ref.encoded)
    )

  @Test def classRefDigitGuardedSegmentsRetainInjectivity(): Unit =
    // After `PyEncoding.sanitizeName` prepends `_scpy_n` to digit-leading
    // segments, two distinct guarded segments must remain distinct at the
    // IR layer (e.g. `_scpy_n16405_package_` vs `_scpy_n9416_package_`).
    val a = PyClassRef(PyClassName("_scpy_n16405_package_"))
    val b = PyClassRef(PyClassName("_scpy_n9416_package_"))
    assertNotEquals(a.encoded, b.encoded)

  @Test def classNameSegmentsCanBeDigitGuarded(): Unit =
    // Multi-segment class FQN where one inner segment is a guarded
    // digit-leading name. PyClassName preserves the segments verbatim;
    // none of the segment-level invariants (validity, distinctness)
    // should regress.
    val name = PyClassName("pkg._scpy_n123impl.Cls")
    val pyIdent = "[A-Za-z_][A-Za-z0-9_]*".r
    for seg <- name.segments do
      assertTrue(s"$seg must be a valid Python identifier", pyIdent.matches(seg))

end PyEncodingInjectivityTest
