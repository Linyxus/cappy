// Pins the lowering for arity > 22 tuple literals.
//
// At erasure, `(1, 2, …, 23)` becomes
// `scala.runtime.TupleXXL.apply(genericWrapArray(elements))`, NOT a
// fixed-arity `TupleN.apply` call — TupleXXL is a real Scala class
// with `elems`/`unapplySeq`/`productElement`, kept as the runtime
// representation for variadic-arity tuples. The Python backend MUST
// route through TupleXXL's compiled support-library implementation
// rather than treating `TupleXXL.apply(seq)` as a fixed-arity tuple
// constructor (the regression: `_scpy_ScalaTuple((seq,))`, a 1-tuple
// wrapping the Seq, broke pattern-match unapplySeq and indexing in
// `tests/run/tuple-patterns.scala`).
//
// Three angles exercised:
//   1. Construction: a 23-element literal yields a TupleXXL instance.
//   2. Element access through `productElement`.
//   3. Pattern match deconstruction (forces `TupleXXL.unapplySeq` +
//      `lengthCompare` against the support-library implementation).

@main def tupleXXLLiteralRoundtrip(): Unit =
  val t23 = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
             11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
             21, 22, 23)

  // Direct projection through `Product` — bypasses the static
  // fixed-arity `_N` accessor path and goes through the
  // `_scpy_product_element` polyfill, which falls through to
  // `productElement` on a non-tuple receiver.
  val asAny: Any = t23
  asAny match
    case p: Product =>
      println(p.productArity)
      println(p.productElement(0))
      println(p.productElement(11))
      println(p.productElement(22))

  // Pattern destructure of a 23-tuple: triggers
  // `TupleXXL.unapplySeq` + `lengthCompare(23)` from the support
  // library, then 23 sequential `productElement` reads.
  t23 match
    case (a, b, c, d, e, f, g, h, i, j,
          k, l, m, n, o, p, q, r, s, u,
          v, w, x) =>
      println(a)
      println(m)
      println(x)

  // Static `Tuple`-typed binding — forces the lowered call site to
  // accept a TupleXXL instance flowing through `Tuples.size` /
  // `Tuples.apply` runtime helpers.
  val tup: Tuple = t23
  println(tup.size)
