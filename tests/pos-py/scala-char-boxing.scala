// Regression test: Char widened to Any/Object must render as the
// character glyph, not the numeric codepoint. Boxed Char operations
// (List[Char].head/last/mkString, Seq[Char].foreach, val: Any = 'x')
// previously printed "97" instead of "a" because the runtime kept
// boxed Chars as bare Python ints. The fix introduces a `_scpy_Char`
// wrapper class with a `toString__Ljava_lang_String` hook so the
// `_scpy_to_str` path renders it correctly while still behaving as an
// int for primitive Char arithmetic (the wrapper subclasses int).

@main def scalaCharBoxing(): Unit =
  // Static Char positions still work — sanity baseline.
  val ch: Char = 'A'
  println("static:" + ch)
  println("static-tostring:" + ch.toString)

  // val any: Any = 'x' goes through Char$.box then `_scpy_to_str`.
  val anyChar: Any = 'x'
  println("any-direct:" + anyChar)
  println("any-tostring:" + anyChar.toString)

  // List[Char] / Seq[Char] return boxed `Object` from head/last/apply.
  val xs: List[Char] = List('a', 'b', 'c')
  println("list-head:" + xs.head)
  println("list-last:" + xs.last)
  println("list-mkstring:" + xs.mkString)
  println("list-mkstring-sep:" + xs.mkString(","))
  println("list-apply:" + xs(1))

  // String.toList — same boxed-Char Seq path.
  val cs = "abc".toList
  println("string-tolist-head:" + cs.head)
  println("string-tolist-mkstring:" + cs.mkString)

  // foreach over Seq[Char] hits the boxed-iter path — each emitted
  // value flows through Predef.println(Any: Any). Without the wrapper
  // we would print 104 / 105.
  print("foreach-seq:")
  Seq('h', 'i').foreach(c => print(c))
  println()

  // Vector[Char].mkString should also work.
  println("vector-mkstring:" + Vector('p', 'q', 'r').mkString)

  // Equality semantics: boxed Character does NOT equal boxed Integer
  // in Java; our wrapper preserves that. Still: two boxed Chars with
  // the same codepoint are equal.
  val a1: Any = 'a'
  val a2: Any = 'a'
  println("any-eq:" + (a1 == a2))

  // mkString concatenation uses Java's String + Object machinery; the
  // result must be the rendered chars, not "979899".
  val sb = new StringBuilder()
  xs.foreach(sb.append)
  println("sb-result:" + sb.toString)
