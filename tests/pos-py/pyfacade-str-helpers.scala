/*
 * Direct coverage of the `_scpy_str_*` runtime helpers added for L2.1.
 * Each scenario isolates a helper's edge case so a regression pinpoints
 * the helper, not the method that happens to call it.
 */
private def caughtIae(body: () => Unit): String =
  try
    body()
    "ok"
  catch case _: IllegalArgumentException => "iae"

private def caughtSioobe(body: () => Unit): String =
  try
    body()
    "ok"
  catch case _: StringIndexOutOfBoundsException => "sioobe"

@main def pyfacadeStrHelpers(): Unit =
  // stripIndent — the regression fence. Trailing \n, non-trailing \n,
  // and all-blank input must all produce Java-faithful output.
  println("stripindent-nl:[" + "  a\n    b\n".stripIndent().replace('\n', '|') + "]")
  println("stripindent-nonl:[" + "  a\n    b".stripIndent().replace('\n', '|') + "]")
  println("stripindent-blank:[" + "   \n   \n".stripIndent().replace('\n', '|') + "]")
  println("stripindent-mixed:[" + "  a\n\n  b\n".stripIndent().replace('\n', '|') + "]")

  // indent — positive pads each line; negative strips up to |n| leading
  // whitespace chars per line.
  println("indent-pos:[" + "a\nb".indent(2).replace('\n', '|') + "]")
  println("indent-neg:[" + "    a\n  b".indent(-2).replace('\n', '|') + "]")

  // repeat — 0 returns empty; negative raises IllegalArgumentException.
  println("repeat-zero:[" + "ab".repeat(0) + "]")
  println("repeat-one:[" + "ab".repeat(1) + "]")
  println("repeat-neg:" + caughtIae(() => { "ab".repeat(-3); () }))

  // hashCode — Java-contract Σ ch*31^(n-1-k). Empty → 0; specific ASCII
  // cases must reproduce JVM values bit-for-bit.
  println("hash-empty:" + "".hashCode())
  println("hash-a:" + "a".hashCode())       // Java: 97
  println("hash-abc:" + "abc".hashCode())   // Java: 96354
  println("hash-hello:" + "hello".hashCode()) // Java: 99162322

  // charAt / codePointAt — bounds checks throw
  // StringIndexOutOfBoundsException, not a bare Python IndexError.
  println("charat-oob:" + caughtSioobe(() => { "ab".charAt(5); () }))
  println("codepoint-oob:" + caughtSioobe(() => { "ab".codePointAt(5); () }))
  println("codepoint-before-zero:" + caughtSioobe(() => { "ab".codePointBefore(0); () }))

  // substring — bounds, empty slice, full string.
  println("substring-empty:[" + "abc".substring(1, 1) + "]")
  println("substring-full:[" + "abc".substring(0, 3) + "]")
  println("substring-oob:" + caughtSioobe(() => { "abc".substring(0, 5); () }))

  // indexOf / lastIndexOf corner cases
  println("index-empty:" + "abc".indexOf(""))                    // Java: 0
  println("index-empty-from:" + "abc".indexOf("", 5))            // Java: 3
  println("last-index-empty:" + "abc".lastIndexOf(""))           // Java: 3
  println("last-index-empty-from-neg:" + "abc".lastIndexOf("", -1)) // Java: -1

  // startsWith with offset clamping — negative or beyond-end returns
  // false, doesn't throw.
  println("starts-neg-offset:" + "abc".startsWith("a", -1))
  println("starts-beyond:" + "abc".startsWith("", 10))

  // trim (≤ U+0020 only) vs strip (Character.isWhitespace, which
  // excludes NBSP). NBSP = \u00A0.
  println("trim-nbsp:[" + "\u00A0x\u00A0".trim() + "]")    // trim strips: no (U+00A0 > U+0020) → "\u00A0x\u00A0"
  println("strip-nbsp:[" + "\u00A0x\u00A0".strip() + "]")  // strip (Java) keeps NBSP → "\u00A0x\u00A0"

  // translateEscapes — covers \n, octal, \u, line continuation.
  println("escapes-n:[" + "a\\nb".translateEscapes().replace('\n', '|') + "]")
  println("escapes-octal:[" + "\\141".translateEscapes() + "]")              // \141 = 'a'
  println("escapes-u:[" + "\\u0041".translateEscapes() + "]")                // \u0041 = 'A'
  println("escapes-line-cont:[" + "a\\\nb".translateEscapes() + "]")         // \<LF> → nothing
