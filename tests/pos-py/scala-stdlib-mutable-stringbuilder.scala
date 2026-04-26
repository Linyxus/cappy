// Bug-finding mutable.StringBuilder coverage beyond the build/access/transform/mutate bench.
// 10-12 labelled println lines exercising distinct codegen paths.
//
// Skipped ops:
//   - setCharAt (notes/issue-stringbuilder-setcharat-dropped.md)

import scala.collection.mutable.StringBuilder

@main def scalaStdlibMutableStringBuilder(): Unit =
  // 1. Construction variants
  val a = new StringBuilder("hello")
  val b = new StringBuilder()
  val c = new StringBuilder(16)
  println("ctor:" + a.length + ":" + b.length + ":" + c.length)

  // 2. Accessors — charAt + boxed-via-Any reads (.head/.last) all render the
  //     character glyph now that Char auto-boxes through `_scpy_Char`.
  println("access:" + a.charAt(0) + ":" + a.charAt(a.length - 1) + ":" + a.head + ":" + a.last + ":" + a.size + ":" + a.isEmpty + ":" + a.nonEmpty)

  // 3. Append overloads (String, Char, Int, Boolean)
  val app = new StringBuilder("x")
  app.append("yz")
  app.append(' ')
  app.append(42)
  app.append(true)
  println("append:" + app.toString + ":" + app.length)

  // 4. insert + deleteCharAt
  val ins = new StringBuilder("bcde")
  ins.insert(0, "X")
  ins.deleteCharAt(1)
  println("insert-delete:" + ins.toString + ":" + ins.length)

  // 5. delete range
  val del = new StringBuilder("abcdef")
  del.delete(0, 2)
  println("delete:" + del.toString + ":" + del.length)

  // 6. replace range
  val rep = new StringBuilder("abcdef")
  rep.replace(0, 2, "XY")
  println("replace:" + rep.toString + ":" + rep.length)

  // 7. reverse — returns a fresh reversed StringBuilder (Seq-style), not in-place.
  val rev = new StringBuilder("abcdef")
  val revOut = rev.reverse
  println("reverse:" + revOut.toString + ":" + rev.toString)

  // 8. clear
  val cl = new StringBuilder("nonempty")
  cl.clear()
  println("clear:" + cl.length + ":" + cl.isEmpty)

  // 9. indexOf overloads + contains-as-Seq[Char]
  //    contains(String) on mutable.StringBuilder is Seq[Char].contains, not substring search.
  val s = new StringBuilder("hello world")
  println("search:" + s.indexOf("ll") + ":" + s.indexOf('o') + ":" + s.contains('e'))

  // 10. substring + subSequence (latter via .toString to keep Char path off Any)
  val ss = new StringBuilder("abcdefgh")
  println("substring:" + ss.substring(1, 4) + ":" + ss.subSequence(1, 4).toString)

  // 11. Conversions: toString, toArray render, toList.mkString — all of these
  //     run Chars through boxed `Any` (via the iterator boundary). The
  //     `_scpy_Char` wrapper keeps them rendering as glyphs.
  val cv = new StringBuilder("abc")
  println("convert:" + cv.toString + ":" + cv.toArray.length + ":" + cv.toList.mkString)

  // 12. Chained appends — fluent return value preserves identity.
  val ch = new StringBuilder
  val ret = ch.append("a").append("b").append("c")
  println("chain:" + ret.toString + ":" + (ret eq ch))
