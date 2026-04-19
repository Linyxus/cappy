private def caught(body: => Any): String =
  try
    body
    "ok"
  catch
    case _: StringIndexOutOfBoundsException => "sioobe"
    case _: NullPointerException            => "npe"
    case _: IndexOutOfBoundsException       => "ioobe"

@main def javalibLangStringBuilder(): Unit =
  val empty = new java.lang.StringBuilder()
  println("empty:" + empty.length() + ":" + empty.capacity())

  println("append-str:" + new java.lang.StringBuilder().append("scala"))
  println("append-char:" + new java.lang.StringBuilder().append('Z'))
  println("append-int:" + new java.lang.StringBuilder().append(42))
  println("append-long:" + new java.lang.StringBuilder().append(1234567890123L))
  println("append-double:" + new java.lang.StringBuilder().append(2.25d))

  val codePoint = new java.lang.StringBuilder().appendCodePoint(0x1F600)
  println("append-codepoint:" + codePoint + ":" + codePoint.length())

  println("append-csq:" + new java.lang.StringBuilder().append(CharSequence.ofArray(Array[Char]('p', 'y'))))
  println("append-csq-range:" + new java.lang.StringBuilder().append("scalapy", 1, 5))
  println("append-array:" + new java.lang.StringBuilder().append(Array[Char]('a', 'b', 'c')))
  println("append-array-range:" + new java.lang.StringBuilder().append(Array[Char]('x', 'y', 'z', 'w'), 1, 2))

  val chained = new java.lang.StringBuilder()
  val chainResult = chained.append("a").append("b")
  println("chain:" + (chainResult eq chained) + ":" + chained)

  val inserted = new java.lang.StringBuilder("ab")
  inserted.insert(1, Array[Char]('p', 'x', 'y', 'q'), 1, 2)
  inserted.insert(0, "!")
  println("insert:" + inserted)

  val deleted = new java.lang.StringBuilder("abcdef")
  deleted.delete(1, 4)
  deleted.deleteCharAt(1)
  println("delete:" + deleted)

  println("replace:" + new java.lang.StringBuilder("abcdef").replace(2, 10, "XY"))

  val setChar = new java.lang.StringBuilder("scala")
  setChar.setCharAt(2, 'X')
  println("setcharat:" + setChar)

  val setLength = new java.lang.StringBuilder("go")
  setLength.setLength(4)
  val grown = "" + setLength.length() + ":" + setLength.charAt(2).toInt + ":" + setLength.charAt(3).toInt
  setLength.setLength(1)
  println("setlength:" + grown + ":" + setLength)

  val indexed = new java.lang.StringBuilder("banana")
  println("indexof:" + indexed.indexOf("na") + ":" + indexed.indexOf("na", 3))
  println("lastindexof:" + indexed.lastIndexOf("na") + ":" + indexed.lastIndexOf("na", 3))
  val reversed = new java.lang.StringBuilder("a😀b").reverse()
  println("reverse:" + reversed.length() + ":" + reversed.charAt(0).toInt + ":" + reversed.charAt(1).toInt + ":" + reversed.charAt(2).toInt + ":" + reversed.charAt(3).toInt)

  val cs: CharSequence = new java.lang.StringBuilder("scala")
  println("charsequence:" + cs.length() + ":" + cs.charAt(2).toInt + ":" + cs.subSequence(1, 4))

  val sub = new java.lang.StringBuilder("scala")
  println("substring:" + sub.substring(2) + ":" + sub.substring(1, 4))

  val dst = Array[Char]('_', '_', '_', '_', '_')
  new java.lang.StringBuilder("scala").getChars(1, 4, dst, 1)
  println("getchars:" + new String(dst))

  val capacity = new java.lang.StringBuilder()
  val initialCapacity = capacity.capacity()
  capacity.ensureCapacity(40)
  val ensuredCapacity = capacity.capacity()
  capacity.append("xy")
  capacity.trimToSize()
  println("capacity:" + initialCapacity + ":" + (ensuredCapacity >= 40) + ":" + capacity.capacity())

  // Bounds-check coverage: every path that Java contracts to throw
  // SIOOBE / IOOBE. Each line pins the exception family we surface.
  println("bounds-substring-hi:" + caught(new java.lang.StringBuilder("abc").substring(4)))
  println("bounds-substring-lo:" + caught(new java.lang.StringBuilder("abc").substring(-1)))
  println("bounds-substring-range:" + caught(new java.lang.StringBuilder("abc").substring(2, 1)))
  println("bounds-charat-hi:" + caught(new java.lang.StringBuilder("abc").charAt(3)))
  println("bounds-charat-neg:" + caught(new java.lang.StringBuilder("abc").charAt(-1)))
  println("bounds-setcharat:" + caught { val sb = new java.lang.StringBuilder("abc"); sb.setCharAt(5, 'X'); sb.toString() })
  println("bounds-setlength-neg:" + caught { val sb = new java.lang.StringBuilder("abc"); sb.setLength(-1); sb.toString() })
  println("bounds-insert-neg:" + caught(new java.lang.StringBuilder("abc").insert(-1, "X")))
  println("bounds-insert-hi:" + caught(new java.lang.StringBuilder("abc").insert(5, "X")))
  println("bounds-getchars-src:" + caught {
    val dst = new Array[Char](3)
    new java.lang.StringBuilder("abc").getChars(0, 5, dst, 0)
    dst
  })
  println("bounds-getchars-dst:" + caught {
    val dst = new Array[Char](2)
    new java.lang.StringBuilder("abc").getChars(0, 3, dst, 1)
    dst
  })
  println("bounds-replace-neg:" + caught(new java.lang.StringBuilder("abc").replace(-1, 2, "X")))
  println("bounds-append-csq-range:" + caught(new java.lang.StringBuilder().append("ab": CharSequence, 0, 10)))

  // Null-handling coverage. Java contract distinguishes:
  //   - `new StringBuilder(null: String)` / `CharSequence` → NPE
  //   - `replace(_, _, null: String)`                     → NPE
  //   - `append(null: AnyRef|String|CharSequence)`        → appends "null"
  // We capture both ends.
  println("null-ctor-str:" + caught(new java.lang.StringBuilder(null.asInstanceOf[String])))
  println("null-ctor-csq:" + caught(new java.lang.StringBuilder(null.asInstanceOf[CharSequence])))
  println("null-replace:" + caught(new java.lang.StringBuilder("abc").replace(0, 1, null)))
  println("null-append-str:" + new java.lang.StringBuilder("[").append(null.asInstanceOf[String]).append("]"))
  println("null-append-csq:" + new java.lang.StringBuilder("[").append(null.asInstanceOf[CharSequence]).append("]"))
  println("null-append-obj:" + new java.lang.StringBuilder("[").append(null.asInstanceOf[AnyRef]).append("]"))
