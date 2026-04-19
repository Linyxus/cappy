@main def javalibLangStringBuffer(): Unit =
  val appends = new java.lang.StringBuffer("scala")
  appends.append(':').append(true).append(':').append(7).append(':').append(Array[Char]('x', 'y'))
  println("appends:" + appends)

  val chained = new java.lang.StringBuffer()
  val chainResult = chained.append("a").append("b")
  println("chain:" + (chainResult eq chained) + ":" + chained)

  val deleted = new java.lang.StringBuffer("abcde")
  deleted.delete(1, 4)
  deleted.insert(1, 'Z')
  deleted.deleteCharAt(0)
  println("delete:" + deleted)

  val sub = new java.lang.StringBuffer("scala")
  println("substring:" + sub.substring(2) + ":" + sub.substring(1, 4))

  val fromSeq = new java.lang.StringBuffer(CharSequence.ofArray(Array[Char]('p', 'y')))
  fromSeq.appendCodePoint(0x1F600)
  println("tostring:" + fromSeq.toString())
