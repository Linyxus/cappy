@main def javalibLangCharacter(): Unit =
  val cp = 0x1F600
  println("type:" + java.lang.Character.TYPE)
  println("ascii:" + java.lang.Character.isDigit('7') + ":" + java.lang.Character.isLetter('A') + ":" + java.lang.Character.isWhitespace(' '))
  println("unicode:" + java.lang.Character.toUpperCase('é') + ":" + java.lang.Character.toLowerCase('Ω') + ":" + java.lang.Character.getType('Ж'))
  println("supplementary:" + java.lang.Character.isSupplementaryCodePoint(cp) + ":" + java.lang.Character.charCount(cp) + ":" + java.lang.Character.toString(cp))
  println("digit:" + java.lang.Character.digit('f', 16) + ":" + java.lang.Character.digit('９', 10) + ":" + java.lang.Character.forDigit(15, 16))
