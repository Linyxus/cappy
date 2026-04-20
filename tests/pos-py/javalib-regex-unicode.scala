import java.util.regex.Pattern

@main def javalibRegexUnicode(): Unit =
  println("posix:" + Pattern.matches("\\p{Alpha}+", "Cafe"))
  println("unicode-prop:" + Pattern.matches("\\p{Lu}+", "ÄÖ"))
  println("case-insensitive-unicode:" + Pattern.compile("ä", Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE).matcher("Ä").matches())
  println("java-whitespace:" + Pattern.matches("\\p{javaWhitespace}+", " \u1680"))
