import java.lang.StringBuilder
import java.util.regex.Pattern

@main def javalibRegexMatcher(): Unit =
  val findMatcher = Pattern.compile("(a)(b+)").matcher("xxabbbzzab")
  val firstFind = findMatcher.find()
  val firstGroup = findMatcher.group()
  val secondFind = findMatcher.find()
  val secondGroup = findMatcher.group()
  println("find:" + firstFind + ":" + firstGroup + ":" + secondFind + ":" + secondGroup)

  val matchesMatcher = Pattern.compile("(a)(b+)").matcher("abbb")
  println("matches:" + matchesMatcher.matches() + ":" + matchesMatcher.group(1) + ":" + matchesMatcher.group(2))

  val posMatcher = Pattern.compile("(ab)(c)?").matcher("zabc")
  posMatcher.find()
  println("start-end:" + posMatcher.start() + ":" + posMatcher.end() + ":" + posMatcher.start(1) + ":" + posMatcher.end(1) + ":" + posMatcher.start(2) + ":" + posMatcher.end(2))

  val resetMatcher = Pattern.compile("a").matcher("ba")
  resetMatcher.find()
  resetMatcher.reset("aa")
  resetMatcher.find()
  println("reset:" + resetMatcher.start() + ":" + resetMatcher.end())

  val regionMatcher = Pattern.compile("a+").matcher("xxaaay")
  regionMatcher.region(2, 5)
  println("region:" + regionMatcher.find() + ":" + regionMatcher.start() + ":" + regionMatcher.end())

  val lookingAtMatcher = Pattern.compile("ab").matcher("abzz")
  val laterMatcher = Pattern.compile("ab").matcher("zzab")
  println("lookingAt:" + lookingAtMatcher.lookingAt() + ":" + laterMatcher.lookingAt())

  val replaceMatcher = Pattern.compile("(a)(b)").matcher("abxab")
  val sb = new StringBuilder()
  while replaceMatcher.find() do
    replaceMatcher.appendReplacement(sb, "$2$1")
  replaceMatcher.appendTail(sb)
  println("appendReplacement:" + sb.toString())

  println("replaceAll:" + Pattern.compile("a").matcher("aba").replaceAll("x"))
  println("replaceFirst:" + Pattern.compile("a").matcher("aba").replaceFirst("x"))
