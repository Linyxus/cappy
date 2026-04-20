import java.util.regex.Pattern

@main def javalibRegexNamedGroups(): Unit =
  val matcher = Pattern.compile("(?<word>a+)(?<sep>b)(?<tail>a+)").matcher("aaabaa")
  println("by-name:" + matcher.matches() + ":" + matcher.group("word") + ":" + matcher.group("tail"))
  println("start-end:" + matcher.start("sep") + ":" + matcher.end("tail"))
  println("backref:" + Pattern.matches("(?<x>ab)\\k<x>", "abab"))
