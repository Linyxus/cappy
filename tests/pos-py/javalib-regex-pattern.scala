import java.util.regex.Pattern

private def joinStrings(xs: Array[String]): String =
  var out = ""
  var i = 0
  while i < xs.length do
    if i != 0 then
      out += "|"
    out += xs(i)
    i += 1
  out

@main def javalibRegexPattern(): Unit =
  println("match:" + Pattern.matches("a.*", "abc"))
  println("flags:" + Pattern.compile("(?i-x)ab", Pattern.DOTALL | Pattern.COMMENTS).flags())
  println("flags-bridge:" + Pattern.compile("ä", Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE).matcher("Ä").matches())
  println("split:" + joinStrings(Pattern.compile(",").split("a,b,,c")))
  println("split-limit:" + joinStrings(Pattern.compile(",").split("a,b,c", 2)))
  println("quote:" + Pattern.matches(Pattern.quote("a+b"), "a+b"))
  println("quote-inline:" + Pattern.matches("\\Q[a-z]+\\E", "[a-z]+"))
