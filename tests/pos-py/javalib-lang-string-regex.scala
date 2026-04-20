private def joinStrings(xs: Array[String]): String =
  var out = ""
  var i = 0
  while i < xs.length do
    if i != 0 then
      out += "|"
    out += xs(i)
    i += 1
  out

@main def javalibLangStringRegex(): Unit =
  println("matches:" + "abc".matches("a.*"))
  println("split1:" + joinStrings("a,b,c".split(",")))
  println("split2:" + joinStrings("a,b,c".split(",", 2)))
  println("replaceAll:" + "aba".replaceAll("a", "x"))
  println("replaceFirst:" + "aba".replaceFirst("a", "x"))
