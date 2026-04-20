import java.util.StringTokenizer

@main def javalibUtilStringTokenizer(): Unit =
  val tokenizer = new StringTokenizer("a,b,,c", ",", true)
  var result = ""
  var first = true
  while tokenizer.hasMoreTokens() do
    if first then first = false
    else result += "|"
    result += tokenizer.nextToken()
  println("tokens:" + result)
  println("count:" + new StringTokenizer("a b c").countTokens())
