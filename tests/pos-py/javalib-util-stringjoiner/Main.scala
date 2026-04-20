import java.util.StringJoiner

@main def javalibUtilStringJoiner(): Unit =
  val joiner = new StringJoiner(", ", "[", "]")
  joiner.add("a").add("b")
  val merged = new StringJoiner(", ")
  merged.add("c").add("d")
  joiner.merge(merged)
  println("join:" + joiner.toString())
  println("length:" + joiner.length())
