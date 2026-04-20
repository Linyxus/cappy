import java.util.{HashSet, LinkedHashSet}

private def joinSet[E](set: java.util.Set[E]): String =
  val iter = set.iterator()
  var first = true
  var result = ""
  while iter.hasNext() do
    if !first then
      result += ","
    result += String.valueOf(iter.next())
    first = false
  result

@main def javalibUtilHashSet(): Unit =
  val set = new HashSet[String]()
  set.add("four")
  set.add("one")
  set.add("four")
  set.add("three")

  val iter = set.iterator()
  var sum = 0
  while iter.hasNext() do
    sum += iter.next().length()

  println("hashset:" + set.size() + ":" + set.contains("one") + ":" + set.contains("nine") + ":" + sum)
  println("remove:" + set.remove("one") + ":" + set.remove("one") + ":" + set.size())

  val linked = new LinkedHashSet[String]()
  linked.add("b")
  linked.add("a")
  linked.add("c")
  linked.add("a")
  println("linked:" + joinSet(linked))
