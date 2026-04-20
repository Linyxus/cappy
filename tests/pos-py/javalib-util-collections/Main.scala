import java.util.{ArrayList, Collections, Random}

private def showList[A](list: java.util.List[A]): String =
  var out = ""
  var i = 0
  while i < list.size() do
    if i != 0 then out += ","
    out += String.valueOf(list.get(i))
    i += 1
  out

private def captureUoe(tag: String)(body: => Any): Unit =
  try
    body
    println(tag + ":no-throw")
  catch
    case _: UnsupportedOperationException =>
      println(tag + ":uoe")

@main def javalibUtilCollections(): Unit =
  val words = new ArrayList[String]()
  words.add("c")
  words.add("a")
  words.add("b")

  Collections.sort(words)
  println("sort:" + showList(words))

  Collections.reverse(words)
  println("reverse:" + showList(words))

  Collections.shuffle(words, new Random(7L))
  println("shuffle:" + showList(words))

  println("minmax:" + Collections.min(words) + ":" + Collections.max(words))

  captureUoe("unmodifiable") {
    Collections.unmodifiableList(words).add("z")
  }

  println("sync:" + Collections.synchronizedList(words).get(0))

  println(
    "emptyviews:" +
      Collections.emptyList[String]().size() + ":" +
      Collections.emptySet[String]().size() + ":" +
      Collections.emptyMap[String, String]().size()
  )

  val singletonMap = Collections.singletonMap("k", "v")
  println(
    "singletons:" +
      Collections.singleton("x").contains("x") + ":" +
      Collections.singletonList("y").get(0) + ":" +
      singletonMap.get("k")
  )

  val enumeration = Collections.enumeration(Collections.singletonList("enum"))
  println("enumeration:" + enumeration.nextElement())

  val listed = Collections.list(Collections.enumeration(Collections.singletonList("list")))
  println("list:" + listed.get(0))
