import java.util.{LinkedHashMap, Map}

private def joinKeys(map: Map[String, String]): String =
  val iter = map.keySet().iterator()
  var first = true
  var result = ""
  while iter.hasNext() do
    if !first then
      result += ","
    result += iter.next()
    first = false
  result

final class EvictingMap(limit: Int) extends LinkedHashMap[String, String]:
  override protected def removeEldestEntry(eldest: Map.Entry[String, String]): Boolean =
    size() > limit

@main def javalibUtilLinkedHashMap(): Unit =
  val insertion = new LinkedHashMap[String, String]()
  insertion.put("a", "alpha")
  insertion.put("b", "beta")
  insertion.put("c", "gamma")
  println("insertion:" + joinKeys(insertion))

  val access = new LinkedHashMap[String, String](16, 0.75f, true)
  access.put("a", "alpha")
  access.put("b", "beta")
  access.put("c", "gamma")
  access.get("a")
  access.get("b")
  println("access:" + joinKeys(access))

  val evicting = new EvictingMap(2)
  evicting.put("a", "alpha")
  evicting.put("b", "beta")
  evicting.put("c", "gamma")
  evicting.put("d", "delta")
  println("eldest:" + joinKeys(evicting))
