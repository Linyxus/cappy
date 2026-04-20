import java.util.ArrayList
import java.util.List

private def join(list: List[Int]): String =
  val iter = list.iterator()
  var first = true
  var result = ""
  while iter.hasNext() do
    if !first then
      result += ","
    result += String.valueOf(iter.next())
    first = false
  result

@main def javalibUtilArrayList(): Unit =
  val list = new ArrayList[Int]()
  list.add(1)
  list.add(3)
  list.add(1, 2)
  list.set(0, 0)
  val removed = list.remove(2)
  println("crud:" + removed + ":" + join(list))

  val grown = new ArrayList[Int](1)
  var i = 0
  while i < 8 do
    grown.add(i)
    i += 1
  println("grow:" + grown.size() + ":" + grown.get(7))

  val iter = grown.iterator()
  var sum = 0
  while iter.hasNext() do
    sum += iter.next()
  println("iterate:" + sum)

  val sub = grown.subList(2, 5)
  sub.remove(1)
  sub.add(9)
  println("subList:" + join(grown))
