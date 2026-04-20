import java.util.{Comparator, PriorityQueue}

private object AscComparator extends Comparator[Int]:
  def compare(left: Int, right: Int): Int =
    if left < right then -1
    else if left > right then 1
    else 0

private def drain(queue: PriorityQueue[Int]): String =
  var first = true
  var result = ""
  while !queue.isEmpty() do
    val value = queue.poll()
    if !first then
      result += ","
    result += String.valueOf(value)
    first = false
  result

private object ReverseComparator extends Comparator[Int]:
  def compare(left: Int, right: Int): Int =
    if left < right then 1
    else if left > right then -1
    else 0

@main def javalibUtilPriorityQueue(): Unit =
  val queue = new PriorityQueue[Int](AscComparator)
  queue.add(4)
  queue.add(1)
  queue.add(3)
  queue.add(2)

  val iter = queue.iterator()
  var sum = 0
  while iter.hasNext() do
    sum += iter.next()

  println("peek-size:" + queue.peek() + ":" + queue.size() + ":" + sum)
  println("drain:" + drain(queue))

  val reverse = new PriorityQueue[Int](ReverseComparator)
  reverse.add(4)
  reverse.add(1)
  reverse.add(3)
  reverse.add(2)
  println("reverse:" + drain(reverse))
