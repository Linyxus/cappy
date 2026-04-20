import java.util.ArrayDeque

private def join(deque: ArrayDeque[Int]): String =
  val iter = deque.iterator()
  var first = true
  var result = ""
  while iter.hasNext() do
    if !first then
      result += ","
    result += String.valueOf(iter.next())
    first = false
  result

@main def javalibUtilArrayDeque(): Unit =
  val deque = new ArrayDeque[Int]()
  var i = 0
  while i < 12 do
    deque.addLast(i)
    i += 1

  var removed = 0
  while removed < 5 do
    deque.removeFirst()
    removed += 1

  var next = 12
  while next < 18 do
    deque.addLast(next)
    next += 1

  println("wrap:" + join(deque))
  deque.push(99)
  val popped = deque.pop()
  val tail = deque.removeLast()
  println("push-pop:" + popped + ":" + tail + ":" + deque.size())
  println("peek:" + deque.peekFirst() + ":" + deque.peekLast())
