import java.util.LinkedList

private def join(list: LinkedList[Int]): String =
  val iter = list.iterator()
  var first = true
  var result = ""
  while iter.hasNext() do
    if !first then
      result += ","
    result += String.valueOf(iter.next())
    first = false
  result

@main def javalibUtilLinkedList(): Unit =
  val deque = new LinkedList[Int]()
  deque.addFirst(2)
  deque.addLast(3)
  deque.offerFirst(1)
  println("deque-ops:" + deque.removeFirst() + ":" + deque.removeLast() + ":" + deque.peek())

  val list = new LinkedList[Int]()
  list.add(1)
  list.add(3)
  val it = list.listIterator(1)
  it.add(2)
  it.next()
  it.set(4)
  println("listiterator:" + join(list))

  val removing = new LinkedList[Int]()
  removing.add(1)
  removing.add(2)
  removing.add(3)
  removing.add(4)
  val removeIter = removing.iterator()
  while removeIter.hasNext() do
    if removeIter.next() % 2 == 0 then
      removeIter.remove()
  println("remove-while-iterating:" + join(removing))
