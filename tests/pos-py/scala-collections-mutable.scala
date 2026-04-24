import scala.collection.mutable

@main def scalaCollectionsMutable(): Unit =
  val arrayBuffer = mutable.ArrayBuffer(1, 2, 3)
  arrayBuffer += 4
  val removedArrayBuffer = arrayBuffer.remove(1)
  arrayBuffer(0) = 9
  val removedFirstArrayBuffer = arrayBuffer.remove(0)
  println("arraybuffer:" + removedArrayBuffer + ":" + removedFirstArrayBuffer + ":" + arrayBuffer.length + ":" + arrayBuffer.isEmpty)

  val listBuffer = mutable.ListBuffer.empty[String]
  listBuffer += "a"
  listBuffer ++= List("b", "c")
  val removedListBuffer = listBuffer.remove(1)
  listBuffer.prepend("z")
  println("listbuffer:" + removedListBuffer + ":" + listBuffer.length + ":" + listBuffer.isEmpty)

  val queue = mutable.Queue.empty[Int]
  queue.enqueue(1)
  queue.enqueue(2, 3)
  val dequeued = queue.dequeue()
  queue.enqueue(4)
  println("mqueue:" + dequeued + ":" + queue.front + ":" + queue.last + ":" + queue.size + ":" + queue.isEmpty)

  val stack = mutable.Stack.empty[Int]
  stack.push(1)
  stack.push(2)
  val top = stack.top
  val popped = stack.pop()
  println("stack:" + top + ":" + popped + ":" + stack.top + ":" + stack.size + ":" + stack.isEmpty)
