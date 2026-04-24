import scala.collection.immutable.Queue

@main def scalaCollectionsImmutable(): Unit =
  val list = List(1, 2, 2, 3)
  println("list:" + list.head + ":" + list.tail.head + ":" + list.size + ":" + (list.tail.head == list.tail.tail.head))
  println("list-empty:" + List.empty[Int].isEmpty + ":" + List(5).tail.isEmpty)

  val vector = Vector.empty[String]
  println("vector:" + vector.isEmpty + ":" + vector.size)

  val range = Range.inclusive(1, 5)
  val stepped = Range(0, 10, 3)
  println("range:" + range.head + ":" + range.last + ":" + range.size + ":" + range.contains(5) + ":" + range.contains(6) + ":" + stepped.head + ":" + stepped.last + ":" + stepped.size)

  val queue = Queue.empty[Int].enqueue(1).enqueue(2)
  val queue2 = queue.enqueue(3)
  val (first, queue3) = queue2.dequeue
  println("queue:" + first + ":" + queue3.front + ":" + queue3.size + ":" + queue.size + ":" + Queue.empty[Int].isEmpty)

  val map = Map("b" -> 2, "a" -> 1).updated("c", 3)
  val missing = map.get("z").getOrElse(-1)
  println("imap:" + map("a") + ":" + map("b") + ":" + map("c") + ":" + map.contains("a") + ":" + missing + ":" + map.size + ":" + Map.empty[String, Int].isEmpty)

  val set = Set(3, 1, 2, 2)
  val set2 = set + 4 - 1
  println("iset:" + set.size + ":" + set2.size + ":" + set.contains(1) + ":" + set2.contains(4) + ":" + set2.contains(1) + ":" + Set.empty[Int].contains(1))
  println("string:" + list.toString + ":" + ("prefix:" + vector))
