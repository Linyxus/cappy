// Bug-finding ListBuffer coverage beyond the build/access/transform/mutate bench.
// 10-12 labelled println lines exercising distinct codegen paths.

import scala.collection.mutable.ListBuffer

@main def scalaStdlibListBuffer(): Unit =
  // 1. Construction variants (apply, empty, fill, tabulate, from)
  val a = ListBuffer(1, 2, 3, 6, 7, 8)
  val b = ListBuffer.empty[Int]
  val c = ListBuffer.fill(3)(7)
  val d = ListBuffer.tabulate(5)(_ * 2)
  val e = ListBuffer.from(Vector(10, 20, 30))
  println("ctor:" + a.size + ":" + b.size + ":" + c.size + ":" + d.size + ":" + e.size)

  // 2. Accessors
  println("access:" + a.head + ":" + a.last + ":" + a.tail.size + ":" + a.init.size + ":" + a.isEmpty + ":" + a.nonEmpty)

  // 3. Search predicates
  println("search:" + a.contains(2) + ":" + a.indexOf(7) + ":" + a.exists(_ > 5) + ":" + a.forall(_ > 0) + ":" + a.count(_ > 5))

  // 4. find / getOrElse on Option
  val found = a.find(_ > 5).getOrElse(-1)
  println("find:" + found)

  // 5. Slicing
  println("slice:" + a.take(2).size + ":" + a.drop(2).size + ":" + a.slice(1, 3).size + ":" + a.takeWhile(_ < 5).size + ":" + a.dropWhile(_ < 5).size)

  // 6. partition (returns tuple)
  val (lo, hi) = a.partition(_ < 5)
  println("partition:" + lo.size + ":" + hi.size)

  // 7. Higher-order ops: filter / flatMap / collect
  val collected = a.collect { case i if i > 5 => i * 10 }
  println("hof:" + a.filter(_ > 5).size + ":" + a.flatMap(i => List(i, i * 2)).size + ":" + collected.size + ":" + collected.head)

  // 8. Folds and reductions
  println("fold:" + a.sum + ":" + a.foldLeft(0)(_ + _) + ":" + a.reduce(_ + _) + ":" + a.min + ":" + a.max)

  // 9. Mutation: prepend, append, insert, remove
  val m = ListBuffer(1, 2, 3)
  m.prepend(0)
  m.append(99)
  m.insert(0, -1)
  val removed = m.remove(0)
  println("mutate:" + m.head + ":" + m.last + ":" + m.size + ":" + removed)

  // 10. ++= and clone
  val m2 = ListBuffer(1, 2, 3)
  m2 ++= List(4, 5)
  val cloned = m2.clone
  println("concat-clone:" + m2.size + ":" + cloned.size + ":" + (m2 eq cloned))

  // 11. Sorting and reverse
  val u = ListBuffer(3, 1, 4, 1, 5, 9, 2, 6)
  println("sort:" + u.sorted.head + ":" + u.sortWith(_ > _).head + ":" + u.reverse.head)

  // 12. Conversion + clear
  val toList = a.toList
  val toVec = a.toVector
  val toSet = a.toSet
  val joined = a.mkString(",")
  val toClear = ListBuffer(1, 2, 3)
  toClear.clear()
  println("convert:" + toList.size + ":" + toVec.size + ":" + toSet.size + ":" + joined + ":" + toClear.size)
