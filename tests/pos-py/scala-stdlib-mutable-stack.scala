// Coverage for `mutable.Stack`. Stack is LIFO: `push` prepends, `pop`
// removes-and-returns the head, `top` peeks. `pop()` mutates in place AND
// returns; verify pre/post state. Each printed line is a labelled,
// derived value (sizes, elements) — never a raw `toString`.

import scala.collection.mutable.Stack

@main def scalaStdlibMutableStack(): Unit =
  val size = 16
  val s = Stack(1, 2, 3, 4, 5, 6, 7, 8)

  // build:
  // Stack(1, 2, 3, ...) puts 1 at the top (head).
  println("build:" + s.size + ":" + s.head + ":" + s.last + ":" + s.top)
  val empty = Stack.empty[Int]
  println("empty:" + empty.size + ":" + empty.isEmpty)
  val fromRange = Stack.from(0 until size)
  println("from:" + fromRange.size + ":" + fromRange.top)

  // access:
  println("access:" + s.head + ":" + s.last + ":" + s.top)
  println("hOpt:" + s.headOption.getOrElse(-1) + ":" + s.lastOption.getOrElse(-1))
  println("emptyChk:" + s.isEmpty + ":" + s.nonEmpty)

  // iter order: head-first (LIFO order from top to bottom)
  println("order:" + Stack(1, 2, 3, 4, 5).mkString(","))

  // transform:
  val mapped = s.map(_ + 1)
  println("map:" + mapped.size + ":" + mapped.head + ":" + mapped.last)
  val filtered = s.filter(_ % 2 == 0)
  println("filter:" + filtered.size + ":" + filtered.head + ":" + filtered.last)

  // aggregate:
  val small = Stack(1, 2, 3, 4)
  println("folds:" + small.sum + ":" + small.product + ":" + small.foldLeft(10)(_ + _))
  println("reduce:" + s.reduce(_ + _) + ":" + s.min + ":" + s.max)
  println("preds:" + s.contains(5) + ":" + s.indexOf(5) + ":" + s.exists(_ > 7) + ":" + s.forall(_ > 0))
  println("findCount:" + s.find(_ > 5).getOrElse(-1) + ":" + s.count(_ > 5))

  // slicing:
  println("slice:" + s.take(3).size + ":" + s.drop(3).size + ":" + s.slice(2, 5).size)

  // mutate: push / pop / pre/post state
  val m = Stack(10, 20, 30)
  println("preMut:" + m.size + ":" + m.top)
  m.push(40)
  println("push:" + m.size + ":" + m.top)
  val popped = m.pop()
  println("pop:" + popped + ":" + m.size + ":" + m.top)
  m.push(50, 60)
  println("pushMulti:" + m.size + ":" + m.top)

  // pushAll
  val pa = Stack(1, 2)
  pa.pushAll(List(3, 4, 5))
  println("pushAll:" + pa.size + ":" + pa.top)

  // clone independence
  val orig = Stack(1, 2, 3)
  val cloned = orig.clone()
  cloned.push(99)
  println("clone:" + orig.size + ":" + cloned.size + ":" + cloned.top)

  // clear
  val toClear = Stack(1, 2, 3)
  toClear.clear()
  println("clear:" + toClear.isEmpty + ":" + toClear.size)

  // convert:
  println("convList:" + s.toList.length + ":" + s.toList.head + ":" + s.toList.last)
  println("convVec:" + s.toVector.size + ":" + s.toVector.head + ":" + s.toVector.last)
  println("convSet:" + s.toSet.size)
  println("mkString:" + Stack(1, 2, 3).mkString(","))
