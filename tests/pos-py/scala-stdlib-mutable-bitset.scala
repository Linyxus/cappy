// Coverage for scala.collection.mutable.BitSet — sorted set of non-negative
// Ints with bit-level set algebra.

import scala.collection.mutable.BitSet

@main def scalaStdlibMutableBitSet(): Unit =
  val a = BitSet(1, 3, 5, 7, 9)
  val b = BitSet(2, 3, 4, 5, 6)

  // 1. build
  val empty = BitSet.empty
  val fromList = BitSet.fromSpecific(List(8, 2, 5, 2, 8))
  println("build:" + empty.size + ":" + empty.isEmpty + ":" + fromList.size)

  // 2. access — head / last / contains / size
  println("access:" + a.head + ":" + a.last + ":" + a.contains(5) + ":" + a.contains(63) + ":" + a.size)

  // 3. order — sorted iteration via foreach
  val sb = StringBuilder()
  a.foreach { e =>
    if sb.nonEmpty then sb.append(",")
    sb.append(e.toString)
  }
  println("order:" + sb.toString)

  // 4. transform — set algebra union / intersect / diff (return new sets)
  val u = a.union(b)
  println("union:" + u.size + ":" + u.contains(2) + ":" + u.contains(7))
  val inter = a.intersect(b)
  println("intersect:" + inter.size + ":" + inter.contains(3) + ":" + inter.contains(5))
  val df = a.diff(b)
  println("diff:" + df.size + ":" + df.contains(1) + ":" + df.contains(3))

  // 5. mutate — += / -= / ++= / --= (values < 64)
  a += 11
  println("mutate1:" + a.size + ":" + a.contains(11) + ":" + a.last)
  a -= 1
  println("mutate2:" + a.size + ":" + a.contains(1) + ":" + a.head)
  a ++= List(13, 15)
  println("mutate3:" + a.size + ":" + a.last)
  a --= List(3, 5)
  println("mutate4:" + a.size + ":" + a.contains(3))

  // 6. in-place set algebra: |=, &=, &~= (where supported)
  val c = BitSet(1, 2, 3, 4)
  val d = BitSet(3, 4, 5, 6)
  c |= d
  println("orEq:" + c.size + ":" + c.contains(5) + ":" + c.contains(6))
  val e = BitSet(1, 2, 3, 4)
  e &= BitSet(2, 3, 4, 5)
  println("andEq:" + e.size + ":" + e.contains(1) + ":" + e.contains(5))

  // 7. aggregate
  var total = 0
  a.foreach(total += _)
  println("aggregate:" + total)

  // 8. convert
  println("convert:" + a.toList.size + ":" + a.toSeq.size + ":" + a.iterator.size)

  // 9. clone + clear
  val cloned = a.clone()
  println("clone:" + cloned.size)
  a.clear()
  println("clear:" + a.size + ":" + cloned.size)

  // 10. large elements crossing word boundary (>= 64)
  val big = BitSet(1, 5, 9)
  big += 64
  println("big1:" + big.size + ":" + big.contains(64) + ":" + big.last)
  big ++= List(65, 130)
  println("big2:" + big.size + ":" + big.last + ":" + big.contains(130))
  big -= 64
  println("big3:" + big.size + ":" + big.contains(64) + ":" + big.last)
  val big4 = BitSet(63, 64, 127, 128)
  val sb2 = StringBuilder()
  big4.foreach { e =>
    if sb2.nonEmpty then sb2.append(",")
    sb2.append(e.toString)
  }
  println("big4:" + big4.size + ":" + sb2.toString)
  big4 |= BitSet(64, 200)
  println("big5:" + big4.size + ":" + big4.last + ":" + big4.contains(200))
