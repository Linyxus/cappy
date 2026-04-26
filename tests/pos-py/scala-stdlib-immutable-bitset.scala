// Coverage for scala.collection.immutable.BitSet — sorted set of non-negative
// Ints with bit-level set algebra.

import scala.collection.immutable.BitSet

@main def scalaStdlibImmutableBitSet(): Unit =
  val a = BitSet(1, 3, 5, 7, 9)
  val b = BitSet(2, 3, 4, 5, 6)

  // 1. build
  val empty = BitSet.empty
  val fromList = BitSet.fromSpecific(List(8, 2, 5, 2, 8))
  println("build:" + empty.size + ":" + empty.isEmpty + ":" + fromList.size)

  // 2. access — head / last / contains / size
  println("access:" + a.head + ":" + a.last + ":" + a.contains(5) + ":" + a.contains(63) + ":" + a.size)

  // 3. order — sorted iteration (BitSet iterates in increasing order)
  println("order:" + a.iterator.toList.mkString(","))

  // 4. transform — set algebra union / intersect / diff
  val u = a.union(b)
  println("union:" + u.size + ":" + u.iterator.toList.mkString(","))
  val inter = a.intersect(b)
  println("intersect:" + inter.size + ":" + inter.iterator.toList.mkString(","))
  val df = a.diff(b)
  println("diff:" + df.size + ":" + df.iterator.toList.mkString(","))

  // 5. + / - / ++ / -- — values < 64
  val plus = a + 11
  println("plus:" + plus.size + ":" + plus.contains(11))
  val minus = a - 1
  println("minus:" + minus.size + ":" + minus.contains(1))
  val plusMany = a ++ BitSet(11, 13)
  println("plusMany:" + plusMany.size + ":" + plusMany.last)
  val minusMany = a -- List(1, 3)
  println("minusMany:" + minusMany.size + ":" + minusMany.head)

  // 6. aggregate
  println("aggregate:" + a.foldLeft(0)(_ + _) + ":" + a.sum + ":" + a.filter(_ > 3).size + ":" + a.count(_ > 5))

  // 7. convert
  println("convert:" + a.toList.size + ":" + a.toSeq.size + ":" + a.iterator.size)

  // 8. large elements crossing word boundary (>= 64)
  val big = a + 64
  println("big1:" + big.size + ":" + big.contains(64) + ":" + big.last)
  val big2 = a ++ BitSet(64, 65, 130)
  println("big2:" + big2.size + ":" + big2.last + ":" + big2.contains(130))
  val big3 = big2 - 64
  println("big3:" + big3.size + ":" + big3.contains(64) + ":" + big3.last)
  val big4 = BitSet(63, 64, 127, 128)
  println("big4:" + big4.size + ":" + big4.iterator.toList.mkString(","))
  val big5 = big4.union(BitSet(64, 200))
  println("big5:" + big5.size + ":" + big5.last + ":" + big5.contains(200))
