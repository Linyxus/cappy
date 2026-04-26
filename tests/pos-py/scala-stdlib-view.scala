// `xs.view.map(...).filter(...).toList` chain — high-risk for IndexedSeqView
// MRO bugs. Forces the lazy view machinery and the strict-result rendezvous
// when the chain is forced via `.toList`.

import scala.collection.mutable.ArrayBuffer

@main def scalaStdlibView(): Unit =
  // 1. List.view chain — fundamental SeqView path.
  val list = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val ld = list.view.map(_ + 1).filter(_ < 10).toList
  println("list:" + ld.size + ":" + ld.head + ":" + ld.last + ":" + ld.sum)

  // 2. Vector.view — IndexedSeqView path.
  val vec = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val vd = vec.view.map(_ * 2).filter(_ < 16).toList
  println("vec:" + vd.size + ":" + vd.head + ":" + vd.last + ":" + vd.sum)

  // 3. ArrayBuffer.view — mutable IndexedSeqView path.
  val buf = ArrayBuffer(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val bd = buf.view.map(_ + 10).filter(_ % 2 == 0).toList
  println("buf:" + bd.size + ":" + bd.head + ":" + bd.last + ":" + bd.sum)

  // 4. Force through .toVector instead of .toList — different builder.
  val toVec = list.view.map(_ * 3).filter(_ > 5).toVector
  println("toVec:" + toVec.size + ":" + toVec.head + ":" + toVec.last)

  // 5. Chain length 3 — map + filter + map.
  val chain3 = list.view.map(_ + 1).filter(_ < 8).map(_ * 10).toList
  println("chain3:" + chain3.size + ":" + chain3.head + ":" + chain3.last + ":" + chain3.sum)

  // 6. Empty result after filter.
  val empty = list.view.filter(_ > 100).toList
  println("empty:" + empty.size + ":" + empty.isEmpty)
