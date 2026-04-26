// `grouped(n)` and `sliding(n)` cover Iterable-yielding-Iterator-of-collections
// — exercises iterator + nested-collection materialization across List, Vector,
// LazyList. Each chunk is forced to .toList for deterministic size/content.

@main def scalaStdlibGroupedSliding(): Unit =
  // List grouped — non-overlapping chunks of fixed size; tail chunk smaller.
  val xs = List(1, 2, 3, 4, 5, 6, 7, 8)
  val lg = xs.grouped(3).toList
  println("list-grouped:" + lg.size + ":" + lg.head.mkString(",") + ":" + lg.last.mkString(","))

  // List sliding — overlapping windows of fixed size, step=1 by default.
  val ls = xs.sliding(3).toList
  println("list-sliding:" + ls.size + ":" + ls.head.mkString(",") + ":" + ls.last.mkString(","))

  // List sliding with step.
  val lss = xs.sliding(3, 2).toList
  println("list-sliding-step:" + lss.size + ":" + lss.head.mkString(",") + ":" + lss.last.mkString(","))

  // Vector grouped / sliding.
  val v = Vector(10, 20, 30, 40, 50, 60)
  val vg = v.grouped(2).toList
  println("vec-grouped:" + vg.size + ":" + vg.head.mkString(",") + ":" + vg.last.mkString(","))
  val vs = v.sliding(2).toList
  println("vec-sliding:" + vs.size + ":" + vs.head.mkString(",") + ":" + vs.last.mkString(","))

  // LazyList grouped / sliding.
  val lz = LazyList(1, 2, 3, 4, 5)
  val lzg = lz.grouped(2).toList
  println("lazy-grouped:" + lzg.size + ":" + lzg.head.mkString(",") + ":" + lzg.last.mkString(","))
  val lzs = lz.sliding(2).toList
  println("lazy-sliding:" + lzs.size + ":" + lzs.head.mkString(",") + ":" + lzs.last.mkString(","))

  // Remainder behaviour: grouped surfaces the partial tail; sliding does not.
  val rem = List(1, 2, 3, 4, 5).grouped(2).toList
  println("remainder:" + rem.size + ":" + rem.last.size + ":" + rem.last.mkString(","))
