// `groupBy` / `groupMap` / `groupMapReduce` on List and Vector. Closure-heavy
// + tuple build + Map result, so these exercise the Function1 / pair / map
// builder boundaries together. Result keys are non-deterministic in iteration
// order (Map uses HashMap), so always derive prints via key lookup or sorted
// projections.

@main def scalaStdlibGroupBy(): Unit =
  val list = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  val vec = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9)

  // 1. List.groupBy(_ % 3) — buckets size, sample contents.
  val lg = list.groupBy(_ % 3)
  println("list-groupBy:" + lg.size + ":" + lg(0).size + ":" + lg(1).size + ":" + lg(2).size + ":" + lg(0).sum + ":" + lg(1).sum)

  // 2. Vector.groupBy(_ % 3).
  val vg = vec.groupBy(_ % 3)
  println("vec-groupBy:" + vg.size + ":" + vg(0).size + ":" + vg(1).size + ":" + vg(2).size + ":" + vg(0).sum + ":" + vg(2).sum)

  // 3. List.groupMap — group by mod-3, map values to *10.
  val lgm = list.groupMap(_ % 3)(_ * 10)
  println("list-groupMap:" + lgm.size + ":" + lgm(0).sum + ":" + lgm(1).sum + ":" + lgm(2).sum)

  // 4. Vector.groupMap.
  val vgm = vec.groupMap(_ % 3)(_ * 10)
  println("vec-groupMap:" + vgm.size + ":" + vgm(0).sum + ":" + vgm(1).sum + ":" + vgm(2).sum)

  // 5. List.groupMapReduce — sum within each bucket directly.
  val lgmr = list.groupMapReduce(_ % 3)(identity)(_ + _)
  println("list-groupMapReduce:" + lgmr.size + ":" + lgmr(0) + ":" + lgmr(1) + ":" + lgmr(2))

  // 6. Vector.groupMapReduce.
  val vgmr = vec.groupMapReduce(_ % 3)(identity)(_ + _)
  println("vec-groupMapReduce:" + vgmr.size + ":" + vgmr(0) + ":" + vgmr(1) + ":" + vgmr(2))

  // 7. groupBy with a complex closure — bucket by sign.
  val signed = List(-3, -1, 0, 1, 2, 3).groupBy(x => if x < 0 then "neg" else if x == 0 then "zero" else "pos")
  println("signed:" + signed.size + ":" + signed("neg").size + ":" + signed("zero").size + ":" + signed("pos").size)
