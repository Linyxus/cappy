package java.util

@main def javalibUtilInternal(): Unit =
  val mixed1 = internal.MurmurHash3.mix(0, 17)
  val mixed2 = internal.MurmurHash3.mix(mixed1, 99)
  val finalized = internal.MurmurHash3.finalizeHash(mixed2, 2)

  val intRef = internal.IntRef.create(4)
  intRef.elem += 3

  val objRef = internal.ObjectRef.create("box")
  objRef.elem = objRef.elem + "-ref"

  val tuple2 = new internal.Tuple2("a", 1)
  val tuple4 = new internal.Tuple4("b", 2, true, "z")

  println("murmur:" + finalized)
  println("refs:" + intRef + ":" + objRef)
  println("tuples:" + tuple2._1 + ":" + tuple2._2 + ":" + tuple4._3 + ":" + tuple4._4)
