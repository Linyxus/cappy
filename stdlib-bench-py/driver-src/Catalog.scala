package dotty.tools.benchmarks.py

object Catalog:
  final case class Entry(
      benchId:    String,
      bundleName: String,
      source:     String,
      ops:        List[String],
      sizes:      List[Int],
  )

  val standardOps:   List[String] = List("build", "access", "transform", "mutate")
  val standardSizes: List[Int]    = List(16, 256, 4096)

  private def imm(name: String): Entry = Entry(
    s"immutable.$name",
    s"$name.py",
    s"stdlib-bench-py/src/main/scala/dotty/tools/benchmarks/py/immutable/$name.scala",
    standardOps,
    standardSizes,
  )
  private def mut(name: String): Entry = Entry(
    s"mutable.$name",
    s"$name.py",
    s"stdlib-bench-py/src/main/scala/dotty/tools/benchmarks/py/mutable/$name.scala",
    standardOps,
    standardSizes,
  )

  val entries: List[Entry] = List(
    imm("VectorBench"),
    imm("ListBench"),
    imm("LazyListBench"),
    imm("HashMapBench"),
    imm("HashSetBench"),
    imm("TreeMapBench"),
    mut("ArrayBufferBench"),
    mut("ArrayDequeBench"),
    mut("HashMapBench"),
    mut("HashSetBench"),
    mut("ListBufferBench"),
    mut("StringBuilderBench"),
  )
