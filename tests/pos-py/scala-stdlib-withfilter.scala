// for-comprehension lowering: `for x <- xs if p(x) yield f(x)` desugars to
// `xs.withFilter(p).map(f)`, exercising the WithFilter wrapper class on
// List and Vector.

@main def scalaStdlibWithFilter(): Unit =
  val list = List(1, 2, 3, 4, 5, 6, 7, 8)
  val vec = Vector(1, 2, 3, 4, 5, 6, 7, 8)

  // 1. List for-yield with single guard — single map at the end.
  val l1 = for x <- list if x > 3 yield x * 2
  println("list-yield:" + l1.size + ":" + l1.head + ":" + l1.last + ":" + l1.sum)

  // 2. List for-yield with multiple guards — chained withFilter.
  val l2 = for
    x <- list
    if x > 2
    if x < 7
  yield x * 10
  println("list-multi:" + l2.size + ":" + l2.head + ":" + l2.last + ":" + l2.sum)

  // 3. Vector for-yield with guard.
  val v1 = for x <- vec if x % 2 == 0 yield x + 100
  println("vec-yield:" + v1.size + ":" + v1.head + ":" + v1.last + ":" + v1.sum)

  // 4. Direct withFilter().map — same shape, no syntactic sugar.
  val direct = list.withFilter(_ > 5).map(_ * 3)
  println("direct:" + direct.size + ":" + direct.head + ":" + direct.last)

  // 5. Direct withFilter().foreach — side-effect path.
  var c = 0
  list.withFilter(_ > 5).foreach(c += _)
  println("foreach:" + c)

  // 6. Empty filter result.
  val e = for x <- list if x > 100 yield x
  println("empty:" + e.size + ":" + e.isEmpty)
