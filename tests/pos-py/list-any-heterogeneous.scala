// `List[Any]` built from heterogeneous elements (Tuple2, String, ...)
// has its varargs LUB collapse to `java.io.Serializable` (and friends).
// Scala's `List.apply(xs: Any*)` routes through
// `ScalaRunTime.genericWrapArray` → `ArraySeq.unsafeWrapArray`, whose
// `(x: @unchecked) match { ... case x: Array[AnyRef] => ... }` test
// must accept any `Array[T]` whose element type is a non-primitive
// reference. Before the fix, `_scpy_is_assignable(Object, Serializable)`
// returned False (interfaces are registered with `superclass=None`,
// so the walk chain never reached `Object`), so the dispatch fell
// through to the catch-all `MatchError`.
@main def listAnyHeterogeneous(): Unit =
  val xs: List[Any] = List((1, 2), "hello", (3, 4), "", "world")
  println(xs.length)
  xs.foreach(println)

  // Direct exercise of the underlying covariance: an `Array` whose
  // element type is `java.io.Serializable` must be `instanceOf` an
  // `Array[Object]` (i.e. `Array[AnyRef]`).
  val arr: Array[java.io.Serializable] =
    Array("hello", (1, 2): java.io.Serializable, "world")
  println(arr.isInstanceOf[Array[AnyRef]])
  println(arr.isInstanceOf[Array[java.io.Serializable]])
  println(arr.length)
