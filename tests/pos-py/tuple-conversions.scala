@main def run(): Unit =
  val t = (1, "two", 3.0)
  val lst = t.toList
  println(lst)
  println(lst.length)

  val arr = t.toArray
  println(arr.length)
  println(arr(0))
  println(arr(1))
  println(arr(2))

  arr(0) = "mutated"
  println(t._1)
  println(arr(0))

  val ia = t.toIArray
  val t2 = Tuple.fromIArray(ia)
  println(t == t2)
