@main def run(): Unit =
  val t: (Int, String, Double) = (1, "two", 3.0)
  val s: String = t(1)
  println(s.length)
  println(s.toUpperCase)
  println(t(0) + 100)
  for i <- 0 until t.size do
    println(s"${t(i)} == ${t.productElement(i)}: ${t(i) == t.productElement(i)}")
