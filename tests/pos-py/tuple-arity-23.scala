@main def run(): Unit =
  val t22 = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
  println(t22.size)
  println(t22._1)
  println(t22._22)

  val t23a = t22 :* 23
  println(t23a.size)
  println(t23a.productElement(0))
  println(t23a.productElement(11))
  println(t23a.productElement(22))

  val t23b = 0 *: t22
  println(t23b.size)
  println(t23b.productElement(0))
  println(t23b.productElement(22))

  val t23c = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) ++ (12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
  println(t23c.size)
  println(t23c.productElement(0))
  println(t23c.productElement(22))

  val tailed = t23a.tail
  println(tailed.size)
  println(tailed.productElement(0))
  println(tailed.productElement(21))

  val inited = t23a.init
  println(inited.size)
  println(inited.productElement(0))
  println(inited.productElement(21))
