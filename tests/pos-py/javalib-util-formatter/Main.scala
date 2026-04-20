import java.util.Formatter

private def captureFormatterUoe(tag: String)(body: => Any): Unit =
  try
    body
    println(tag + ":no-throw")
  catch
    case e: UnsupportedOperationException =>
      println(tag + ":" + e.getMessage())

@main def javalibUtilFormatter(): Unit =
  println("d:" + new Formatter().format("%d", Array[AnyRef](Int.box(42))).toString())
  println("s:" + new Formatter().format("%s", Array[AnyRef]("scala-py")).toString())
  println("f3:" + new Formatter().format("%.3f", Array[AnyRef](Double.box(3.14159))).toString())
  println("x:" + new Formatter().format("%x", Array[AnyRef](Int.box(255))).toString())
  println("o:" + new Formatter().format("%o", Array[AnyRef](Int.box(9))).toString())
  println("pad:" + new Formatter().format("%05d", Array[AnyRef](Int.box(12))).toString())
  println("upper:" + new Formatter().format("%S", Array[AnyRef]("mix")).toString())
  val newline = new Formatter().format("a%nb", new Array[AnyRef](0)).toString()
  println("n:" + newline.replace("\n", "|"))

  captureFormatterUoe("tc") {
    new Formatter().format("%tc", Array[AnyRef](Long.box(0L)))
  }
  captureFormatterUoe("group") {
    new Formatter().format("%,d", Array[AnyRef](Int.box(12)))
  }
