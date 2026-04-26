import scala.collection.mutable.StringBuilder

@main def reproSb(): Unit =
  val sb = new StringBuilder(4)
  sb.append('a')
  sb.append('b')
  sb.setCharAt(0, 'X')
  println(sb.toString)
  println(sb.length)
  println("ch:" + sb.charAt(1))
