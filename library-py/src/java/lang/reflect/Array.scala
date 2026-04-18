package java.lang.reflect

object Array:
  def newInstance(componentType: Class[?], length: Int): AnyRef =
    val tag = componentType.asInstanceOf[String]
    if tag == "I" then
      new scala.Array[Int](length)
    else
      new scala.Array[Object](length)
