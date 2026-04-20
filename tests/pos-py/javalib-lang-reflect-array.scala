@main def javalibLangReflectArray(): Unit =
  val ints = java.lang.reflect.Array.newInstance(classOf[Int], 4).asInstanceOf[scala.Array[Int]]
  println("new:" + java.lang.reflect.Array.getLength(ints.asInstanceOf[AnyRef]))

  java.lang.reflect.Array.setInt(ints.asInstanceOf[AnyRef], 0, 7)
  println("get-set:int:" + java.lang.reflect.Array.getInt(ints.asInstanceOf[AnyRef], 0))

  val doubles = java.lang.reflect.Array.newInstance(classOf[Double], 2).asInstanceOf[scala.Array[Double]]
  java.lang.reflect.Array.setDouble(doubles.asInstanceOf[AnyRef], 1, 2.5)
  println("get-set:double:" + java.lang.reflect.Array.getDouble(doubles.asInstanceOf[AnyRef], 1))

  val objs = java.lang.reflect.Array.newInstance(classOf[String], 2).asInstanceOf[scala.Array[Object]]
  java.lang.reflect.Array.set(objs.asInstanceOf[AnyRef], 0, "hi")
  println("get-set:object:" + java.lang.reflect.Array.get(objs.asInstanceOf[AnyRef], 0))

  val bytes = new scala.Array[Byte](1)
  bytes(0) = 5.toByte
  val ints2 = new scala.Array[Int](1)
  ints2(0) = 6
  println("coerce:int:" + java.lang.reflect.Array.getInt(bytes.asInstanceOf[AnyRef], 0))
  println("coerce:long:" + java.lang.reflect.Array.getLong(ints2.asInstanceOf[AnyRef], 0))

  try
    java.lang.reflect.Array.getBoolean(ints.asInstanceOf[AnyRef], 0)
  catch
    case e: IllegalArgumentException =>
      println("mismatch:" + e.getMessage())

  val matrix = java.lang.reflect.Array
    .newInstance(classOf[Int], scala.Array(2, 3))
    .asInstanceOf[scala.Array[scala.Array[Int]]]
  matrix(1)(2) = 9
  println("multidim:" + matrix.length + ":" + matrix(0).length + ":" + matrix(1)(2))
  println("multidim-class:" + matrix.getClass().getName() + ":" + matrix(0).getClass().getName())
