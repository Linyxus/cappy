package java.lang.reflect

object Array {
  @inline
  def newInstance(componentType: Class[?], length: Int): AnyRef = {
    if componentType == java.lang.Boolean.TYPE then new scala.Array[Boolean](length)
    else if componentType == java.lang.Character.TYPE then new scala.Array[Char](length)
    else if componentType == java.lang.Byte.TYPE then new scala.Array[Byte](length)
    else if componentType == java.lang.Short.TYPE then new scala.Array[Short](length)
    else if componentType == java.lang.Integer.TYPE then new scala.Array[Int](length)
    else if componentType == java.lang.Long.TYPE then new scala.Array[Long](length)
    else if componentType == java.lang.Float.TYPE then new scala.Array[Float](length)
    else if componentType == java.lang.Double.TYPE then new scala.Array[Double](length)
    else if componentType == java.lang.Void.TYPE then throw new IllegalArgumentException("component type is void")
    // GenPython lowers calls to this overload directly to the tagged
    // runtime helper for reference and nested-array component types.
    else new scala.Array[Object](length)
  }

  def newInstance(componentType: Class[?], dimensions: scala.Array[Int]): AnyRef =
    if dimensions.length == 0 then
      throw new IllegalArgumentException("dimensions")
    else if dimensions.length == 1 then
      newInstance(componentType, dimensions(0))
    else
      val tail = new scala.Array[Int](dimensions.length - 1)
      var i = 1
      while i < dimensions.length do
        tail(i - 1) = dimensions(i)
        i += 1

      val sample = newInstance(componentType, tail).asInstanceOf[AnyRef]
      val outer = newInstance(sample.getClass(), dimensions(0)).asInstanceOf[Array[AnyRef]]

      if outer.length != 0 then
        outer(0) = sample
        i = 1
        while i < outer.length do
          outer(i) = newInstance(componentType, tail).asInstanceOf[AnyRef]
          i += 1

      outer

  def getLength(array: AnyRef): scala.Int =
    if array.isInstanceOf[Array[Object]] then array.asInstanceOf[Array[Object]].length
    else if array.isInstanceOf[Array[Boolean]] then array.asInstanceOf[Array[Boolean]].length
    else if array.isInstanceOf[Array[Char]] then array.asInstanceOf[Array[Char]].length
    else if array.isInstanceOf[Array[Byte]] then array.asInstanceOf[Array[Byte]].length
    else if array.isInstanceOf[Array[Short]] then array.asInstanceOf[Array[Short]].length
    else if array.isInstanceOf[Array[Int]] then array.asInstanceOf[Array[Int]].length
    else if array.isInstanceOf[Array[Long]] then array.asInstanceOf[Array[Long]].length
    else if array.isInstanceOf[Array[Float]] then array.asInstanceOf[Array[Float]].length
    else if array.isInstanceOf[Array[Double]] then array.asInstanceOf[Array[Double]].length
    else mismatch(array)

  def get(array: AnyRef, index: scala.Int): AnyRef =
    if array.isInstanceOf[Array[Object]] then array.asInstanceOf[Array[Object]](index)
    else if array.isInstanceOf[Array[Boolean]] then java.lang.Boolean.valueOf(array.asInstanceOf[Array[Boolean]](index))
    else if array.isInstanceOf[Array[Char]] then java.lang.Character.valueOf(array.asInstanceOf[Array[Char]](index))
    else if array.isInstanceOf[Array[Byte]] then java.lang.Byte.valueOf(array.asInstanceOf[Array[Byte]](index))
    else if array.isInstanceOf[Array[Short]] then java.lang.Short.valueOf(array.asInstanceOf[Array[Short]](index))
    else if array.isInstanceOf[Array[Int]] then java.lang.Integer.valueOf(array.asInstanceOf[Array[Int]](index))
    else if array.isInstanceOf[Array[Long]] then java.lang.Long.valueOf(array.asInstanceOf[Array[Long]](index))
    else if array.isInstanceOf[Array[Float]] then java.lang.Float.valueOf(array.asInstanceOf[Array[Float]](index))
    else if array.isInstanceOf[Array[Double]] then java.lang.Double.valueOf(array.asInstanceOf[Array[Double]](index))
    else mismatch(array)

  def getBoolean(array: AnyRef, index: scala.Int): scala.Boolean =
    if array.isInstanceOf[Array[Boolean]] then array.asInstanceOf[Array[Boolean]](index)
    else mismatch(array)

  def getChar(array: AnyRef, index: scala.Int): scala.Char =
    if array.isInstanceOf[Array[Char]] then array.asInstanceOf[Array[Char]](index)
    else mismatch(array)

  def getByte(array: AnyRef, index: scala.Int): scala.Byte =
    if array.isInstanceOf[Array[Byte]] then array.asInstanceOf[Array[Byte]](index)
    else mismatch(array)

  def getShort(array: AnyRef, index: scala.Int): scala.Short =
    if array.isInstanceOf[Array[Short]] then array.asInstanceOf[Array[Short]](index)
    else if array.isInstanceOf[Array[Byte]] then narrowToShort(array.asInstanceOf[Array[Byte]](index).toInt)
    else mismatch(array)

  def getInt(array: AnyRef, index: scala.Int): scala.Int =
    if array.isInstanceOf[Array[Int]] then array.asInstanceOf[Array[Int]](index)
    else if array.isInstanceOf[Array[Char]] then array.asInstanceOf[Array[Char]](index).toInt
    else if array.isInstanceOf[Array[Byte]] then array.asInstanceOf[Array[Byte]](index).toInt
    else if array.isInstanceOf[Array[Short]] then array.asInstanceOf[Array[Short]](index).toInt
    else mismatch(array)

  def getLong(array: AnyRef, index: scala.Int): scala.Long =
    if array.isInstanceOf[Array[Long]] then array.asInstanceOf[Array[Long]](index)
    else if array.isInstanceOf[Array[Char]] then array.asInstanceOf[Array[Char]](index).toLong
    else if array.isInstanceOf[Array[Byte]] then array.asInstanceOf[Array[Byte]](index).toLong
    else if array.isInstanceOf[Array[Short]] then array.asInstanceOf[Array[Short]](index).toLong
    else if array.isInstanceOf[Array[Int]] then array.asInstanceOf[Array[Int]](index).toLong
    else mismatch(array)

  def getFloat(array: AnyRef, index: scala.Int): scala.Float =
    if array.isInstanceOf[Array[Float]] then array.asInstanceOf[Array[Float]](index)
    else if array.isInstanceOf[Array[Char]] then array.asInstanceOf[Array[Char]](index).toFloat
    else if array.isInstanceOf[Array[Byte]] then array.asInstanceOf[Array[Byte]](index).toFloat
    else if array.isInstanceOf[Array[Short]] then array.asInstanceOf[Array[Short]](index).toFloat
    else if array.isInstanceOf[Array[Int]] then array.asInstanceOf[Array[Int]](index).toFloat
    else if array.isInstanceOf[Array[Long]] then array.asInstanceOf[Array[Long]](index).toFloat
    else mismatch(array)

  def getDouble(array: AnyRef, index: scala.Int): scala.Double =
    if array.isInstanceOf[Array[Double]] then array.asInstanceOf[Array[Double]](index)
    else if array.isInstanceOf[Array[Char]] then array.asInstanceOf[Array[Char]](index).toDouble
    else if array.isInstanceOf[Array[Byte]] then array.asInstanceOf[Array[Byte]](index).toDouble
    else if array.isInstanceOf[Array[Short]] then array.asInstanceOf[Array[Short]](index).toDouble
    else if array.isInstanceOf[Array[Int]] then array.asInstanceOf[Array[Int]](index).toDouble
    else if array.isInstanceOf[Array[Long]] then array.asInstanceOf[Array[Long]](index).toDouble
    else if array.isInstanceOf[Array[Float]] then array.asInstanceOf[Array[Float]](index).toDouble
    else mismatch(array)

  def set(array: AnyRef, index: scala.Int, value: AnyRef): Unit =
    if array.isInstanceOf[Array[Object]] then
      array.asInstanceOf[Array[Object]](index) = value
    else if value.isInstanceOf[scala.Boolean] then
      setBoolean(array, index, value.asInstanceOf[scala.Boolean])
    else if value.isInstanceOf[scala.Char] then
      setChar(array, index, value.asInstanceOf[scala.Char])
    else if value.isInstanceOf[scala.Byte] then
      setByte(array, index, value.asInstanceOf[scala.Byte])
    else if value.isInstanceOf[scala.Short] then
      setShort(array, index, value.asInstanceOf[scala.Short])
    else if value.isInstanceOf[scala.Int] then
      setInt(array, index, value.asInstanceOf[scala.Int])
    else if value.isInstanceOf[scala.Long] then
      setLong(array, index, value.asInstanceOf[scala.Long])
    else if value.isInstanceOf[scala.Float] then
      setFloat(array, index, value.asInstanceOf[scala.Float])
    else if value.isInstanceOf[scala.Double] then
      setDouble(array, index, value.asInstanceOf[scala.Double])
    else
      mismatch(array)

  def setBoolean(array: AnyRef, index: scala.Int, value: scala.Boolean): Unit =
    if array.isInstanceOf[Array[Boolean]] then array.asInstanceOf[Array[Boolean]](index) = value
    else mismatch(array)

  def setChar(array: AnyRef, index: scala.Int, value: scala.Char): Unit =
    if array.isInstanceOf[Array[Char]] then array.asInstanceOf[Array[Char]](index) = value
    else if array.isInstanceOf[Array[Int]] then array.asInstanceOf[Array[Int]](index) = value.toInt
    else if array.isInstanceOf[Array[Long]] then array.asInstanceOf[Array[Long]](index) = value.toLong
    else if array.isInstanceOf[Array[Float]] then array.asInstanceOf[Array[Float]](index) = value.toFloat
    else if array.isInstanceOf[Array[Double]] then array.asInstanceOf[Array[Double]](index) = value.toDouble
    else mismatch(array)

  def setByte(array: AnyRef, index: scala.Int, value: scala.Byte): Unit =
    if array.isInstanceOf[Array[Byte]] then array.asInstanceOf[Array[Byte]](index) = value
    else if array.isInstanceOf[Array[Short]] then array.asInstanceOf[Array[Short]](index) = narrowToShort(value.toInt)
    else if array.isInstanceOf[Array[Int]] then array.asInstanceOf[Array[Int]](index) = value.toInt
    else if array.isInstanceOf[Array[Long]] then array.asInstanceOf[Array[Long]](index) = value.toLong
    else if array.isInstanceOf[Array[Float]] then array.asInstanceOf[Array[Float]](index) = value.toFloat
    else if array.isInstanceOf[Array[Double]] then array.asInstanceOf[Array[Double]](index) = value.toDouble
    else mismatch(array)

  def setShort(array: AnyRef, index: scala.Int, value: scala.Short): Unit =
    if array.isInstanceOf[Array[Short]] then array.asInstanceOf[Array[Short]](index) = value
    else if array.isInstanceOf[Array[Int]] then array.asInstanceOf[Array[Int]](index) = value.toInt
    else if array.isInstanceOf[Array[Long]] then array.asInstanceOf[Array[Long]](index) = value.toLong
    else if array.isInstanceOf[Array[Float]] then array.asInstanceOf[Array[Float]](index) = value.toFloat
    else if array.isInstanceOf[Array[Double]] then array.asInstanceOf[Array[Double]](index) = value.toDouble
    else mismatch(array)

  def setInt(array: AnyRef, index: scala.Int, value: scala.Int): Unit =
    if array.isInstanceOf[Array[Int]] then array.asInstanceOf[Array[Int]](index) = value
    else if array.isInstanceOf[Array[Long]] then array.asInstanceOf[Array[Long]](index) = value.toLong
    else if array.isInstanceOf[Array[Float]] then array.asInstanceOf[Array[Float]](index) = value.toFloat
    else if array.isInstanceOf[Array[Double]] then array.asInstanceOf[Array[Double]](index) = value.toDouble
    else mismatch(array)

  def setLong(array: AnyRef, index: scala.Int, value: scala.Long): Unit =
    if array.isInstanceOf[Array[Long]] then array.asInstanceOf[Array[Long]](index) = value
    else if array.isInstanceOf[Array[Float]] then array.asInstanceOf[Array[Float]](index) = value.toFloat
    else if array.isInstanceOf[Array[Double]] then array.asInstanceOf[Array[Double]](index) = value.toDouble
    else mismatch(array)

  def setFloat(array: AnyRef, index: scala.Int, value: scala.Float): Unit =
    if array.isInstanceOf[Array[Float]] then array.asInstanceOf[Array[Float]](index) = value
    else if array.isInstanceOf[Array[Double]] then array.asInstanceOf[Array[Double]](index) = value.toDouble
    else mismatch(array)

  def setDouble(array: AnyRef, index: scala.Int, value: scala.Double): Unit =
    if array.isInstanceOf[Array[Double]] then array.asInstanceOf[Array[Double]](index) = value
    else mismatch(array)

  private def mismatch(array: AnyRef): Nothing = {
    if (array == null) throw new NullPointerException()
    throw new IllegalArgumentException("argument type mismatch")
  }

  @inline private def narrowToShort(value: Int): scala.Short =
    value.asInstanceOf[scala.Short]
}
