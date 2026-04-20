import java.util.{Comparator, Objects}

@main def javalibUtilObjects(): Unit =
  val alpha = "alpha"
  val beta = "beta"
  val gamma = "gamma"

  val nestedAInner = java.lang.reflect.Array.newInstance(classOf[Object], 1).asInstanceOf[Array[AnyRef]]
  nestedAInner(0) = alpha
  val nestedAInts = new Array[Int](2)
  nestedAInts(0) = 1
  nestedAInts(1) = 2
  val nestedA = java.lang.reflect.Array.newInstance(classOf[Object], 2).asInstanceOf[Array[AnyRef]]
  nestedA(0) = nestedAInner
  nestedA(1) = nestedAInts.asInstanceOf[AnyRef]

  val nestedBInner = java.lang.reflect.Array.newInstance(classOf[Object], 1).asInstanceOf[Array[AnyRef]]
  nestedBInner(0) = alpha
  val nestedBInts = new Array[Int](2)
  nestedBInts(0) = 1
  nestedBInts(1) = 2
  val nestedB = java.lang.reflect.Array.newInstance(classOf[Object], 2).asInstanceOf[Array[AnyRef]]
  nestedB(0) = nestedBInner
  nestedB(1) = nestedBInts.asInstanceOf[AnyRef]

  val nestedCInner = java.lang.reflect.Array.newInstance(classOf[Object], 1).asInstanceOf[Array[AnyRef]]
  nestedCInner(0) = gamma
  val nestedCInts = new Array[Int](2)
  nestedCInts(0) = 1
  nestedCInts(1) = 3
  val nestedC = java.lang.reflect.Array.newInstance(classOf[Object], 2).asInstanceOf[Array[AnyRef]]
  nestedC(0) = nestedCInner
  nestedC(1) = nestedCInts.asInstanceOf[AnyRef]

  val hashValues = java.lang.reflect.Array.newInstance(classOf[Object], 2).asInstanceOf[Array[AnyRef]]
  hashValues(0) = alpha
  hashValues(1) = beta

  val comparator = Comparator.comparingInt[String]((value: String) => value.length)

  println("equals:" + Objects.equals(alpha, "alpha") + ":" + Objects.equals(null, null) + ":" + Objects.equals(alpha, beta))
  println("deepequals:" + Objects.deepEquals(nestedA, nestedB) + ":" + Objects.deepEquals(nestedA, nestedC))
  println("requirenonnull:" + (Objects.requireNonNull(alpha) eq alpha) + ":" + Objects.nonNull(alpha))
  println("hash:" + Objects.hashCode(alpha) + ":" + Objects.hash(hashValues))
  println("tostring:" + Objects.toString(alpha) + ":" + Objects.toString(null, "fallback"))
  println("compare:" + Objects.compare(alpha, alpha, comparator) + ":" + Objects.compare(alpha, beta, comparator))
