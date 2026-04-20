import java.util.Random

@main def javalibUtilRandom(): Unit =
  val left = new Random(1234L)
  val right = new Random(1234L)
  println("seeded:" + (left.nextInt() == right.nextInt()) + ":" + (left.nextLong() == right.nextLong()) + ":" + (left.nextDouble() == right.nextDouble()))

  val bounded = new Random(99L)
  val next10 = bounded.nextInt(10)
  val nextRange = bounded.nextInt(5, 10)
  println("bounds:" + (next10 >= 0 && next10 < 10) + ":" + (nextRange >= 5 && nextRange < 10))

  val bytesA = new Array[Byte](4)
  val bytesB = new Array[Byte](4)
  val seededA = new Random(7L)
  val seededB = new Random(7L)
  seededA.nextBytes(bytesA)
  seededB.nextBytes(bytesB)
  println("bytes:" + java.util.Arrays.equals(bytesA, bytesB))
