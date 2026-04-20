import java.util.SplittableRandom

@main def javalibUtilSplittableRandom(): Unit =
  val left = new SplittableRandom(42L)
  val right = new SplittableRandom(42L)
  println("seeded:" + (left.nextInt() == right.nextInt()) + ":" + (left.nextLong() == right.nextLong()))

  val parent = new SplittableRandom(9L)
  val child = parent.split()
  val parentValue = parent.nextInt(100)
  val childValue = child.nextInt(100)
  println("split:" + (parentValue >= 0 && parentValue < 100) + ":" + (childValue >= 0 && childValue < 100) + ":" + (parentValue != childValue))
