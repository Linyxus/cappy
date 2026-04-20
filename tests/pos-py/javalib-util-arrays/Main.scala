import java.util.{Arrays, Comparator}

final class Ranked(val key: Int, val label: String) extends Comparable[Ranked]:
  override def compareTo(that: Ranked): Int =
    if key < that.key then -1
    else if key > that.key then 1
    else 0

  override def toString(): String =
    label

@main def javalibUtilArrays(): Unit =
  val ints = new Array[Int](3)
  ints(0) = 3
  ints(1) = 1
  ints(2) = 2
  Arrays.sort(ints)

  val doubles = new Array[Double](3)
  doubles(0) = 2.5
  doubles(1) = -1.0
  doubles(2) = 2.0
  Arrays.sort(doubles)

  val ranks = new Array[Ranked](3)
  ranks(0) = new Ranked(3, "pear")
  ranks(1) = new Ranked(1, "apple")
  ranks(2) = new Ranked(2, "banana")
  Arrays.sort[Ranked](ranks, null)

  val words = java.lang.reflect.Array.newInstance(classOf[String], 3).asInstanceOf[Array[String]]
  words(0) = "bbb"
  words(1) = "a"
  words(2) = "cc"
  Arrays.sort[String](
    words,
    Comparator.comparingInt[String]((value: String) => value.length)
  )

  val filled = new Array[Int](4)
  Arrays.fill(filled, 1, 3, 9)

  val deepInts = new Array[Int](2)
  deepInts(0) = 1
  deepInts(1) = 2
  val deepStrings = java.lang.reflect.Array.newInstance(classOf[Object], 2).asInstanceOf[Array[AnyRef]]
  deepStrings(0) = "x"
  deepStrings(1) = "y"
  val deep = java.lang.reflect.Array.newInstance(classOf[Object], 2).asInstanceOf[Array[AnyRef]]
  deep(0) = deepInts.asInstanceOf[AnyRef]
  deep(1) = deepStrings

  val cycle = java.lang.reflect.Array.newInstance(classOf[Object], 1).asInstanceOf[Array[AnyRef]]
  cycle(0) = cycle

  val listBacking = java.lang.reflect.Array.newInstance(classOf[Object], 2).asInstanceOf[Array[AnyRef]]
  listBacking(0) = "x"
  listBacking(1) = "y"
  val asList = Arrays.asList(listBacking)
  asList.set(1, "z")

  val equalIntsA = new Array[Int](2)
  equalIntsA(0) = 1
  equalIntsA(1) = 2
  val equalIntsB = new Array[Int](2)
  equalIntsB(0) = 1
  equalIntsB(1) = 2
  val unequalInts = new Array[Int](2)
  unequalInts(0) = 2
  unequalInts(1) = 1

  val equalRefsA = java.lang.reflect.Array.newInstance(classOf[Object], 2).asInstanceOf[Array[AnyRef]]
  equalRefsA(0) = "a"
  equalRefsA(1) = "b"
  val equalRefsB = java.lang.reflect.Array.newInstance(classOf[Object], 2).asInstanceOf[Array[AnyRef]]
  equalRefsB(0) = "a"
  equalRefsB(1) = "b"
  val unequalRefsA = java.lang.reflect.Array.newInstance(classOf[Object], 1).asInstanceOf[Array[AnyRef]]
  unequalRefsA(0) = "a"
  val unequalRefsB = java.lang.reflect.Array.newInstance(classOf[Object], 1).asInstanceOf[Array[AnyRef]]
  unequalRefsB(0) = "c"

  val hashArray = new Array[Int](3)
  hashArray(0) = 1
  hashArray(1) = 2
  hashArray(2) = 3

  val copySource = new Array[Int](2)
  copySource(0) = 4
  copySource(1) = 5
  val rangeSource = new Array[Int](4)
  rangeSource(0) = 4
  rangeSource(1) = 5
  rangeSource(2) = 6
  rangeSource(3) = 7

  val bools = new Array[Boolean](2)
  bools(0) = true
  bools(1) = false

  println("sort-int:" + Arrays.toString(ints))
  println("sort-double:" + Arrays.toString(doubles))
  println("sort-object:" + Arrays.toString(ranks.asInstanceOf[Array[AnyRef]]))
  println("sort-comparator:" + Arrays.toString(words.asInstanceOf[Array[AnyRef]]))
  println("binarysearch:" + Arrays.binarySearch(ints, 2) + ":" + Arrays.binarySearch(ints, 4))
  println("equals-prim:" + Arrays.equals(equalIntsA, equalIntsB) + ":" + Arrays.equals(equalIntsA, unequalInts))
  println("equals-ref:" + Arrays.equals(equalRefsA, equalRefsB) + ":" + Arrays.equals(unequalRefsA, unequalRefsB))
  println("hashcode:" + Arrays.hashCode(hashArray))
  println("deephash:" + Arrays.deepHashCode(deep))
  println("fill:" + Arrays.toString(filled))
  println("copyof:" + Arrays.toString(Arrays.copyOf(copySource, 4)))
  println("copyofrange:" + Arrays.toString(Arrays.copyOfRange(rangeSource, 1, 3)))
  println("aslist:" + asList.get(0) + ":" + asList.get(1))
  println("tostring:" + Arrays.toString(bools))
  println("deeptostring:" + Arrays.deepToString(deep))
  println("deeptostring-cycle:" + Arrays.deepToString(cycle))
