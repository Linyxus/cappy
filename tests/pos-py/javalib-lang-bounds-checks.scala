package java.lang

private def caught(body: () => Unit): String =
  try
    body()
    "ok"
  catch
    case _: IllegalArgumentException => "iae"
    case _: IndexOutOfBoundsException => "ioobe"

@main def javalibLangBoundsChecks(): Unit =
  println("capacity:" + caught(() => BoundsChecks.checkCapacity(-1)))
  println("index:" + caught(() => BoundsChecks.checkIndex(3, 3)))
  println("inclusive:" + caught(() => BoundsChecks.checkIndexInclusive(4, 3)))
  println("range:" + caught(() => BoundsChecks.checkStartEnd(2, 5, 4)))
  println("offset:" + caught(() => BoundsChecks.checkOffsetCount(2, 3, 4)))
