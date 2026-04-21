import java.util.concurrent.ThreadLocalRandom

@main def javalibConcurrentThreadLocalRandom(): Unit =
  val mainFirst = ThreadLocalRandom.current()
  val mainSecond = ThreadLocalRandom.current()

  var childSame = false
  var childDifferentFromMain = false

  val child = new java.lang.Thread(() =>
    val childFirst = ThreadLocalRandom.current()
    val childSecond = ThreadLocalRandom.current()
    childSame = childFirst eq childSecond
    childDifferentFromMain = !(childFirst eq mainFirst)
  )

  child.start()
  child.join()

  println(
    "threadlocalrandom:" +
      (mainFirst eq mainSecond) + ":" +
      childSame + ":" +
      childDifferentFromMain
  )
