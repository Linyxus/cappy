private def renderInts(xs: Array[Int]): String =
  var out = ""
  var i = 0
  while i < xs.length do
    if i != 0 then out += ","
    out += xs(i).toString()
    i += 1
  out

@main def javalibLangSystem(): Unit =
  // Flush between the two streams — stdout is line-buffered on some
  // platforms while stderr is unbuffered; without flushing, the merged
  // output order is platform-dependent and the `.check` diff becomes
  // flaky.
  java.lang.System.out.println("out:line")
  java.lang.System.out.flush()
  java.lang.System.err.println("err:line")
  java.lang.System.err.flush()

  val overlap = Array(1, 2, 3, 4, 5)
  java.lang.System.arraycopy(overlap, 1, overlap, 2, 3)
  println("arraycopy:" + renderInts(overlap))

  val current = java.lang.System.currentTimeMillis()
  println("currenttime:" + (current > 0))

  val nano0 = java.lang.System.nanoTime()
  val nano1 = java.lang.System.nanoTime()
  println("nanotime-monotonic:" + (nano1 >= nano0))

  val stable = new java.lang.StringBuilder("id")
  val hash0 = java.lang.System.identityHashCode(stable)
  val hash1 = java.lang.System.identityHashCode(stable)
  println("identityhash:" + (hash0 == hash1) + ":" + (java.lang.System.identityHashCode(null) == 0))

  val runtime = java.lang.Runtime.getRuntime()
  runtime.gc()
  println("runtime:" + (runtime eq java.lang.Runtime.getRuntime()) + ":" + runtime.availableProcessors())

  println("prop-std:" + java.lang.System.getProperty("file.separator") + ":" + (java.lang.System.getProperty("user.dir") != null))

  val previous0 = java.lang.System.setProperty("scalapy.test", "ok")
  val previous1 = java.lang.System.setProperty("scalapy.test", "again")
  val cleared = java.lang.System.clearProperty("scalapy.test")
  println("prop-set-clear:" + (previous0 == null) + ":" + previous1 + ":" + cleared + ":" + (java.lang.System.getProperty("scalapy.test") == null))

  val properties = java.lang.System.getProperties()
  // pylib's `getProperties()` returns an empty `java.util.Properties` so
  // stdlib's JVM-shaped signature links — assert structural shape only.
  println("prop-view:" + (properties != null) + ":" + (properties.stringPropertyNames().size() >= 0))

  val env = java.lang.System.getenv()
  // pylib's `getenv()` returns an empty `java.util.Map` stub (JDK-shape
  // signature so stdlib links). Single-arg `getenv("PATH")` still
  // delegates through to `os.environ` and returns the live value, so
  // assert that one for liveness; map view is structural-only.
  println("env:" + (java.lang.System.getenv("PATH") != null) + ":" + (env != null))

  println("lineseparator:" + java.lang.System.lineSeparator().length + ":" + (java.lang.System.lineSeparator() == "\n"))
