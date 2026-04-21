import scala.python.runtime.PyThreading

private def joinAll(threads: Array[java.lang.Thread]): Unit =
  var i = 0
  while i < threads.length do
    threads(i).join()
    i += 1

@main def javalibStringBufferThreaded(): Unit =
  val tokens = Array("[A]", "[B]", "[C]", "[D]")
  val iterations = 500
  val start = PyThreading.newEvent()
  val buffer = new java.lang.StringBuffer()
  val threads = new Array[java.lang.Thread](tokens.length)

  var threadIndex = 0
  while threadIndex < threads.length do
    val token = tokens(threadIndex)
    threads(threadIndex) = new java.lang.Thread(
      () => {
        start.waitReady()
        var i = 0
        while i < iterations do
          buffer.append(token)
          i += 1
      },
      "sb-" + threadIndex
    )
    threads(threadIndex).start()
    threadIndex += 1

  start.set()
  joinAll(threads)

  val rendered = buffer.toString()
  val counts = Array(0, 0, 0, 0)
  val chunkSize = tokens(0).length()
  var valid = rendered.length() % chunkSize == 0
  var i = 0
  while valid && i < rendered.length() do
    val chunk = rendered.substring(i, i + chunkSize)
    if chunk == tokens(0) then counts(0) += 1
    else if chunk == tokens(1) then counts(1) += 1
    else if chunk == tokens(2) then counts(2) += 1
    else if chunk == tokens(3) then counts(3) += 1
    else valid = false
    i += chunkSize

  println(
    "threaded:" +
      rendered.length() + ":" +
      counts(0) + ":" +
      counts(1) + ":" +
      counts(2) + ":" +
      counts(3) + ":" +
      valid
  )
