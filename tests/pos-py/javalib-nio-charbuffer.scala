@main def javalibNioCharBuffer(): Unit =
  val allocated = java.nio.CharBuffer.allocate(5)
  println("allocate:" + allocated.capacity() + ":" + allocated.order())

  val putGet = java.nio.CharBuffer.allocate(5)
  putGet.put('s').put('c').put('a').put('l').put('a')
  putGet.flip()
  println("put-get:" + putGet.get() + ":" + putGet.get() + ":" + putGet.remaining())

  val wrapped = java.nio.CharBuffer.wrap("hello")
  println("wrap:" + wrapped.subSequence(1, 4))

  val base = java.nio.CharBuffer.wrap(Array('x', 'y', 'z', 'w'))
  base.position(1)
  base.limit(3)
  val slice = base.slice()
  slice.put(0, 'Q')
  println("slice:" + slice.toString() + ":" + new String(base.array()))

  val src = java.nio.CharBuffer.wrap("abc")
  val dst = java.nio.CharBuffer.allocate(3)
  val count = src.read(dst)
  dst.flip()
  println("read:" + count + ":" + dst.toString())
