@main def javalibNioBufferExceptions(): Unit =
  val overflow =
    try
      val buf = java.nio.ByteBuffer.allocate(1)
      buf.put(1.toByte)
      buf.put(2.toByte)
      false
    catch
      case _: java.nio.BufferOverflowException => true

  val underflow =
    try
      val buf = java.nio.ByteBuffer.allocate(1)
      buf.flip()
      buf.get()
      false
    catch
      case _: java.nio.BufferUnderflowException => true

  val invalidMark =
    try
      java.nio.ByteBuffer.allocate(1).reset()
      false
    catch
      case _: java.nio.InvalidMarkException => true

  val readOnly =
    try
      java.nio.ByteBuffer.allocate(1).asReadOnlyBuffer().put(1.toByte)
      false
    catch
      case _: java.nio.ReadOnlyBufferException => true

  println("overflow:" + overflow)
  println("underflow:" + underflow)
  println("invalid-mark:" + invalidMark)
  println("read-only:" + readOnly)
