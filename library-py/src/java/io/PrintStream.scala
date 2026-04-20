package java.io

import java.util.Formatter

import scala.python.runtime.PySys

class PrintStream(_out: OutputStream, autoFlush: Boolean, encoding: String | Null)
    extends FilterOutputStream(_out) with Appendable with Closeable {

  private var encoderRef: OutputStreamWriter | Null = null

  private var closing: Boolean = false
  private var closed: Boolean = false
  private var errorFlag: Boolean = false

  def this(out: OutputStream) =
    this(out, false, null)

  def this(out: OutputStream, autoFlush: Boolean) =
    this(out, autoFlush, null)

  def this(file: File) =
    this(new BufferedOutputStream(new FileOutputStream(file)))

  def this(file: File, csn: String) =
    this(new BufferedOutputStream(new FileOutputStream(file)), false, csn)

  def this(fileName: String) =
    this(new File(fileName))

  def this(fileName: String, csn: String) =
    this(new File(fileName), csn)

  override def flush(): Unit =
    ensureOpenAndTrapIOExceptions(() => out.flush())

  override def close(): Unit = trapIOExceptions { () =>
    if (!closing) {
      closing = true
      if (encoderRef != null)
        encoder().close()
      flush()
      closed = true
      out.close()
    }
  }

  def checkError(): Boolean = {
    if (closed) {
      errorFlag
    } else {
      flush()
      errorFlag || (out match {
        case out: PrintStream => out.checkError()
        case _                => false
      })
    }
  }

  protected[io] def setError(): Unit = errorFlag = true
  protected[io] def clearError(): Unit = errorFlag = false

  override def write(b: Int): Unit =
    ensureOpenAndTrapIOExceptions { () =>
      out.write(b)
      if (autoFlush && b == '\n')
        flush()
    }

  override def write(buf: Array[Byte], off: Int, count: Int): Unit =
    ensureOpenAndTrapIOExceptions { () =>
      out.write(buf, off, count)
      if (autoFlush)
        flush()
    }

  def print(b: Boolean): Unit = printString(String.valueOf(b))
  def print(c: Char): Unit = printString(String.valueOf(c))
  def print(i: Int): Unit = printString(String.valueOf(i))
  def print(l: Long): Unit = printString(String.valueOf(l))
  def print(f: Float): Unit = printString(String.valueOf(f))
  def print(d: Double): Unit = printString(String.valueOf(d))
  def print(s: String): Unit = printString(if (s == null) "null" else s)
  def print(obj: AnyRef): Unit = printString(String.valueOf(obj))

  private def printString(s: String): Unit =
    ensureOpenAndTrapIOExceptions { () =>
      encoder().write(s)
      if (autoFlush)
        flush()
    }

  def print(s: Array[Char]): Unit =
    ensureOpenAndTrapIOExceptions { () =>
      encoder().write(s)
      if (autoFlush)
        flush()
    }

  def println(): Unit = ensureOpenAndTrapIOExceptions { () =>
    encoder().write('\n')
    if (autoFlush)
      flush()
  }

  def println(b: Boolean): Unit = { print(b); println() }
  def println(c: Char): Unit = { print(c); println() }
  def println(i: Int): Unit = { print(i); println() }
  def println(l: Long): Unit = { print(l); println() }
  def println(f: Float): Unit = { print(f); println() }
  def println(d: Double): Unit = { print(d); println() }
  def println(s: Array[Char]): Unit = { print(s); println() }
  def println(s: String): Unit = { print(s); println() }
  def println(obj: AnyRef): Unit = { print(obj); println() }

  def printf(fmt: String, args: Array[Object]): PrintStream =
    format(fmt, args)

  def format(fmt: String, args: Array[Object]): PrintStream = {
    new Formatter(this).format(fmt, args)
    this
  }

  def append(csq: CharSequence): PrintStream = {
    print(if (csq == null) "null" else csq.toString)
    this
  }

  def append(csq: CharSequence, start: Int, end: Int): PrintStream = {
    val csq1 = if (csq == null) "null" else csq
    print(csq1.subSequence(start, end).toString)
    this
  }

  def append(c: Char): PrintStream = {
    print(c)
    this
  }

  private def encoder(): OutputStreamWriter = {
    if (encoderRef == null)
      encoderRef = new OutputStreamWriter(this, encoding)
    encoderRef.asInstanceOf[OutputStreamWriter]
  }

  private def trapIOExceptions(body: Runnable): Unit =
    try body.run()
    catch case _: IOException => setError()

  private def ensureOpenAndTrapIOExceptions(body: Runnable): Unit =
    if (closed) setError()
    else trapIOExceptions(body)
}

object PrintStream {
  private final class PyBufferOutputStream(isErr: Boolean) extends OutputStream {
    override def write(b: Int): Unit =
      write(Array((b & 0xff).toByte))

    override def write(b: Array[Byte], off: Int, count: Int): Unit = {
      BoundsChecks.checkOffsetCount(off, count, b.length)
      val out =
        if (off == 0 && count == b.length) b
        else java.util.Arrays.copyOfRange(b, off, off + count)
      if (isErr) PySys.stderr_buffer_write(out)
      else PySys.stdout_buffer_write(out)
    }

    override def flush(): Unit =
      if (isErr) PySys.stderr_buffer_flush()
      else PySys.stdout_buffer_flush()
  }

  def stdout(): PrintStream =
    new PrintStream(new PyBufferOutputStream(false), true, null)

  def stderr(): PrintStream =
    new PrintStream(new PyBufferOutputStream(true), true, null)
}
