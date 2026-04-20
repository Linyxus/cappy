/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package java.io

import java.util.Formatter

class PrintWriter(protected[io] var out: Writer, autoFlush: Boolean)
    extends Writer {

  def this(out: Writer) = this(out, false)

  def this(out: OutputStream, autoFlush: Boolean) =
    this(new OutputStreamWriter(out), autoFlush)

  def this(out: OutputStream) =
    this(out, false)

  def this(file: File) =
    this(new BufferedOutputStream(new FileOutputStream(file)))

  def this(file: File, csn: String) =
    this(new OutputStreamWriter(new BufferedOutputStream(new FileOutputStream(file)), csn))

  def this(fileName: String) =
    this(new File(fileName))

  def this(fileName: String, csn: String) =
    this(new File(fileName), csn)

  private var closed: Boolean = false
  private var errorFlag: Boolean = false

  def flush(): Unit =
    ensureOpenAndTrapIOExceptions(() => out.flush())

  def close(): Unit = trapIOExceptions { () =>
    if (!closed) {
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
        case out: PrintWriter => out.checkError()
        case _                => false
      })
    }
  }

  protected[io] def setError(): Unit = errorFlag = true
  protected[io] def clearError(): Unit = errorFlag = false

  override def write(c: Int): Unit =
    ensureOpenAndTrapIOExceptions(() => out.write(c))

  override def write(buf: Array[Char], off: Int, len: Int): Unit =
    ensureOpenAndTrapIOExceptions(() => out.write(buf, off, len))

  override def write(buf: Array[Char]): Unit =
    ensureOpenAndTrapIOExceptions(() => out.write(buf))

  override def write(s: String, off: Int, len: Int): Unit =
    ensureOpenAndTrapIOExceptions(() => out.write(s, off, len))

  override def write(s: String): Unit =
    ensureOpenAndTrapIOExceptions(() => out.write(s))

  def print(b: Boolean): Unit = write(String.valueOf(b))
  def print(c: Char): Unit = write(c)
  def print(i: Int): Unit = write(String.valueOf(i))
  def print(l: Long): Unit = write(String.valueOf(l))
  def print(f: Float): Unit = write(String.valueOf(f))
  def print(d: Double): Unit = write(String.valueOf(d))
  def print(s: Array[Char]): Unit = write(s)
  def print(s: String): Unit = write(if (s == null) "null" else s)
  def print(obj: AnyRef): Unit = write(String.valueOf(obj))

  def println(): Unit = {
    write('\n')
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

  def printf(fmt: String, args: Array[Object]): PrintWriter =
    format(fmt, args)

  def format(fmt: String, args: Array[Object]): PrintWriter = {
    new Formatter(this).format(fmt, args)
    if (autoFlush)
      flush()
    this
  }

  override def append(csq: CharSequence): PrintWriter = {
    super.append(csq)
    this
  }

  override def append(csq: CharSequence, start: Int, end: Int): PrintWriter = {
    super.append(csq, start, end)
    this
  }

  override def append(c: Char): PrintWriter = {
    super.append(c)
    this
  }

  private def trapIOExceptions(body: Runnable): Unit =
    try body.run()
    catch case _: IOException => setError()

  private def ensureOpenAndTrapIOExceptions(body: Runnable): Unit =
    if (closed) setError()
    else trapIOExceptions(body)
}

