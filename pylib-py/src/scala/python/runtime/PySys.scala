package scala.python.runtime

import scala.python.{PyAny, PyDynamic, extern, native}

object PySys:
  @extern("sys")
  private object sys extends PyAny:
    val stdout: PyTextWriter = native
    val stderr: PyTextWriter = native
    def exit(code: Int): Unit = native

  @extern("sys")
  private class PyTextWriter extends PyAny:
    val buffer: PyBinaryWriter = native
    def write(text: String): Any = native
    def flush(): Unit = native

  @extern("sys")
  private class PyBinaryWriter extends PyAny:
    def write(data: PyDynamic): Any = native
    def flush(): Unit = native

  def stdout_write(text: String): Unit =
    sys.stdout.write(text)
    ()

  def stderr_write(text: String): Unit =
    sys.stderr.write(text)
    ()

  def stdout_flush(): Unit =
    sys.stdout.flush()

  def stderr_flush(): Unit =
    sys.stderr.flush()

  def stdout_buffer_write(bytes: Array[Byte]): Unit =
    sys.stdout.buffer.write(PyBytes.toPyBytes(bytes))
    ()

  def stderr_buffer_write(bytes: Array[Byte]): Unit =
    sys.stderr.buffer.write(PyBytes.toPyBytes(bytes))
    ()

  def stdout_buffer_flush(): Unit =
    sys.stdout.buffer.flush()

  def stderr_buffer_flush(): Unit =
    sys.stderr.buffer.flush()

  def exit(code: Int): Unit =
    sys.exit(code)
