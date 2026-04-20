package scala.python.runtime

import scala.python.{PyAny, extern, native}

object PySys:
  @extern("sys")
  private object sys extends PyAny:
    val stdout: PyTextWriter = native
    val stderr: PyTextWriter = native
    def exit(code: Int): Unit = native

  @extern("sys")
  private class PyTextWriter extends PyAny:
    def write(text: String): Any = native
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

  def exit(code: Int): Unit =
    sys.exit(code)
