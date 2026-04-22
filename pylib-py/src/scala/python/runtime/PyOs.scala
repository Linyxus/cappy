package scala.python.runtime

import scala.python.{PyAny, extern, native}

object PyOs:
  @extern("os")
  private object os extends PyAny:
    val name: String = native
    val environ: PyEnviron = native
    def getcwd(): String = native

  @extern("os")
  private class PyEnviron extends PyAny:
    def get(key: String): String | Null = native

  def os_name(): String =
    os.name

  def getcwd(): String =
    os.getcwd()

  def getenv(key: String): String | Null =
    os.environ.get(key)
