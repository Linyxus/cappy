import scala.python.*

object X:
  val M = "builtins"

@extern(X.M, "someFunc")
def someFunc(): Unit = native // error
