import scala.python.*

@main def keywordAttrs(): Unit =
  val builtins = Dynamic.module("builtins")

  // Keyword-named attributes must fall back to `getattr` in generated Python.
  val t = builtins.selectDynamic("True")
  val f = builtins.selectDynamic("False")

  // Non-literal names take the generic `getattr` fallback path.
  val name = "True"
  val t2 = builtins.selectDynamic(name)

  builtins.print("t=", t)
  builtins.print("f=", f)
  builtins.print("t2=", t2)
