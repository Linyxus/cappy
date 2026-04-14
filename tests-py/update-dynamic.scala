import scala.python.*

@main def updateDynamic(): Unit =
  // Build a `types.SimpleNamespace` instance via Dynamic.
  val types = Dynamic.module("types")
  val obj = types.applyDynamic("SimpleNamespace")()

  // Regular attribute write via `updateDynamic` - valid identifier.
  obj.answer = 42
  // Regular attribute write via `updateDynamic` - non-identifier name.
  obj.applyDynamic("__setattr__")("weird name", "edge")

  val p = Dynamic.attr("print")
  p.applyDynamic("__call__")("answer=", obj.selectDynamic("answer"))
  p.applyDynamic("__call__")("weird=", obj.applyDynamic("__getattribute__")("weird name"))
