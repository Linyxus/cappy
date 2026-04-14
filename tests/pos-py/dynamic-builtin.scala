import scala.python.*

@main def dynamicBuiltin(): Unit =
  val builtins = Dynamic.module("builtins")
  builtins.print("hello from dynamic")

  val moduleName = "builtins"
  Dynamic.module(moduleName).print("hello from importlib")
