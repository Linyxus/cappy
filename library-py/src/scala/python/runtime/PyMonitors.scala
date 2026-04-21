package scala.python.runtime

import scala.python.{PyAny, PyDynamic, extern, name, native}

object PyMonitors:
  @extern("__main__")
  private object runtime extends PyAny:
    @name("_scpy_monitor_for")
    def monitorFor0(obj: Any): PyDynamic = native

    @name("_scpy_condition_for")
    def conditionFor0(obj: Any): PyDynamic = native

  def monitorFor(obj: Any): PyRLock =
    new PyRLock(runtime.monitorFor0(obj))

  def conditionFor(obj: Any): PyCondition =
    new PyCondition(runtime.conditionFor0(obj))
