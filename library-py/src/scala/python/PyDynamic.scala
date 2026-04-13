package scala.python

import scala.language.dynamics

trait PyDynamic extends PyAny, scala.Dynamic:
  def selectDynamic(name: String): PyDynamic = native
  def updateDynamic(name: String)(value: PyAny): Unit = native
  def applyDynamic(name: String)(args: PyAny*): PyDynamic = native
  def applyDynamicNamed(name: String)(args: (String, PyAny)*): PyDynamic = native
