package dotty.tools.dotc.config

import dotty.tools.dotc.core.*
import Contexts.*
import Symbols.*

/** Compiler platform active when `-scalapy` is set.
 *
 *  The Python backend has no JVM `invokedynamic`/LambdaMetaFactory, so
 *  every SAM closure must be expanded to an anonymous class before
 *  erasure. Only Scala's own `FunctionN` classes are still treated as
 *  platform-native lambdas (they round-trip through the normal
 *  Closure machinery in `GenPython.genClosure`).
 */
class ScalaPyPlatform extends JavaPlatform {

  override def isSam(cls: ClassSymbol)(using Context): Boolean =
    defn.isFunctionClass(cls)
}
