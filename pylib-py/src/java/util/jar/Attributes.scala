package java.util.jar

// Minimal port of java.util.jar.Attributes for the ScalaPy backend.
// Only the inner `Name` class is needed: scala.util.Properties references
// it via `import java.util.jar.Attributes.{Name => AttributeName}` and
// instantiates it once for the `ScalaCompilerVersion` constant.
//
// Note: the JDK's `Attributes.Name.equals` is case-insensitive (it's used
// as a manifest header key). We use case-sensitive comparison here because
// the only call site never compares Names; this is enough to satisfy the
// linker reference and keep the constructor side-effect-free.
object Attributes {

  final class Name(name: String) {
    override def toString(): String = name

    override def equals(other: Any): Boolean = other match {
      case that: Name => that.toString() == name
      case _          => false
    }

    override def hashCode(): Int = name.hashCode()
  }

}
