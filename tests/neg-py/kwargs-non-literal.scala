import scala.python.*

// Phase 2 gap: `applyDynamicNamed` cannot accept runtime-computed keyword
// names because Python named-call syntax requires static identifiers.
// The backend emits a clear error at `genApplyDynamicNamedCall` when a
// tuple's first element is not a literal string.

@main def kwargsNonLiteral(): Unit =
  val d = Dynamic.module("builtins")
  val keyName = "end"
  // Explicit applyDynamicNamed with a computed key - must fail.
  d.applyDynamicNamed("print")(("", "hi"), (keyName, "!!")) // error
