package scala

// Override of the stdlib `scala.NotImplementedError`, required for
// scala-library-py to produce the matching `.pyir`. The class is load-
// bearing even when no test calls `???` directly: `PyReachability` walks
// every method body of every reachable class, and `scala.Predef.???`
// contains `throw new NotImplementedError`, enqueuing this class as soon
// as `Predef` is reachable (i.e. almost always, via `println`).
final class NotImplementedError(msg: String = "an implementation is missing")
    extends Error(msg)
