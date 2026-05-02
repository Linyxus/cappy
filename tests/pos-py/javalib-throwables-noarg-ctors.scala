// Wave 5 item 09: regression coverage for the no-arg ctors of
// `OutOfMemoryError`, `IndexOutOfBoundsException`, and `AssertionError`.
// Before the fix, these calls failed at link time with
// `Unresolved java.lang.<Foo>.<init>():V` because the pylib ports only
// exposed message-bearing aux ctors despite the JVM contract providing
// a public no-arg ctor on each.

import java.lang.{AssertionError, IndexOutOfBoundsException, OutOfMemoryError}

@main def javalibThrowablesNoargCtors(): Unit =
  // OutOfMemoryError(): JVM contract is `getMessage() == null`.
  val oom = new OutOfMemoryError()
  println("oom-message-is-null:" + (oom.getMessage() == null))
  println("oom-class:" + oom.getClass().getName())

  // IndexOutOfBoundsException(): JVM contract is `getMessage() == null`.
  val ioobe = new IndexOutOfBoundsException()
  println("ioobe-message-is-null:" + (ioobe.getMessage() == null))
  println("ioobe-class:" + ioobe.getClass().getName())

  // AssertionError(): JVM contract is `getMessage() == null`. This must
  // be distinct from `new AssertionError(null)` (which produces "null"
  // per `String.valueOf((Object) null)`).
  val ae = new AssertionError()
  println("ae-message-is-null:" + (ae.getMessage() == null))
  println("ae-class:" + ae.getClass().getName())

  // Throw + catch round trip exercises that the linker resolves the
  // no-arg ctor at the throw site.
  val caught: String =
    try
      throw new OutOfMemoryError()
    catch
      case e: OutOfMemoryError =>
        if e.getMessage() == null then "oom-caught-null"
        else "oom-caught:" + e.getMessage()
  println(caught)

  val caught2: String =
    try
      throw new IndexOutOfBoundsException()
    catch
      case e: IndexOutOfBoundsException =>
        if e.getMessage() == null then "ioobe-caught-null"
        else "ioobe-caught:" + e.getMessage()
  println(caught2)

  val caught3: String =
    try
      throw new AssertionError()
    catch
      case e: AssertionError =>
        if e.getMessage() == null then "ae-caught-null"
        else "ae-caught:" + e.getMessage()
  println(caught3)
