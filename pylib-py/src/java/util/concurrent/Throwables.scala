package java.util.concurrent

import java.lang.ThrowablesSupport

class ExecutionException(primary: Any = null, cause: Throwable | Null = null)
    extends Exception(
      ThrowablesSupport.throwableMessage(primary, cause),
      ThrowablesSupport.throwableCause(primary, cause)
    ):
  def this(message: String) = this(message: Any, null)
  def this(message: String, cause: Throwable) = this(message: Any, cause)
  def this(cause: Throwable) = this(null, cause)

class CancellationException(message: String | Null = null) extends IllegalStateException(message)

class TimeoutException(message: String | Null = null) extends Exception(message):
  // JDK-shaped ctor overloads. Java's `TimeoutException` exposes `()`
  // and `(String)`; the default-arg primary alone doesn't produce a
  // PyIR `<init>():V` signature.
  def this() = this(null)

class BrokenBarrierException(message: String | Null = null) extends Exception(message)

class RejectedExecutionException(primary: Any = null, cause: Throwable | Null = null)
    extends RuntimeException(
      ThrowablesSupport.throwableMessage(primary, cause),
      ThrowablesSupport.throwableCause(primary, cause)
    )
