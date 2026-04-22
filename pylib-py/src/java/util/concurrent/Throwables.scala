package java.util.concurrent

import java.lang.ThrowablesSupport

class ExecutionException(primary: Any = null, cause: Throwable | Null = null)
    extends Exception(
      ThrowablesSupport.throwableMessage(primary, cause),
      ThrowablesSupport.throwableCause(primary, cause)
    )

class CancellationException(message: String | Null = null) extends IllegalStateException(message)

class TimeoutException(message: String | Null = null) extends Exception(message)

class BrokenBarrierException(message: String | Null = null) extends Exception(message)

class RejectedExecutionException(primary: Any = null, cause: Throwable | Null = null)
    extends RuntimeException(
      ThrowablesSupport.throwableMessage(primary, cause),
      ThrowablesSupport.throwableCause(primary, cause)
    )
