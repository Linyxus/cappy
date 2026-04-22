package java.security

/** Link-time stub. Stdlib's `sys.SystemProperties` / security paths
 *  reference `AccessController.doPrivileged`. No pos-py test exercises
 *  security manager semantics. */
object AccessController:
  def doPrivileged[T](action: PrivilegedAction[T]): T =
    action.run()
  def doPrivileged[T](action: PrivilegedExceptionAction[T]): T =
    action.run()

trait PrivilegedAction[T]:
  def run(): T

trait PrivilegedExceptionAction[T]:
  def run(): T
