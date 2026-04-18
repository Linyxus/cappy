package scala.runtime

import scala.collection.immutable.ArraySeq

/** Minimal ScalaRunTime for the Python backend.
 *
 *  Case-class synthesis and other compiler-generated code calls
 *  `ScalaRunTime._toString`, `_hashCode`, `_equals`.
 */
object ScalaRunTime:
  def _toString(x: Any): String =
    if x == null then "null" else x.toString

  def _hashCode(x: Any): Int =
    if x == null then 0 else x.hashCode

  def _equals(x: Any, y: Any): Boolean =
    if x == null then y == null
    else x.equals(y)

  def hash(x: Any): Int =
    if x == null then 0 else x.hashCode

  /** Needed for dynamic-call varargs lowering inside `library-py`.
   *
   *  Scala rewrites `foo(a, b)` for an `Any*` parameter to
   *  `foo(ScalaRunTime.genericWrapArray(Array(a, b)))`.
   */
  def genericWrapArray[T](xs: Array[T]): ArraySeq[T] =
    if xs == null then null.asInstanceOf[ArraySeq[T]]
    else ArraySeq.unsafeWrapArray(xs)

  def wrapRefArray[T <: AnyRef | Null](xs: Array[T]): ArraySeq[T] =
    if xs == null then null.asInstanceOf[ArraySeq[T]]
    else new ArraySeq.ofRef[T](xs)
