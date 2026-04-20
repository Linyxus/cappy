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

  // The `wrap*Array` family below mirrors stdlib `ScalaRunTime`.
  // Frontend lowering of varargs on primitive/ref element types rewrites
  // `foo(a, b)` (where `foo` takes `T*`) into
  // `foo(ScalaRunTime.wrap<T>Array(Array(a, b)))`. Call sites are
  // compiler-synthesized, so these definitions have to exist by name
  // even though library-py never calls them directly. Without them,
  // library-py code that uses primitive varargs (e.g. _String split/
  // formatter paths via the regex port) fails to link.
  def wrapRefArray[T <: AnyRef | Null](xs: Array[T]): ArraySeq[T] =
    if xs == null then null.asInstanceOf[ArraySeq[T]]
    else new ArraySeq.ofRef[T](xs)

  def wrapIntArray(xs: Array[Int]): ArraySeq[Int] =
    if xs == null then null.asInstanceOf[ArraySeq[Int]]
    else new ArraySeq.ofInt(xs)

  def wrapLongArray(xs: Array[Long]): ArraySeq[Long] =
    if xs == null then null.asInstanceOf[ArraySeq[Long]]
    else new ArraySeq.ofLong(xs)

  def wrapDoubleArray(xs: Array[Double]): ArraySeq[Double] =
    if xs == null then null.asInstanceOf[ArraySeq[Double]]
    else new ArraySeq.ofDouble(xs)

  def wrapFloatArray(xs: Array[Float]): ArraySeq[Float] =
    if xs == null then null.asInstanceOf[ArraySeq[Float]]
    else new ArraySeq.ofFloat(xs)

  def wrapCharArray(xs: Array[Char]): ArraySeq[Char] =
    if xs == null then null.asInstanceOf[ArraySeq[Char]]
    else new ArraySeq.ofChar(xs)

  def wrapByteArray(xs: Array[Byte]): ArraySeq[Byte] =
    if xs == null then null.asInstanceOf[ArraySeq[Byte]]
    else new ArraySeq.ofByte(xs)

  def wrapShortArray(xs: Array[Short]): ArraySeq[Short] =
    if xs == null then null.asInstanceOf[ArraySeq[Short]]
    else new ArraySeq.ofShort(xs)

  def wrapBooleanArray(xs: Array[Boolean]): ArraySeq[Boolean] =
    if xs == null then null.asInstanceOf[ArraySeq[Boolean]]
    else new ArraySeq.ofBoolean(xs)

  def wrapUnitArray(xs: Array[Unit]): ArraySeq[Unit] =
    if xs == null then null.asInstanceOf[ArraySeq[Unit]]
    else ArraySeq.unsafeWrapArray(xs)
