/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package java.util

import scala.annotation.targetName
import java.util.function.Supplier

object Objects {

  @inline
  def equals(a: Any, b: Any): Boolean =
    if (a == null) b == null
    else a.equals(b)

  @inline
  def deepEquals(a: Any, b: Any): Boolean = {
    if (a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef]) true
    else if (a == null || b == null) false
    else {
      if (a.isInstanceOf[Array[AnyRef]] && b.isInstanceOf[Array[AnyRef]])
        Arrays.deepEquals(a.asInstanceOf[Array[AnyRef]], b.asInstanceOf[Array[AnyRef]])
      else if (a.isInstanceOf[Array[Long]] && b.isInstanceOf[Array[Long]])
        Arrays.equals(a.asInstanceOf[Array[Long]], b.asInstanceOf[Array[Long]])
      else if (a.isInstanceOf[Array[Int]] && b.isInstanceOf[Array[Int]])
        Arrays.equals(a.asInstanceOf[Array[Int]], b.asInstanceOf[Array[Int]])
      else if (a.isInstanceOf[Array[Short]] && b.isInstanceOf[Array[Short]])
        Arrays.equals(a.asInstanceOf[Array[Short]], b.asInstanceOf[Array[Short]])
      else if (a.isInstanceOf[Array[Byte]] && b.isInstanceOf[Array[Byte]])
        Arrays.equals(a.asInstanceOf[Array[Byte]], b.asInstanceOf[Array[Byte]])
      else if (a.isInstanceOf[Array[Char]] && b.isInstanceOf[Array[Char]])
        Arrays.equals(a.asInstanceOf[Array[Char]], b.asInstanceOf[Array[Char]])
      else if (a.isInstanceOf[Array[Boolean]] && b.isInstanceOf[Array[Boolean]])
        Arrays.equals(a.asInstanceOf[Array[Boolean]], b.asInstanceOf[Array[Boolean]])
      else if (a.isInstanceOf[Array[Float]] && b.isInstanceOf[Array[Float]])
        Arrays.equals(a.asInstanceOf[Array[Float]], b.asInstanceOf[Array[Float]])
      else if (a.isInstanceOf[Array[Double]] && b.isInstanceOf[Array[Double]])
        Arrays.equals(a.asInstanceOf[Array[Double]], b.asInstanceOf[Array[Double]])
      else
        Objects.equals(a, b)
    }
  }

  @inline
  def hashCode(o: Any): Int =
    if (o == null) 0
    else if (o.isInstanceOf[String]) o.asInstanceOf[String].hashCode()
    else o.hashCode()

  @inline
  def hash(values: Array[AnyRef]): Int =
    Arrays.hashCode(values)

  @inline
  @targetName("toStringAny")
  def toString(o: Any): String =
    String.valueOf(o)

  @inline
  @targetName("toStringOrDefault")
  def toString(o: Any, nullDefault: String): String =
    if (o == null) nullDefault
    else o.toString

  @inline
  def compare[T](a: T, b: T, c: Comparator[_ >: T]): Int =
    if (a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef]) 0
    else c.compare(a, b)

  // Intrinsic
  @inline def requireNonNull[T](obj: T): T =
    if (obj == null) throw new NullPointerException()
    else obj

  // Intrinsic
  @inline
  def requireNonNull[T](obj: T, message: String): T =
    if (obj == null) throwNPEWithMessage(message)
    else obj

  @inline
  def isNull(obj: Any): Boolean =
    obj == null

  @inline
  def nonNull(obj: Any): Boolean =
    obj != null

  // Intrinsic
  @inline
  def requireNonNull[T](obj: T, messageSupplier: Supplier[String]): T =
    if (obj == null) throwNPEWithMessage(messageSupplier)
    else obj

  /* The following methods are our best attempt to deal with the overloads of
   * `requireNonNull` with an explicit message. We want to trigger a UB NPE.
   * However, if we do that, we lose the `message`. This is fine for the
   * Unchecked behavior, debatable for Fatal, and plain wrong for Compliant.
   *
   * To recover the message in Compliant mode, we immediately catch a genuine
   * NPE if that is what the UB throws, and rethrow a genuine NPE with the
   * correct message.
   *
   * In Fatal mode, there is unfortunately nothing we can do. We have no way
   * of constructing another UndefinedBehaviorError with a different cause.
   *
   * ---
   *
   * For the overload with a Supplier, there is an additional semantic decision
   * to make: when `obj == null`, when exactly do we call
   * `messageSupplier.get()`? There are two valid choices:
   *
   * - always, regardless of the checked behavior, or
   * - only if and when we actually need a message, which would only happen in
   *   Compliant mode.
   *
   * We choose the latter alternative, because it is better optimizable.
   * A program that would rely on the supplier being evaluated in non-Compliant
   * mode would be dubious anyway. That would only result in well-defined
   * semantics if its `get()` method threw an exception itself.
   *
   * ---
   *
   * The methods are `@noinline` because they are in a slow path anyway.
   * We don't want the try..catch'es to appear at call site. That pollutes
   * performance for the entire enclosing function in some engines.
   */

  @noinline private def throwNPEWithMessage(message: String): Nothing =
    throw new NullPointerException()

  @noinline private def throwNPEWithMessage(messageSupplier: Supplier[String]): Nothing =
    throw new NullPointerException()
}
