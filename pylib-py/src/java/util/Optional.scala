package java.util

import java.util.function.*
import java.util.Objects.requireNonNull

final class Optional[T] private (value: T) {
  import Optional.*

  def get(): T =
    if !isPresent() then throw new NoSuchElementException()
    else value

  def isPresent(): Boolean = value != null

  def isEmpty(): Boolean = value == null

  def ifPresent(action: Consumer[_ >: T]): Unit =
    if isPresent() then action.accept(value)

  def ifPresentOrElse(action: Consumer[_ >: T], emptyAction: Runnable): Unit =
    if isPresent() then action.accept(value)
    else emptyAction.run()

  def filter(predicate: Predicate[_ >: T]): Optional[T] =
    if isEmpty() || predicate.test(value) then this else Optional.empty()

  def map[U](mapper: Function[_ >: T, _ <: U]): Optional[U] =
    if isEmpty() then emptyCast[U](this) else Optional.ofNullable(mapper(value))

  def flatMap[U](mapper: Function[_ >: T, Optional[_ <: U]]): Optional[U] =
    if isEmpty() then emptyCast[U](this) else upcast[U](mapper(value))

  def or(supplier: Supplier[_ <: Optional[_ <: T]]): Optional[T] =
    if isPresent() then this else upcast[T](supplier.get())

  def orElse(other: T): T =
    if isPresent() then value else other

  def orElseGet(supplier: Supplier[_ <: T]): T =
    if isPresent() then value else supplier.get()

  def orElseThrow(): T =
    if isPresent() then value else throw new NoSuchElementException()

  def orElseThrow[X <: Throwable](exceptionSupplier: Supplier[_ <: X]): T =
    if isPresent() then value else throw exceptionSupplier.get()

  override def equals(obj: Any): Boolean =
    obj match
      case opt: Optional[?] =>
        (!isPresent() && !opt.isPresent()) ||
          (isPresent() && opt.isPresent() && value.equals(opt.get()))
      case _ => false

  override def hashCode(): Int =
    if isPresent() then value.hashCode() else 0

  override def toString(): String =
    if isPresent() then s"Optional[$value]" else "Optional.empty"
}

object Optional:
  def empty[T](): Optional[T] = new Optional[T](null.asInstanceOf[T])

  def of[T](value: T): Optional[T] =
    new Optional[T](requireNonNull(value))

  def ofNullable[T](value: T): Optional[T] =
    new Optional[T](value)

  private def upcast[T](optional: Optional[_ <: T]): Optional[T] =
    optional.asInstanceOf[Optional[T]]

  private def emptyCast[T](empty: Optional[?]): Optional[T] =
    empty.asInstanceOf[Optional[T]]
