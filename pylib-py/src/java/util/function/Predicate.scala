package java.util.function

trait Predicate[T]:
  def test(t: T): Boolean

  def and(other: Predicate[_ >: T]): Predicate[T] =
    new Predicate[T]:
      def test(t: T): Boolean =
        Predicate.this.test(t) && other.test(t)

  def negate(): Predicate[T] =
    new Predicate[T]:
      def test(t: T): Boolean =
        !Predicate.this.test(t)

  def or(other: Predicate[_ >: T]): Predicate[T] =
    new Predicate[T]:
      def test(t: T): Boolean =
        Predicate.this.test(t) || other.test(t)

object Predicate:
  def isEqual[T](targetRef: Any): Predicate[T] =
    new Predicate[T]:
      def test(t: T): Boolean =
        if targetRef == null then t == null
        else targetRef.equals(t)
