package scala.reflect

/** Marker trait mixed into enum classes by the compiler. */
trait Enum extends scala.Product with java.io.Serializable:
  def ordinal: Int
