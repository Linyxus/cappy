package scala

/** Equality protocol mixed into Product types. */
trait Equals:
  def canEqual(that: Any): Boolean
