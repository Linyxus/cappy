package scala

/** Minimal Product trait for the Python backend.
 *
 *  Case classes extend Product; the compiler synthesizes implementations
 *  of `productArity`, `productElement`, `productPrefix`, `canEqual`.
 */
trait Product extends Equals:
  def productArity: Int
  def productElement(n: Int): Any
  def productPrefix: String = ""
  def canEqual(that: Any): Boolean
