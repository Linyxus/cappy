package scala

/** Minimal Product trait for the Python backend.
 *
 *  Case classes extend Product; the compiler synthesizes implementations
 *  of `productArity`, `productElement`, `productPrefix`, `canEqual`.
 */
trait Product extends Equals:
  def productArity: Int
  def productElement(n: Int): Any

  // `productIterator` / `productElementNames` signatures exist so the
  // compiler-synthesised case-class members that override them type-check
  // against this trait. The defaults can only run if a user class doesn't
  // override them — for now we fail loudly instead of returning null,
  // pending a real `scala.collection.Iterator` port.
  def productIterator: scala.collection.Iterator[Any] =
    throw new UnsupportedOperationException(
      "Product.productIterator pending scala.collection port"
    )

  def productPrefix: String = ""

  def productElementName(n: Int): String =
    if n >= 0 && n < productArity then ""
    else throw new IndexOutOfBoundsException(n.toString)

  def productElementNames: scala.collection.Iterator[String] =
    throw new UnsupportedOperationException(
      "Product.productElementNames pending scala.collection port"
    )

  def canEqual(that: Any): Boolean
