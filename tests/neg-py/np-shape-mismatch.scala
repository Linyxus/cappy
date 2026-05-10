// Compile-time enforcement of numpy shape arguments. The minimal numpy
// facade below accepts a shape that is either a single `Int` (1-D shape)
// or a tuple whose elements are all `Int` (any arity, including the
// empty tuple for a 0-D shape).
//
// The constraint is encoded with a tiny typeclass that uses
// `Tuple.Union[T] <:< Int` for the tuple case:
//
//   * `Tuple.Union[(Int, Int)]    = Int`            ⇒ accepted
//   * `Tuple.Union[EmptyTuple]    = Nothing`        ⇒ accepted (0-D)
//   * `Tuple.Union[(Int, String)] = Int | String`   ⇒ rejected
//   * `Tuple.Union[(Double,)]     = Double`         ⇒ rejected
//
// All non-int / mixed-element / non-tuple shapes must fail at the typer.

import scala.python.*

object np:
  opaque type NDArray = PyDynamic
  inline def fromPy(d: PyDynamic): NDArray = d

  @extern("numpy")
  private object _np extends PyDynamic

  opaque type IsShape[T] = Unit
  object IsShape:
    given IsShape[Int] = ()
    given [T <: Tuple](using Tuple.Union[T] <:< Int): IsShape[T] = ()

  def zeros[T: IsShape](shape: T): NDArray = _np.zeros(shape)

@main def npShapeMismatch(): Unit =
  // --- Accepted shapes (must NOT produce errors) ----------------------
  val a = np.zeros(3)              // single Int  → 1-D
  val b = np.zeros(EmptyTuple)     // 0-D
  val c = np.zeros(Tuple1(3))      // (3,)
  val d = np.zeros((2, 3))         // (2, 3)
  val e = np.zeros((2, 3, 4))      // (2, 3, 4)

  // --- Rejected shapes (each line must error) -------------------------
  val f = np.zeros("hello")        // error
  val g = np.zeros((2, "x"))       // error
  val h = np.zeros((1.0, 2.0))     // error
  val i = np.zeros(3.5)            // error
  val j = np.zeros(true)           // error
