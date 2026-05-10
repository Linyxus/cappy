// A complete, runnable Scala facade for `numpy.ndarray`.
//
// `NDArray` is an **opaque type alias** for `PyDynamic`. At runtime an
// `NDArray` value IS the underlying Python `numpy.ndarray` — there is no
// Scala wrapper instance, no extra field to deref, and no per-call
// allocation. The opaqueness is purely a compile-time discipline that
// hides the raw `PyDynamic` API and gives callers a typed surface.
//
// Why the extensions live OUTSIDE the `np` object
// -----------------------------------------------
// Inside the defining object the alias is transparent: `NDArray` and
// `PyDynamic` are the same type, so `a.foo` on a value of either static
// type can match an extension method on `NDArray` — recursing forever
// instead of falling through to `PyDynamic`'s `Dynamic` dispatch.
// Defining the extensions in a sibling scope (`object NDArrayOps`) makes
// the alias opaque at the point where the bodies are checked, so
// `a.py.foo(...)` goes through `selectDynamic` / `applyDynamic` exactly
// like raw `PyDynamic`. The bridge is a pair of inline no-op casts on
// `np`: `asPy: NDArray => PyDynamic` and `fromPy: PyDynamic => NDArray`,
// each alias-transparent inside `np`.
//
// Coverage targets the 99% of typical numpy use:
//   * Constructors (free): array, zeros, ones, full, empty, arange,
//     linspace, eye, identity, *_like, asarray.
//   * Attributes:   shape, ndim, size, dtype, itemsize, nbytes, T, real, imag.
//   * Reshaping:    reshape, ravel, flatten, transpose, squeeze,
//                   swapaxes, expand_dims, moveaxis.
//   * Indexing:     apply(i), apply(i, j), apply(i, j, k), `range` slice,
//                   item, tolist, take, update.
//   * Reductions:   sum, mean, min, max, std, variance, prod, all, any,
//                   argmin, argmax, cumsum, cumprod (with optional axis).
//   * Universal funcs (free `np.*`): sqrt, exp, log/log2/log10, abs, sign,
//                   sin/cos/tan, floor/ceil/round, clip, maximum, minimum.
//   * Linear algebra: dot, matmul (`@@`), inner, outer, cross, trace.
//   * Combining:    concatenate, stack, vstack, hstack, split.
//   * Search/sort:  where, sort, argsort, unique, isnan, isfinite,
//                   array_equal, allclose.
//   * Operators:    +, -, *, /, %, **, unary -, @@, <, >, <=, >=,
//                   plus elementwise equ/neq.
//   * Type/copy:    astype, copy.

import scala.python.*

// File-private extern surface, accessible to both `np` and the sibling
// extension object below.

@extern("numpy")
private object _np extends PyDynamic

@extern("operator")
private object _pyOp extends PyAny:
  @name("getitem") def getItem(c: Any, k: Any): PyDynamic = native
  @name("setitem") def setItem(c: Any, k: Any, v: Any): Unit = native

@extern("builtins")
private object _pyBuiltins extends PyAny:
  @name("len")   def lengthOf(c: Any): Int = native
  @name("float") def toFloat(v: Any): Double = native
  @name("int")   def toInt(v: Any): Int = native
  @name("slice") def slice(start: Any, stop: Any, step: Any): PyDynamic = native

// ====================================================================
//                           The opaque type
// ====================================================================

object np:

  /** Typed view over a Python `numpy.ndarray`. Erases to `PyDynamic`. */
  opaque type NDArray = PyDynamic

  /** Inline no-op cast. Public so the sibling extension object can
   *  reach the underlying `PyDynamic` and the alias-transparent
   *  resolution stays inside `np`. */
  inline def asPy(a: NDArray): PyDynamic = a
  inline def fromPy(d: PyDynamic): NDArray = d

  /** Evidence that `T` is a valid numpy shape: either `Int` (a 1-D
   *  shape) or a tuple whose elements are all `Int` (any arity, including
   *  empty for a 0-D shape).
   *
   *  The tuple case uses `Tuple.Union[T] <:< Int` — `Tuple.Union[T]`
   *  collapses the element types to a union, and `<:< Int` requires that
   *  union to be a subtype of `Int`. So `(Int, Int)` (union `Int`) is
   *  accepted, `(Int, String)` (union `Int | String`) is rejected.
   *  `Tuple.Union[EmptyTuple] = Nothing` and `Nothing <:< Int` is true,
   *  so the empty tuple — numpy's 0-D shape — is also accepted. */
  opaque type IsShape[T] = Unit
  object IsShape:
    given IsShape[Int] = ()
    given [T <: Tuple](using Tuple.Union[T] <:< Int): IsShape[T] = ()

  // ----- Constants ------------------------------------------------------

  def pi: Double  = _np.pi.asInstanceOf[Double]
  def e: Double   = _np.e.asInstanceOf[Double]
  def inf: Double = _np.inf.asInstanceOf[Double]
  def nan: Double = _np.nan.asInstanceOf[Double]

  // ----- Constructors ---------------------------------------------------

  def array(values: Any): NDArray = _np.array(values)
  def array(values: Any, dtype: String): NDArray =
    _np.array(values, dtype = dtype)
  def asarray(values: Any): NDArray = _np.asarray(values)
  def zeros[T: IsShape](shape: T): NDArray = _np.zeros(shape)
  def zeros[T: IsShape](shape: T, dtype: String): NDArray =
    _np.zeros(shape, dtype = dtype)
  def ones[T: IsShape](shape: T): NDArray = _np.ones(shape)
  def ones[T: IsShape](shape: T, dtype: String): NDArray =
    _np.ones(shape, dtype = dtype)
  def full[T: IsShape](shape: T, fillValue: Any): NDArray =
    _np.full(shape, fillValue)
  def full[T: IsShape](shape: T, fillValue: Any, dtype: String): NDArray =
    _np.full(shape, fillValue, dtype = dtype)
  def empty[T: IsShape](shape: T): NDArray = _np.empty(shape)
  def eye(n: Int): NDArray         = _np.eye(n)
  def eye(n: Int, m: Int): NDArray = _np.eye(n, m)
  def identity(n: Int): NDArray    = _np.identity(n)

  def arange(stop: Int): NDArray = _np.arange(stop)
  def arange(start: Int, stop: Int): NDArray = _np.arange(start, stop)
  def arange(start: Int, stop: Int, step: Int): NDArray =
    _np.arange(start, stop, step)
  def arange(stop: Double): NDArray = _np.arange(stop)
  def arange(start: Double, stop: Double, step: Double): NDArray =
    _np.arange(start, stop, step)

  def linspace(start: Double, stop: Double, num: Int): NDArray =
    _np.linspace(start, stop, num = num)
  def linspace(start: Double, stop: Double, num: Int, endpoint: Boolean): NDArray =
    _np.linspace(start, stop, num = num, endpoint = endpoint)

  def zerosLike(a: NDArray): NDArray = _np.zeros_like(a)
  def onesLike(a: NDArray): NDArray  = _np.ones_like(a)
  def fullLike(a: NDArray, value: Any): NDArray = _np.full_like(a, value)
  def emptyLike(a: NDArray): NDArray = _np.empty_like(a)

  // ----- Universal functions (free, elementwise) -----------------------

  def sqrt(a: NDArray): NDArray  = _np.sqrt(a)
  def exp(a: NDArray): NDArray   = _np.exp(a)
  def log(a: NDArray): NDArray   = _np.log(a)
  def log2(a: NDArray): NDArray  = _np.log2(a)
  def log10(a: NDArray): NDArray = _np.log10(a)
  def absOf(a: NDArray): NDArray = _np.abs(a)
  def sign(a: NDArray): NDArray  = _np.sign(a)
  def sin(a: NDArray): NDArray   = _np.sin(a)
  def cos(a: NDArray): NDArray   = _np.cos(a)
  def tan(a: NDArray): NDArray   = _np.tan(a)
  def floor(a: NDArray): NDArray = _np.floor(a)
  def ceil(a: NDArray): NDArray  = _np.ceil(a)
  def round(a: NDArray): NDArray = _np.round(a)
  def round(a: NDArray, decimals: Int): NDArray =
    _np.round(a, decimals = decimals)

  def clip(a: NDArray, lo: Any, hi: Any): NDArray = _np.clip(a, lo, hi)
  def maximum(a: NDArray, b: NDArray): NDArray    = _np.maximum(a, b)
  def minimum(a: NDArray, b: NDArray): NDArray    = _np.minimum(a, b)

  // ----- Linear algebra -------------------------------------------------

  def dot(a: NDArray, b: NDArray): NDArray    = _np.dot(a, b)
  def matmul(a: NDArray, b: NDArray): NDArray = _np.matmul(a, b)
  def inner(a: NDArray, b: NDArray): NDArray  = _np.inner(a, b)
  def outer(a: NDArray, b: NDArray): NDArray  = _np.outer(a, b)
  def cross(a: NDArray, b: NDArray): NDArray  = _np.cross(a, b)
  def trace(a: NDArray): PyDynamic            = _np.trace(a)

  // ----- Combining / splitting -----------------------------------------

  def concatenate(arrays: NDArray*): NDArray =
    _np.concatenate(arrays.toArray)
  def concatenateOn(axis: Int, arrays: NDArray*): NDArray =
    _np.concatenate(arrays.toArray, axis = axis)
  def stack(arrays: NDArray*): NDArray =
    _np.stack(arrays.toArray)
  def stackOn(axis: Int, arrays: NDArray*): NDArray =
    _np.stack(arrays.toArray, axis = axis)
  def vstack(arrays: NDArray*): NDArray = _np.vstack(arrays.toArray)
  def hstack(arrays: NDArray*): NDArray = _np.hstack(arrays.toArray)
  def split(a: NDArray, sections: Int): PyDynamic = _np.split(a, sections)
  def split(a: NDArray, sections: Int, axis: Int): PyDynamic =
    _np.split(a, sections, axis = axis)

  // ----- Reshape / axis manipulation -----------------------------------
  //
  // `reshape`, `transpose`, `squeeze` are reachable as instance extensions
  // (`arr.reshape(2, 3)`, `arr.transpose()`, `arr.squeeze()`).
  // `expandDims` and `moveaxis` need a non-array argument up front so
  // they live here.

  def expandDims(a: NDArray, axis: Int): NDArray = _np.expand_dims(a, axis = axis)
  def moveaxis(a: NDArray, src: Int, dst: Int): NDArray =
    _np.moveaxis(a, src, dst)

  // ----- Search / sort / where -----------------------------------------

  def where(cond: NDArray, x: NDArray, y: NDArray): NDArray =
    _np.where(cond, x, y)
  def whereOf(cond: NDArray): PyDynamic = _np.where(cond)
  def sort(a: NDArray): NDArray         = _np.sort(a)
  def argsort(a: NDArray): NDArray      = _np.argsort(a)
  def unique(a: NDArray): NDArray       = _np.unique(a)
  def nonzero(a: NDArray): PyDynamic    = _np.nonzero(a)
  def take(a: NDArray, indices: Any): NDArray = _np.take(a, indices)
  def isnan(a: NDArray): NDArray        = _np.isnan(a)
  def isfinite(a: NDArray): NDArray     = _np.isfinite(a)
  def isinf(a: NDArray): NDArray        = _np.isinf(a)
  def arrayEqual(a: NDArray, b: NDArray): Boolean =
    _np.array_equal(a, b).asInstanceOf[Boolean]
  def allclose(a: NDArray, b: NDArray): Boolean =
    _np.allclose(a, b).asInstanceOf[Boolean]
  def allclose(a: NDArray, b: NDArray, rtol: Double, atol: Double): Boolean =
    _np.allclose(a, b, rtol = rtol, atol = atol).asInstanceOf[Boolean]

end np

// ====================================================================
//                           Extension methods
//
// Defined OUTSIDE `np`, where the alias is opaque. `a.py` lifts back
// to `PyDynamic` via the public `np.asPy`; calls of the form
// `a.py.foo(...)` therefore go through `PyDynamic`'s `Dynamic` dispatch
// (the receiver's static type is `PyDynamic`, not `np.NDArray`, so no
// extension method on `np.NDArray` is in the lookup set). Returns of
// the form `np.fromPy(...)` lift the `PyDynamic` back to `NDArray`.
// ====================================================================

extension (a: np.NDArray)

  /** Escape hatch back to `PyDynamic`. Inline + no-op at runtime. */
  inline def py: PyDynamic = np.asPy(a)

  // ----- Attributes ----------------------------------------------------

  def shape: PyDynamic   = a.py.shape
  def ndim: Int          = a.py.ndim.asInstanceOf[Int]
  def size: Int          = a.py.size.asInstanceOf[Int]
  def dtype: PyDynamic   = a.py.dtype
  def dtypeName: String  = a.py.dtype.name.asInstanceOf[String]
  def itemsize: Int      = a.py.itemsize.asInstanceOf[Int]
  def nbytes: Int        = a.py.nbytes.asInstanceOf[Int]
  def T: np.NDArray      = np.fromPy(a.py.T)
  def real: np.NDArray   = np.fromPy(a.py.real)
  def imag: np.NDArray   = np.fromPy(a.py.imag)

  // ----- Reshaping / axes ---------------------------------------------

  def reshape(s0: Int): np.NDArray =
    np.fromPy(a.py.reshape(s0))
  def reshape(s0: Int, s1: Int): np.NDArray =
    np.fromPy(a.py.reshape(s0, s1))
  def reshape(s0: Int, s1: Int, s2: Int): np.NDArray =
    np.fromPy(a.py.reshape(s0, s1, s2))
  def reshape[T: np.IsShape](shape: T): np.NDArray =
    np.fromPy(a.py.reshape(shape))
  def ravel(): np.NDArray     = np.fromPy(a.py.ravel())
  def flatten(): np.NDArray   = np.fromPy(a.py.flatten())
  def transpose(): np.NDArray = np.fromPy(a.py.transpose())
  def squeeze(): np.NDArray   = np.fromPy(a.py.squeeze())
  def swapaxes(x: Int, y: Int): np.NDArray =
    np.fromPy(a.py.swapaxes(x, y))

  // ----- Indexing / assignment ----------------------------------------

  def apply(i: Int): np.NDArray =
    np.fromPy(_pyOp.getItem(a.py, i))
  def apply(i: Int, j: Int): np.NDArray =
    np.fromPy(_pyOp.getItem(a.py, (i, j)))
  def apply(i: Int, j: Int, k: Int): np.NDArray =
    np.fromPy(_pyOp.getItem(a.py, (i, j, k)))

  /** Slice along the leading axis: `arr.range(start, stop)`. */
  def range(start: Int, stop: Int): np.NDArray =
    np.fromPy(_pyOp.getItem(a.py, _pyBuiltins.slice(start, stop, null)))
  def range(start: Int, stop: Int, step: Int): np.NDArray =
    np.fromPy(_pyOp.getItem(a.py, _pyBuiltins.slice(start, stop, step)))

  def update(i: Int, value: Any): Unit =
    _pyOp.setItem(a.py, i, value)
  def update(i: Int, j: Int, value: Any): Unit =
    _pyOp.setItem(a.py, (i, j), value)

  // ----- Materialise / convert ----------------------------------------

  def item(): PyDynamic    = a.py.item()
  def itemAsDouble: Double = _pyBuiltins.toFloat(a.py.item())
  def itemAsInt: Int       = _pyBuiltins.toInt(a.py.item())
  def tolist(): PyDynamic  = a.py.tolist()
  def copy(): np.NDArray   = np.fromPy(a.py.copy())
  def astype(dtype: String): np.NDArray = np.fromPy(a.py.astype(dtype))

  // ----- Reductions ---------------------------------------------------

  def sum(): PyDynamic            = a.py.sum()
  def sum(axis: Int): np.NDArray  = np.fromPy(a.py.sum(axis = axis))
  def mean(): Double              = _pyBuiltins.toFloat(a.py.mean())
  def mean(axis: Int): np.NDArray = np.fromPy(a.py.mean(axis = axis))
  def min(): PyDynamic            = a.py.min()
  def min(axis: Int): np.NDArray  = np.fromPy(a.py.min(axis = axis))
  def max(): PyDynamic            = a.py.max()
  def max(axis: Int): np.NDArray  = np.fromPy(a.py.max(axis = axis))
  def std(): Double               = _pyBuiltins.toFloat(a.py.std())
  def std(axis: Int): np.NDArray  = np.fromPy(a.py.std(axis = axis))
  def variance(): Double          = _pyBuiltins.toFloat(a.py.`var`())
  def prod(): PyDynamic           = a.py.prod()
  def prod(axis: Int): np.NDArray = np.fromPy(a.py.prod(axis = axis))
  def all(): Boolean              = a.py.all().asInstanceOf[Boolean]
  def any(): Boolean              = a.py.any().asInstanceOf[Boolean]
  def argmin(): Int               = a.py.argmin().asInstanceOf[Int]
  def argmax(): Int               = a.py.argmax().asInstanceOf[Int]
  def cumsum(): np.NDArray        = np.fromPy(a.py.cumsum())
  def cumprod(): np.NDArray       = np.fromPy(a.py.cumprod())

  // ----- Arithmetic operators (Python dunders) -----------------------

  def +(other: np.NDArray): np.NDArray = np.fromPy(a.py.__add__(other.py))
  def +(other: Double): np.NDArray     = np.fromPy(a.py.__add__(other))
  def +(other: Int): np.NDArray        = np.fromPy(a.py.__add__(other))
  def -(other: np.NDArray): np.NDArray = np.fromPy(a.py.__sub__(other.py))
  def -(other: Double): np.NDArray     = np.fromPy(a.py.__sub__(other))
  def -(other: Int): np.NDArray        = np.fromPy(a.py.__sub__(other))
  def *(other: np.NDArray): np.NDArray = np.fromPy(a.py.__mul__(other.py))
  def *(other: Double): np.NDArray     = np.fromPy(a.py.__mul__(other))
  def *(other: Int): np.NDArray        = np.fromPy(a.py.__mul__(other))
  def /(other: np.NDArray): np.NDArray = np.fromPy(a.py.__truediv__(other.py))
  def /(other: Double): np.NDArray     = np.fromPy(a.py.__truediv__(other))
  def /(other: Int): np.NDArray        = np.fromPy(a.py.__truediv__(other))
  def %(other: np.NDArray): np.NDArray = np.fromPy(a.py.__mod__(other.py))
  def %(other: Int): np.NDArray        = np.fromPy(a.py.__mod__(other))
  def **(other: np.NDArray): np.NDArray = np.fromPy(a.py.__pow__(other.py))
  def **(other: Double): np.NDArray     = np.fromPy(a.py.__pow__(other))
  def **(other: Int): np.NDArray        = np.fromPy(a.py.__pow__(other))

  /** `a @@ b` → numpy `a @ b` matrix multiplication. */
  def @@(other: np.NDArray): np.NDArray =
    np.fromPy(a.py.__matmul__(other.py))

  def unary_- : np.NDArray = np.fromPy(a.py.__neg__())

  // ----- Comparisons --------------------------------------------------
  //
  // `==` / `!=` are final on Any, so we use `equ` / `neq` for
  // elementwise equality. Ordering operators ARE overridable.

  def equ(other: np.NDArray): np.NDArray = np.fromPy(a.py.__eq__(other.py))
  def equ(other: Double): np.NDArray     = np.fromPy(a.py.__eq__(other))
  def equ(other: Int): np.NDArray        = np.fromPy(a.py.__eq__(other))
  def neq(other: np.NDArray): np.NDArray = np.fromPy(a.py.__ne__(other.py))
  def neq(other: Double): np.NDArray     = np.fromPy(a.py.__ne__(other))

  def <(other: np.NDArray): np.NDArray  = np.fromPy(a.py.__lt__(other.py))
  def <(other: Double): np.NDArray      = np.fromPy(a.py.__lt__(other))
  def <(other: Int): np.NDArray         = np.fromPy(a.py.__lt__(other))
  def <=(other: np.NDArray): np.NDArray = np.fromPy(a.py.__le__(other.py))
  def <=(other: Double): np.NDArray     = np.fromPy(a.py.__le__(other))
  def >(other: np.NDArray): np.NDArray  = np.fromPy(a.py.__gt__(other.py))
  def >(other: Double): np.NDArray      = np.fromPy(a.py.__gt__(other))
  def >(other: Int): np.NDArray         = np.fromPy(a.py.__gt__(other))
  def >=(other: np.NDArray): np.NDArray = np.fromPy(a.py.__ge__(other.py))
  def >=(other: Double): np.NDArray     = np.fromPy(a.py.__ge__(other))

// ====================================================================
//                          Demo / smoke test
// ====================================================================

@main def npFacade(): Unit =

  println("--- construction ---")
  val z = np.zeros((2, 3))
  println("zeros.shape: " + z.shape)
  println("zeros.ndim: "  + z.ndim)
  println("zeros.size: "  + z.size)
  println("zeros.dtype: " + z.dtypeName)
  println("zeros.tolist: " + z.tolist())

  val o = np.ones(3, "int32")
  println("ones.dtype: "  + o.dtypeName)
  println("ones.tolist: " + o.tolist())

  val f = np.full((2, 2), 7)
  println("full.tolist: " + f.tolist())

  val r = np.arange(0, 10, 2)
  println("arange.tolist: " + r.tolist())
  println("arange.size: "   + r.size)

  val ls = np.linspace(0.0, 1.0, 5)
  println("linspace.tolist: " + ls.tolist())

  val id = np.eye(3)
  println("eye.tolist: " + id.tolist())

  val a = np.array(Array(1.0, 2.0, 3.0, 4.0)).reshape(2, 2)
  println("a.tolist: " + a.tolist())
  println("a.shape: "  + a.shape)

  println("--- attributes ---")
  println("a.T.tolist: " + a.T.tolist())
  println("a.itemsize: " + a.itemsize)

  println("--- reshape / axes ---")
  val m = np.arange(6).reshape(2, 3)
  println("m.tolist: "         + m.tolist())
  println("m.ravel.tolist: "   + m.ravel().tolist())
  println("m.flatten.tolist: " + m.flatten().tolist())
  println("m.transpose: "      + m.transpose().tolist())
  println("m.swapaxes: "       + m.swapaxes(0, 1).tolist())
  println("expand_dims.shape: " + np.expandDims(m, 0).shape)
  println("squeeze.shape: "     + np.expandDims(m, 0).squeeze().shape)

  println("--- indexing ---")
  println("m(1).tolist: "    + m(1).tolist())
  println("m(1, 2): "        + m(1, 2).item())
  println("m.range(0, 2).tolist: " + m.range(0, 2).tolist())
  val mut = np.zeros((2, 2))
  mut(0, 0) = 1.0
  mut(1, 1) = 2.0
  println("mut.tolist: " + mut.tolist())

  println("--- arithmetic ---")
  val u = np.array(Array(1.0, 2.0, 3.0))
  val v = np.array(Array(4.0, 5.0, 6.0))
  println("u + v: " + (u + v).tolist())
  println("u - v: " + (u - v).tolist())
  println("u * v: " + (u * v).tolist())
  println("u / v: " + (u / v).tolist())
  println("u + 10: " + (u + 10).tolist())
  println("u ** 2: " + (u ** 2).tolist())
  println("-u: "    + (-u).tolist())

  println("--- comparisons ---")
  println("u < v: "    + (u < v).tolist())
  println("u equ u: "  + u.equ(u).tolist())
  println("u >= 2.0: " + (u >= 2.0).tolist())

  println("--- reductions ---")
  println("u.sum: "    + u.sum())
  println("u.mean: "   + u.mean())
  println("u.min: "    + u.min())
  println("u.max: "    + u.max())
  println("u.argmax: " + u.argmax())
  println("u.prod: "   + u.prod())
  println("u.cumsum: " + u.cumsum().tolist())
  val mat = np.arange(6).reshape(2, 3)
  println("mat.sum(0): " + mat.sum(0).tolist())
  println("mat.sum(1): " + mat.sum(1).tolist())
  println("mat.all: " + mat.all())
  println("mat.any: " + mat.any())

  println("--- universal funcs ---")
  println("sqrt: "  + np.sqrt(np.array(Array(1.0, 4.0, 9.0))).tolist())
  println("exp(0): " + np.exp(np.array(Array(0.0))).tolist())
  println("log: "   + np.log(np.array(Array(1.0, np.e))).tolist())
  println("log2: "  + np.log2(np.array(Array(1.0, 2.0, 8.0))).tolist())
  println("abs: "   + np.absOf(np.array(Array(-1.0, 2.0, -3.0))).tolist())
  println("floor: " + np.floor(np.array(Array(1.7, -1.3))).tolist())
  println("ceil: "  + np.ceil(np.array(Array(1.2, -1.7))).tolist())
  println("clip: "  + np.clip(np.arange(0, 10), 2, 7).tolist())
  println("max-of: " + np.maximum(u, v).tolist())
  println("min-of: " + np.minimum(u, v).tolist())

  println("--- linear algebra ---")
  val A = np.array(Array(Array(1.0, 2.0), Array(3.0, 4.0)))
  val B = np.array(Array(Array(5.0, 6.0), Array(7.0, 8.0)))
  println("A.tolist: " + A.tolist())
  println("dot: "    + np.dot(A, B).tolist())
  println("matmul: " + np.matmul(A, B).tolist())
  println("A @@ B: " + (A @@ B).tolist())
  println("inner: "  + np.inner(u, v).item())
  println("outer: "  + np.outer(u, v).tolist())
  println("trace: "  + np.trace(A))

  println("--- combining ---")
  println("concat: "  + np.concatenate(u, v).tolist())
  println("vstack: "  + np.vstack(u, v).tolist())
  println("hstack: "  + np.hstack(u, v).tolist())
  println("stack0.shape: " + np.stackOn(0, u, v).shape)
  println("stack1.shape: " + np.stackOn(1, u, v).shape)

  println("--- where / sort / unique ---")
  val cond = u > 1.5
  println("where: "  + np.where(cond, u, np.zerosLike(u)).tolist())
  val unsorted = np.array(Array(3, 1, 4, 1, 5, 9, 2, 6))
  println("sort: "   + np.sort(unsorted).tolist())
  println("argsort: " + np.argsort(unsorted).tolist())
  println("unique: " + np.unique(unsorted).tolist())

  println("--- type / copy ---")
  val asInt = u.astype("int64")
  println("astype.dtype: "  + asInt.dtypeName)
  println("astype.tolist: " + asInt.tolist())
  val cp = u.copy()
  cp(0) = 99.0
  println("u (after copy mut): " + u.tolist())
  println("cp: " + cp.tolist())

  println("--- equality ---")
  println("array_equal: " + np.arrayEqual(u, np.array(Array(1.0, 2.0, 3.0))))
  println("allclose:    " + np.allclose(u, np.array(Array(1.0 + 1e-9, 2.0, 3.0))))

  println("--- nan / finite ---")
  val withNan = np.array(Array(1.0, np.nan, np.inf, -np.inf, 0.0))
  println("isnan: "    + np.isnan(withNan).tolist())
  println("isfinite: " + np.isfinite(withNan).tolist())
  println("isinf: "    + np.isinf(withNan).tolist())

  println("--- escape hatch ---")
  // `arr.py` is the no-op cast back to PyDynamic for any not-yet-typed
  // numpy method.
  println("py.tolist: " + u.py.tolist())

  println("--- typed shapes ---")
  // Single Int shape (1-D)
  println("zeros(4): " + np.zeros(4).tolist())
  // Tuple-of-Int shape (any arity)
  println("zeros(()): "       + np.zeros(EmptyTuple).shape)  // 0-D
  println("zeros((3,)): "     + np.zeros(Tuple1(3)).shape)
  println("zeros((2, 3)): "   + np.zeros((2, 3)).shape)
  println("zeros((2, 3, 4)): " + np.zeros((2, 3, 4)).shape)
  // arr.reshape with a tuple
  println("u.reshape((1, 3)).shape: " + u.reshape((1, 3)).shape)
  // Mismatched shapes are rejected at compile time:
  //   np.zeros("hello")           // no IsShape[String]
  //   np.zeros((2, "x"))          // Tuple.Union = Int | String, not <:< Int
  //   np.zeros((1.0, 2.0))        // Tuple.Union = Double, not <:< Int
