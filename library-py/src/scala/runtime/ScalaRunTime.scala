package scala.runtime

import scala.collection.immutable.ArraySeq

/** Minimal ScalaRunTime for the Python backend.
 *
 *  Important: downstream ScalaPy compilation typechecks against the JVM
 *  stdlib `scala.runtime.ScalaRunTime`, while link-time PyIR comes from this
 *  shadow implementation. Signatures for any overridden helpers therefore
 *  need to stay ABI-compatible with the real stdlib surface.
 *
 *  Case-class synthesis and other compiler-generated code calls
 *  `ScalaRunTime._toString`, `_hashCode`, `_equals`.
 */
object ScalaRunTime:
  def _toString(x: Product): String =
    x.productIterator.mkString(x.productPrefix + "(", ",", ")")

  def _hashCode(x: Product): Int =
    scala.util.hashing.MurmurHash3.caseClassHash(x)

  def typedProductIterator[T](x: Product): Iterator[T] =
    new scala.collection.AbstractIterator[T]:
      private var c: Int = 0
      private val cmax = x.productArity
      def hasNext: Boolean = c < cmax
      def next(): T =
        val result = x.productElement(c)
        c += 1
        result.asInstanceOf[T]

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

  /** Generic array indexing — stdlib dispatches on element type for
   *  primitive boxing on the JVM. Python has no boxed-vs-unboxed
   *  distinction, so a single cast suffices.
   */
  def array_apply(xs: AnyRef, idx: Int): Any =
    xs.asInstanceOf[Array[Any]](idx)

  def array_update(xs: AnyRef, idx: Int, value: Any): Unit =
    xs.asInstanceOf[Array[Any]](idx) = value

  def array_clone(xs: AnyRef): AnyRef =
    xs.asInstanceOf[Array[Any]].clone()

  def array_length(xs: AnyRef): Int =
    xs.asInstanceOf[Array[Any]].length

  // Mirrors the upstream stdlib body. Do NOT write
  // `a.isInstanceOf[Array[?]]` here: the compiler's TypeTestsCasts
  // erasure rewrites that into `ScalaRunTime.isArray(a, 1)`, turning
  // this method into an infinite tail-call loop under the Python
  // backend's tailrec lowering. `getClass.isArray` goes through
  // `_scpy_Class._scpy_kind == "array"` instead.
  def isArray(a: Any, atLevel: Int = 1): Boolean =
    a != null && isArrayClass(a.getClass, atLevel)

  private def isArrayClass(clazz: Class[?], atLevel: Int): Boolean =
    clazz.isArray && (atLevel == 1 || isArrayClass(clazz.getComponentType, atLevel - 1))

  /** Inline helpers used by stdlib `Predef.locally`/`mapNull` etc. Must
   *  match the stdlib signature so call sites inline correctly. */
  inline def mapNull[A, B](a: A, inline f: B): B =
    if a == null then null.asInstanceOf[B] else f

  inline def nullForGC[T]: T = null.asInstanceOf[T]

  /** Convert any array into an `Array[Object]`. Mirrors stdlib
   *  `ScalaRunTime.toObjectArray`: needed when a primitive array is
   *  passed to a generic `T*` Java vararg, so the compiler emits
   *  `Java.asList(toObjectArray(Array[Int](...)))`. The Python backend
   *  does not distinguish boxed vs unboxed arrays at runtime, so a
   *  shallow copy through `AnyRef` is sufficient.
   */
  def toObjectArray(src: AnyRef): Array[Object] =
    if src == null then throw new NullPointerException
    else src match
      case x: Array[Object @unchecked] => x
      case x: Array[Int]     => copyToObjectArray(x.length, i => x(i).asInstanceOf[Object])
      case x: Array[Long]    => copyToObjectArray(x.length, i => x(i).asInstanceOf[Object])
      case x: Array[Double]  => copyToObjectArray(x.length, i => x(i).asInstanceOf[Object])
      case x: Array[Float]   => copyToObjectArray(x.length, i => x(i).asInstanceOf[Object])
      case x: Array[Char]    => copyToObjectArray(x.length, i => x(i).asInstanceOf[Object])
      case x: Array[Byte]    => copyToObjectArray(x.length, i => x(i).asInstanceOf[Object])
      case x: Array[Short]   => copyToObjectArray(x.length, i => x(i).asInstanceOf[Object])
      case x: Array[Boolean] => copyToObjectArray(x.length, i => x(i).asInstanceOf[Object])
      case _                 => src.asInstanceOf[Array[Object]]

  private def copyToObjectArray(n: Int, get: Int => Object): Array[Object] =
    val out = new Array[Object](n)
    var i = 0
    while i < n do
      out(i) = get(i)
      i += 1
    out

  /** Pretty-print any value, mirroring stdlib `ScalaRunTime.stringOf`.
   *  Used by tests and by Scala 3 `repl`-style helpers. The Python
   *  backend has no JVM reflection, so we approximate the stdlib
   *  behavior by special-casing the common cases (null, String, Array,
   *  Map, Iterable, Tuple, primitive) and falling back to `toString`. */
  def stringOf(arg: Any): String = stringOf(arg, scala.Int.MaxValue)

  def stringOf(arg: Any, maxElements: Int): String =
    def isTuple(x: Any): Boolean = x match
      case _: Product if x.getClass.getName.startsWith("scala.Tuple") => true
      case _ => false

    def useOwnToString(x: Any): Boolean = x match
      case _: scala.collection.immutable.Range => true
      case _: scala.collection.immutable.NumericRange[?] => true
      case _: scala.collection.SortedOps[?, ?] => true
      case _: scala.collection.StringOps => true
      case _: StringBuilder => true
      case _: scala.collection.View[?] => true
      case _ => false

    def mapInner(arg: Any): String = arg match
      case (k, v) => inner(k) + " -> " + inner(v)
      case _      => inner(arg)

    def arrayToString(x: AnyRef): String =
      val arr = x.asInstanceOf[Array[Any]]
      val n = math.min(arr.length, maxElements)
      val parts = new Array[String](n)
      var i = 0
      while i < n do
        parts(i) = inner(arr(i))
        i += 1
      parts.mkString("Array(", ", ", ")")

    def inner(arg: Any): String = arg match
      case null                                => "null"
      case ""                                  => "\"\""
      case s: String                           =>
        if s.length > 0 && (s.head.isWhitespace || s.last.isWhitespace) then "\"" + s + "\""
        else s
      case x if useOwnToString(x)              => x.toString
      case x: AnyRef if isArray(x)             => arrayToString(x)
      case x: scala.collection.Map[?, ?]       =>
        x.iterator.take(maxElements).map(mapInner)
          .mkString(x.collectionClassName + "(", ", ", ")")
      case x: Iterable[?]                      =>
        x.iterator.take(maxElements).map(inner)
          .mkString(x.collectionClassName + "(", ", ", ")")
      case x: Product if isTuple(x) && x.productArity == 1 =>
        "(" + inner(x.productElement(0)) + ",)"
      case x: Product if isTuple(x)            =>
        x.productIterator.map(inner).mkString("(", ",", ")")
      case x                                   => "" + x

    try inner(arg)
    catch
      case _: UnsupportedOperationException | _: AssertionError => "" + arg
