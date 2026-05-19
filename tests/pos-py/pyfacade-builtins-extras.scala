import scala.python.runtime.PyBuiltins

// Compile-only proof that the exception type facades resolve.
// (No catch-site usage — see comment on the facade in PyBuiltins.)
private def _takesValueError(e: PyBuiltins.ValueError): Unit = ()
private def _takesIndexError(e: PyBuiltins.IndexError): Unit = ()
private def _takesStopIteration(e: PyBuiltins.StopIteration): Unit = ()

@main def pyfacadeBuiltinsExtras(): Unit =
  // --- I/O ---------------------------------------------------------
  PyBuiltins.printLine("io-print-1")
  PyBuiltins.printLine("io-print-2a", "io-print-2b")
  PyBuiltins.printNoLine("io-no-nl|")
  PyBuiltins.printLine("after")
  PyBuiltins.printWith("io-sep", "-", "<end>\n")

  // --- repr / ascii / format ---------------------------------------
  println("repr-int:" + PyBuiltins.repr(42))
  println("repr-str:" + PyBuiltins.repr("hi"))
  println("repr-list:" + PyBuiltins.repr(PyBuiltins.listOf(PyBuiltins.rangeOf(3))))
  println("ascii-a:" + PyBuiltins.ascii("a"))
  println("fmt-spec:" + PyBuiltins.formatValue(255, "x"))
  println("fmt-bare:" + PyBuiltins.formatValue(42))

  // --- Type constructors -------------------------------------------
  println("intOf-pi:" + PyBuiltins.intOf(3.7))
  println("intOf-base:" + PyBuiltins.intOf("ff", 16))
  println("strOf-num:" + PyBuiltins.strOf(123))
  println("boolOf-0:" + PyBuiltins.boolOf(0))
  println("boolOf-1:" + PyBuiltins.boolOf(1))
  println("boolOf-empty:" + PyBuiltins.boolOf(""))

  // --- list / tuple / dict / set -----------------------------------
  val xs = PyBuiltins.listOf(PyBuiltins.rangeOf(5))
  println("len-list:" + PyBuiltins.len(xs))
  println("list-repr:" + PyBuiltins.repr(xs))
  val nums = PyBuiltins.listOf(PyBuiltins.rangeOf(3, 8))
  println("range-3-8:" + PyBuiltins.repr(nums))
  val tup = PyBuiltins.tupleOf(PyBuiltins.rangeOf(3))
  println("tuple-repr:" + PyBuiltins.repr(tup))
  val dd = PyBuiltins.dictEmpty()
  println("dict-empty-len:" + PyBuiltins.len(dd))
  val ss = PyBuiltins.setOf(PyBuiltins.rangeOf(3))
  println("set-len:" + PyBuiltins.len(ss))

  // --- sorted / reversed -------------------------------------------
  val reversedList = PyBuiltins.listOf(PyBuiltins.reversedOf(PyBuiltins.listOf(PyBuiltins.rangeOf(5))))
  println("reversed:" + PyBuiltins.repr(reversedList))
  println("sorted-asc:" + PyBuiltins.repr(PyBuiltins.sortedOf(reversedList)))
  println("sorted-desc:" + PyBuiltins.repr(PyBuiltins.sortedDesc(PyBuiltins.listOf(PyBuiltins.rangeOf(5)))))

  // --- Numeric ------------------------------------------------------
  val dm = PyBuiltins.divMod(17L, 5L)
  println("divmod-17-5:" + dm._1 + "," + dm._2)
  println("pow-2-10:" + PyBuiltins.pow(2L, 10L))
  println("pow-mod:" + PyBuiltins.pow(2L, 10L, 1000L))
  println("pow-double:" + PyBuiltins.pow(4.0, 0.5))
  println("sum-list:" + PyBuiltins.sum(PyBuiltins.listOf(PyBuiltins.rangeOf(5))))
  println("sum-start:" + PyBuiltins.sum(PyBuiltins.listOf(PyBuiltins.rangeOf(5)), 10L))

  // --- any / all ----------------------------------------------------
  println("any-0-1-2:" + PyBuiltins.any(PyBuiltins.listOf(PyBuiltins.rangeOf(3))))
  println("all-0-1-2:" + PyBuiltins.all(PyBuiltins.listOf(PyBuiltins.rangeOf(3))))
  println("all-1-4:" + PyBuiltins.all(PyBuiltins.tupleOf(PyBuiltins.rangeOf(1, 5))))

  // --- isInstance / isSubclass / isCallable ------------------------
  println("isinst-str:" + PyBuiltins.isInstance("hi", PyBuiltins.typeOf("")))
  println("isinst-not:" + PyBuiltins.isInstance(42, PyBuiltins.typeOf("")))
  println("callable-append:" + PyBuiltins.isCallable(PyBuiltins.getAttr(PyBuiltins.listEmpty(), "append")))
  println("callable-int:" + PyBuiltins.isCallable(42))

  // --- Attribute ---------------------------------------------------
  val s = "hello"
  println("hasattr-upper:" + PyBuiltins.hasAttr(s, "upper"))
  println("hasattr-missing:" + PyBuiltins.hasAttr(s, "missing_xyz"))
  println("getattr-default:" + PyBuiltins.getAttrOrElse(s, "missing_xyz", "fallback"))

  // --- zip / enumerate ---------------------------------------------
  val pairs = PyBuiltins.listOf(PyBuiltins.zipOf(PyBuiltins.rangeOf(3), PyBuiltins.rangeOf(10, 13)))
  println("zip:" + PyBuiltins.repr(pairs))
  val enumPairs = PyBuiltins.listOf(PyBuiltins.enumerateOf(PyBuiltins.tupleOf(PyBuiltins.rangeOf(3))))
  println("enum:" + PyBuiltins.repr(enumPairs))

  // --- Constants ---------------------------------------------------
  println("ellipsis-repr:" + PyBuiltins.repr(PyBuiltins.ellipsis))
  println("notimpl-repr:" + PyBuiltins.repr(PyBuiltins.notImplemented))

  // --- eval / exec ------------------------------------------------
  println("eval-1plus2:" + PyBuiltins.strOf(PyBuiltins.eval("1 + 2")))
  PyBuiltins.exec("pass")
  println("exec-ran")
