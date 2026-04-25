// Regression: closures of arity 3..22 must extend the matching nominal
// `scala.FunctionN` Python base, so that `f.isInstanceOf[FunctionN[...]]`
// (and constructor-dispatch sites with a `FunctionN` parameter) succeed
// at runtime. Before the fix, only Function0/1/2 had nominal carriers
// (`_scpy_Fn{0,1,2}`); higher arities fell back to plain `_scpy_Fn`,
// which `_scpy_class_of_instance` reported as `java.lang.Object`.
@main def scalaFunctionArity(): Unit =
  // -- Arity 3 --
  val f3: (Int, Int, Int) => Int = (a, b, c) => a + b + c
  println("f3:" + f3(1, 2, 3))
  println("f3.isFn3:" + f3.isInstanceOf[Function3[?, ?, ?, ?]])
  println("f3.isFn2:" + f3.isInstanceOf[Function2[?, ?, ?]])

  // -- Arity 5 (mid-range, bridges across the loop) --
  val f5: (Int, Int, Int, Int, Int) => Int =
    (a, b, c, d, e) => a + b + c + d + e
  println("f5:" + f5(1, 2, 3, 4, 5))
  println("f5.isFn5:" + f5.isInstanceOf[Function5[?, ?, ?, ?, ?, ?]])

  // -- Arity 22 (the maximum supported by Scala) --
  val f22: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int,
            Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int =
    (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
     a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22) =>
      a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 +
        a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22
  println("f22:" + f22(
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
    12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22))
  println("f22.isFn22:" +
    f22.isInstanceOf[Function22[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,
                                 ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]])

  // -- Pass a Function3 through a parameter typed as `Function3` --
  // Forces the constructor-dispatch / argument-guard path that the
  // original `IndexedSeqView_Map` issue surfaced for `Function1`.
  def applyFn3(f: (Int, Int, Int) => Int, a: Int, b: Int, c: Int): Int =
    f(a, b, c)
  println("applyFn3:" + applyFn3((x, y, z) => x * y + z, 3, 4, 5))

  // -- Sanity: Function0/1/2 still work --
  val f0: () => Int = () => 11
  val f1: Int => Int = x => x + 1
  val f2: (Int, Int) => Int = (a, b) => a * b
  println("f0:" + f0() + " isFn0:" + f0.isInstanceOf[Function0[?]])
  println("f1:" + f1(41) + " isFn1:" + f1.isInstanceOf[Function1[?, ?]])
  println("f2:" + f2(6, 7) + " isFn2:" + f2.isInstanceOf[Function2[?, ?, ?]])
