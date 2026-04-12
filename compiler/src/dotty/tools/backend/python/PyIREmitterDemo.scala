package dotty.tools.backend.python

import dotty.tools.backend.python.ir.pyir.*

/** Runnable smoke test / demo for `PyIREmitter`.
 *
 *  Hand-builds a few minimal `PyClassDef` fixtures, emits Python via
 *  `PyIREmitter`, runs the output through `python3`, and asserts the
 *  observed stdout matches expectations.
 *
 *  Intended to be invoked via:
 *  {{{
 *  sbt 'scala3-compiler-bootstrapped/runMain dotty.tools.backend.python.PyIREmitterDemo'
 *  }}}
 *
 *  Exits with status 1 if any fixture fails; otherwise prints
 *  `ALL SMOKE TESTS PASS`.
 */
object PyIREmitterDemo:

  def main(args: Array[String]): Unit =
    var allPassed = true
    allPassed &= runHelloWorld()
    allPassed &= runArithmetic()
    allPassed &= runControlFixture()

    if allPassed then
      println("\n[PyIREmitter] ALL SMOKE TESTS PASS")
    else
      println("\n[PyIREmitter] SOME SMOKE TESTS FAILED")
      sys.exit(1)

  // --- Test 1: Hello, world -------------------------------------

  private def runHelloWorld(): Boolean =
    println("\n=== Smoke test: Hello, world ===")
    val cls = buildHelloWorldClassDef()
    val src = PyIREmitter.emitToString(List(cls), Some((PyClassName("Hello"), PyClassKind.Class)))
    runAndAssert("Hello", src, "Hello, world\n")

  private def buildHelloWorldClassDef(): PyClassDef =
    val pos = PyPosition.NoPosition

    // Body: print("Hello, world")
    val body = PyApplyExternal(
      callee = PyExternalName("print"),
      args   = List(PyStringLit("Hello, world")(pos))
    )(PyVoidType, pos)

    val mainMethod = PyMethodDef(
      flags        = PyMemberFlags.empty.withNamespace(PyMemberNamespace.PublicStatic),
      name         = PyMethodName(
        PySimpleMethodName("main"),
        Nil,
        PyPrimRef.VoidRef
      ),
      originalName = PyOriginalName.NoOriginalName,
      args         = Nil,
      resultType   = PyVoidType,
      body         = Some(body),
      pos          = pos
    )

    PyClassDef(
      name         = PyClassName("Hello"),
      originalName = PyOriginalName.NoOriginalName,
      kind         = PyClassKind.Class,
      superClass   = None,
      interfaces   = Nil,
      fields       = Nil,
      methods      = List(mainMethod),
      pos          = pos
    )

  // --- Test 2: Integer arithmetic -------------------------------

  private def runArithmetic(): Boolean =
    println("\n=== Smoke test: Arithmetic (40 + 2) ===")
    val cls = buildArithmeticClassDef()
    val src = PyIREmitter.emitToString(List(cls), Some((PyClassName("Arith"), PyClassKind.Class)))
    runAndAssert("Arith", src, "42\n")

  private def buildArithmeticClassDef(): PyClassDef =
    val pos = PyPosition.NoPosition

    // 40 + 2 (int arithmetic, wrapped by _scpy_i32)
    val compute = PyBinaryOp(
      PyBinaryCode.IntAdd,
      PyIntLit(40)(pos),
      PyIntLit(2)(pos)
    )(pos)

    // print(40 + 2)
    val body = PyApplyExternal(
      callee = PyExternalName("print"),
      args   = List(compute)
    )(PyVoidType, pos)

    val mainMethod = PyMethodDef(
      flags        = PyMemberFlags.empty.withNamespace(PyMemberNamespace.PublicStatic),
      name         = PyMethodName(
        PySimpleMethodName("main"),
        Nil,
        PyPrimRef.VoidRef
      ),
      originalName = PyOriginalName.NoOriginalName,
      args         = Nil,
      resultType   = PyVoidType,
      body         = Some(body),
      pos          = pos
    )

    PyClassDef(
      name         = PyClassName("Arith"),
      originalName = PyOriginalName.NoOriginalName,
      kind         = PyClassKind.Class,
      superClass   = None,
      interfaces   = Nil,
      fields       = Nil,
      methods      = List(mainMethod),
      pos          = pos
    )

  // --- Test 3: Conditional + local variable + while ------------

  private def runControlFixture(): Boolean =
    println("\n=== Smoke test: Control flow (if/var/while) ===")
    val cls = buildControlClassDef()
    val src = PyIREmitter.emitToString(List(cls), Some((PyClassName("Ctrl"), PyClassKind.Class)))
    // Expected: sum of 1..5 = 15, printed once, then "done"
    runAndAssert("Ctrl", src, "15\ndone\n")

  private def buildControlClassDef(): PyClassDef =
    val pos = PyPosition.NoPosition

    // var sum = 0
    val sumVar = PyVarDef(
      name         = PyLocalName("sum_"),
      originalName = PyOriginalName.NoOriginalName,
      vtpe         = PyIntType,
      mutable      = true,
      rhs          = PyIntLit(0)(pos)
    )(pos)

    // var i = 1
    val iVar = PyVarDef(
      name         = PyLocalName("i"),
      originalName = PyOriginalName.NoOriginalName,
      vtpe         = PyIntType,
      mutable      = true,
      rhs          = PyIntLit(1)(pos)
    )(pos)

    // while i <= 5:
    //     sum = sum + i
    //     i = i + 1
    val whileLoop = PyWhile(
      cond = PyBinaryOp(
        PyBinaryCode.IntLe,
        PyVarRef(PyLocalName("i"))(PyIntType, pos),
        PyIntLit(5)(pos)
      )(pos),
      body = PyBlock(
        stats = List(
          PyAssign(
            PyVarRef(PyLocalName("sum_"))(PyIntType, pos),
            PyBinaryOp(
              PyBinaryCode.IntAdd,
              PyVarRef(PyLocalName("sum_"))(PyIntType, pos),
              PyVarRef(PyLocalName("i"))(PyIntType, pos)
            )(pos)
          )(pos),
          PyAssign(
            PyVarRef(PyLocalName("i"))(PyIntType, pos),
            PyBinaryOp(
              PyBinaryCode.IntAdd,
              PyVarRef(PyLocalName("i"))(PyIntType, pos),
              PyIntLit(1)(pos)
            )(pos)
          )(pos)
        ),
        expr = PyUnitLit()(pos)
      )(pos)
    )(pos)

    // print(sum)
    val printSum = PyApplyExternal(
      callee = PyExternalName("print"),
      args   = List(PyVarRef(PyLocalName("sum_"))(PyIntType, pos))
    )(PyVoidType, pos)

    // if True: print("done")
    val printDone = PyApplyExternal(
      callee = PyExternalName("print"),
      args   = List(PyStringLit("done")(pos))
    )(PyVoidType, pos)

    val ifStmt = PyIf(
      cond  = PyBooleanLit(true)(pos),
      thenp = printDone,
      elsep = PyUnitLit()(pos)
    )(PyVoidType, pos)

    val body = PyBlock(
      stats = List(sumVar, iVar, whileLoop, printSum, ifStmt),
      expr  = PyUnitLit()(pos)
    )(pos)

    val mainMethod = PyMethodDef(
      flags        = PyMemberFlags.empty.withNamespace(PyMemberNamespace.PublicStatic),
      name         = PyMethodName(
        PySimpleMethodName("main"),
        Nil,
        PyPrimRef.VoidRef
      ),
      originalName = PyOriginalName.NoOriginalName,
      args         = Nil,
      resultType   = PyVoidType,
      body         = Some(body),
      pos          = pos
    )

    PyClassDef(
      name         = PyClassName("Ctrl"),
      originalName = PyOriginalName.NoOriginalName,
      kind         = PyClassKind.Class,
      superClass   = None,
      interfaces   = Nil,
      fields       = Nil,
      methods      = List(mainMethod),
      pos          = pos
    )

  // --- Harness: write + syntax-check + run python3 --------------

  private def runAndAssert(testName: String, src: String, expected: String): Boolean =
    println(s"--- Generated Python for '$testName' ---")
    println(src)
    println(s"--- End generated Python for '$testName' ---")

    val tmpFile = java.io.File.createTempFile(s"pyir-smoke-$testName-", ".py")
    tmpFile.deleteOnExit()
    val writer = new java.io.PrintWriter(tmpFile)
    try writer.write(src) finally writer.close()

    try
      // 1. Syntax check via ast.parse
      val astProc = new ProcessBuilder(
        "python3", "-c",
        s"import ast; ast.parse(open('${tmpFile.getAbsolutePath}').read())"
      ).redirectErrorStream(true).start()
      val astOutput = new String(astProc.getInputStream.readAllBytes(), "UTF-8")
      val astExit = astProc.waitFor()
      if astExit != 0 then
        println(s"[$testName] SYNTAX CHECK FAILED:")
        println(astOutput)
        return false

      // 2. Run and capture stdout
      val runProc = new ProcessBuilder("python3", tmpFile.getAbsolutePath)
        .redirectErrorStream(true)
        .start()
      val runOutput = new String(runProc.getInputStream.readAllBytes(), "UTF-8")
      val runExit = runProc.waitFor()

      if runExit == 0 && runOutput == expected then
        println(s"[$testName] PASS")
        true
      else
        println(s"[$testName] FAIL:")
        println(s"  exit code: $runExit")
        println(s"  expected : ${expected.replace("\n", "\\n")}")
        println(s"  actual   : ${runOutput.replace("\n", "\\n")}")
        false
    catch
      case e: Exception =>
        println(s"[$testName] ERROR running python3: ${e.getMessage}")
        false
