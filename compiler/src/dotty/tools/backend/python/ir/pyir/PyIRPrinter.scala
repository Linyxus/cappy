package dotty.tools.backend.python.ir.pyir

import java.io.{PrintWriter, StringWriter}

/** Diagnostic text printer for PyIR.
 *
 *  Direction: IR → text only. Output is an indented Scala-like
 *  pseudo-code, designed to be human-scannable. Not a parser - the
 *  binary serializer is the round-trippable channel.
 */
object PyIRPrinter:

  def show(tree: PyTree): String = withWriter(p => p.printTree(tree))
  def show(cls: PyClassDef): String = withWriter(p => p.printClassDef(cls))
  def show(unit: List[PyClassDef]): String = withWriter { p =>
    var first = true
    for cls <- unit do
      if first then first = false else p.println()
      p.printClassDef(cls)
  }

  def print(tree: PyTree, out: PrintWriter): Unit =
    new Printer(out).printTree(tree)

  def print(cls: PyClassDef, out: PrintWriter): Unit =
    new Printer(out).printClassDef(cls)

  // -----------------------------------------------------------------

  private def withWriter(op: Printer => Unit): String =
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    val p  = new Printer(pw)
    op(p)
    pw.flush()
    sw.toString

  private final class Printer(out: PrintWriter):
    private var indent: Int = 0
    private val tab: String = "  "

    private def indentStr: String = tab * indent

    def println(): Unit = out.println()

    private def line(s: String): Unit =
      out.print(indentStr)
      out.println(s)

    private def withIndent(op: => Unit): Unit =
      indent += 1
      try op
      finally indent -= 1

    // -----------------------------------------------------------
    //  Class / field / method definitions
    // -----------------------------------------------------------

    def printClassDef(cls: PyClassDef): Unit =
      val kindStr = cls.kind match
        case PyClassKind.Class         => "class"
        case PyClassKind.ModuleClass   => "module class"
        case PyClassKind.Interface     => "interface"
      val sup = cls.superClass.fold("")(s => s" extends ${s.nameString}")
      val ifs =
        if cls.interfaces.isEmpty then ""
        else cls.interfaces.map(_.nameString).mkString(" implements ", ", ", "")
      line(s"$kindStr ${cls.name.nameString}$sup$ifs {")
      withIndent {
        cls.fields.foreach(printFieldDef)
        if cls.fields.nonEmpty && cls.methods.nonEmpty then println()
        var first = true
        for m <- cls.methods do
          if first then first = false else println()
          printMethodDef(m)
      }
      line("}")

    private def printFieldDef(f: PyFieldDef): Unit =
      val ns = nsString(f.flags.namespace)
      val mut = if f.flags.isMutable then "var" else "val"
      val orig = origAnnot(f.originalName)
      line(s"$mut $ns ${f.name.simple.name}$orig: ${typeString(f.ftpe)}")

    private def printMethodDef(m: PyMethodDef): Unit =
      val ns      = nsString(m.flags.namespace)
      val orig    = origAnnot(m.originalName)
      val argList = m.args.map(paramString).mkString(", ")
      val sig =
        s"def $ns ${quoteName(m.name.encoded)}$orig($argList): ${typeString(m.resultType)}"
      m.body match
        case None =>
          line(s"$sig = <abstract>")
        case Some(b) =>
          line(s"$sig = {")
          withIndent {
            printTreeAsBlock(b)
          }
          line("}")

    private def paramString(p: PyParamDef): String =
      val mut = if p.mutable then "var " else ""
      val orig = origAnnot(p.originalName)
      s"$mut${p.name.name}$orig: ${typeString(p.ptpe)}"

    private def origAnnot(o: PyOriginalName): String =
      o.value match
        case None    => ""
        case Some(v) => s"{orig=\"$v\"}"

    private def nsString(ns: PyMemberNamespace): String = ns match
      case PyMemberNamespace.Public            => "public"
      case PyMemberNamespace.PublicStatic      => "public static"
      case PyMemberNamespace.Private           => "private"
      case PyMemberNamespace.PrivateStatic     => "private static"
      case PyMemberNamespace.Constructor       => "ctor"
      case PyMemberNamespace.StaticConstructor => "static ctor"

    // -----------------------------------------------------------
    //  Types and type refs
    // -----------------------------------------------------------

    private def typeString(t: PyType): String = t match
      case PyAnyType         => "any"
      case PyVoidType        => "void"
      case PyNothingType     => "nothing"
      case PyNullType        => "null"
      case PyBooleanType     => "bool"
      case PyCharType        => "char"
      case PyByteType        => "byte"
      case PyShortType       => "short"
      case PyIntType         => "int"
      case PyLongType        => "long"
      case PyFloatType       => "float"
      case PyDoubleType      => "double"
      case PyStringType      => "string"
      case PyArrayType       => "array"
      case PyClassType(cn)   => cn.nameString

    private def typeRefString(tr: PyTypeRef): String = tr match
      case PyPrimRef(t) => t.encoded
      case PyClassRef(cn) => cn.nameString
      case PyArrayRef(base, dims) =>
        ("[" * dims) + typeRefString(base) + ("]" * dims)

    // -----------------------------------------------------------
    //  Trees
    // -----------------------------------------------------------

    /** Print a tree in statement position. Many tree kinds are best
     *  rendered as a one-line expression; some (blocks, ifs) emit
     *  multiple lines. */
    def printTree(tree: PyTree): Unit = printTreeAsBlock(tree)

    private def printTreeAsBlock(tree: PyTree): Unit = tree match
      case PyBlock(stats, expr) =>
        stats.foreach(printTreeAsBlock)
        printTreeAsBlock(expr)
      case PySkip() =>
        line("()")
      case PyIf(cond, thenp, elsep) =>
        line(s"if (${exprString(cond)}) {")
        withIndent { printTreeAsBlock(thenp) }
        line("} else {")
        withIndent { printTreeAsBlock(elsep) }
        line(s"}: ${typeString(tree.tpe)}")
      case PyTryCatch(block, errVar, _, handler) =>
        line("try {")
        withIndent { printTreeAsBlock(block) }
        line(s"} catch ${errVar.name} {")
        withIndent { printTreeAsBlock(handler) }
        line(s"}: ${typeString(tree.tpe)}")
      case PyTryFinally(block, finalizer) =>
        line("try {")
        withIndent { printTreeAsBlock(block) }
        line("} finally {")
        withIndent { printTreeAsBlock(finalizer) }
        line("}")
      case PyMatch(selector, cases, default) =>
        line(s"match ${exprString(selector)} {")
        withIndent {
          for (lits, body) <- cases do
            line(s"case ${lits.map(l => exprString(l: PyTree)).mkString(" | ")} =>")
            withIndent { printTreeAsBlock(body) }
          line("case _ =>")
          withIndent { printTreeAsBlock(default) }
        }
        line(s"}: ${typeString(tree.tpe)}")
      case PyWhile(cond, body) =>
        line(s"while (${exprString(cond)}) {")
        withIndent { printTreeAsBlock(body) }
        line("}")
      case PyVarDef(name, _, vt, mut, rhs) =>
        val kw = if mut then "var" else "val"
        line(s"$kw ${name.name}: ${typeString(vt)} = ${exprString(rhs)}")
      case PyAssign(lhs, rhs) =>
        line(s"${exprString(lhs)} = ${exprString(rhs)}")
      case PyReturn(value) =>
        line(s"return ${exprString(value)}")
      case PyLabeled(label, body) =>
        line(s"${label.name}: {")
        withIndent { printTreeAsBlock(body) }
        line(s"}: ${typeString(tree.tpe)}")
      case PyLabelReturn(label, value) =>
        line(s"return@${label.name} ${exprString(value)}")
      case other =>
        line(exprString(other))

    private def exprString(tree: PyTree): String = tree match
      // Literals
      case PyBooleanLit(v) => v.toString
      case PyCharLit(v)    => s"'${v.toString}'"
      case PyByteLit(v)    => s"${v}b"
      case PyShortLit(v)   => s"${v}s"
      case PyIntLit(v)     => v.toString
      case PyLongLit(v)    => s"${v}L"
      case PyFloatLit(v)   => s"${v}f"
      case PyDoubleLit(v)  => v.toString
      case PyStringLit(v)  => s"\"$v\""
      case PyNullLit()     => "null"
      case PyUnitLit()     => "()"
      // References
      case PyVarRef(name)         => name.name
      case PyThis()               => "this"
      case PySelect(qual, field)  =>
        s"${exprString(qual)}.${field.simple.name}"
      case PySelectStatic(field)  =>
        s"${field.owner.nameString}.${field.simple.name}"
      // Calls
      case PyApply(_, recv, _, m, args) =>
        s"${exprString(recv)}.${m.encoded}(${args.map(exprString).mkString(", ")})"
      case PyApplyStatically(_, recv, cn, m, args) =>
        s"${exprString(recv)}.${cn.nameString}::${m.encoded}(${args.map(exprString).mkString(", ")})"
      case PyApplyStatic(_, cn, m, args) =>
        s"${cn.nameString}::${m.encoded}(${args.map(exprString).mkString(", ")})"
      case PyApplyExternal(callee, args) =>
        s"extern[${callee.name}](${args.map(exprString).mkString(", ")})"
      case PyExternalRef(module, path) =>
        s"externRef[$module${if path.isEmpty then "" else path.mkString(".", ".", "")}]"
      case PyAttrAccess(obj, name) =>
        s"${exprString(obj)}.$name"
      case PyApplyDynamic(callee, args, kwargs) =>
        val a = args.map(exprString)
        val k = kwargs.map { case (k, v) => s"$k=${exprString(v)}" }
        s"${exprString(callee)}.dynamic(${(a ++ k).mkString(", ")})"
      // Construction
      case PyNew(cn, _, args) =>
        s"new ${cn.nameString}(${args.map(exprString).mkString(", ")})"
      case PyLoadModule(cn) =>
        s"loadModule(${cn.nameString})"
      // Type tests / casts
      case PyIsInstanceOf(expr, tt) =>
        s"${exprString(expr)}.isInstanceOf[${typeRefString(tt)}]"
      case PyAsInstanceOf(expr, tpe) =>
        s"${exprString(expr)}.asInstanceOf[${typeString(tpe)}]"
      // Arrays
      case PyNewArray(elem, length) =>
        s"newArray[${typeRefString(elem)}](${exprString(length)})"
      case PyArrayValue(elem, elems) =>
        s"array[${typeRefString(elem)}](${elems.map(exprString).mkString(", ")})"
      case PyArraySelect(arr, idx) =>
        s"${exprString(arr)}[${exprString(idx)}]"
      case PyTupleValue(elems) =>
        s"tuple(${elems.map(exprString).mkString(", ")})"
      case PyDictValue(entries) =>
        val pairs = entries.map((k, v) => s"${exprString(k)}: ${exprString(v)}")
        s"dict(${pairs.mkString(", ")})"
      // Operators
      case PyUnaryOp(op, lhs) =>
        s"$op(${exprString(lhs)})"
      case PyBinaryOp(op, lhs, rhs) =>
        s"(${exprString(lhs)} $op ${exprString(rhs)})"
      // Closures / misc
      case PyClassOf(tr) =>
        s"classOf[${typeRefString(tr)}]"
      case PyClosure(params, _, body) =>
        val ps = params.map(_.name.name).mkString(", ")
        s"(($ps) => ${exprString(body)})"
      // Block as expression
      case PyBlock(stats, expr) =>
        if stats.isEmpty then exprString(expr)
        else s"{ ...; ${exprString(expr)} }"
      case PyIf(cond, thenp, elsep) =>
        s"(if ${exprString(cond)} then ${exprString(thenp)} else ${exprString(elsep)})"
      case PyMatch(selector, cases, _) =>
        s"(match ${exprString(selector)} <${cases.size} cases>)"
      case PyLabeled(label, body)      => s"labeled@${label.name}(${exprString(body)})"
      case PyLabelReturn(label, value) => s"return@${label.name} ${exprString(value)}"
      case PyVarDef(name, _, _, _, rhs) => s"<vardef ${name.name} = ${exprString(rhs)}>"
      case PyAssign(lhs, rhs)           => s"${exprString(lhs)} := ${exprString(rhs)}"
      case PyReturn(value)              => s"return(${exprString(value)})"
      case PyWhile(cond, body)          => s"while(${exprString(cond)}) ${exprString(body)}"
      case PySkip()                     => "skip"
      case PyTryCatch(block, errVar, _, handler) =>
        s"try ${exprString(block)} catch ${errVar.name} => ${exprString(handler)}"
      case PyTryFinally(block, finalizer) =>
        s"try ${exprString(block)} finally ${exprString(finalizer)}"

    private def quoteName(n: String): String =
      "\"" + n + "\""
