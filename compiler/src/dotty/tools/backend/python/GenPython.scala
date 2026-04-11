package dotty.tools.backend.python

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import Constants.*
import Contexts.*
import Decorators.*
import Flags.*
import Names.*
import NameOps.*
import Phases.*
import Symbols.*
import Types.*
import StdNames.*

import dotty.tools.dotc.report
import dotty.tools.dotc.transform.Erasure

import dotty.tools.backend.ScalaPrimitives
import dotty.tools.backend.ScalaPrimitivesOps.*

import dotty.tools.backend.python.ir.*

import scala.collection.mutable

/** Generates Python source files for the compilation unit. */
class GenPython extends Phase:

  override def phaseName: String = GenPython.name

  override def description: String = GenPython.description

  override def isEnabled(using Context): Boolean =
    ctx.settings.scalapy.value

  override def isRunnable(using Context): Boolean =
    super.isRunnable && !ctx.usedBestEffortTasty

  override protected def run(using Context): Unit =
    new PyCodeGen().run()

object GenPython:
  val name: String = "genPython"
  val description: String = "generate Python source files"

/** Main code generator that translates post-erasure Scala trees into Python IR.
  *
  * Modeled after `dotty.tools.backend.sjs.JSCodeGen`.
  */
private class PyCodeGen()(using genCtx: Context):

  private val encoding = new PyEncoding()
  private val primitives = new ScalaPrimitives(genCtx)
  private val generatedModules = mutable.ListBuffer.empty[PyModule]

  // ─── Scoped state ──────────────────────────────────────────────────

  private var currentClassSym: Symbol = _
  private var currentMethodSym: Symbol = _

  /** Side-channel for local function defs emitted by closure generation.
    * Accumulated during expression generation, flushed by the enclosing statement.
    */
  private val pendingLocalDefs = mutable.ListBuffer.empty[PyStmt]

  // ─── Entry point ───────────────────────────────────────────────────

  def run(): Unit =
    genCompilationUnit(genCtx.compilationUnit)
    linkAndWrite()

  // ─── Compilation unit traversal ────────────────────────────────────

  private def genCompilationUnit(cunit: CompilationUnit): Unit =
    def collectTypeDefs(tree: Tree): List[TypeDef] = tree match
      case EmptyTree            => Nil
      case PackageDef(_, stats) => stats.flatMap(collectTypeDefs)
      case cd: TypeDef          => cd :: Nil
      case _: ValDef            => Nil // module instances
      case _                    => Nil

    val allTypeDefs = collectTypeDefs(cunit.tpdTree)

    for td <- allTypeDefs do
      val sym = td.symbol
      if !sym.isPrimitiveValueClass && sym != defn.ArrayClass then
        currentClassSym = sym
        encoding.resetLocalNames()
        val module0 =
          if sym.is(Trait) then genInterface(td)
          else if isStaticModule(sym) then genModuleClass(td)
          else genScalaClass(td)
        // Add if __name__ == "__main__" entry point for main classes
        val mainStmts =
          if genCtx.platform.hasMainMethod(sym) then
            val className = encoding.encodeSimpleClassName(sym)
            List(PyStmt.If(
              cond = PyExpr.Compare(
                PyExpr.Name(PyName("__name__")),
                List(PyCmpOp.Eq),
                List(PyExpr.StringLit("__main__"))),
              body = List(PyStmt.ExprStmt(
                PyExpr.Call(
                  PyExpr.Attr(PyExpr.Name(className), PyName("main")),
                  List(PyExpr.Name(PyName("sys.argv[1:]")))))),
              elifs = Nil,
              elseBody = None))
          else Nil

        val module = module0.copy(initStmts = module0.initStmts ::: mainStmts)
        generatedModules += module

  // ─── Class generation ──────────────────────────────────────────────

  private def genScalaClass(td: TypeDef): PyModule =
    val sym = td.symbol.asClass
    val className = encoding.encodeSimpleClassName(sym)
    val qualName = encoding.encodeClassName(sym)

    val bases = genBases(sym)
    val (fields, methods) = genClassMembers(td)

    val initMethod = genInitMethod(sym, td, fields)
    val allMembers: List[PyClassMember] =
      initMethod.toList ::: methods

    val classDef = PyClassDef(className, PyClassKind.Class, bases, allMembers, posOf(td))
    val defs = List(PyTopLevelDef.TLClassDef(classDef))

    PyModule(qualName, Nil, defs, Nil, posOf(td))

  private def genModuleClass(td: TypeDef): PyModule =
    val sym = td.symbol.asClass
    val className = encoding.encodeSimpleClassName(sym)
    val qualName = encoding.encodeClassName(sym)

    val bases = genBases(sym)
    val (fields, methods) = genClassMembers(td)

    val initMethod = genInitMethod(sym, td, fields)
    val allMembers: List[PyClassMember] =
      initMethod.toList ::: methods

    val classDef = PyClassDef(className, PyClassKind.ModuleClass, bases, allMembers, posOf(td))
    val moduleName = PyName(PyEmitter.Prefix + "mod_" + className.value)
    val defs = List(PyTopLevelDef.TLClassDef(classDef))
    val initStmts = List(
      PyStmt.Assign(PyExpr.Name(moduleName), PyExpr.New(PyExpr.Name(className), Nil))
    )

    PyModule(qualName, Nil, defs, initStmts, posOf(td))

  private def genInterface(td: TypeDef): PyModule =
    val sym = td.symbol.asClass
    val className = encoding.encodeSimpleClassName(sym)
    val qualName = encoding.encodeClassName(sym)

    val bases = genBases(sym)

    val methods = mutable.ListBuffer.empty[PyClassMember]
    for tree <- collectMemberDefs(td) do
      tree match
        case dd: DefDef =>
          genMethod(dd).foreach(m => methods += m)
        case _ => ()

    val members = if methods.isEmpty then List(PyClassMember.Pass) else methods.toList
    val classDef = PyClassDef(className, PyClassKind.Trait, bases, members, posOf(td))
    val defs = List(PyTopLevelDef.TLClassDef(classDef))

    PyModule(qualName, Nil, defs, Nil, posOf(td))

  // ─── Class member collection ───────────────────────────────────────

  private def genBases(sym: ClassSymbol): List[PyExpr] =
    val superClass = sym.superClass
    val superExpr =
      if superClass != defn.ObjectClass && superClass != NoSymbol then
        List(PyExpr.Name(encoding.encodeSimpleClassName(superClass)))
      else Nil
    val interfaceExprs = sym.directlyInheritedTraits.map { intf =>
      PyExpr.Name(encoding.encodeSimpleClassName(intf))
    }
    superExpr ::: interfaceExprs

  private def genClassMembers(td: TypeDef): (List[(PyName, Option[PyExpr])], List[PyClassMember]) =
    val fields = mutable.ListBuffer.empty[(PyName, Option[PyExpr])]
    val methods = mutable.ListBuffer.empty[PyClassMember]

    for tree <- collectMemberDefs(td) do
      tree match
        case vd: ValDef =>
          val sym = vd.symbol
          if !sym.is(Module) then
            val name = encoding.encodeFieldName(sym)
            val init = if vd.rhs.isEmpty then None else Some(genExpr(vd.rhs))
            fields += ((name, init))

        case dd: DefDef =>
          if !dd.symbol.isClassConstructor then
            genMethod(dd).foreach(m => methods += m)
        case _ => ()

    (fields.toList, methods.toList)

  private def genInitMethod(sym: ClassSymbol, td: TypeDef, fields: List[(PyName, Option[PyExpr])]): Option[PyClassMember] =
    // Find constructor DefDef
    val ctorOpt = collectMemberDefs(td).collectFirst {
      case dd: DefDef if dd.symbol.isClassConstructor => dd
    }

    ctorOpt match
      case None => None
      case Some(ctorDef) =>
        val ctorSym = ctorDef.symbol
        currentMethodSym = ctorSym

        val params = ctorDef.termParamss.flatten.map { p =>
          PyParam(encoding.encodeLocalName(p.symbol), encoding.toPyType(p.symbol.info), None)
        }
        val selfParam = PyParam(PyName("self"), None, None)

        // Generate field initializations: self.field = param or default
        val fieldInits: List[PyStmt] = fields.map { (name, defaultOpt) =>
          val value = defaultOpt.getOrElse(PyExpr.NoneLit)
          PyStmt.Assign(
            PyExpr.Attr(PyExpr.Name(PyName("self")), name),
            value
          )
        }

        // Generate constructor body
        val bodyStmts = if ctorDef.rhs.isEmpty then fieldInits
          else fieldInits ::: flattenToStmts(genStat(ctorDef.rhs))

        val body = if bodyStmts.isEmpty then List(PyStmt.Pass) else bodyStmts

        val funcDef = PyFuncDef(PyName("__init__"), selfParam :: params, body, Some(PyTypeAnnot.Named("None")), posOf(ctorDef))
        Some(PyClassMember.Method(funcDef, Nil))

  // ─── Method generation ─────────────────────────────────────────────

  private def genMethod(dd: DefDef): Option[PyClassMember] =
    val sym = dd.symbol

    // Skip primitives and bridges
    if primitives.isPrimitive(sym) then return None
    if sym.is(Bridge) then return None

    currentMethodSym = sym
    encoding.resetLocalNames()

    val methodName = encoding.encodeMethodName(sym)
    val isStatic = sym.is(JavaStatic) || (sym.owner.is(ModuleClass) && !sym.isClassConstructor)

    val params = dd.termParamss.flatten.map { p =>
      PyParam(encoding.encodeLocalName(p.symbol), encoding.toPyType(p.symbol.info), None)
    }

    val allParams =
      if isStatic then params
      else PyParam(PyName("self"), None, None) :: params

    val decorators =
      if isStatic then List(PyDecorator.StaticMethod)
      else Nil

    val returnType = encoding.toPyType(sym.info.finalResultType)

    val isUnitReturn = sym.info.finalResultType.isRef(defn.UnitClass)
                    || sym.info.finalResultType.isRef(defn.NothingClass)

    val bodyStmts0 =
      if sym.is(Deferred) then
        List(PyStmt.Raise(Some(PyExpr.Call(PyExpr.Name(PyName("NotImplementedError")), Nil))))
      else if dd.rhs.isEmpty then
        List(PyStmt.Pass)
      else
        flattenToStmts(genStat(dd.rhs))

    val bodyStmts =
      if isUnitReturn || sym.isClassConstructor then bodyStmts0
      else wrapLastReturn(bodyStmts0)

    val body = if bodyStmts.isEmpty then List(PyStmt.Pass) else bodyStmts

    val funcDef = PyFuncDef(methodName, allParams, body, returnType, posOf(dd))
    Some(PyClassMember.Method(funcDef, decorators))

  // ─── Statement generation ──────────────────────────────────────────

  private def genStat(tree: Tree): PyStmt =
    tree match
      case vd @ ValDef(name, _, _) =>
        val sym = vd.symbol
        val rhs = if vd.rhs.isEmpty then PyExpr.NoneLit else genExpr(vd.rhs)
        PyStmt.VarDef(encoding.encodeLocalName(sym), encoding.toPyType(sym.info), sym.is(Mutable), rhs)

      case If(cond, thenp, elsep) =>
        val thenStmts = flattenToStmts(genStat(thenp))
        val elseStmts = if elsep.isEmpty then None
          else
            val es = flattenToStmts(genStat(elsep))
            if es.isEmpty || es == List(PyStmt.ExprStmt(PyExpr.NoneLit)) then None
            else Some(es)
        PyStmt.If(genExpr(cond), nonEmpty(thenStmts), Nil, elseStmts)

      case Labeled(bind, expr) =>
        PyStmt.Labeled(encoding.encodeLabelName(bind.symbol), flattenToStmts(genStat(expr)))

      case Return(expr, from) =>
        val fromSym = from.symbol
        if fromSym.is(Label) then
          PyStmt.LabelReturn(encoding.encodeLabelName(fromSym), genExpr(expr))
        else
          val value = if expr.tpe.isRef(defn.UnitClass) then None else Some(genExpr(expr))
          PyStmt.Return(value)

      case WhileDo(cond, body) =>
        val genCond = if cond == EmptyTree then PyExpr.BoolLit(true) else genExpr(cond)
        PyStmt.While(genCond, nonEmpty(flattenToStmts(genStat(body))))

      case t: Try =>
        genTry(t)

      case Assign(lhs, rhs) =>
        PyStmt.Assign(genExpr(lhs), genExpr(rhs))

      case Block(stats, expr) =>
        val stmts = stats.map(genStat) :+ genStat(expr)
        PyStmt.Block(stmts)

      case app: Apply =>
        genApply(app) match
          case Left(stmt) => stmt
          case Right(expr) => PyStmt.ExprStmt(expr)

      case app: TypeApply =>
        PyStmt.ExprStmt(genTypeApply(app))

      case EmptyTree =>
        PyStmt.Pass

      case _ =>
        // Fall back: try to generate as expression
        PyStmt.ExprStmt(genExpr(tree))

  // ─── Expression generation ─────────────────────────────────────────

  private def genExpr(tree: Tree): PyExpr =
    tree match
      case Literal(value) =>
        genLiteral(value)

      case If(cond, thenp, elsep) =>
        PyExpr.IfExpr(genExpr(cond), genExpr(thenp), genExpr(elsep))

      case tree: This =>
        if tree.symbol.is(ModuleClass) && tree.symbol != currentClassSym then
          loadModule(encoding.encodeClassName(tree.symbol))
        else
          PyExpr.This

      case Select(qualifier, _) =>
        val sym = tree.symbol
        if sym.is(Module) then
          loadModule(encoding.encodeClassName(sym.moduleClass))
        else
          PyExpr.Attr(genExpr(qualifier), encoding.encodeFieldName(sym))

      case tree: Ident =>
        val sym = tree.symbol
        if sym.is(Module) then
          loadModule(encoding.encodeClassName(sym.moduleClass))
        else
          PyExpr.Name(encoding.encodeLocalName(sym))

      case Block(stats, expr) =>
        // In expression position, blocks are tricky.
        // Emit stats as pending side effects, return final expression.
        for s <- stats do
          pendingLocalDefs += genStat(s)
        genExpr(expr)

      case Typed(expr, _) =>
        expr match
          case _: Super => PyExpr.This
          case _        => genExpr(expr)

      case app: Apply =>
        genApply(app) match
          case Left(stmt) =>
            // Statement in expression position — shouldn't happen often
            pendingLocalDefs += stmt
            PyExpr.NoneLit
          case Right(expr) => expr

      case app: TypeApply =>
        genTypeApply(app)

      case tree @ Closure(env, meth, tpt) =>
        genClosure(tree)

      case Match(selector, cases) =>
        genMatchExpr(selector, cases)

      case EmptyTree =>
        PyExpr.NoneLit

      case _ =>
        // Fallback for unhandled trees
        PyExpr.NoneLit

  // ─── Literal generation ────────────────────────────────────────────

  private def genLiteral(value: Constant): PyExpr =
    value.tag match
      case UnitTag    => PyExpr.NoneLit
      case BooleanTag => PyExpr.BoolLit(value.booleanValue)
      case ByteTag    => PyExpr.IntLit(value.byteValue.toLong)
      case ShortTag   => PyExpr.IntLit(value.shortValue.toLong)
      case CharTag    => PyExpr.IntLit(value.charValue.toLong)
      case IntTag     => PyExpr.IntLit(value.intValue.toLong)
      case LongTag    => PyExpr.IntLit(value.longValue)
      case FloatTag   => PyExpr.FloatLit(value.floatValue.toDouble)
      case DoubleTag  => PyExpr.FloatLit(value.doubleValue)
      case StringTag  => PyExpr.StringLit(value.stringValue)
      case NullTag    => PyExpr.NoneLit
      case ClazzTag   => PyExpr.Name(PyName(encoding.encodeClassName(value.typeValue.typeSymbol).toString))
      case _          => PyExpr.NoneLit

  // ─── Apply dispatch ────────────────────────────────────────────────

  private def genApply(app: Apply): Either[PyStmt, PyExpr] =
    val sym = app.fun.symbol
    val args = app.args

    app.fun match
      // super.method(args)
      case Select(sup @ Super(_, _), _) =>
        Right(genSuperCall(app))

      // new ClassName(args)
      case Select(New(tpt), nme.CONSTRUCTOR) =>
        Right(genApplyNew(app))

      case _ =>
        if primitives.isPrimitive(app) then
          Right(genPrimitiveOp(app))
        else if Erasure.Boxing.isBox(sym) then
          Right(genExpr(args.head))
        else if Erasure.Boxing.isUnbox(sym) then
          Right(genExpr(args.head))
        else
          Right(genNormalApply(app))

  private def genSuperCall(app: Apply): PyExpr =
    val sym = app.fun.symbol
    val args = app.args.map(genExpr)
    val methodName = encoding.encodeMethodName(sym)
    PyExpr.Call(
      PyExpr.Attr(PyExpr.SuperRef(PyExpr.This, None), methodName),
      args
    )

  private def genApplyNew(app: Apply): PyExpr =
    val Apply(Select(New(tpt), _), args) = app: @unchecked
    val classSym = tpt.tpe.typeSymbol
    val className = encoding.encodeSimpleClassName(classSym)
    PyExpr.New(PyExpr.Name(className), args.map(genExpr))

  private def genNormalApply(app: Apply): PyExpr =
    val sym = app.fun.symbol
    val args = app.args.map(genExpr)

    app.fun match
      case Select(receiver, _) =>
        val methodName = encoding.encodeMethodName(sym)
        PyExpr.Call(PyExpr.Attr(genExpr(receiver), methodName), args)

      case Ident(_) =>
        val methodName = encoding.encodeMethodName(sym)
        // Static method call or local function call
        if sym.owner.is(ModuleClass) then
          val module = loadModule(encoding.encodeClassName(sym.owner))
          PyExpr.Call(PyExpr.Attr(module, methodName), args)
        else
          PyExpr.Call(PyExpr.Name(methodName), args)

      case _ =>
        PyExpr.Call(genExpr(app.fun), args)

  // ─── TypeApply (isInstanceOf / asInstanceOf) ───────────────────────

  private def genTypeApply(app: TypeApply): PyExpr =
    val TypeApply(fun, targs) = app
    val sym = fun.symbol

    if sym == defn.Any_isInstanceOf then
      val receiver = qualifierOf(fun)
      val targetTpe = targs.head.tpe
      val targetName = encoding.encodeSimpleClassName(targetTpe.typeSymbol)
      PyExpr.IsInstance(genExpr(receiver), PyExpr.Name(targetName))
    else if sym == defn.Any_asInstanceOf then
      val receiver = qualifierOf(fun)
      val targetTpe = targs.head.tpe
      val targetName = encoding.encodeSimpleClassName(targetTpe.typeSymbol)
      PyExpr.Cast(genExpr(receiver), PyExpr.Name(targetName))
    else
      // Other type applications (e.g., classOf) — just evaluate the expression
      genExpr(fun)

  // ─── Primitive operations ──────────────────────────────────────────

  private def genPrimitiveOp(app: Apply): PyExpr =
    val Apply(fun, args) = app
    val receiver = qualifierOf(fun)
    val code = primitives.getPrimitive(app, receiver.tpe)

    if isArithmeticOp(code) || isLogicalOp(code) || isComparisonOp(code) then
      genSimpleOp(app, receiver, args, code)
    else if code == CONCAT then
      genStringConcat(receiver, args)
    else if code == HASH then
      PyExpr.Call(PyExpr.Name(PyName("hash")), List(genExpr(receiver)))
    else if isArrayOp(code) then
      genArrayOp(app, receiver, args, code)
    else if code == SYNCHRONIZED then
      // Python has no monitors; just evaluate the body
      genExpr(args.head)
    else if isCoercion(code) then
      genCoercion(receiver, code)
    else
      // Unknown primitive — fall back to method call
      genNormalApply(app)

  private def genSimpleOp(app: Apply, receiver: Tree, args: List[Tree], code: Int): PyExpr =
    import PyBinOp.*, PyUnaryOp.*, PyCmpOp.*, PyBoolOp.*

    val receiverType = receiver.tpe

    args match
      // Unary operations
      case Nil =>
        code match
          case POS  => genExpr(receiver)
          case NEG  => wrapNumeric(PyExpr.UnaryOp(Neg, genExpr(receiver)), receiverType)
          case NOT  => wrapNumeric(PyExpr.UnaryOp(Invert, genExpr(receiver)), receiverType)
          case ZNOT => PyExpr.UnaryOp(Not, genExpr(receiver))
          case _    => genExpr(receiver) // shouldn't happen

      // Binary operations
      case List(rhs) =>
        val lhs = genExpr(receiver)
        val rhsExpr = genExpr(rhs)

        code match
          // Short-circuit booleans
          case ZOR  => PyExpr.BoolOp(Or, List(lhs, rhsExpr))
          case ZAND => PyExpr.BoolOp(And, List(lhs, rhsExpr))

          // Reference equality
          case ID => PyExpr.Compare(lhs, List(Is), List(rhsExpr))
          case NI => PyExpr.Compare(lhs, List(IsNot), List(rhsExpr))

          // Universal equality
          case EQ => PyExpr.Compare(lhs, List(Eq), List(rhsExpr))
          case NE => PyExpr.Compare(lhs, List(NotEq), List(rhsExpr))

          // Numeric comparisons
          case LT => PyExpr.Compare(lhs, List(Lt), List(rhsExpr))
          case LE => PyExpr.Compare(lhs, List(LtE), List(rhsExpr))
          case GT => PyExpr.Compare(lhs, List(Gt), List(rhsExpr))
          case GE => PyExpr.Compare(lhs, List(GtE), List(rhsExpr))

          // Arithmetic
          case ADD => wrapNumeric(PyExpr.BinOp(Add, lhs, rhsExpr), receiverType)
          case SUB => wrapNumeric(PyExpr.BinOp(Sub, lhs, rhsExpr), receiverType)
          case MUL => wrapNumeric(PyExpr.BinOp(Mul, lhs, rhsExpr), receiverType)
          case DIV =>
            if encoding.isIntType(receiverType) || encoding.isLongType(receiverType) then
              wrapNumeric(PyExpr.BinOp(FloorDiv, lhs, rhsExpr), receiverType)
            else
              PyExpr.BinOp(Div, lhs, rhsExpr)
          case MOD => wrapNumeric(PyExpr.BinOp(Mod, lhs, rhsExpr), receiverType)

          // Bitwise
          case OR  => wrapNumeric(PyExpr.BinOp(BitOr, lhs, rhsExpr), receiverType)
          case XOR => wrapNumeric(PyExpr.BinOp(BitXor, lhs, rhsExpr), receiverType)
          case AND => wrapNumeric(PyExpr.BinOp(BitAnd, lhs, rhsExpr), receiverType)

          // Shifts
          case LSL => wrapNumeric(PyExpr.BinOp(LShift, lhs, rhsExpr), receiverType)
          case ASR => wrapNumeric(PyExpr.BinOp(RShift, lhs, rhsExpr), receiverType)
          case LSR =>
            // Unsigned right shift: mask to unsigned first
            if encoding.isIntType(receiverType) then
              PyExpr.IntWrap32(
                PyExpr.BinOp(RShift,
                  PyExpr.BinOp(BitAnd, lhs, PyExpr.IntLit(0xFFFFFFFFL)),
                  rhsExpr))
            else if encoding.isLongType(receiverType) then
              PyExpr.IntWrap64(
                PyExpr.BinOp(RShift,
                  PyExpr.BinOp(BitAnd, lhs, PyExpr.IntLit(Long.MaxValue)),
                  rhsExpr))
            else
              PyExpr.BinOp(RShift, lhs, rhsExpr)

          case _ => PyExpr.NoneLit // shouldn't happen

      case _ => PyExpr.NoneLit // shouldn't happen

  /** Wrap arithmetic result in IntWrap32/64 or FloatWrap based on result type. */
  private def wrapNumeric(expr: PyExpr, tp: Type): PyExpr =
    if encoding.isIntType(tp) then PyExpr.IntWrap32(expr)
    else if encoding.isLongType(tp) then PyExpr.IntWrap64(expr)
    else if encoding.isFloatType(tp) then PyExpr.FloatWrap(expr)
    else expr // Double or other: no wrapping

  private def genStringConcat(receiver: Tree, args: List[Tree]): PyExpr =
    val lhs = genExpr(receiver)
    val rhs = genExpr(args.head)
    PyExpr.StringConcat(List(lhs, rhs))

  private def genArrayOp(app: Apply, receiver: Tree, args: List[Tree], code: Int): PyExpr =
    if isArrayLength(code) then
      PyExpr.ArrayLength(genExpr(receiver))
    else if isArrayGet(code) then
      PyExpr.ArraySelect(genExpr(receiver), genExpr(args.head))
    else if isArraySet(code) then
      pendingLocalDefs += PyStmt.Assign(
        PyExpr.ArraySelect(genExpr(receiver), genExpr(args(0))),
        genExpr(args(1))
      )
      PyExpr.NoneLit
    else if isArrayNew(code) then
      val init = code match
        case NEW_ZARRAY => PyExpr.BoolLit(false)
        case NEW_BARRAY | NEW_SARRAY | NEW_CARRAY | NEW_IARRAY | NEW_LARRAY => PyExpr.IntLit(0)
        case NEW_FARRAY | NEW_DARRAY => PyExpr.FloatLit(0.0)
        case _ => PyExpr.NoneLit
      PyExpr.NewArray(genExpr(args.head), init)
    else
      PyExpr.NoneLit

  private def genCoercion(receiver: Tree, code: Int): PyExpr =
    val src = genExpr(receiver)
    code match
      // Identity coercions
      case B2B | S2S | C2C | I2I | L2L | F2F | D2D => src

      // To Int types (just truncate)
      case B2I | S2I | C2I => src
      case L2I => PyExpr.IntWrap32(src)
      case F2I | D2I => PyExpr.IntWrap32(PyExpr.Call(PyExpr.Name(PyName("int")), List(src)))

      // To Long
      case B2L | S2L | C2L | I2L => src
      case F2L | D2L => PyExpr.IntWrap64(PyExpr.Call(PyExpr.Name(PyName("int")), List(src)))

      // To Float
      case B2F | S2F | C2F | I2F | L2F =>
        PyExpr.FloatWrap(PyExpr.Call(PyExpr.Name(PyName("float")), List(src)))
      case D2F => PyExpr.FloatWrap(src)

      // To Double
      case B2D | S2D | C2D | I2D | L2D | F2D =>
        PyExpr.Call(PyExpr.Name(PyName("float")), List(src))

      // To Byte/Short/Char (narrowing)
      case I2B | L2B | F2B | D2B | S2B | C2B =>
        PyExpr.IntWrap32(PyExpr.BinOp(PyBinOp.BitAnd, src, PyExpr.IntLit(0xFF)))
      case I2S | L2S | F2S | D2S | B2S | C2S =>
        PyExpr.IntWrap32(PyExpr.BinOp(PyBinOp.BitAnd, src, PyExpr.IntLit(0xFFFF)))
      case I2C | L2C | F2C | D2C | B2C | S2C =>
        PyExpr.BinOp(PyBinOp.BitAnd, src, PyExpr.IntLit(0xFFFF))

      case _ => src

  // ─── Exception handling ────────────────────────────────────────────

  private def genTry(tree: Try): PyStmt =
    val Try(block, catches, finalizer) = tree

    val bodyStmts = nonEmpty(flattenToStmts(genStat(block)))

    val handlers = catches.map { caseDef =>
      val CaseDef(pat, _, body) = caseDef

      val (exnType, boundName) = pat match
        case Typed(Ident(nme.WILDCARD), tpt) =>
          (Some(PyExpr.Name(encoding.encodeSimpleClassName(tpt.tpe.typeSymbol))), None)
        case Ident(nme.WILDCARD) =>
          (Some(PyExpr.Name(PyName("Exception"))), None)
        case Bind(name, Typed(_, tpt)) =>
          (Some(PyExpr.Name(encoding.encodeSimpleClassName(tpt.tpe.typeSymbol))),
           Some(encoding.encodeLocalName(pat.symbol)))
        case Bind(name, _) =>
          (Some(PyExpr.Name(PyName("Exception"))),
           Some(encoding.encodeLocalName(pat.symbol)))
        case _ =>
          (None, None)

      PyExceptHandler(exnType, boundName, nonEmpty(flattenToStmts(genStat(body))), posOf(caseDef))
    }

    val fin = if finalizer.isEmpty then None
      else Some(nonEmpty(flattenToStmts(genStat(finalizer))))

    PyStmt.Try(bodyStmts, handlers, None, fin)

  // ─── Match generation ──────────────────────────────────────────────

  private def genMatchExpr(selector: Tree, cases: List[CaseDef]): PyExpr =
    cases match
      case Nil => PyExpr.NoneLit
      case List(CaseDef(_, _, body)) => genExpr(body)
      case _ =>
        cases.foldRight(PyExpr.NoneLit: PyExpr) { (caseDef, elsePart) =>
          val CaseDef(pat, guard, body) = caseDef
          pat match
            case Literal(c) =>
              val test = PyExpr.Compare(
                genExpr(selector),
                List(PyCmpOp.Eq),
                List(genLiteral(c))
              )
              PyExpr.IfExpr(test, genExpr(body), elsePart)
            case _ =>
              genExpr(body)
        }

  // ─── Closure generation ────────────────────────────────────────────

  private def genClosure(tree: Closure): PyExpr =
    val Closure(env, meth, tpt) = tree
    val targetSym = meth.symbol

    if env.isEmpty then
      PyExpr.Attr(PyExpr.This, encoding.encodeMethodName(targetSym))
    else
      val methodRef = PyExpr.Attr(PyExpr.This, encoding.encodeMethodName(targetSym))
      methodRef // simplified — captures are already lifted

  // ─── File output ───────────────────────────────────────────────────

  /** Link all generated modules into a single bundled .py file. */
  private def linkAndWrite(): Unit =
    val outputDirectory = genCtx.settings.outputDir.value
    // Use the source file name (without extension) as output name
    val sourceName = genCtx.compilationUnit.source.file.name.stripSuffix(".scala")
    val outfile = outputDirectory.fileNamed(sourceName + ".py")
    val output = outfile.bufferedOutput
    try
      val writer = new java.io.PrintWriter(output)
      try
        PyLinker.link(generatedModules.toList, ScalaPyRuntime.content, writer)
        writer.flush()
      finally writer.close()
    finally output.close()

  // ─── Helpers ───────────────────────────────────────────────────────

  private def posOf(tree: Tree): PyPos =
    val pos = tree.sourcePos
    if pos.exists then PyPos(pos.source.path, pos.line, pos.column)
    else PyPos.No

  private def qualifierOf(tree: Tree): Tree = tree match
    case Select(qualifier, _) => qualifier
    case _                    => EmptyTree

  private def collectMemberDefs(td: TypeDef): List[ValOrDefDef] =
    val impl = td.rhs.asInstanceOf[Template]
    val b = List.newBuilder[ValOrDefDef]
    for stat <- impl.constr :: impl.body do
      stat match
        case stat: ValDef => b += stat
        case stat: DefDef => b += stat
        case _            => ()
    b.result()

  private def isStaticModule(sym: Symbol): Boolean =
    sym.is(ModuleClass) && !sym.isAnonymousClass

  private def flattenToStmts(stmt: PyStmt): List[PyStmt] =
    val prefix = pendingLocalDefs.toList
    pendingLocalDefs.clear()
    val flat = stmt match
      case PyStmt.Block(stmts) => stmts.flatMap(flattenToStmts)
      case other               => List(other)
    prefix ::: flat

  private def nonEmpty(stmts: List[PyStmt]): List[PyStmt] =
    if stmts.isEmpty then List(PyStmt.Pass) else stmts

  /** Wrap the last expression in a list of statements with `return`.
    * Recurses into blocks, if/elif/else branches so every exit path returns.
    */
  private def wrapLastReturn(stmts: List[PyStmt]): List[PyStmt] =
    if stmts.isEmpty then stmts
    else stmts.init :+ wrapStmtReturn(stmts.last)

  private def wrapStmtReturn(stmt: PyStmt): PyStmt = stmt match
    case PyStmt.ExprStmt(PyExpr.NoneLit) => stmt // don't return None for Unit-typed tails
    case PyStmt.ExprStmt(expr)           => PyStmt.Return(Some(expr))
    case PyStmt.VarDef(n, t, m, rhs)     => PyStmt.VarDef(n, t, m, rhs) // can't return a vardef
    case PyStmt.Block(inner)             => PyStmt.Block(wrapLastReturn(inner))
    case PyStmt.If(cond, body, elifs, elseBody) =>
      PyStmt.If(
        cond, wrapLastReturn(body),
        elifs.map((c, b) => (c, wrapLastReturn(b))),
        elseBody.map(wrapLastReturn))
    case PyStmt.Try(body, handlers, elseBody, fin) =>
      PyStmt.Try(
        wrapLastReturn(body),
        handlers.map(h => h.copy(body = wrapLastReturn(h.body))),
        elseBody.map(wrapLastReturn),
        fin)
    case _: PyStmt.Return | _: PyStmt.Raise | _: PyStmt.LabelReturn => stmt // already exits
    case _ => stmt

  private def loadModule(qualName: QualName): PyExpr =
    PyExpr.LoadModule(qualName)


/** Hard-coded Python runtime for generated code.
  *
  * Naming convention:
  *   - Compiler-invented names use the `_scpy_` prefix (e.g., `_scpy_i32`, `_scpy_mod_Foo`)
  *   - Real Scala class/module names keep their natural encoding (e.g., `CommandLineParser_ParseError`)
  */
private object ScalaPyRuntime:
  private val P = PyEmitter.Prefix
  val content: String =
    s"""|# Scala.py runtime — generated by the Scala 3 Python backend
        |#
        |# Names prefixed with ${P} are compiler-invented and don't correspond
        |# to Scala source identifiers. All other names are real Scala classes.
        |import struct
        |import builtins as _builtins
        |from typing import Any
        |
        |# ── Compiler-invented: numeric wrapping (Scala overflow semantics) ──
        |
        |def ${P}i32(x):
        |    return ((_builtins.int(x) + 0x80000000) & 0xFFFFFFFF) - 0x80000000
        |
        |def ${P}i64(x):
        |    return ((_builtins.int(x) + 0x8000000000000000) & 0xFFFFFFFFFFFFFFFF) - 0x8000000000000000
        |
        |def ${P}f32(x):
        |    return struct.unpack('f', struct.pack('f', _builtins.float(x)))[0]
        |
        |def ${P}to_str(x):
        |    return _builtins.str(x)
        |
        |# ── scala.Predef ──
        |
        |class _Predef:
        |    def println(self, *args):
        |        _builtins.print(*args)
        |
        |    def print_(self, *args):
        |        _builtins.print(*args, end="")
        |
        |    def assert_(self, cond, msg=None):
        |        if msg is not None:
        |            assert cond, msg
        |        else:
        |            assert cond
        |
        |    def require(self, cond, msg=None):
        |        if not cond:
        |            raise ValueError(msg if msg else "requirement failed")
        |
        |    def identity(self, x):
        |        return x
        |
        |    def locally(self, x):
        |        return x
        |
        |${P}mod_Predef = _Predef()
        |
        |# ── scala.util.CommandLineParser (@main wrapper support) ──
        |
        |class CommandLineParser_ParseError(Exception):
        |    pass
        |
        |class _CommandLineParser:
        |    def showError(self, error):
        |        _builtins.print(_builtins.str(error))
        |
        |${P}mod_CommandLineParser = _CommandLineParser()
        |
        |# ── scala.runtime.ModuleSerializationProxy ──
        |
        |class ModuleSerializationProxy:
        |    def __init__(self, cls):
        |        self.cls = cls
        |""".stripMargin
