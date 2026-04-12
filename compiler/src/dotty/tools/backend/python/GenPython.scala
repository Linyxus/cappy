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
import dotty.tools.dotc.util.{NoSourcePosition, SourcePosition}
import dotty.tools.dotc.util.Spans.Span

import dotty.tools.backend.ScalaPrimitives
import dotty.tools.backend.ScalaPrimitivesOps.*

import dotty.tools.backend.python.ir.pyir.*

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

/** Main code generator that translates post-erasure Scala trees into the
  * typed PyIR. Modeled after `dotty.tools.backend.sjs.JSCodeGen`.
  */
private class PyCodeGen()(using genCtx: Context):

  private val encoding = new PyEncoding()
  private val primitives = new ScalaPrimitives(genCtx)
  private val generatedClasses = mutable.ListBuffer.empty[PyClassDef]
  private var mainEntry: Option[PyIREmitter.MainEntry] = None

  // --- Scoped state --------------------------------------------------

  private var currentClassSym: Symbol = NoSymbol
  private var currentMethodSym: Symbol = NoSymbol

  /** Side-channel for statements produced during expression generation
    * (Block-in-expression-position). Drained by `flattenToStmts` at the
    * enclosing statement. */
  private val pendingLocalDefs = mutable.ListBuffer.empty[PyTree]

  // --- Entry point ---------------------------------------------------

  def run(): Unit =
    genCompilationUnit(genCtx.compilationUnit)
    linkAndWrite()

  // --- Compilation unit traversal ------------------------------------

  private def genCompilationUnit(cunit: CompilationUnit): Unit =
    def collectTypeDefs(tree: Tree): List[TypeDef] = tree match
      case EmptyTree            => Nil
      case PackageDef(_, stats) => stats.flatMap(collectTypeDefs)
      case cd: TypeDef          => cd :: Nil
      case _: ValDef            => Nil
      case _                    => Nil

    val allTypeDefs = collectTypeDefs(cunit.tpdTree)

    for td <- allTypeDefs do
      val sym = td.symbol
      if !sym.isPrimitiveValueClass && sym != defn.ArrayClass then
        currentClassSym = sym
        val kind =
          if sym.is(Trait) then PyClassKind.Interface
          else if isStaticModule(sym) then PyClassKind.ModuleClass
          else PyClassKind.Class
        val classDef = genClassDef(td, kind)
        generatedClasses += classDef
        if genCtx.platform.hasMainMethod(sym) then
          mainEntry = Some((classDef.name, kind))

  // --- Class generation ----------------------------------------------

  private def genClassDef(td: TypeDef, kind: PyClassKind): PyClassDef =
    val sym = td.symbol.asClass
    val className = encoding.encodeClassName(sym)
    val (superClass, interfaces) = genBases(sym)
    val (fields, methodDefs) = genClassMembers(td)
    val initMethod = genInitMethod(sym, td, fields)
    val allMethods = initMethod.toList ::: methodDefs

    PyClassDef(
      name         = className,
      originalName = encoding.originalNameOf(sym),
      kind         = kind,
      superClass   = superClass,
      interfaces   = interfaces,
      fields       = fields,
      methods      = allMethods,
      pos          = posOf(td)
    )

  private def genBases(sym: ClassSymbol): (Option[PyClassName], List[PyClassName]) =
    val superSym = sym.superClass
    val superName =
      if superSym != defn.ObjectClass && superSym != NoSymbol then
        Some(encoding.encodeClassName(superSym))
      else None
    val interfaceNames = sym.directlyInheritedTraits.map(encoding.encodeClassName)
    (superName, interfaceNames)

  // --- Class member collection ---------------------------------------

  private def genClassMembers(td: TypeDef): (List[PyFieldDef], List[PyMethodDef]) =
    val fields = mutable.ListBuffer.empty[PyFieldDef]
    val methods = mutable.ListBuffer.empty[PyMethodDef]

    for tree <- collectMemberDefs(td) do
      tree match
        case vd: ValDef =>
          val sym = vd.symbol
          if !sym.is(Module) then
            fields += PyFieldDef(
              flags        = PyMemberFlags.empty.withMutable(sym.is(Mutable)),
              name         = encoding.encodeFieldName(sym),
              originalName = encoding.originalNameOf(sym),
              ftpe         = encoding.encodeType(sym.info),
              pos          = posOf(vd)
            )

        case dd: DefDef =>
          if !dd.symbol.isClassConstructor then
            genMethod(dd).foreach(methods += _)

        case _ => ()

    (fields.toList, methods.toList)

  // --- __init__ synthesis --------------------------------------------

  private def genInitMethod(
      sym: ClassSymbol, td: TypeDef, fields: List[PyFieldDef]
  ): Option[PyMethodDef] =
    val ctorOpt = collectMemberDefs(td).collectFirst {
      case dd: DefDef if dd.symbol.isClassConstructor => dd
    }

    ctorOpt.map { ctorDef =>
      val ctorSym = ctorDef.symbol
      currentMethodSym = ctorSym
      val ctorPos = posOf(ctorDef)
      val classTpe = PyClassType(encoding.encodeClassName(sym))

      val params = ctorDef.termParamss.flatten.map(genParamDef)

      val fieldInits: List[PyTree] = fields.map { f =>
        PyAssign(
          PySelect(
            PyThis()(classTpe, ctorPos),
            f.name
          )(f.ftpe, ctorPos),
          defaultValueFor(f.ftpe, ctorPos)
        )(ctorPos)
      }

      val bodyStmts =
        if ctorDef.rhs.isEmpty then fieldInits
        else fieldInits ::: flattenToStmts(genStat(ctorDef.rhs))

      val body = stmtsToBody(bodyStmts, ctorPos)

      PyMethodDef(
        flags        = PyMemberFlags.empty.withNamespace(PyMemberNamespace.Constructor),
        name         = encoding.encodeMethodName(ctorSym),
        originalName = encoding.originalNameOf(ctorSym),
        args         = params,
        resultType   = PyVoidType,
        body         = Some(body),
        pos          = ctorPos
      )
    }

  private def defaultValueFor(tpe: PyType, pos: PyPosition): PyTree = tpe match
    case PyBooleanType => PyBooleanLit(false)(pos)
    case PyByteType    => PyByteLit(0)(pos)
    case PyShortType   => PyShortLit(0)(pos)
    case PyCharType    => PyCharLit('\u0000')(pos)
    case PyIntType     => PyIntLit(0)(pos)
    case PyLongType    => PyLongLit(0L)(pos)
    case PyFloatType   => PyFloatLit(0.0f)(pos)
    case PyDoubleType  => PyDoubleLit(0.0)(pos)
    case _             => PyNullLit()(pos)

  // --- Method generation ---------------------------------------------

  private def genMethod(dd: DefDef): Option[PyMethodDef] =
    val sym = dd.symbol

    // Skip primitives and bridges
    if primitives.isPrimitive(sym) then return None
    if sym.is(Bridge) then return None

    currentMethodSym = sym
    val pos = posOf(dd)

    val params = dd.termParamss.flatten.map(genParamDef)
    val resultType = encoding.encodeType(sym.info.finalResultType)

    val isStatic =
      sym.is(JavaStatic) || (sym.owner.is(ModuleClass) && !sym.isClassConstructor)
    val namespace = (isStatic, sym.is(Private)) match
      case (true,  true)  => PyMemberNamespace.PrivateStatic
      case (true,  false) => PyMemberNamespace.PublicStatic
      case (false, true)  => PyMemberNamespace.Private
      case (false, false) => PyMemberNamespace.Public

    val body: Option[PyTree] =
      if sym.is(Deferred) then None
      else if dd.rhs.isEmpty then Some(PySkip()(pos))
      else Some(stmtsToBody(flattenToStmts(genStat(dd.rhs)), pos))

    Some(PyMethodDef(
      flags        = PyMemberFlags.empty.withNamespace(namespace),
      name         = encoding.encodeMethodName(sym),
      originalName = encoding.originalNameOf(sym),
      args         = params,
      resultType   = resultType,
      body         = body,
      pos          = pos
    ))

  private def genParamDef(p: ValDef): PyParamDef =
    val sym = p.symbol
    PyParamDef(
      name         = encoding.encodeLocalName(sym),
      originalName = encoding.originalNameOf(sym),
      ptpe         = encoding.encodeType(sym.info),
      mutable      = sym.is(Mutable),
      pos          = posOf(p)
    )

  private def stmtsToBody(stmts: List[PyTree], pos: PyPosition): PyTree =
    stmts match
      case Nil       => PySkip()(pos)
      case hd :: Nil => hd
      case _         => PyBlock(stmts.init, stmts.last)(pos)

  // --- Statement generation ------------------------------------------

  private def genStat(tree: Tree): PyTree =
    val pos = posOf(tree)
    tree match
      case vd: ValDef =>
        val sym = vd.symbol
        val rhs = if vd.rhs.isEmpty then PyNullLit()(pos) else genExpr(vd.rhs)
        PyVarDef(
          name         = encoding.encodeLocalName(sym),
          originalName = encoding.originalNameOf(sym),
          vtpe         = encoding.encodeType(sym.info),
          mutable      = sym.is(Mutable),
          rhs          = rhs
        )(pos)

      case If(cond, thenp, elsep) =>
        PyIf(genExpr(cond), genStat(thenp), genStat(elsep))(PyVoidType, pos)

      case Labeled(bind, expr) =>
        PyLabeled(
          encoding.encodeLabelName(bind.symbol),
          genStat(expr)
        )(PyVoidType, pos)

      case Return(expr, from) =>
        val fromSym = from.symbol
        val value =
          if expr == EmptyTree || expr.tpe.isRef(defn.UnitClass) then PyUnitLit()(pos)
          else genExpr(expr)
        if fromSym.is(Label) then
          PyLabelReturn(encoding.encodeLabelName(fromSym), value)(pos)
        else
          PyReturn(value)(pos)

      case WhileDo(cond, body) =>
        val genCond = if cond == EmptyTree then PyBooleanLit(true)(pos) else genExpr(cond)
        PyWhile(genCond, genStat(body))(pos)

      case t: Try =>
        genTry(t)

      case Assign(lhs, rhs) =>
        PyAssign(genAssignableLhs(lhs), genExpr(rhs))(pos)

      case Block(stats, expr) =>
        val statTrees = stats.map(genStat)
        val exprTree  = genStat(expr)
        PyBlock(statTrees, exprTree)(pos)

      case app: Apply =>
        genApply(app)

      case app: TypeApply =>
        genTypeApply(app)

      case EmptyTree =>
        PySkip()(pos)

      case _ =>
        // Fall through: treat as expression. The emitter emits any non-void
        // expression as a statement by printing its text on a line.
        genExpr(tree)

  /** Build a `PyAssignable` LHS for an assignment. Scala's typer
   *  guarantees `Assign.lhs` is a valid LHS tree, so the matched cases
   *  cover everything. */
  private def genAssignableLhs(tree: Tree): PyAssignable =
    val pos = posOf(tree)
    tree match
      case id: Ident =>
        PyVarRef(encoding.encodeLocalName(id.symbol))(encoding.encodeType(tree.tpe), pos)
      case sel @ Select(qual, _) =>
        PySelect(
          genExpr(qual),
          encoding.encodeFieldName(sel.symbol)
        )(encoding.encodeType(tree.tpe), pos)
      case _ =>
        report.error(s"Unsupported assignment LHS: ${tree.show}", tree.sourcePos)
        PyVarRef(PyLocalName("_scpy_error"))(PyAnyType, pos)

  // --- Expression generation -----------------------------------------

  private def genExpr(tree: Tree): PyTree =
    val pos = posOf(tree)
    tree match
      case Literal(value) =>
        genLiteral(value, pos)

      case If(cond, thenp, elsep) =>
        PyIf(genExpr(cond), genExpr(thenp), genExpr(elsep))(encoding.encodeType(tree.tpe), pos)

      case t: This =>
        if t.symbol.is(ModuleClass) && t.symbol != currentClassSym then
          PyLoadModule(encoding.encodeClassName(t.symbol))(pos)
        else
          PyThis()(encoding.encodeType(tree.tpe), pos)

      case Select(qualifier, _) =>
        val sym = tree.symbol
        if sym.is(Module) then
          PyLoadModule(encoding.encodeClassName(sym.moduleClass))(pos)
        else
          PySelect(
            genExpr(qualifier),
            encoding.encodeFieldName(sym)
          )(encoding.encodeType(tree.tpe), pos)

      case id: Ident =>
        val sym = id.symbol
        if sym.is(Module) then
          PyLoadModule(encoding.encodeClassName(sym.moduleClass))(pos)
        else
          PyVarRef(encoding.encodeLocalName(sym))(encoding.encodeType(tree.tpe), pos)

      case Block(stats, expr) =>
        // Side effects become pending local defs; return the final expr.
        for s <- stats do pendingLocalDefs += genStat(s)
        genExpr(expr)

      case Typed(sup: Super, _) =>
        PyThis()(encoding.encodeType(tree.tpe), pos)
      case Typed(inner, _) =>
        genExpr(inner)

      case app: Apply =>
        genApply(app)

      case app: TypeApply =>
        genTypeApply(app)

      case tree: Closure =>
        genClosure(tree)

      case Match(selector, cases) =>
        genMatchExpr(selector, cases, pos, encoding.encodeType(tree.tpe))

      case EmptyTree =>
        PyUnitLit()(pos)

      case _ =>
        // Unhandled - silent fallback matching legacy behavior.
        PyUnitLit()(pos)

  // --- Literal generation --------------------------------------------

  private def genLiteral(value: Constant, pos: PyPosition): PyTree =
    value.tag match
      case UnitTag    => PyUnitLit()(pos)
      case BooleanTag => PyBooleanLit(value.booleanValue)(pos)
      case ByteTag    => PyByteLit(value.byteValue)(pos)
      case ShortTag   => PyShortLit(value.shortValue)(pos)
      case CharTag    => PyCharLit(value.charValue)(pos)
      case IntTag     => PyIntLit(value.intValue)(pos)
      case LongTag    => PyLongLit(value.longValue)(pos)
      case FloatTag   => PyFloatLit(value.floatValue)(pos)
      case DoubleTag  => PyDoubleLit(value.doubleValue)(pos)
      case StringTag  => PyStringLit(value.stringValue)(pos)
      case NullTag    => PyNullLit()(pos)
      case ClazzTag   => PyClassOf(encoding.encodeTypeRef(value.typeValue))(pos)
      case _          => PyUnitLit()(pos)

  // --- Apply dispatch ------------------------------------------------

  private def genApply(app: Apply): PyTree =
    val sym = app.fun.symbol
    val pos = posOf(app)

    app.fun match
      // super.method(args)
      case Select(_: Super, _) =>
        genSuperCall(app, pos)

      // new ClassName(args)
      case Select(New(tpt), nme.CONSTRUCTOR) =>
        genApplyNew(app, pos)

      case _ =>
        if primitives.isPrimitive(app) then
          genPrimitiveOp(app, pos)
        else if Erasure.Boxing.isBox(sym) || Erasure.Boxing.isUnbox(sym) then
          genExpr(app.args.head)
        else
          genNormalApply(app, pos)

  private def genSuperCall(app: Apply, pos: PyPosition): PyTree =
    val sym = app.fun.symbol
    val args = app.args.map(genExpr)
    val ownerName = encoding.encodeClassName(sym.owner)
    val methodName = encoding.encodeMethodName(sym)
    val tpe = encoding.encodeType(sym.info.finalResultType)
    val classTpe = PyClassType(encoding.encodeClassName(currentClassSym))
    PyApplyStatically(
      PyApplyFlags.empty,
      PyThis()(classTpe, pos),
      ownerName,
      methodName,
      args
    )(tpe, pos)

  private def genApplyNew(app: Apply, pos: PyPosition): PyTree =
    val Apply(fun @ Select(New(tpt), _), args) = app: @unchecked
    val classSym = tpt.tpe.typeSymbol
    val className = encoding.encodeClassName(classSym)
    val ctorName = encoding.encodeMethodName(fun.symbol)
    PyNew(className, ctorName, args.map(genExpr))(pos)

  private def genNormalApply(app: Apply, pos: PyPosition): PyTree =
    val sym = app.fun.symbol
    val args = app.args.map(genExpr)
    val methodName = encoding.encodeMethodName(sym)
    val ownerName  = encoding.encodeClassName(sym.owner)
    val resultTpe  = encoding.encodeType(sym.info.finalResultType)
    val isStaticTarget =
      sym.is(JavaStatic) || (sym.owner.is(ModuleClass) && !sym.isClassConstructor)

    app.fun match
      case _ if isStaticTarget =>
        PyApplyStatic(
          PyApplyFlags.empty,
          ownerName,
          methodName,
          args
        )(resultTpe, pos)

      case Select(receiver, _) =>
        PyApply(
          PyApplyFlags.empty,
          genExpr(receiver),
          ownerName,
          methodName,
          args
        )(resultTpe, pos)

      case Ident(_) =>
        if sym.owner.is(ModuleClass) then
          PyApplyStatic(
            PyApplyFlags.empty,
            ownerName,
            methodName,
            args
          )(resultTpe, pos)
        else
          PyApplyExternal(
            PyExternalName(methodName.encoded),
            args
          )(resultTpe, pos)

      case _ =>
        PyApplyExternal(
          PyExternalName(methodName.encoded),
          args
        )(resultTpe, pos)

  // --- TypeApply (isInstanceOf / asInstanceOf) -----------------------

  private def genTypeApply(app: TypeApply): PyTree =
    val TypeApply(fun, targs) = app
    val pos = posOf(app)
    val sym = fun.symbol

    if sym == defn.Any_isInstanceOf then
      val receiver = qualifierOf(fun)
      PyIsInstanceOf(genExpr(receiver), encoding.encodeTypeRef(targs.head.tpe))(pos)
    else if sym == defn.Any_asInstanceOf then
      val receiver = qualifierOf(fun)
      PyAsInstanceOf(genExpr(receiver), encoding.encodeType(targs.head.tpe))(pos)
    else
      genExpr(fun)

  // --- Primitive operations ------------------------------------------

  private def genPrimitiveOp(app: Apply, pos: PyPosition): PyTree =
    val Apply(fun, args) = app
    val receiver = qualifierOf(fun)
    val code = primitives.getPrimitive(app, receiver.tpe)

    if isArithmeticOp(code) || isLogicalOp(code) || isComparisonOp(code) then
      genSimpleOp(receiver, args, code, pos)
    else if code == CONCAT then
      genStringConcat(receiver, args, pos)
    else if code == HASH then
      PyApplyExternal(PyExternalName("hash"), List(genExpr(receiver)))(PyIntType, pos)
    else if isArrayOp(code) then
      genArrayOp(app, receiver, args, code, pos)
    else if code == SYNCHRONIZED then
      genExpr(args.head)
    else if isCoercion(code) then
      genCoercion(receiver, code, pos)
    else
      genNormalApply(app, pos)

  private def genSimpleOp(
      receiver: Tree, args: List[Tree], code: Int, pos: PyPosition
  ): PyTree =
    import PyBinaryCode.*
    import PyUnaryCode.*

    val receiverType = receiver.tpe
    val lhs = genExpr(receiver)

    args match
      // Unary operations
      case Nil =>
        code match
          case POS => lhs
          case NEG =>
            if encoding.isIntType(receiverType) then PyUnaryOp(IntNeg, lhs)(pos)
            else if encoding.isLongType(receiverType) then PyUnaryOp(LongNeg, lhs)(pos)
            else if encoding.isFloatType(receiverType) then PyUnaryOp(FloatNeg, lhs)(pos)
            else PyUnaryOp(DoubleNeg, lhs)(pos)
          case NOT =>
            if encoding.isIntType(receiverType) then PyUnaryOp(IntNot, lhs)(pos)
            else PyUnaryOp(LongNot, lhs)(pos)
          case ZNOT => PyUnaryOp(BoolNot, lhs)(pos)
          case _    => lhs

      // Binary operations
      case List(rhs) =>
        val rhsExpr = genExpr(rhs)
        val op: PyBinaryCode = code match
          // Short-circuit booleans
          case ZOR  => BoolOr
          case ZAND => BoolAnd
          // Reference equality
          case ID => RefEq
          case NI => RefNe
          // Equality dispatched by type
          case EQ =>
            if encoding.isIntType(receiverType) then IntEq
            else if encoding.isLongType(receiverType) then LongEq
            else if encoding.isFloatType(receiverType) then FloatEq
            else if encoding.isDoubleType(receiverType) then DoubleEq
            else if encoding.isBooleanType(receiverType) then BoolEq
            else if encoding.isStringType(receiverType) then StringEq
            else RefEq
          case NE =>
            if encoding.isIntType(receiverType) then IntNe
            else if encoding.isLongType(receiverType) then LongNe
            else if encoding.isFloatType(receiverType) then FloatNe
            else if encoding.isDoubleType(receiverType) then DoubleNe
            else if encoding.isBooleanType(receiverType) then BoolNe
            else RefNe
          case LT =>
            if encoding.isIntType(receiverType) then IntLt
            else if encoding.isLongType(receiverType) then LongLt
            else if encoding.isFloatType(receiverType) then FloatLt
            else DoubleLt
          case LE =>
            if encoding.isIntType(receiverType) then IntLe
            else if encoding.isLongType(receiverType) then LongLe
            else if encoding.isFloatType(receiverType) then FloatLe
            else DoubleLe
          case GT =>
            if encoding.isIntType(receiverType) then IntGt
            else if encoding.isLongType(receiverType) then LongGt
            else if encoding.isFloatType(receiverType) then FloatGt
            else DoubleGt
          case GE =>
            if encoding.isIntType(receiverType) then IntGe
            else if encoding.isLongType(receiverType) then LongGe
            else if encoding.isFloatType(receiverType) then FloatGe
            else DoubleGe
          // Arithmetic
          case ADD =>
            if encoding.isIntType(receiverType) then IntAdd
            else if encoding.isLongType(receiverType) then LongAdd
            else if encoding.isFloatType(receiverType) then FloatAdd
            else DoubleAdd
          case SUB =>
            if encoding.isIntType(receiverType) then IntSub
            else if encoding.isLongType(receiverType) then LongSub
            else if encoding.isFloatType(receiverType) then FloatSub
            else DoubleSub
          case MUL =>
            if encoding.isIntType(receiverType) then IntMul
            else if encoding.isLongType(receiverType) then LongMul
            else if encoding.isFloatType(receiverType) then FloatMul
            else DoubleMul
          case DIV =>
            if encoding.isIntType(receiverType) then IntDiv
            else if encoding.isLongType(receiverType) then LongDiv
            else if encoding.isFloatType(receiverType) then FloatDiv
            else DoubleDiv
          case MOD =>
            if encoding.isIntType(receiverType) then IntMod
            else if encoding.isLongType(receiverType) then LongMod
            else if encoding.isFloatType(receiverType) then FloatMod
            else DoubleMod
          // Bitwise
          case OR  => if encoding.isIntType(receiverType) then IntOr  else LongOr
          case AND => if encoding.isIntType(receiverType) then IntAnd else LongAnd
          case XOR => if encoding.isIntType(receiverType) then IntXor else LongXor
          // Shifts
          case LSL => if encoding.isIntType(receiverType) then IntShl else LongShl
          case ASR => if encoding.isIntType(receiverType) then IntShr else LongShr
          case LSR => if encoding.isIntType(receiverType) then IntUShr else LongUShr
          case _   => RefEq  // fallback - unreachable in practice
        PyBinaryOp(op, lhs, rhsExpr)(pos)

      case _ => PyUnitLit()(pos)

  /** String concatenation. The receiver is always String (post-erasure);
   *  wrap any non-String operand in `_scpy_to_str` so Python `+` succeeds. */
  private def genStringConcat(receiver: Tree, args: List[Tree], pos: PyPosition): PyTree =
    def asString(t: Tree): PyTree =
      val e = genExpr(t)
      if e.tpe == PyStringType then e
      else PyApplyExternal(PyExternalName("_scpy_to_str"), List(e))(PyStringType, pos)
    PyBinaryOp(PyBinaryCode.StringConcat, asString(receiver), asString(args.head))(pos)

  private def genArrayOp(
      app: Apply, receiver: Tree, args: List[Tree], code: Int, pos: PyPosition
  ): PyTree =
    if isArrayLength(code) then
      PyUnaryOp(PyUnaryCode.ArrayLength, genExpr(receiver))(pos)
    else if isArrayGet(code) then
      val elemTpe = encoding.encodeType(app.tpe)
      PyArraySelect(genExpr(receiver), genExpr(args.head))(elemTpe, pos)
    else if isArraySet(code) then
      PyAssign(
        PyArraySelect(genExpr(receiver), genExpr(args(0)))(PyAnyType, pos),
        genExpr(args(1))
      )(pos)
    else if isArrayNew(code) then
      val elemRef: PyTypeRef = code match
        case NEW_ZARRAY => PyPrimRef.BooleanRef
        case NEW_BARRAY => PyPrimRef.ByteRef
        case NEW_SARRAY => PyPrimRef.ShortRef
        case NEW_CARRAY => PyPrimRef.CharRef
        case NEW_IARRAY => PyPrimRef.IntRef
        case NEW_LARRAY => PyPrimRef.LongRef
        case NEW_FARRAY => PyPrimRef.FloatRef
        case NEW_DARRAY => PyPrimRef.DoubleRef
        case _          => PyClassRef(PyClassName.ObjectClass)
      PyNewArray(elemRef, genExpr(args.head))(pos)
    else
      PyUnitLit()(pos)

  private def genCoercion(receiver: Tree, code: Int, pos: PyPosition): PyTree =
    import PyUnaryCode.*
    val src = genExpr(receiver)
    code match
      // Identity coercions
      case B2B | S2S | C2C | I2I | L2L | F2F | D2D => src

      // Widening to Int - pass through (Python int is big)
      case B2I | S2I | C2I => src
      case L2I => PyUnaryOp(LongToInt, src)(pos)
      case F2I => PyUnaryOp(FloatToInt, src)(pos)
      case D2I => PyUnaryOp(DoubleToInt, src)(pos)

      // Widening to Long
      case B2L | S2L | C2L | I2L => PyUnaryOp(IntToLong, src)(pos)
      case F2L => PyUnaryOp(FloatToLong, src)(pos)
      case D2L => PyUnaryOp(DoubleToLong, src)(pos)

      // Widening to Float
      case B2F | S2F | C2F | I2F => PyUnaryOp(IntToFloat, src)(pos)
      case L2F => PyUnaryOp(LongToFloat, src)(pos)
      case D2F => PyUnaryOp(DoubleToFloat, src)(pos)

      // Widening to Double
      case B2D | S2D | C2D | I2D => PyUnaryOp(IntToDouble, src)(pos)
      case L2D => PyUnaryOp(LongToDouble, src)(pos)
      case F2D => PyUnaryOp(FloatToDouble, src)(pos)

      // Narrowing to Byte / Short / Char
      case I2B | L2B | F2B | D2B | S2B | C2B => PyUnaryOp(IntToByte, src)(pos)
      case I2S | L2S | F2S | D2S | B2S | C2S => PyUnaryOp(IntToShort, src)(pos)
      case I2C | L2C | F2C | D2C | B2C | S2C => PyUnaryOp(IntToChar, src)(pos)

      case _ => src

  // --- Exception handling --------------------------------------------

  private def genTry(tree: Try): PyTree =
    val Try(block, catches, finalizer) = tree
    val pos = posOf(tree)
    val bodyTree = genStat(block)

    val tryCatch: PyTree =
      if catches.isEmpty then bodyTree
      else
        val exVar = PyLocalName("_scpy_ex")
        val exVarRef = PyVarRef(exVar)(PyAnyType, pos)

        val handler = catches.foldRight[PyTree](
          PyUnaryOp(PyUnaryCode.Throw, exVarRef)(pos)  // default: rethrow
        ) { (caseDef, elsePart) =>
          val CaseDef(pat, _, body) = caseDef  // guards dropped
          val (exnTypeRef, bindOpt): (Option[PyTypeRef], Option[Symbol]) = pat match
            case Typed(Ident(nme.WILDCARD), tpt) =>
              (Some(encoding.encodeTypeRef(tpt.tpe)), None)
            case Ident(nme.WILDCARD) =>
              (None, None)
            case Bind(_, Typed(_, tpt)) =>
              (Some(encoding.encodeTypeRef(tpt.tpe)), Some(pat.symbol))
            case Bind(_, _) =>
              (None, Some(pat.symbol))
            case _ =>
              (None, None)

          val handlerBody: PyTree = bindOpt match
            case Some(bindSym) =>
              val bindDef = PyVarDef(
                encoding.encodeLocalName(bindSym),
                encoding.originalNameOf(bindSym),
                PyAnyType, false, exVarRef
              )(pos)
              PyBlock(List(bindDef), genStat(body))(pos)
            case None =>
              genStat(body)

          exnTypeRef match
            case Some(ref) =>
              PyIf(
                PyIsInstanceOf(exVarRef, ref)(pos),
                handlerBody,
                elsePart
              )(PyVoidType, pos)
            case None =>
              handlerBody  // catch-all
        }

        PyTryCatch(
          bodyTree, exVar, PyOriginalName.NoOriginalName, handler
        )(PyVoidType, pos)

    if finalizer.isEmpty then tryCatch
    else PyTryFinally(tryCatch, genStat(finalizer))(pos)

  // --- Match generation ----------------------------------------------

  private def genMatchExpr(
      selector: Tree, cases: List[CaseDef], pos: PyPosition, resultTpe: PyType
  ): PyTree =
    val sel = genExpr(selector)
    val litCases = mutable.ListBuffer.empty[(List[PyMatchableLiteral], PyTree)]
    var defaultTree: PyTree = PyUnitLit()(pos)
    var defaultSet = false

    for caseDef <- cases do
      caseDef match
        case CaseDef(Literal(c), _, body) =>
          val lit = genLiteral(c, pos) match
            case ml: PyMatchableLiteral => ml
            case _ => PyNullLit()(pos): PyMatchableLiteral
          litCases += ((List(lit), genExpr(body)))
        case CaseDef(_, _, body) =>
          if !defaultSet then
            defaultTree = genExpr(body)
            defaultSet = true

    PyMatch(sel, litCases.toList, defaultTree)(resultTpe, pos)

  // --- Closure generation --------------------------------------------

  /** Emit a Scala closure. Known limitation: captures are silently
   *  dropped (same broken behavior as the legacy backend). We emit a
   *  `self.<method>` Python bound-method reference which at least
   *  syntactically typechecks. */
  private def genClosure(tree: Closure): PyTree =
    val pos = posOf(tree)
    val targetSym = tree.meth.symbol
    val methodName = encoding.encodeMethodName(targetSym)
    val owner = encoding.encodeClassName(currentClassSym)
    PySelect(
      PyThis()(PyClassType(owner), pos),
      PyFieldName(owner, PySimpleFieldName(methodName.encoded))
    )(PyAnyType, pos)

  // --- File output ---------------------------------------------------

  private def linkAndWrite(): Unit =
    val linkedBundle =
      try
        PyLinker.link(List(PyLinker.Input(generatedClasses.toList, mainEntry)))
      catch
        case err: PyLinkingException =>
          reportLinkerErrors(err.errors)
          return

    val outputDirectory = genCtx.settings.outputDir.value
    val sourceName = genCtx.compilationUnit.source.file.name.stripSuffix(".scala")
    val outfile = outputDirectory.fileNamed(sourceName + ".py")
    val output = outfile.bufferedOutput
    try
      val writer = new java.io.PrintWriter(output)
      try
        PyIREmitter.emit(linkedBundle.classes, linkedBundle.mainEntry, writer)
        writer.flush()
      finally writer.close()
    finally output.close()

  private def reportLinkerErrors(errors: List[PyLinkingError]): Unit =
    errors.foreach { err =>
      report.error(err.message, sourcePosOf(err.pos))
    }

  private def sourcePosOf(pos: PyPosition): SourcePosition =
    if pos.isEmpty then NoSourcePosition
    else
      val source = genCtx.compilationUnit.source
      if source.path != pos.source then NoSourcePosition
      else
        source.lineToOffsetOpt(pos.line) match
          case Some(lineOffset) =>
            val offset = (lineOffset + pos.column).max(0).min(source.length)
            source.atSpan(Span(offset))
          case None =>
            NoSourcePosition

  // --- Helpers -------------------------------------------------------

  private def posOf(tree: Tree): PyPosition =
    val pos = tree.sourcePos
    if pos.exists then PyPosition(pos.source.path, pos.line, pos.column)
    else PyPosition.NoPosition

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

  /** Flush `pendingLocalDefs` and flatten nested `PyBlock`s. */
  private def flattenToStmts(tree: PyTree): List[PyTree] =
    val prefix = pendingLocalDefs.toList
    pendingLocalDefs.clear()
    val flat: List[PyTree] = tree match
      case PyBlock(stats, expr) =>
        stats.flatMap(flattenToStmts) ::: flattenToStmts(expr)
      case _: PyUnitLit => Nil
      case _: PySkip    => Nil
      case other        => List(other)
    prefix ::: flat
