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
  private val pyDefn = PyDefinitions.pydefn
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
    pyDefn.force()
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
      if encoding.hasExternAnnotation(sym) then
        // Force the binding read even if no call site reaches this facade -
        // otherwise malformed `@extern` args go unreported when the class
        // is never referenced. `externBindingOf` reports the diagnostic
        // through a dedupe set so repeated calls are safe.
        encoding.externBindingOf(sym)
        validateFacadeMemberNames(td)
      else if !sym.isPrimitiveValueClass && sym != defn.ArrayClass then
        currentClassSym = sym
        val kind =
          if sym.is(Trait) then PyClassKind.Interface
          else if isStaticModule(sym) then PyClassKind.ModuleClass
          else PyClassKind.Class
        val classDef = genClassDef(td, kind)
        generatedClasses += classDef
        if genCtx.platform.hasMainMethod(sym) then
          mainEntry = Some((classDef.name, kind))

  /** For each `@extern`-annotated class/object, check that no two members
   *  resolve to the same Python name. Python has no overloading, so
   *  colliding facade members would silently shadow each other on calls.
   *
   *  Only `DefDef`s are checked: every `val` generates a synthetic
   *  accessor `DefDef` that shares its name, so ValDefs would double-count
   *  if included. Call sites always resolve to the accessor, so the
   *  DefDef-only view matches runtime semantics. */
  private def validateFacadeMemberNames(td: TypeDef): Unit =
    val seen = mutable.Map.empty[String, Symbol]
    for tree <- collectMemberDefs(td) do tree match
      case dd: DefDef if !dd.symbol.isClassConstructor =>
        checkFacadeMember(dd.symbol, seen)
      case _ => ()

  private def checkFacadeMember(sym: Symbol, seen: mutable.Map[String, Symbol]): Unit =
    if encoding.externBindingOf(sym).isEmpty then
      val pyName = encoding.externMemberNameOf(sym)
      seen.get(pyName) match
        case Some(prior) =>
          report.error(
            s"Facade member '${sym.name.show}' resolves to Python name '$pyName', " +
              s"which collides with '${prior.name.show}' in the same facade. " +
              "Python has no overloading; use `@name` to disambiguate.",
            sym.srcPos
          )
        case None =>
          seen(pyName) = sym

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
          if sym.is(Module) then ()
          else if encoding.hasExternAnnotation(sym) then
            // Proactive validation: force binding read so malformed args
            // are reported even if the val is never referenced.
            encoding.externBindingOf(sym)
          else
            fields += PyFieldDef(
              flags        = PyMemberFlags.empty.withMutable(sym.is(Mutable)),
              name         = encoding.encodeFieldName(sym),
              originalName = encoding.originalNameOf(sym),
              ftpe         = encoding.encodeType(sym.info),
              pos          = posOf(vd)
            )

        case dd: DefDef =>
          if dd.symbol.isClassConstructor then ()
          else if encoding.hasExternAnnotation(dd.symbol) then
            // Proactive validation: force binding read so malformed args
            // are reported even if the def is never called.
            encoding.externBindingOf(dd.symbol)
          else
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
        val sel = tree.asInstanceOf[Select]
        if encoding.isFacadeOwner(sel.symbol.owner) then
          genFacadeSelect(sel, pos)
        else
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
        encoding.externBindingOf(sym) match
          case Some(binding) =>
            genExternRef(binding, tree.tpe, pos)
          case None =>
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
    val pos = posOf(app)

    // `throw <expr>` is encoded as `Apply(<special-ops>.throw, [expr])`
    // by `tpd.Throw`. The owner `<special-ops>` is a synthetic package
    // class with no Python representation, so we lower the call to a
    // PyIR Throw unary op.
    if app.fun.symbol == defn.throwMethod then
      return PyUnaryOp(PyUnaryCode.Throw, genExpr(app.args.head))(pos)

    app.fun match
      case id: Ident if encoding.externBindingOf(id.symbol).isDefined =>
        genExternCall(id.symbol, app.args, pos)

      // super.method(args)
      case Select(_: Super, _) =>
        genSuperCall(app, pos)

      // new ClassName(args)
      case Select(New(tpt), nme.CONSTRUCTOR) if encoding.externBindingOf(tpt.tpe.typeSymbol).isDefined =>
        genFacadeNew(app, pos)

      case Select(New(tpt), nme.CONSTRUCTOR) =>
        genApplyNew(app, pos)

      case sel @ Select(_, _) if encoding.isFacadeOwner(sel.symbol.owner) =>
        if sel.symbol.is(Accessor) && app.args.isEmpty then genFacadeSelect(sel, pos)
        else genFacadeCall(sel, app.args, pos)

      case _ =>
        val sym = app.fun.symbol
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

  private def genFacadeNew(app: Apply, pos: PyPosition): PyTree =
    val Apply(Select(New(tpt), _), args) = app: @unchecked
    val classSym = tpt.tpe.typeSymbol
    val binding = encoding.externBindingOf(classSym).get
    PyApplyDynamic(
      genExternRef(binding, tpt.tpe, pos),
      args.map(genExpr),
      Nil
    )(encoding.encodeType(app.tpe), pos)

  private def genNormalApply(app: Apply, pos: PyPosition): PyTree =
    val sym = app.fun.symbol
    genDynamicApply(app, pos).getOrElse {
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
    }

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

  private def genExternRef(binding: ExternBinding, tp: Type, pos: PyPosition): PyExternalRef =
    PyExternalRef(binding.module, binding.path)(encoding.encodeType(tp), pos)

  private def genExternCall(sym: Symbol, args: List[Tree], pos: PyPosition): PyTree =
    val binding = encoding.externBindingOf(sym).get
    PyApplyDynamic(
      genExternRef(binding, sym.info.finalResultType, pos),
      args.map(genExpr),
      Nil
    )(encoding.encodeType(sym.info.finalResultType), pos)

  private def genFacadeSelect(sel: Select, pos: PyPosition): PyTree =
    // Chained facade rebinding: if the selected member has its own @extern
    // binding, produce a fresh PyExternalRef from that binding instead of
    // extending the qualifier's path. This lets nested @extern objects
    // resolve to their declared Python module even when accessed through
    // an enclosing facade whose module is different (e.g. `np.linalg`
    // where `linalg` is rebound to `scipy.linalg`). For the same-module
    // case the rebinding is a no-op because the extension would produce
    // the same PyExternalRef.
    encoding.externBindingOf(sel.symbol) match
      case Some(binding) =>
        genExternRef(binding, sel.tpe, pos)
      case None =>
        val memberName = encoding.externMemberNameOf(sel.symbol)
        genDynamicSelect(
          genExpr(sel.qualifier),
          PyStringLit(memberName)(pos),
          Some(memberName),
          encoding.encodeType(sel.tpe),
          pos
        )

  private def genFacadeCall(sel: Select, args: List[Tree], pos: PyPosition): PyTree =
    PyApplyDynamic(
      genFacadeSelect(sel, pos),
      args.map(genExpr),
      Nil
    )(encoding.encodeType(sel.symbol.info.finalResultType), pos)

  private def genDynamicApply(app: Apply, pos: PyPosition): Option[PyTree] =
    val sym = app.fun.symbol
    if matchesPySymbol(sym, pyDefn.PyDynamic_selectDynamic, "scala.python.PyDynamic.selectDynamic") then
      app.args match
        case nameArg :: Nil =>
          val nameExpr = genExpr(nameArg)
          Some(genDynamicSelect(
            genExpr(qualifierOf(app.fun)),
            nameExpr,
            literalString(nameArg),
            encoding.encodeType(app.tpe),
            pos
          ))
        case _ =>
          Some(PyUnitLit()(pos))
    else if matchesPySymbol(sym, pyDefn.PyDynamic_applyDynamic, "scala.python.PyDynamic.applyDynamic") then
      app.args match
        case nameArg :: dynArgs :: Nil =>
          val callee = genDynamicSelect(
            genExpr(qualifierOf(app.fun)),
            genExpr(nameArg),
            literalString(nameArg),
            PyAnyType,
            pos
          )
          Some(PyApplyDynamic(
            callee,
            extractRepeatedArgs(dynArgs).map(genExpr),
            Nil
          )(encoding.encodeType(app.tpe), pos))
        case _ =>
          Some(PyUnitLit()(pos))
    else if matchesPySymbol(sym, pyDefn.PyDynamic_applyDynamicNamed, "scala.python.PyDynamic.applyDynamicNamed") then
      app.args match
        case nameArg :: kwargsArg :: Nil =>
          Some(genApplyDynamicNamedCall(app, nameArg, kwargsArg, pos))
        case _ =>
          Some(PyUnitLit()(pos))
    else if matchesPySymbol(sym, pyDefn.PyDynamic_updateDynamic, "scala.python.PyDynamic.updateDynamic") then
      app.args match
        case nameArg :: valueArg :: Nil =>
          Some(genDynamicSetAttr(
            genExpr(qualifierOf(app.fun)),
            genExpr(nameArg),
            genExpr(valueArg),
            pos
          ))
        case _ =>
          Some(PyUnitLit()(pos))
    else if matchesPySymbol(sym, pyDefn.DynamicModule_module, "scala.python.Dynamic.module") then
      app.args match
        case moduleArg :: Nil =>
          Some(genDynamicModuleRef(moduleArg, encoding.encodeType(app.tpe), pos))
        case _ =>
          Some(PyUnitLit()(pos))
    else if matchesPySymbol(sym, pyDefn.DynamicModule_attr, "scala.python.Dynamic.attr") then
      app.args match
        case pathArg :: Nil =>
          Some(genDynamicBuiltinsAttr(pathArg, encoding.encodeType(app.tpe), pos))
        case _ =>
          Some(PyUnitLit()(pos))
    else None

  private def genDynamicSelect(
      receiver: PyTree,
      nameExpr: PyTree,
      literalName: Option[String],
      resultTpe: PyType,
      pos: PyPosition
  ): PyTree =
    literalName match
      case Some(name) if encoding.isValidPyAttrName(name) =>
        receiver match
          case ref: PyExternalRef =>
            PyExternalRef(ref.module, ref.path :+ name)(resultTpe, pos)
          case _ =>
            PyAttrAccess(receiver, name)(resultTpe, pos)
      case _ =>
        genGetAttr(receiver, nameExpr, resultTpe, pos)

  private def genGetAttr(
      receiver: PyTree,
      nameExpr: PyTree,
      resultTpe: PyType,
      pos: PyPosition
  ): PyTree =
    PyApplyDynamic(
      PyExternalRef("builtins", List("getattr"))(PyAnyType, pos),
      List(receiver, nameExpr),
      Nil
    )(resultTpe, pos)

  /** Emit an attribute assignment. Always lowered to `setattr(obj, name, value)`
   *  so keyword names and non-literal names work uniformly. Phase 2 can
   *  optimize the literal-identifier case to direct `obj.name = value` via
   *  a dedicated IR shape if desired. */
  private def genDynamicSetAttr(
      receiver: PyTree,
      nameExpr: PyTree,
      value: PyTree,
      pos: PyPosition
  ): PyTree =
    PyApplyDynamic(
      PyExternalRef("builtins", List("setattr"))(PyAnyType, pos),
      List(receiver, nameExpr, value),
      Nil
    )(PyVoidType, pos)

  /** Lower a `d.applyDynamicNamed(methodName)(pairs*)` call where each
   *  pair is a `(String, Any)` tuple. Scala desugars both named-argument
   *  calls (`d.foo(k=v)`) and mixed positional+named calls
   *  (`d.foo(x, k=v)`) to this shape, with positional arguments getting
   *  empty-string names as the sentinel.
   *
   *  Pairs with an empty-string name become positional arguments of the
   *  emitted `PyApplyDynamic`; pairs with a non-empty name become keyword
   *  arguments. Runtime-computed keyword names and names that are not
   *  valid Python identifiers are rejected with a compile error — Python
   *  keyword-argument syntax requires static identifiers. */
  private def genApplyDynamicNamedCall(
      app: Apply,
      nameArg: Tree,
      kwargsArg: Tree,
      pos: PyPosition
  ): PyTree =
    val rawPairs = extractRepeatedArgs(kwargsArg)
    val parsed = rawPairs.map(extractKwargPair)
    val firstMissing = parsed.indexWhere(_.isEmpty)
    if firstMissing >= 0 then
      report.error(
        "scala.python.PyDynamic.applyDynamicNamed requires literal-string keyword " +
          "names; runtime-computed names are not supported.",
        rawPairs(firstMissing).sourcePos
      )
      PyUnitLit()(pos)
    else
      val resolved = parsed.flatten
      val invalidKw = resolved.find { case (name, _) =>
        name.nonEmpty && !encoding.isValidPyAttrName(name)
      }
      invalidKw match
        case Some((name, _)) =>
          report.error(
            s"Python keyword-argument name '$name' is not a valid identifier. " +
              "Python named-call syntax requires identifiers that are not reserved words.",
            app.sourcePos
          )
          PyUnitLit()(pos)
        case None =>
          val callee = genDynamicSelect(
            genExpr(qualifierOf(app.fun)),
            genExpr(nameArg),
            literalString(nameArg),
            PyAnyType,
            pos
          )
          val (positional, keyword) = resolved.partition { case (name, _) => name.isEmpty }
          val posArgs = positional.map { case (_, valueTree) => genExpr(valueTree) }
          val kwPairs = keyword.map { case (name, valueTree) => (name, genExpr(valueTree)) }
          PyApplyDynamic(callee, posArgs, kwPairs)(encoding.encodeType(app.tpe), pos)

  /** Unwrap a `(String, Any)` tuple construction tree to its name and value
   *  trees. Returns `None` if the first element is not a string literal.
   *
   *  Post-erasure, Scala tuple constructions appear as `Tuple2.apply(k, v)`
   *  or `new Tuple2(k, v)`. We match any 2-argument `Apply` whose first
   *  argument resolves to a literal string — in the context of
   *  `applyDynamicNamed`'s varargs, the only trees that can reach this
   *  matcher are `Tuple2` constructions anyway, so the permissive form
   *  is safe. */
  private def extractKwargPair(tree: Tree): Option[(String, Tree)] =
    tree match
      case Apply(_, List(nameArg, valueArg)) =>
        literalString(nameArg).map(_ -> valueArg)
      case Typed(inner, _)       => extractKwargPair(inner)
      case Inlined(_, Nil, expr) => extractKwargPair(expr)
      case Block(Nil, expr)      => extractKwargPair(expr)
      case _                     => None

  private def genDynamicModuleRef(moduleArg: Tree, resultTpe: PyType, pos: PyPosition): PyTree =
    literalString(moduleArg) match
      case Some(moduleName) =>
        PyExternalRef(moduleName, Nil)(resultTpe, pos)
      case None =>
        PyApplyDynamic(
          PyExternalRef("importlib", List("import_module"))(PyAnyType, pos),
          List(genExpr(moduleArg)),
          Nil
        )(resultTpe, pos)

  private def genDynamicBuiltinsAttr(pathArg: Tree, resultTpe: PyType, pos: PyPosition): PyTree =
    literalString(pathArg) match
      case Some(path) =>
        PyExternalRef("builtins", path.split('.').toList.filter(_.nonEmpty))(resultTpe, pos)
      case None =>
        genGetAttr(
          PyExternalRef("builtins", Nil)(PyAnyType, pos),
          genExpr(pathArg),
          resultTpe,
          pos
        )

  private def extractRepeatedArgs(tree: Tree): List[Tree] =
    tree match
      case seq: JavaSeqLiteral =>
        seq.elems
      case Apply(_, List(arg)) =>
        extractRepeatedArgs(arg)
      case Typed(inner, _) =>
        extractRepeatedArgs(inner)
      case Block(Nil, expr) =>
        extractRepeatedArgs(expr)
      case Inlined(_, Nil, expr) =>
        extractRepeatedArgs(expr)
      case other =>
        List(other)

  private def matchesPySymbol(sym: Symbol, expected: Symbol, expectedFullName: String): Boolean =
    sym == expected || sym.showFullName == expectedFullName

  private def literalString(tree: Tree): Option[String] =
    tree match
      case Literal(Constant(value: String)) =>
        Some(value)
      case Typed(inner, _) =>
        literalString(inner)
      case Block(Nil, expr) =>
        literalString(expr)
      case Inlined(_, Nil, expr) =>
        literalString(expr)
      case _ =>
        None

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
