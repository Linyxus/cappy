package cavia
package typechecking

import core.*, ast.*
import reporting.IndentedPrinter
import Expr.*
import TypeChecker.*

class ExprPrinter extends IndentedPrinter:
  def showBinders(bds: List[Binder])(using Context): List[String] = bds match
    case Nil => Nil
    case p :: ps =>
      val s = TypePrinter.show(p)
      s :: showBinders(ps)(using ctx.extend(p))

  def showBindersWithVariances(bds: List[Binder], variances: List[Variance])(using Context): List[String] =
    (bds, variances) match
      case (Nil, Nil) => Nil
      case (p :: ps, v :: vs) =>
        val s = TypePrinter.show(p)
        val vStr = showVariance(v)
        s"$vStr$s" :: showBindersWithVariances(ps, vs)(using ctx.extend(p))
      case _ => assert(false)

  def showVariance(v: Variance)(using Context): String = v match
    case Variance.Covariant => "+"
    case Variance.Contravariant => "-"
    case Variance.Invariant => ""

  def show(t: Expr.Term)(using Context): Unit = 
    t match
      case Term.BinderRef(idx) => print(getBinder(idx).name)
      case Term.SymbolRef(sym) => print(sym.name)
      case Term.StrLit(value) => print(s""""$value"""")
      case Term.IntLit(value) => print(value.toString)
      case Term.CharLit(value) =>
        value match
          case '\n' => print("'\\n'")
          case '\r' => print("'\\r'")
          case '\t' => print("'\\t'")
          case '\\' => print("'\\\\'")
          case _ => print(s"'${value}'")
      case Term.BoolLit(value) => print(value.toString)
      case Term.UnitLit() => print("()")
      case Term.TermLambda(params, body, _) =>
        print("(")
        print(showBinders(params).mkString(", "))
        print(") => {")
        newline()
        indented:
          show(body)(using ctx.extend(params))
        newline()
        println("}")
      case Term.Select(base, fieldInfo) =>
        show(base)
        print(".")
        print(fieldInfo.name)
      case Term.TypeLambda(params, body) =>
        print("[")
        print(showBinders(params).mkString(", "))
        print("] => {")
        newline()
        indented:
          show(body)(using ctx.extend(params))
        newline()
        println("}")
      case Term.Bind(binder, recursive, expr, body) =>
        print("val ")
        print(binder.name)
        print(": ")
        showType(binder.tpe)
        print(" = ")
        newline()
        indented:
          val ctx1 = if recursive then ctx.extend(binder) else ctx
          show(expr)(using ctx1)
        newline()
        show(body)(using ctx.extend(binder))
      case Term.PrimOp(op, targs, args) =>
        print(op.toString)
        if targs.nonEmpty then
          print("[")
          targs.zipWithIndex.foreach: (targ, idx) =>
            showType(targ)
            if idx < targs.size - 1 then
              print(", ")
          print("]")
        print("(")
        args.zipWithIndex.foreach: (arg, idx) =>
          show(arg)
          if idx < args.size - 1 then
            print(", ")
        print(")")
      case Term.StructInit(sym, targs, args) =>
        print(sym.name)
        if targs.nonEmpty then
          print("[")
          targs.zipWithIndex.foreach: (targ, idx) =>
            showType(targ)
            if idx < targs.size - 1 then
              print(", ")
          print("]")
        print("(")
        args.zipWithIndex.foreach: (arg, idx) =>
          show(arg)
          if idx < args.size - 1 then
            print(", ")
        print(")")
      case Term.Apply(fun, args) =>
        show(fun)
        print("(")
        args.zipWithIndex.foreach: (arg, idx) =>
          show(arg)
          if idx < args.size - 1 then
            print(", ")
        print(")")
      case Term.TypeApply(term, targs) =>
        show(term)
        print("[")
        targs.zipWithIndex.foreach: (targ, idx) =>
          targ match
            case targ: CaptureSet =>
              print(TypePrinter.show(targ))
            case targ: Type =>
              print(TypePrinter.show(targ))
          if idx < targs.size - 1 then
            print(", ")
        print("]")
      case Term.If(cond, thenBranch, elseBranch) =>
        print("if ")
        show(cond)
        print(" then")
        newline()
        indented:
          show(thenBranch)
        newline()
        print("else")
        newline()
        indented:
          show(elseBranch)
      case Term.Match(scrutinee, cases) =>
        show(scrutinee)
        print(" match {")
        newline()
        indented:
          cases.foreach: cas =>
            print("case ")
            showPattern(cas.pat)
            print(" => {")
            newline()
            indented:
              show(cas.body)(using ctx.extend(bindersInPattern(cas.pat)))
            newline()
            print("}")
            newline()
        print("}")
      case Term.ResolveExtension(sym, targs, methodName) =>
        print(s"extension(${sym.name})")
        if targs.nonEmpty then
          print("[")
          targs.zipWithIndex.foreach: (targ, idx) =>
            showType(targ)
            if idx < targs.size - 1 then
              print(", ")
          print("]")
        print(".")
        print(methodName)
  def showType(tpe: Type | CaptureSet)(using Context): Unit =
    tpe match
      case tpe: Type =>
        print(TypePrinter.show(tpe))
      case cs: CaptureSet =>
        print(TypePrinter.show(cs))

  def showPattern(pat: Pattern)(using Context): Unit =
    pat match
      case Pattern.Wildcard() =>
        print("_")
      case Pattern.Bind(binder, pat) =>
        print(binder.name)
        print(" @ ")
        showPattern(pat)
      case Pattern.EnumVariant(constructor, _, _, fields) =>
        print(constructor.name)
        print("(")
        fields.zipWithIndex.foreach: (field, idx) =>
          showPattern(field)
          if idx < fields.size - 1 then
            print(", ")
        print(")")
        
  def showModule(mod: Module)(using Context): Unit =
    print(s"module ${mod.name} {")
    newline()
    indented:
      mod.defns.foreach(showDefinition)
    println("}")

  def showDefinition(defn: Definition)(using Context): Unit =
    defn match
      case Definition.ValDef(sym, expr) =>
        print("val ")
        print(sym.name)
        print(": ")
        showType(sym.tpe)
        print(" = ")
        newline()
        indented:
          show(expr)
        newline()
      case Definition.StructDef(sym) =>
        print("struct ")
        print(sym.name)
        val binders = sym.info.targs
        val binderStrs = showBindersWithVariances(binders, sym.info.variances)
        if binders.nonEmpty then
          print("[")
          print(binderStrs.mkString(", "))
          print("]")
        print("(")
        val ctx1 = ctx.extend(binders)
        sym.info.fields.zipWithIndex.foreach: (field, idx) =>
          if field.mutable then
            print("var ")
          print(field.name)
          print(": ")
          showType(field.tpe)(using ctx1)
          if idx < sym.info.fields.size - 1 then
            print(", ")
        print(")")
        newline()
      case Definition.EnumDef(sym) =>
        print("enum ")
        print(sym.name)
        val info = sym.info
        val binders = info.targs
        val variances = info.variances
        val binderStrs = showBindersWithVariances(binders, variances)
        if binders.nonEmpty then
          print("[")
          print(binderStrs.mkString(", "))
          print("]")
        print(" {")
        newline()
        indented:
          info.variants.foreach: variantSymbol =>
            showDefinition(Definition.StructDef(variantSymbol))
        print("}")
        newline()
      case Definition.ExtensionDef(sym) =>
        print("extension ")
        print(sym.name)
        val info = sym.info
        val binderStrs = showBinders(info.typeParams)
        if info.typeParams.nonEmpty then
          print("[")
          print(binderStrs.mkString(", "))
          print("]")
        val ctx1 = ctx.extend(info.typeParams)
        print("(?: ")
        print(TypePrinter.show(info.selfArgType)(using ctx1))
        print(") {")
        newline()
        indented:
          info.methods.foreach: method =>
            print("val ")
            print(method.name)
            print(": ")
            showType(method.tpe)(using ctx1)
            print(" = ")
            newline()
            indented:
              show(method.body)(using ctx1)
            newline()
        newline()
        print("}")
        newline()
      case Definition.TypeDef(sym) =>
        val TypeDefInfo(typeBinders, variances, body) = sym.info
        print("type ")
        print(sym.name)
        val binderStrs = showBindersWithVariances(typeBinders, variances)
        if binderStrs.nonEmpty then
          print("[")
          print(binderStrs.mkString(", "))
          print("]")
        print(" = ")
        showType(body)(using ctx.extend(typeBinders))
        newline()

object ExprPrinter:
  def show(t: Expr.Term)(using Context): String =
    val printer = ExprPrinter()
    printer.show(t)
    printer.result()

  def show(mod: Module)(using Context): String =
    val printer = ExprPrinter()
    printer.showModule(mod)
    printer.result()

extension (self: Term)
  def like(other: Term)(using Context): self.type =
    self.setCV(other.cv)
    self.setTpe(other.tpe)
    if other.hasPos then
      self.setPos(other.pos)
    self.setMeta(other.meta)
    self

  def derivedBinderRef(idx: Int)(using Context): Term =
    self match
      case Term.BinderRef(idx1) if idx1 == idx => self
      case _ => Term.BinderRef(idx).like(self)

  def derivedSymbolRef(sym: DefSymbol)(using Context): Term =
    self match
      case Term.SymbolRef(sym1) if sym1 eq sym => self
      case _ => Term.SymbolRef(sym).like(self)

  def derivedStrLit(value: String)(using Context): Term =
    self match
      case Term.StrLit(value1) if value1 == value => self
      case _ => Term.StrLit(value).like(self)

  def derivedIntLit(value: Int)(using Context): Term =
    self match
      case Term.IntLit(value1) if value1 == value => self
      case _ => Term.IntLit(value).like(self)

  def derivedCharLit(value: Char)(using Context): Term =
    self match
      case Term.CharLit(value1) if value1 == value => self
      case _ => Term.CharLit(value).like(self)

  def derivedUnitLit(using Context): Term =
    self match
      case Term.UnitLit() => self
      case _ => Term.UnitLit().like(self)

  def derivedTermLambda(params: List[Binder.TermBinder], body: Term, skolemizedBinders: List[Binder.TermBinder])(using Context): Term =
    self match
      case Term.TermLambda(params1, body1, skolemizedBinders1) if (params1 eq params) && (body1 eq body) && (skolemizedBinders1 eq skolemizedBinders) => self
      case _ => Term.TermLambda(params, body, skolemizedBinders).like(self)

  def derivedTypeLambda(params: List[Binder.TypeBinder | Binder.CaptureBinder], body: Term)(using Context): Term =
    self match
      case Term.TypeLambda(params1, body1) if (params1 eq params) && (body1 eq body) => self
      case _ => Term.TypeLambda(params, body).like(self)

  def derivedBind(binder: Binder.TermBinder, recursive: Boolean, expr: Term, body: Term)(using Context): Term =
    self match
      case Term.Bind(binder1, recursive1, expr1, body1) if (binder1 eq binder) && (recursive1 == recursive) && (expr1 eq expr) && (body1 eq body) => self
      case _ => Term.Bind(binder, recursive, expr, body).like(self)

  def derivedPrimOp(op: PrimitiveOp, targs: List[Type], args: List[Term])(using Context): Term =
    self match
      case Term.PrimOp(op1, targs1, args1) if (op1 == op) && (targs1 eq targs) && (args1 eq args) => self
      case _ => Term.PrimOp(op, targs, args).like(self)

  def derivedStructInit(sym: StructSymbol, targs: List[Type | CaptureSet], args: List[Term])(using Context): Term =
    self match
      case Term.StructInit(sym1, targs1, args1) if (sym1 eq sym) && (targs1 eq targs) && (args1 eq args) => self
      case _ => Term.StructInit(sym, targs, args).like(self)
      
  def derivedApply(fun: Term, args: List[Term])(using Context): Term =
    self match
      case Term.Apply(fun1, args1) if (fun1 eq fun) && (args1 eq args) => self
      case _ => Term.Apply(fun, args).like(self)
      
  def derivedTypeApply(term: Term, targs: List[Type | CaptureSet])(using Context): Term =
    self match
      case Term.TypeApply(term1, targs1) if (term1 eq term) && (targs1 eq targs) => self
      case _ => Term.TypeApply(term, targs).like(self)

  def derivedSelect(base: Term, field: FieldInfo)(using Context): Term =
    self match
      case Term.Select(base1, field1) if (base1 eq base) && (field1 eq field) => self
      case _ => Term.Select(base, field).like(self)

  def derivedIf(cond: Term, thenBranch: Term, elseBranch: Term)(using Context): Term =
    self match
      case Term.If(cond1, thenBranch1, elseBranch1) if (cond1 eq cond) && (thenBranch1 eq thenBranch) && (elseBranch1 eq elseBranch) => self
      case _ => Term.If(cond, thenBranch, elseBranch).like(self)

  def derivedResolveExtension(sym: ExtensionSymbol, targs: List[Type | CaptureSet], methodName: String)(using Context): Term =
    self match
      case Term.ResolveExtension(sym1, targs1, methodName1) if (sym1 eq sym) && (targs1 eq targs) && (methodName1 == methodName) => self
      case _ => Term.ResolveExtension(sym, targs, methodName).like(self)

  def derivedMatch(scrutinee: Term, cases: List[MatchCase])(using Context): Term =
    self match
      case Term.Match(scrutinee1, cases1) if (scrutinee1 eq scrutinee) && (cases1 eq cases) => self
      case _ => Term.Match(scrutinee, cases).like(self)

def makeUnitLit(srcPos: SourcePos)(using Context): Term =
  Term.UnitLit().withPos(srcPos).withTpe(Definitions.unitType).withCV(CaptureSet.empty)

extension (self: Term)
  def isTypePolymorphic: Boolean =
    self match
      case Term.TypeLambda(params, _) => params.exists:
        case binder: Binder.TypeBinder => true
        case _ => false
      case _ => false

object TypePolymorphism:
  def unapply(t: Term): Option[(List[Binder.TypeBinder | Binder.CaptureBinder], Term)] =
    t match
      case Term.TypeLambda(params, body) if t.isTypePolymorphic => 
        Some((params, body))
      case _ => None

object Synthetics:
  def etaExpand(t: Syntax.Term, binders: List[Binder.TermBinder])(using Context): Syntax.Term = 
    val params = binders.map: binder =>
      Syntax.TermParam(Fresh.freshName("local"), Syntax.Type.Splice(binder.tpe).withPosFrom(binder.tpe), binder.isConsume).withPosFrom(binder)
    val lambdaBody = Syntax.Term.Apply(t, params.map(p => Syntax.Term.Ident(p.name).withPosFrom(t))).withPosFrom(t)
    val lambda = Syntax.Term.Lambda(params, lambdaBody).withPosFrom(t)
    lambda
