package cavia
package parsing
import core.ast.*
import tokenizing.*
import scala.collection.mutable

object Parsers:
  import Syntax.*
  import Parser.*

  enum Assoc:
    case Left, Right, Non
  enum OpRule:
    case Infix(assoc: Assoc, op: Parser[InfixOp])
    case Prefix(op: Parser[PrefixOp])
  enum OpGroup:
    case InfixGroup(rules: List[OpRule.Infix])
    case PrefixGroup(rules: List[OpRule.Prefix])
  object OpGroup:
    def infix(rules: OpRule.Infix*): OpGroup = OpGroup.InfixGroup(rules.toList)
    def prefix(rules: OpRule.Prefix*): OpGroup = OpGroup.PrefixGroup(rules.toList)
  import OpRule.*, OpGroup.*
  val parseTable: List[OpGroup] = List(
    // prefix(
    //   Prefix(keywordP("return").map(_ => PrefixOp.Return).positioned),
    // ),
    infix(
      Infix(Assoc.Right, tokenP[Token.DOUBLE_AMP].map(_ => InfixOp.And).positioned),
      Infix(Assoc.Right, tokenP[Token.DOUBLE_BAR].map(_ => InfixOp.Or).positioned),
    ),
    infix(
      Infix(Assoc.Non, tokenP[Token.LESS].map(_ => InfixOp.Lt).positioned),
      Infix(Assoc.Non, tokenP[Token.GREATER].map(_ => InfixOp.Gt).positioned),
      Infix(Assoc.Non, tokenP[Token.LESS_EQUAL].map(_ => InfixOp.Lte).positioned),
      Infix(Assoc.Non, tokenP[Token.GREATER_EQUAL].map(_ => InfixOp.Gte).positioned),
      Infix(Assoc.Non, tokenP[Token.DOUBLE_EQUAL].map(_ => InfixOp.Eq).positioned),
      Infix(Assoc.Non, tokenP[Token.BANG_EQUAL].map(_ => InfixOp.Neq).positioned),
    ),
    infix(
      Infix(Assoc.Left, tokenP[Token.PLUS].map(_ => InfixOp.Plus).positioned),
      Infix(Assoc.Left, tokenP[Token.MINUS].map(_ => InfixOp.Minus).positioned),
      Infix(Assoc.Left, tokenP[Token.DOUBLE_PLUS].map(_ => InfixOp.Concat).positioned),
    ),
    infix(
      Infix(Assoc.Left, tokenP[Token.STAR].map(_ => InfixOp.Mul).positioned),
      Infix(Assoc.Left, tokenP[Token.SLASH].map(_ => InfixOp.Div).positioned),
      Infix(Assoc.Left, tokenP[Token.PERCENT].map(_ => InfixOp.Mod).positioned),
    ),
    prefix(
      Prefix(tokenP[Token.MINUS].map(_ => PrefixOp.Neg).positioned),
      Prefix(tokenP[Token.BANG].map(_ => PrefixOp.Not).positioned),
    ),
  )

  def valDefP: Parser[Definition] =
    val tpeP = (tokenP[Token.COLON], typeP).p.map((_, tpe) => tpe)
    val p = (keywordP("val"), tokenP[Token.IDENT], tpeP.tryIt, tokenP[Token.EQUAL], termP).p.map: (_, tk, maybeTpe, _, body) =>
      Definition.ValDef(tk.name, maybeTpe, body)
    p.positioned.withWhat("a value definition")

  def typeParamListP: Parser[TypeParamList] =
    val paramP: Parser[TypeParam | CaptureParam] = captureParamP `or` typeParamP
    val p = paramP.sepBy(tokenP[Token.COMMA]).surroundedBy(tokenP[Token.LBRACK], tokenP[Token.RBRACK])
    p.map(params => TypeParamList(params)).positioned.withWhat("a type parameter list")

  def termParamListP: Parser[TermParamList] =
    val p = termParamP.sepBy(tokenP[Token.COMMA]).surroundedBy(tokenP[Token.LPAREN], tokenP[Token.RPAREN])
    p.map(params => TermParamList(params)).positioned.withWhat("a term parameter list")

  def paramListP: Parser[TypeParamList | TermParamList] = typeParamListP `or` termParamListP

  def defDefP: Parser[Definition] =
    val tpeP = (tokenP[Token.COLON], typeP).p.map((_, tpe) => tpe)
    val p = (keywordP("def"), tokenP[Token.IDENT], paramListP.many, tpeP.tryIt, tokenP[Token.EQUAL], termP).p.map: (_, tk, paramss, maybeTpe, _, body) =>
      Definition.DefDef(tk.name, None, paramss, maybeTpe, body)
    p.positioned.withWhat("a function definition")

  private var nextExtensionNameId = 0
  def getExtensionName: String =
    val name = s"$$ext${nextExtensionNameId}"
    nextExtensionNameId += 1
    name

  def extensionDefP: Parser[Definition] =
    val selfArgP = termParamP.surroundedBy(tokenP[Token.LPAREN], tokenP[Token.RPAREN])
    val defGroupP = defDefP.sepBy(tokenP[Token.NEWLINE]).asInstanceOf[Parser[List[Definition.DefDef]]]
    val defGroupP1 = defGroupP.surroundedBy(tokenP[Token.INDENT], tokenP[Token.DEDENT].optional)
    val p = 
      (keywordP("extension"), typeParamListP.optional, selfArgP, defGroupP1).p.map: 
        case (_, typeArgs, selfArg, defs) =>
          val extName = getExtensionName
          val ps = typeArgs match
            case Some(p) => p.params
            case None => Nil
          Definition.ExtensionDef(extName, ps, selfArg, defs)
    p.positioned.withWhat("an extension definition")

  def fieldDefP: Parser[FieldDef] =
    val varP = keywordP("var")
    val p = (varP.tryIt, tokenP[Token.IDENT], tokenP[Token.COLON], typeP).p.map: (varTk, nameTk, _, tpe) =>
      val isVar = varTk.isDefined
      FieldDef(nameTk.name, isVar, tpe)
    p.positioned.withWhat("a struct field definition")

  def fieldListDefP: Parser[FieldDef] =
    val varP = keywordP("var").map(_ => true)
    val valP = keywordP("val").map(_ => false)
    val mutableP = varP `or` valP
    val p = (mutableP, tokenP[Token.IDENT], tokenP[Token.COLON], typeP).p.map: (isVar, nameTk, _, tpe) =>
      FieldDef(nameTk.name, isVar, tpe)
    p.positioned.withWhat("a struct field definition")

  def varianceP: Parser[Int] =
    val covP = tokenP[Token.PLUS].map(_ => 1)
    val contraP = tokenP[Token.MINUS].map(_ => -1)
    val p = (covP `or` contraP).optional.map(_.getOrElse(0))
    p

  def structTypeParamP: Parser[ConstructorTypeParam] =
    val typP = (varianceP, typeParamP).p.map: (v, tp) =>
      ConstructorTypeParam.Typ(tp, v)
    val capP = (varianceP, captureParamP).p.map: (v, cp) =>
      ConstructorTypeParam.Cap(cp, v)
    val p = capP `or` typP
    p.positioned.withWhat("a struct type parameter")

  def structP: Parser[Definition] =
    val inlineFieldsP = fieldDefP.sepBy(tokenP[Token.COMMA]).surroundedBy(tokenP[Token.LPAREN], tokenP[Token.RPAREN])
    val listFieldsP = 
      (
        tokenP[Token.COLON], 
        fieldListDefP.sepBy(tokenP[Token.NEWLINE]).surroundedBy(tokenP[Token.INDENT], tokenP[Token.DEDENT].optional)
      ).p.map: (_, fields) =>
        fields
    val fieldsP: Parser[List[FieldDef]] = inlineFieldsP `or` listFieldsP
    val paramP: Parser[ConstructorTypeParam] = structTypeParamP
    val paramsP = paramP.sepBy(tokenP[Token.COMMA]).surroundedBy(tokenP[Token.LBRACK], tokenP[Token.RBRACK])
    val p = (keywordP("struct"), tokenP[Token.IDENT], paramsP.optional, fieldsP).p.map: (_, nameTk, maybeParams, fields) =>
      Definition.StructDef(nameTk.name, maybeParams.getOrElse(Nil), fields)
    p.positioned.withWhat("a struct definition")

  def typeDefP: Parser[Definition] =
    val params = structTypeParamP.sepBy(tokenP[Token.COMMA]).surroundedBy(tokenP[Token.LBRACK], tokenP[Token.RBRACK])
    val p = (keywordP("type"), tokenP[Token.IDENT], params.optional, tokenP[Token.EQUAL], typeP).p.map: (_, nameTk, maybeParams, _, body) =>
      val targs = maybeParams.getOrElse(Nil)
      Definition.TypeDef(nameTk.name, targs, body)
    p.positioned.withWhat("a type definition")

  def definitionP: Parser[Definition] = 
    valDefP `or` defDefP `or` structP `or` extensionDefP `or` typeDefP

  def moduleNameP: Parser[ModuleName] =
    val moreP = (tokenP[Token.DOT], tokenP[Token.IDENT]).p.map((_, nameTk) => nameTk)
    val p = (tokenP[Token.IDENT], moreP.many).p.map: (nameTk, more) =>
      var result = ModuleName.Root()
      val names = nameTk :: more
      for nameTk <- names do
        result = ModuleName.Qualified(result, nameTk.name)
      result
    p.withWhat("a module name")

  def programP: Parser[List[Definition]] =
    (tokenP[Token.NEWLINE].optional, definitionP.sepBy(tokenP[Token.NEWLINE]), wsUntilEndP).p.map((_, defs, _) => defs)

  def moduleP: Parser[Module] =
    val headerP = (keywordP("module"), moduleNameP).p.map(_._2)
    val p = (tokenP[Token.NEWLINE].optional, headerP.optional, programP).p.map: (_, maybeHeader, defs) =>
      val name = maybeHeader match
        case Some(name) => name
        case None => ModuleName.Root()
      Module(name, defs)
    p.positioned.withWhat("a module")

  def identP: Parser[Term] = 
    tokenP[Token.IDENT].map(t => Term.Ident(t.name)).positioned

  def termP: Parser[Term] = lazyP:
    longestMatch(
      termLambdaP,
      typeLambdaP,
      ifP,
      //applyP,
      getParserOf(0),
      blockP,
    )

  private val opParsers = mutable.Map[Int, Parser[Term]]()
  def getParserOf(prec: Int): Parser[Term] =
    if prec >= parseTable.size then applyP  // the base case
    else
      opParsers.get(prec) match
        case Some(p) => p
        case None =>
          val group = parseTable(prec)
          val baseParser = getParserOf(prec + 1)
          val p = group match
            case InfixGroup(infixRules) => 
              val contParsers: List[Parser[(InfixOp, Assoc, Term)]] = infixRules.map: rule =>
                (rule.op, baseParser).p.map((op, base) => (op, rule.assoc, base))
              val contParser: Parser[(InfixOp, Assoc, Term)] = longestMatch(contParsers*)
              val p: Parser[Term] = (baseParser, contParser.many).p.flatMap: (base, conts) =>
                val assocs: List[Assoc] = conts.map(x => x._2)
                if assocs.contains(Assoc.Non) && assocs.size > 2 then
                  fail("non-associative operator")
                else if assocs.distinct.size > 1 then
                  fail("mixed associativity")
                else
                  val isLeftAssoc = assocs.distinct.headOption match
                    case Some(Assoc.Left) => true
                    case _ => false
                  if isLeftAssoc then
                    var result = base
                    for (op, _, t) <- conts do
                      result = Term.Infix(op, result, t).withPosFrom(result, t)
                    pureP(result)
                  else
                    val terms = (base :: conts.map(_._3)).reverse
                    val ops = conts.map(_._1)
                    var result = terms.head
                    for (t, op) <- (terms.tail `zip` ops) do
                      result = Term.Infix(op, t, result).withPosFrom(t, result)
                    pureP(result)
              p.positioned.withWhat("an infix expression")
            case PrefixGroup(prefixRules) => 
              val opParsers: List[Parser[PrefixOp]] = prefixRules.map(rule => rule.op)
              val opParser: Parser[PrefixOp] = longestMatch(opParsers*)
              val p = (opParser.many, baseParser).p.map: (ops, base) =>
                def go(xs: List[PrefixOp]): Term = xs match
                  case Nil => base
                  case op :: ops =>
                    val t = go(ops)
                    Term.Prefix(op, t).withPosFrom(op, t)
                go(ops)
              p.positioned.withWhat("an prefix expression")
          opParsers(prec) = p
          p

  def stringLitP: Parser[Term] =
    tokenP[Token.STR].map(t => Term.StrLit(t.content.substring(1, t.content.length - 1))).positioned
  
  def charLitP: Parser[Term] =
    tokenP[Token.CHAR].map(t => Term.CharLit(t.content)).positioned

  def intLitP: Parser[Term] =
    tokenP[Token.INT].map(t => Term.IntLit(t.content.toInt)).positioned

  def boolLitP: Parser[Term] =
    val trueP = keywordP("true").map(_ => Term.BoolLit(true))
    val falseP = keywordP("false").map(_ => Term.BoolLit(false))
    val p = trueP `or` falseP
    p.positioned

  def unitLitP: Parser[Term] =
    (tokenP[Token.LPAREN], tokenP[Token.RPAREN]).p.map(_ => Term.UnitLit()).positioned

  def ifP: Parser[Term] =
    val elseBranchP = (tokenP[Token.NEWLINE].optional, keywordP("else"), termP).p.map: (_, _, elseBranch) =>
      elseBranch
    val p = (keywordP("if"), termP, keywordP("then"), termP, elseBranchP.optional).p.map: (_, cond, _, thenBranch, maybeElseBranch) =>
      Term.If(cond, thenBranch, maybeElseBranch)
    p.positioned.withWhat("an if expression")

  def blockP: Parser[Term] = 
    val clauseP: Parser[Definition | Term] = orP(
      definitionP,
      termP,
    ).withWhat("a clause in a block")
    val clausesP = clauseP.sepBy(tokenP[Token.NEWLINE])
    val p = (tokenP[Token.INDENT], clausesP, tokenP[Token.DEDENT].optional).p.flatMap: (_, clauses, _) =>
      pureP(Term.Block(clauses))
    p.positioned.withWhat("a block expression")

  def termAtomP: Parser[Term] = longestMatch(
    boolLitP,
    identP,
    stringLitP,
    charLitP,
    intLitP,
    unitLitP,
    termP.surroundedBy(tokenP[Token.LPAREN], tokenP[Token.RPAREN])
  )

  def termLambdaP: Parser[Term] =
    val paramsP: Parser[List[TermParam]] = 
      termParamP.sepBy(tokenP[Token.COMMA]).surroundedBy(tokenP[Token.LPAREN], tokenP[Token.RPAREN])
    val arrowP = tokenP[Token.FAT_ARROW]
    val bodyP = termP
    val p = (paramsP, arrowP, bodyP).p.map((params, _, body) => Term.Lambda(params, body))
    p.positioned.withWhat("a term lambda")

  def typeLambdaP: Parser[Term] =
    val paramP: Parser[TypeParam | CaptureParam] = captureParamP `or` typeParamP
    val paramsP = paramP.sepBy(tokenP[Token.COMMA]).surroundedBy(tokenP[Token.LBRACK], tokenP[Token.RBRACK])
    val arrowP = tokenP[Token.FAT_ARROW]
    val bodyP = termP
    val p = (paramsP, arrowP, bodyP).p.map((params, _, body) => Term.TypeLambda(params, body))
    p.positioned.withWhat("a type lambda")

  // def captureLambdaP: Parser[Term] =
  //   val paramsP = captureParamP.sepBy(tokenP[Token.COMMA]).surroundedBy((tokenP[Token.LBRACK], keywordP("cap")).p, tokenP[Token.RBRACK])
  //   val arrowP = tokenP[Token.FAT_ARROW]
  //   val bodyP = termP
  //   val p = (paramsP, arrowP, bodyP).p.map((params, _, body) => Term.CaptureLambda(params, body))
  //   p.positioned.withWhat("a capture lambda")

  enum ApplyClause extends core.Positioned:
    case TermApply(terms: List[Term])
    case TypeApply(types: List[Type | CaptureSet])
    case Select(field: Token.IDENT)

  def termApplyClauseP: Parser[ApplyClause] =
    termP
      .sepBy(tokenP[Token.COMMA])
      .surroundedBy(tokenP[Token.LPAREN], tokenP[Token.RPAREN])
      .map(terms => ApplyClause.TermApply(terms))

  def typeApplyClauseP: Parser[ApplyClause] =
    (captureSetP `or` typeP: Parser[CaptureSet | Type])
      .sepBy1(tokenP[Token.COMMA])
      .surroundedBy(tokenP[Token.LBRACK], tokenP[Token.RBRACK])
      .map(types => ApplyClause.TypeApply(types))

  def trailingTermClosureP: Parser[ApplyClause] =
    val lambdaP = (tokenP[Token.COLON], termParamListP, tokenP[Token.FAT_ARROW], blockP).p.map: (_, params, _, body) =>
      val lambda = Term.Lambda(params.params, body)
      lambda
    val p = lambdaP.positioned.withWhat("a trailing lambda").map(t => ApplyClause.TermApply(List(t)))
    p.positioned

  def selectClauseP: Parser[ApplyClause] =
    (tokenP[Token.DOT], tokenP[Token.IDENT]).p.map((_, field) => ApplyClause.Select(field))
  
  def applyP: Parser[Term] =
    val clauseP = longestMatch(
      termApplyClauseP.withWhat("a term apply clause"),
      typeApplyClauseP.withWhat("a type apply clause"),
      selectClauseP.withWhat("a select clause"),
    ).positioned
    val clausesP = (clauseP.many, trailingTermClosureP.optional).p.map: (clauses, maybeTrailing) =>
      maybeTrailing match
        case Some(trailing) => clauses :+ trailing
        case None => clauses
    val appP = (termAtomP, clausesP).p.map: (fun, clauses) =>
      var result = fun
      for clause <- clauses do
        clause match
          case ApplyClause.TermApply(terms) => result = Term.Apply(result, terms).withPosFrom(result, clause)
          case ApplyClause.TypeApply(types) => result = Term.TypeApply(result, types).withPosFrom(result, clause)
          case ApplyClause.Select(field) => result = Term.Select(result, field.name).withPosFrom(result, field)
      result
    val assignP = (tokenP[Token.EQUAL], termP).p.map((_, rhs) => rhs)
    val p = (appP, assignP.tryIt).p.map: (app, maybeRhs) =>
      maybeRhs match
        case Some(rhs) => Term.Assign(app, rhs).withPosFrom(app, rhs)
        case None => app
    p.positioned

  def typeP: Parser[Type] = lazyP:
    longestMatch(
      termArrowP,
      typeArrowP,
      nonDependentTermArrowP,
      //captureArrowP,
      //capturingTypeP,
      simpleFunctionTypeP,
    )

  def typeAtomP: Parser[Type] = longestMatch(
    typeIdentP,
    typeP.surroundedBy(tokenP[Token.LPAREN], tokenP[Token.RPAREN]),
  )

  def termParamP: Parser[TermParam] =
    val consumeP: Parser[Boolean] = keywordP("consume").optional.map(_.isDefined)
    (consumeP, tokenP[Token.IDENT], tokenP[Token.COLON], typeP).p.map((consume, name, _, tpe) => TermParam(name.name, tpe, isConsume = consume)).positioned.withWhat("a term parameter")

  def typeParamP: Parser[TypeParam] =
    val boundP = (tokenP[Token.LESSCOLON], typeP).p.map((_, tpe) => tpe)
    val p = (tokenP[Token.IDENT], boundP.tryIt).p.map: (token, maybeBound) =>
      TypeParam(token.name, maybeBound)
    p.positioned.withWhat("a type parameter")

  def captureParamP: Parser[CaptureParam] =
    val boundP = (tokenP[Token.LESSCOLON], captureSetP).p.map((_, cs) => cs)
    val p = (keywordP("cap"), tokenP[Token.IDENT], boundP.tryIt).p.map: (_, name, maybeBound) =>
      CaptureParam(name.name, maybeBound)
    p.positioned.withWhat("a capture parameter")

  def termArrowP: Parser[Type] = 
    val paramsP = 
      termParamP.sepBy(tokenP[Token.COMMA]).surroundedBy(tokenP[Token.LPAREN], tokenP[Token.RPAREN])
    val p = (paramsP, capturingArrowP, typeP).p.map: (params, maybeCaps, resultType) =>
      val arrowType = Type.Arrow(params, resultType)
      maybeCaps match
        case Some(cs) => Type.Capturing(arrowType, false, cs)
        case None => arrowType
    p.positioned.withWhat("a term function type")

  def nonDependentTermArrowP: Parser[Type] =
    val paramTypesP: Parser[List[Type]] = 
      typeP.sepBy(tokenP[Token.COMMA]).surroundedBy(tokenP[Token.LPAREN], tokenP[Token.RPAREN])
    val arrowP = tokenP[Token.ARROW]
    val capsP = captureSetP.withWhat("a capture set after an arrow")
    val p = (paramTypesP, capturingArrowP, typeP).p.map: (paramTypes, maybeCaps, resultType) =>
      val params = paramTypes.map(tp => TermParam("_", tp, isConsume = false))
      val arrowType = Type.Arrow(params, resultType)
      maybeCaps match
        case Some(cs) => Type.Capturing(arrowType, false, cs)
        case None => arrowType
    p.positioned.withWhat("a non-dependent term function type")

  def typeArrowP: Parser[Type] =
    val paramP: Parser[TypeParam | CaptureParam] = captureParamP `or` typeParamP
    val paramsP = paramP.sepBy(tokenP[Token.COMMA]).surroundedBy(tokenP[Token.LBRACK], tokenP[Token.RBRACK])
    val arrowP = tokenP[Token.ARROW]
    val capsP = captureSetP.withWhat("a capture set after an arrow")
    val p = (paramsP, arrowP, capsP.tryIt, typeP).p.map: (params, _, maybeCaps, resultType) =>
      val arrowType = Type.TypeArrow(params, resultType)
      maybeCaps match
        case Some(cs) => Type.Capturing(arrowType, false, cs)
        case None => arrowType
    p.positioned.withWhat("a type function type")

  // def captureArrowP: Parser[Type] =
  //   val paramsP = captureParamP.sepBy(tokenP[Token.COMMA]).surroundedBy((tokenP[Token.LBRACK], keywordP("cap")).p, tokenP[Token.RBRACK])
  //   val arrowP = tokenP[Token.ARROW]
  //   val capsP = captureSetP.withWhat("a capture set after an arrow")
  //   val p = (paramsP, arrowP, capsP.tryIt, typeP).p.map: (params, _, maybeCaps, resultType) =>
  //     val arrowType = Type.CaptureArrow(params, resultType)
  //     maybeCaps match
  //       case Some(cs) => Type.Capturing(arrowType, cs)
  //       case None => arrowType
  //   p.positioned.withWhat("a capture function type")

  def typeIdentP: Parser[Type] = tokenP[Token.IDENT].map(t => Type.Ident(t.name)).positioned

  def appliedTypeP: Parser[Type] =
    val paramsP: Parser[List[Type | CaptureSet]] = lazyP:
      (captureSetP `or` typeP: Parser[Type | CaptureSet]).sepBy1(tokenP[Token.COMMA]).surroundedBy(tokenP[Token.LBRACK], tokenP[Token.RBRACK])
    val p = (typeAtomP, paramsP.tryIt).p.map:
      case (tycon, Some(params)) => Type.AppliedType(tycon, params)
      case (tycon, None) => tycon
    p.positioned.withWhat("an applied type")

  def capturingTypeP: Parser[Type] =
    val boxP = keywordP("box")
    val roP: Parser[Boolean] = keywordP("ro").optional.map:
      case Some(_) => true
      case None => false
    val capsP: Parser[(Boolean, CaptureSet)] = (tokenP[Token.HAT], roP, captureSetP.tryIt).p.map: (hatToken, isRO, maybeCaps) =>
      maybeCaps match
        case Some(cs) => (isRO, cs)
        case None => 
          val mode = AccessMode.Normal().withPosFrom(hatToken)
          (isRO, CaptureSet(CaptureRef("cap", mode).withPosFrom(hatToken) :: Nil).withPosFrom(hatToken))
    val p = (appliedTypeP, capsP.tryIt).p.map:
      case (ty, Some((isRO, caps))) => Type.Capturing(ty, isRO, caps)
      case (ty, None) => ty
    val coreP = p.positioned.withWhat("a capturing type")
    val outP = (boxP.optional, coreP).p.map:
      case (boxed, core) =>
        if boxed.isDefined && core.isInstanceOf[Type.Capturing] then
          Type.Boxed(core)
        else
          core
    outP.positioned.withWhat("a capturing type")

  def capturingArrowP: Parser[Option[CaptureSet]] =
    val arrowP = tokenP[Token.ARROW]
    val capsP = captureSetP.withWhat("a capture set after an arrow")
    val fatArrowP = tokenP[Token.FAT_ARROW]
    val p1 = (arrowP, capsP.tryIt).p.map((_, maybeCaps) => maybeCaps)
    val p2 = fatArrowP.map(tk => Some(CaptureSet(CaptureRef("cap", AccessMode.Normal().withPosFrom(tk)).withPosFrom(tk) :: Nil).withPosFrom(tk)))
    p1 `or` p2

  def simpleFunctionTypeP: Parser[Type] =
    def buildType(head: Type, more: List[(Option[CaptureSet], Type)]): Type =
      val t :: ts = (head :: more.map(_._2)).reverse: @unchecked
      val todos = more.map(_._1).reverse `zip` ts
      var result = t
      for (maybeCaps, ty) <- todos do
        val param = TermParam("_", ty, isConsume = false).withPosFrom(ty)
        result = Type.Arrow(List(param), result).withPosFrom(param, result)
        maybeCaps match
          case Some(cs) => result = Type.Capturing(result, false, cs).withPosFrom(result, cs)
          case None =>
      result
    val moreP: Parser[(Option[CaptureSet], Type)] = (capturingArrowP, typeP).p
    val p = (capturingTypeP, moreP.many).p.map: (head, more) =>
      buildType(head, more)
    p.positioned.withWhat("a simple function type")

  def captureRefP: Parser[CaptureRef] = 
    val roP = keywordP("ro").map(_ => AccessMode.ReadOnly())
    val consumeP = keywordP("consume").map(_ => AccessMode.Consume())
    val modeP = (roP `or` consumeP).optional.map:
      case Some(mode) => mode
      case None => AccessMode.Normal()
    val p =
      (modeP.positioned, tokenP[Token.IDENT]).p.map: (mode, nameTk) =>
        CaptureRef(nameTk.name, mode)
    p.positioned.withWhat("a capture reference")

  def captureSetP: Parser[CaptureSet] = 
    val p = 
      captureRefP.sepBy(tokenP[Token.COMMA]).map(refs => CaptureSet(refs)).surroundedBy(tokenP[Token.LBRACE], tokenP[Token.RBRACE])
    p.positioned.withWhat("a capture set")
