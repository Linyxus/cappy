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

  def fieldDefP: Parser[FieldDef] =
    val varP = keywordP("var")
    val p = (varP.tryIt, tokenP[Token.IDENT], tokenP[Token.COLON], typeP).p.map: (varTk, nameTk, _, tpe) =>
      val isVar = varTk.isDefined
      FieldDef(nameTk.name, isVar, tpe)
    p.positioned.withWhat("a struct field definition")

  def structP: Parser[Definition] =
    val fieldsP = fieldDefP.sepBy(tokenP[Token.COMMA]).surroundedBy(tokenP[Token.LPAREN], tokenP[Token.RPAREN])
    val p = (keywordP("struct"), tokenP[Token.IDENT], fieldsP).p.map: (_, nameTk, fields) =>
      Definition.StructDef(nameTk.name, fields)
    p.positioned.withWhat("a struct definition")

  def definitionP: Parser[Definition] = 
    valDefP `or` defDefP `or` structP

  def programP: Parser[List[Definition]] =
    (tokenP[Token.NEWLINE].optional, definitionP.sepBy(tokenP[Token.NEWLINE]), wsUntilEndP).p.map((_, defs, _) => defs)

  def identP: Parser[Term] = 
    tokenP[Token.IDENT].map(t => Term.Ident(t.name)).positioned

  def termP: Parser[Term] = lazyP:
    longestMatch(
      termLambdaP,
      typeLambdaP,
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

  def intLitP: Parser[Term] =
    tokenP[Token.INT].map(t => Term.IntLit(t.content.toInt)).positioned

  def unitLitP: Parser[Term] =
    (tokenP[Token.LPAREN], tokenP[Token.RPAREN]).p.map(_ => Term.UnitLit()).positioned

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
    identP,
    stringLitP,
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

  def selectClauseP: Parser[ApplyClause] =
    (tokenP[Token.DOT], tokenP[Token.IDENT]).p.map((_, field) => ApplyClause.Select(field))
  
  def applyP: Parser[Term] =
    val clauseP = longestMatch(
      termApplyClauseP.withWhat("a term apply clause"),
      typeApplyClauseP.withWhat("a type apply clause"),
      selectClauseP.withWhat("a select clause"),
    ).positioned
    val clausesP = clauseP.many
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
    (tokenP[Token.IDENT], tokenP[Token.COLON], typeP).p.map((name, _, tpe) => TermParam(name.name, tpe)).positioned.withWhat("a term parameter")

  def typeParamP: Parser[TypeParam] =
    val boundP = (tokenP[Token.LESSCOLON], typeP).p.map((_, tpe) => tpe)
    val p = (tokenP[Token.IDENT], boundP.tryIt).p.map: (token, maybeBound) =>
      TypeParam(token.name, maybeBound)
    p.positioned.withWhat("a type parameter")

  def captureParamP: Parser[CaptureParam] =
    //tokenP[Token.IDENT].map(t => CaptureParam(t.name)).positioned.withWhat("a capture parameter")
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
        case Some(cs) => Type.Capturing(arrowType, cs)
        case None => arrowType
    p.positioned.withWhat("a term function type")

  def nonDependentTermArrowP: Parser[Type] =
    val paramTypesP: Parser[List[Type]] = 
      typeP.sepBy(tokenP[Token.COMMA]).surroundedBy(tokenP[Token.LPAREN], tokenP[Token.RPAREN])
    val arrowP = tokenP[Token.ARROW]
    val capsP = captureSetP.withWhat("a capture set after an arrow")
    val p = (paramTypesP, capturingArrowP, typeP).p.map: (paramTypes, maybeCaps, resultType) =>
      val params = paramTypes.map(tp => TermParam("_", tp))
      val arrowType = Type.Arrow(params, resultType)
      maybeCaps match
        case Some(cs) => Type.Capturing(arrowType, cs)
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
        case Some(cs) => Type.Capturing(arrowType, cs)
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
    val paramsP = lazyP:
      typeP.sepBy1(tokenP[Token.COMMA]).surroundedBy(tokenP[Token.LBRACK], tokenP[Token.RBRACK])
    val p = (typeAtomP, paramsP.tryIt).p.map:
      case (tycon, Some(params)) => Type.AppliedType(tycon, params)
      case (tycon, None) => tycon
    p.positioned.withWhat("an applied type")

  def capturingTypeP: Parser[Type] =
    val capsP = (tokenP[Token.HAT], captureSetP.tryIt).p.map: (hatToken, maybeCaps) =>
      maybeCaps match
        case Some(cs) => cs
        case None => CaptureSet(CaptureRef("cap").withPosFrom(hatToken) :: Nil).withPosFrom(hatToken)
    val p = (appliedTypeP, capsP.tryIt).p.map:
      case (ty, Some(caps)) => Type.Capturing(ty, caps)
      case (ty, None) => ty
    p.positioned.withWhat("a capturing type")

  def capturingArrowP: Parser[Option[CaptureSet]] =
    val arrowP = tokenP[Token.ARROW]
    val capsP = captureSetP.withWhat("a capture set after an arrow")
    val fatArrowP = tokenP[Token.FAT_ARROW]
    val p1 = (arrowP, capsP.tryIt).p.map((_, maybeCaps) => maybeCaps)
    val p2 = fatArrowP.map(tk => Some(CaptureSet(CaptureRef("cap").withPosFrom(tk) :: Nil).withPosFrom(tk)))
    p1 `or` p2

  def simpleFunctionTypeP: Parser[Type] =
    def buildType(head: Type, more: List[(Option[CaptureSet], Type)]): Type =
      val t :: ts = (head :: more.map(_._2)).reverse: @unchecked
      val todos = more.map(_._1).reverse `zip` ts
      var result = t
      for (maybeCaps, ty) <- todos do
        val param = TermParam("_", ty).withPosFrom(ty)
        result = Type.Arrow(List(param), result).withPosFrom(param, result)
        maybeCaps match
          case Some(cs) => result = Type.Capturing(result, cs).withPosFrom(result, cs)
          case None =>
      result
    val moreP: Parser[(Option[CaptureSet], Type)] = (capturingArrowP, typeP).p
    val p = (capturingTypeP, moreP.many).p.map: (head, more) =>
      buildType(head, more)
    p.positioned.withWhat("a simple function type")

  def captureRefP: Parser[CaptureRef] = tokenP[Token.IDENT].map(t => CaptureRef(t.name)).positioned.withWhat("a capture reference")

  def captureSetP: Parser[CaptureSet] = 
    val p = 
      captureRefP.sepBy(tokenP[Token.COMMA]).map(refs => CaptureSet(refs)).surroundedBy(tokenP[Token.LBRACE], tokenP[Token.RBRACE])
    p.positioned.withWhat("a capture set")
