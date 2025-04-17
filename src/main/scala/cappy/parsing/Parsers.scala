package cappy
package parsing
import core.ast.*
import tokenizing.*
object Parsers:
  import Syntax.*
  import Parser.*

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
      Definition.DefDef(tk.name, paramss, maybeTpe, body)
    p.positioned.withWhat("a function definition")

  def definitionP: Parser[Definition] = 
    valDefP `or` defDefP

  def programP: Parser[List[Definition]] =
    (tokenP[Token.NEWLINE].optional, definitionP.sepBy(tokenP[Token.NEWLINE]), wsUntilEndP).p.map((_, defs, _) => defs)

  def identP: Parser[Term] = 
    tokenP[Token.IDENT].map(t => Term.Ident(t.name)).positioned

  def termP: Parser[Term] = lazyP:
    longestMatch(
      termLambdaP,
      typeLambdaP,
      //captureLambdaP,
      applyP,
      blockP,
      stringLitP,
      intLitP,
      unitLitP,
    )

  def stringLitP: Parser[Term] =
    tokenP[Token.STR].map(t => Term.StrLit(t.content.substring(1, t.content.length - 1))).positioned

  def intLitP: Parser[Term] =
    tokenP[Token.INT].map(t => Term.IntLit(t.content.toInt)).positioned

  def unitLitP: Parser[Term] =
    tokenP[Token.UNIT].map(_ => Term.UnitLit()).positioned

  def blockP: Parser[Term] = 
    val clauseP: Parser[Definition | Term] = orP(
      valDefP,
      termP,
    ).withWhat("a clause in a block")
    val clausesP = clauseP.sepBy(tokenP[Token.NEWLINE])
    val p = (tokenP[Token.INDENT], clausesP, tokenP[Token.DEDENT].optional).p.flatMap: (_, clauses, _) =>
      if clauses.isEmpty then
        fail("A block must contain at least one clause")
      else
        clauses.last match
          case _: Definition.ValDef =>
            fail(s"The last clause in a block must be a term, but got: $clauses")
          case term: Term =>
            val defs = clauses.init.map:
              case defn: Definition => defn
              case term: Term => Definition.ValDef("_", None, term).withPos(term.pos)
            pureP(Term.Block(defs, term))
    p.positioned.withWhat("a block expression")

  def termAtomP: Parser[Term] = longestMatch(
    identP,
    termP.surroundedBy(tokenP[Token.LPAREN], tokenP[Token.RPAREN])
  )

  def termLambdaP: Parser[Term] =
    val paramsP = termParamP.sepBy(tokenP[Token.COMMA]).surroundedBy(tokenP[Token.LPAREN], tokenP[Token.RPAREN])
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

  enum ApplyClause:
    case TermApply(terms: List[Term])
    case TypeApply(types: List[Type])
    case CaptureApply(captures: List[CaptureSet])

  def termApplyClauseP: Parser[ApplyClause] =
    termP
      .sepBy(tokenP[Token.COMMA])
      .surroundedBy(tokenP[Token.LPAREN], tokenP[Token.RPAREN])
      .map(terms => ApplyClause.TermApply(terms))

  def typeApplyClauseP: Parser[ApplyClause] =
    typeP
      .sepBy1(tokenP[Token.COMMA])
      .surroundedBy(tokenP[Token.LBRACK], tokenP[Token.RBRACK])
      .map(types => ApplyClause.TypeApply(types))

  def captureApplyClauseP: Parser[ApplyClause] =
    captureSetP
      .sepBy1(tokenP[Token.COMMA])
      .surroundedBy((tokenP[Token.LBRACK], keywordP("cap")).p, tokenP[Token.RBRACK])
      .map(captures => ApplyClause.CaptureApply(captures))
  
  def applyP: Parser[Term] =
    val clauseP = longestMatch(
      termApplyClauseP.withWhat("a term apply clause"),
      typeApplyClauseP.withWhat("a type apply clause"),
      captureApplyClauseP.withWhat("a capture apply clause"),
    )
    val clausesP = clauseP.many
    val p = (termAtomP, clausesP).p.map: (fun, clauses) =>
      var result = fun
      for clause <- clauses do
        clause match
          case ApplyClause.TermApply(terms) => result = Term.Apply(result, terms)
          case ApplyClause.TypeApply(types) => result = Term.TypeApply(result, types)
          case ApplyClause.CaptureApply(captures) => result = Term.CaptureApply(result, captures)
      result
    p.positioned

  def typeP: Parser[Type] = lazyP:
    longestMatch(
      termArrowP,
      typeArrowP,
      //captureArrowP,
      capturingTypeP,
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
    val paramsP = termParamP.sepBy(tokenP[Token.COMMA]).surroundedBy(tokenP[Token.LPAREN], tokenP[Token.RPAREN])
    val arrowP = tokenP[Token.ARROW]
    val capsP = captureSetP.withWhat("a capture set after an arrow")
    val p = (paramsP, arrowP, capsP.tryIt, typeP).p.map: (params, _, maybeCaps, resultType) =>
      val arrowType = Type.Arrow(params, resultType)
      maybeCaps match
        case Some(cs) => Type.Capturing(arrowType, cs)
        case None => arrowType
    p.positioned.withWhat("a term function type")

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
    val capsP = (tokenP[Token.HAT], captureSetP).p.map((_, set) => set)
    val p = (appliedTypeP, capsP.tryIt).p.map:
      case (ty, Some(caps)) => Type.Capturing(ty, caps)
      case (ty, None) => ty
    p.positioned.withWhat("a capturing type")

  def captureRefP: Parser[CaptureRef] = tokenP[Token.IDENT].map(t => CaptureRef(t.name)).positioned.withWhat("a capture reference")

  def captureSetP: Parser[CaptureSet] = 
    val p = 
      captureRefP.sepBy(tokenP[Token.COMMA]).map(refs => CaptureSet(refs)).surroundedBy(tokenP[Token.LBRACE], tokenP[Token.RBRACE])
    p.positioned.withWhat("a capture set")
