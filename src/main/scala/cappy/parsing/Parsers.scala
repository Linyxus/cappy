package cappy
package parsing
import core.ast.*
import tokenizing.*
object Parsers:
  import Syntax.*
  import Parser.*

  def identP: Parser[Term] = 
    tokenP[Token.IDENT].map(t => Term.Ident(t.name)).positioned

  def termP: Parser[Term] = lazyP:
    longestMatch(
      termLambdaP,
      typeLambdaP,
      captureLambdaP,
      applyP,
    )

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
    val paramsP = typeParamP.sepBy(tokenP[Token.COMMA]).surroundedBy(tokenP[Token.LBRACK], tokenP[Token.RBRACK])
    val arrowP = tokenP[Token.FAT_ARROW]
    val bodyP = termP
    val p = (paramsP, arrowP, bodyP).p.map((params, _, body) => Term.TypeLambda(params, body))
    p.positioned.withWhat("a type lambda")

  def captureLambdaP: Parser[Term] =
    val paramsP = captureParamP.sepBy(tokenP[Token.COMMA]).surroundedBy((tokenP[Token.LBRACK], keywordP("cap")).p, tokenP[Token.RBRACK])
    val arrowP = tokenP[Token.FAT_ARROW]
    val bodyP = termP
    val p = (paramsP, arrowP, bodyP).p.map((params, _, body) => Term.CaptureLambda(params, body))
    p.positioned.withWhat("a capture lambda")

  enum ApplyClause:
    case TermApply(terms: List[Term])
    case TypeApply(types: List[Type])
    case CaptureApply(captures: List[CaptureRef])

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
    captureRefP
      .sepBy(tokenP[Token.COMMA])
      .surroundedBy((tokenP[Token.LBRACE], keywordP("cap")).p, tokenP[Token.RBRACE])
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
      captureArrowP,
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
    tokenP[Token.IDENT].map(t => CaptureParam(t.name)).positioned.withWhat("a capture parameter")

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
    val paramsP = typeParamP.sepBy(tokenP[Token.COMMA]).surroundedBy(tokenP[Token.LBRACK], tokenP[Token.RBRACK])
    val arrowP = tokenP[Token.ARROW]
    val capsP = captureSetP.withWhat("a capture set after an arrow")
    val p = (paramsP, arrowP, capsP.tryIt, typeP).p.map: (params, _, maybeCaps, resultType) =>
      val arrowType = Type.TypeArrow(params, resultType)
      maybeCaps match
        case Some(cs) => Type.Capturing(arrowType, cs)
        case None => arrowType
    p.positioned.withWhat("a type function type")

  def captureArrowP: Parser[Type] =
    val paramsP = captureParamP.sepBy(tokenP[Token.COMMA]).surroundedBy((tokenP[Token.LBRACK], keywordP("cap")).p, tokenP[Token.RBRACK])
    val arrowP = tokenP[Token.ARROW]
    val capsP = captureSetP.withWhat("a capture set after an arrow")
    val p = (paramsP, arrowP, capsP.tryIt, typeP).p.map: (params, _, maybeCaps, resultType) =>
      val arrowType = Type.CaptureArrow(params, resultType)
      maybeCaps match
        case Some(cs) => Type.Capturing(arrowType, cs)
        case None => arrowType
    p.positioned.withWhat("a capture function type")

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
