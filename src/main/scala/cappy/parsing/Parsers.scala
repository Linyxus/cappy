package cappy
package parsing
import core.ast.*
import tokenizing.*
object Parsers:
  import Syntax.*
  import Parser.*

  def identP: Parser[Term] = 
    tokenP[Token.IDENT].map(t => Term.Ident(t.name)).positioned

  def termP: Parser[Term] = identP

  def typeP: Parser[Type] = capturingTypeP

  def typeAtomP: Parser[Type] = typeIdentP

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
