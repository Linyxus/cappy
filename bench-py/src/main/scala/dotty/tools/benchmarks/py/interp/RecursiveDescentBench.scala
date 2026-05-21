package dotty.tools.benchmarks.py.interp

/** Recursive-descent parser+evaluator over a generated token array.
 *  Mutual recursion threads a cursor and returns `(value, nextPos)`
 *  tuples, stressing per-frame tuple allocation alongside ADT match. */
sealed trait Token
case class TNum(value: Int) extends Token
case class TPlus()          extends Token
case class TMinus()         extends Token
case class TStar()          extends Token
case class TLParen()        extends Token
case class TRParen()        extends Token

class RecursiveDescentBench:
  var size: Int = 0
  var tokens: Array[Token] = Array.empty

  def setup(size: Int): Unit =
    this.size = size
    // Encode the expression "(n + n) * (n - (n + n))" repeated, then chained
    // with '+' so the whole token stream is one big expression of length ~size.
    val buf = scala.collection.mutable.ArrayBuffer[Token]()
    var i = 0
    while buf.length < size do
      if i > 0 then buf += TPlus()
      buf += TLParen()
      buf += TNum(i % 9 + 1)
      buf += TPlus()
      buf += TNum(i % 5 + 1)
      buf += TRParen()
      buf += TStar()
      buf += TLParen()
      buf += TNum(i % 7 + 1)
      buf += TMinus()
      buf += TLParen()
      buf += TNum(i % 3 + 1)
      buf += TPlus()
      buf += TNum(i % 4 + 1)
      buf += TRParen()
      buf += TRParen()
      i += 1
    tokens = buf.toArray

  // expr := term (('+' | '-') term)*
  def parseExpr(pos: Int): (Int, Int) =
    var (acc, p) = parseTerm(pos)
    var go = true
    while go && p < tokens.length do
      tokens(p) match
        case TPlus() =>
          val (rhs, np) = parseTerm(p + 1); acc += rhs; p = np
        case TMinus() =>
          val (rhs, np) = parseTerm(p + 1); acc -= rhs; p = np
        case _ => go = false
    (acc, p)

  // term := factor (('*') factor)*
  def parseTerm(pos: Int): (Int, Int) =
    var (acc, p) = parseFactor(pos)
    var go = true
    while go && p < tokens.length do
      tokens(p) match
        case TStar() =>
          val (rhs, np) = parseFactor(p + 1); acc *= rhs; p = np
        case _ => go = false
    (acc, p)

  // factor := NUM | '(' expr ')'
  def parseFactor(pos: Int): (Int, Int) =
    tokens(pos) match
      case TNum(v)   => (v, pos + 1)
      case TLParen() =>
        val (v, p) = parseExpr(pos + 1)
        (v, p + 1) // skip ')'
      case _ => (0, pos + 1)

  val operations: Map[String, () => Any] = Map(
    "parse" -> (() => parseExpr(0)._1),
  )

@main def main(): Unit = ()
