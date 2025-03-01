package cappy.parsing
import cappy.tokenizing.*
import cappy.core.*
import scala.reflect.ClassTag

object Parser:
  case class ParserState(tokens: Array[Token], current: Int):
    def advance: ParserState = copy(current = current + 1)
    def currentToken: Option[Token] = if current < tokens.length then Some(tokens(current)) else None
    def getPos(idx: Int): SourcePos =
      if idx < 0 then tokens.head.pos
      else if idx >= tokens.length then tokens.last.pos
      else tokens(idx).pos
    def currentPos: SourcePos = getPos(current)
    def previousPos: SourcePos = getPos(current - 1)
  enum ParseError extends Positioned:
    case Here(msg: String | Null = null)
    case When(inner: ParseError, what: String)
    def push(what: String): ParseError = When(this, what).withPos(this.pos)
  case class ParseResult[+A](nextState: ParserState, result: Either[ParseError, A]):
    def isOk: Boolean = result.isRight
    def isError: Boolean = result.isLeft

    def push(what: String): ParseResult[A] = 
      copy(
        result = result match
          case Left(err) => Left(err.push(what))
          case Right(res) => Right(res)
      )

  type ParseFn[+A] = ParserState => ParseResult[A]

  trait ParseInfo:
    /** A description of what this parser is parsing */
    def what: String | Null
    /** Whether this parser can match the current state */
    def canMatch(state: ParserState): Boolean
    /** Whether this parser is anonymous */
    def anonymous: Boolean = what == null

    def clearWhat: ParseInfo = 
      val oldInfo = this
      new ParseInfo:
        def what = null
        def canMatch(state: ParserState) = oldInfo.canMatch(state)

  trait Parser[+A]:
    def parse: ParseFn[A]
    def info: ParseInfo

    def runParser(state: ParserState): ParseResult[A] = 
      if info.anonymous then
        parse(state)
      else
        val start = state.current
        val res = parse(state)
        val end = res.nextState.current
        res.push(info.what)

    def withInfo(newInfo: ParseInfo): Parser[A] = 
      val fn = parse
      new Parser[A]:
        def parse: ParseFn[A] = fn
        def info: ParseInfo = newInfo

    def withWhat(newWhat: String | Null): Parser[A] = 
      val info = this.info
      withInfo(new ParseInfo:
        def what = newWhat
        def canMatch(state: ParserState): Boolean = info.canMatch(state)
      )

  def predP(pred: Token => Boolean, desc: String | Null = null): Parser[Token] = new Parser[Token]:
    def parse: ParseFn[Token] = state =>
      state.currentToken match
        case Some(token) if pred(token) => ParseResult(state.advance, Right(token))
        case _ => ParseResult(state, Left(ParseError.Here().withPos(state.currentPos)))
    def info: ParseInfo = new ParseInfo:
      def what = desc
      def canMatch(state: ParserState): Boolean = state.currentToken.exists(pred)

  def pureP[A](a: A, desc: String | Null = null): Parser[A] = new Parser[A]:
    def parse: ParseFn[A] = state => ParseResult(state, Right(a))
    def info: ParseInfo = new ParseInfo:
      def what = desc
      def canMatch(state: ParserState): Boolean = true

  extension [A](p: Parser[A])
    def map[B](f: A => B): Parser[B] = new Parser[B]:
      def parse: ParseFn[B] = state =>
        p.runParser(state) match
          case ParseResult(nextState, Right(result)) => ParseResult(nextState, Right(f(result)))
          case ParseResult(nextState, Left(error)) => ParseResult(nextState, Left(error))
      def info: ParseInfo = p.info.clearWhat

    def flatMap[B](f: A => Parser[B]): Parser[B] = new Parser[B]:
      def parse: ParseFn[B] = state =>
        p.runParser(state) match
          case ParseResult(nextState, Right(result)) => f(result).runParser(nextState)
          case ParseResult(nextState, Left(error)) => ParseResult(nextState, Left(error))
      def info: ParseInfo = p.info.clearWhat

  extension [A <: Positioned](p: Parser[A])
    def positioned: Parser[A] = new Parser[A]:
      def parse: ParseFn[A] = state =>
        val startPos = state.currentPos
        p.runParser(state) match
          case ParseResult(nextState, Left(err)) => ParseResult(nextState, Left(err))
          case ParseResult(nextState, Right(result)) => 
            val endPos = nextState.previousPos
            val pos = startPos `merge` endPos
            ParseResult(nextState, Right(result.withPos(pos)))
      def info: ParseInfo = p.info.clearWhat

  def pairP[A, B](pa: => Parser[A], pb: => Parser[B]): Parser[(A, B)] =
    for
      a <- pa
      b <- pb
    yield (a, b)

  def tupP[A, B <: Tuple](pa: Parser[A], pb: Parser[B]): Parser[A *: B] = pairP(pa, pb).map((a, b) => a *: b)

  type TupleParserResult[X <: Tuple] <: Tuple = X match
    case EmptyTuple => EmptyTuple
    case Parser[a] *: ps => a *: TupleParserResult[ps]
    case _ => Nothing

  def chainP[X <: Tuple](ps: X): Parser[TupleParserResult[X]] = ps match
    case EmptyTuple => pureP(EmptyTuple).asInstanceOf[Parser[TupleParserResult[X]]]
    case (pa: Parser[a]) *: ps => tupP(pa, chainP(ps)).asInstanceOf[Parser[TupleParserResult[X]]]
    case ps => assert(false, s"invalid parser tuple: $ps")

  extension [A <: Tuple](p: A)
    def p: Parser[TupleParserResult[p.type]] = chainP(p)

  /** Run a set of parsers in order, returning the first that succeeds. If all fail, return the one matching the longest input tokens
   * before failing.
   */
  def longestMatch[A](ps: Parser[A]*): Parser[A] = new Parser[A]:
    def parse: ParseFn[A] = state =>
      var succeed = false
      var longest: Int = -1
      var result: ParseResult[A] = null
      var current = 0
      while !succeed && current < ps.length do
        val p = ps(current)
        if p.info.canMatch(state) then
          val res = p.runParser(state)
          if res.isOk then
            succeed = true
            longest = res.nextState.current
            result = res
          else
            if res.nextState.current > longest then
              longest = res.nextState.current
              result = res
        current += 1
      result

    def info: ParseInfo = new ParseInfo:
      def what = null
      def canMatch(state: ParserState): Boolean = ps.exists(_.info.canMatch(state))

  def optionalP[A](p: Parser[A]): Parser[Option[A]] = new Parser[Option[A]]:
    def parse: ParseFn[Option[A]] = state =>
      p.runParser(state) match
        case ParseResult(nextState, Right(result)) => ParseResult(nextState, Right(Some(result)))
        case ParseResult(nextState, Left(error)) => ParseResult(nextState, Right(None))
    def info: ParseInfo = new ParseInfo:
      def what = null
      def canMatch(state: ParserState): Boolean = true  // it always succeeds

  def tryP[A](p: Parser[A]): Parser[Option[A]] = new Parser[Option[A]]:
    def parse: ParseFn[Option[A]] = state =>
      if p.info.canMatch(state) then
        p.runParser(state) match
          case ParseResult(nextState, Right(result)) => ParseResult(nextState, Right(Some(result)))
          case ParseResult(nextState, Left(error)) => ParseResult(nextState, Left(error))
      else
        ParseResult(state, Right(None))
    def info: ParseInfo = new ParseInfo:
      def what = null
      def canMatch(state: ParserState): Boolean = true  // it can always match

  def orP[A, B](p1: => Parser[A], p2: => Parser[B]): Parser[A | B] = new Parser[A | B]:
    def parse: ParseFn[A | B] = state =>
      if p1.info.canMatch(state) then
        p1.runParser(state) match
          case ParseResult(nextState, Right(result)) => ParseResult(nextState, Right(result))
          case ParseResult(nextState, Left(_)) => p2.runParser(state)
      else
        p2.runParser(state)
    def info: ParseInfo = new ParseInfo:
      def what = null
      def canMatch(state: ParserState): Boolean = p1.info.canMatch(state) || p2.info.canMatch(state)

  def manyP[A](p: Parser[A]): Parser[List[A]] = new Parser[List[A]]:
    def parse: ParseFn[List[A]] = state =>
      var results = List[A]()
      var nowState = state
      var nowResult: ParseResult[A] = p.runParser(nowState)
      while nowResult.isOk do
        nowState = nowResult.nextState
        results = nowResult.result.right.get :: results
        nowResult = p.runParser(nowState)
      ParseResult(nowState, Right(results))

    def info: ParseInfo = new ParseInfo:
      def what = null
      def canMatch(state: ParserState): Boolean = true

  def many1P[A](p: Parser[A]): Parser[List[A]] =
    (p, manyP(p)).p.map((a, as) => a :: as)

  def sepByP[A, B](p: Parser[A], sep: Parser[B]): Parser[List[A]] = new Parser[List[A]]:
    def parse: ParseFn[List[A]] = state =>
      var results: List[A] = List()
      var nowState: ParserState = state
      var nowResult: ParseResult[A] = p.runParser(nowState)
      while nowResult.isOk do
        nowState = nowResult.nextState
        results = nowResult.result.right.get :: results
        nowResult = (sep, p).p.map((_, a) => a).runParser(nowState)
      ParseResult(nowState, Right(results.reverse))

    def info: ParseInfo = new ParseInfo:
      def what = null
      def canMatch(state: ParserState): Boolean = true

  def sepBy1P[A, B](p: Parser[A], sep: Parser[B]): Parser[List[A]] = new Parser[List[A]]:
    def parse: ParseFn[List[A]] = state =>
      sepByP(p, sep).runParser(state) match
        case ParseResult(nextState, Right(result)) => 
          if result.isEmpty then
            ParseResult(nextState, Left(ParseError.Here("expected at least one element").withPos(nextState.currentPos)))
          else
            ParseResult(nextState, Right(result))
        case ParseResult(nextState, Left(error)) => ParseResult(nextState, Left(error))
    def info: ParseInfo =
      new ParseInfo:
        def what = null
        def canMatch(state: ParserState): Boolean = p.info.canMatch(state)

  def tokenP[T <: Token: ClassTag]: Parser[T] = 
    predP(token => implicitly[ClassTag[T]].runtimeClass.isInstance(token)).map(_.asInstanceOf[T])

  def surroundedByP[A](open: Parser[Any], close: Parser[Any], p: Parser[A]): Parser[A] =
    (open, p, close).p.map((_, x, _) => x)

  extension [A](p: Parser[A])
    def surroundedBy(open: Parser[Any], close: Parser[Any]): Parser[A] =
      surroundedByP(open, close, p)

    def many: Parser[List[A]] =
      manyP(p)

    def many1: Parser[List[A]] =
      many1P(p)

    def sepBy(sep: Parser[Any]): Parser[List[A]] =
      sepByP(p, sep)

    def sepBy1(sep: Parser[Any]): Parser[List[A]] =
      sepBy1P(p, sep)

    def or[B >: A](p2: => Parser[B]): Parser[B] =
      orP(p, p2)

    def optional: Parser[Option[A]] =
      optionalP(p)

    def tryIt: Parser[Option[A]] =
      tryP(p)

  def lazyP[A](p: => Parser[A]): Parser[A] = new Parser[A]:
    lazy val inner: Parser[A] = p
    def parse: ParseFn[A] = state =>
      inner.runParser(state)
    def info: ParseInfo = new ParseInfo:
      def what = null
      def canMatch(state: ParserState): Boolean = inner.info.canMatch(state)
