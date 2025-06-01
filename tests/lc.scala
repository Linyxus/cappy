struct ParseState(buffer: String^, currentPos: i32)
extension (state: ParseState^)
  def isAtEnd: bool = state.currentPos >= state.buffer.size
  def peek: char = state.buffer(state.currentPos)
  def advance: ParseState^ =
    ParseState(state.buffer, state.currentPos + 1)
  def clone: ParseState^ =
    // BUG? state.buffer should not be fresh
    ParseState(state.buffer, state.currentPos)
enum ParseResult[+T]:
  case Ok(nextState: ParseState^, result: T)
  case Err(msg: String^)
extension [T](self: ParseResult[T]^)
  def print(): Unit =
    self match
      case Ok(_, _) => putStrLn("Ok")
      case Err(msg) =>
        putStr("Err{ ")
        putStr(msg)
        putStrLn(" }")
  def get: T = self match
    case Ok(_, v) => v
struct Parser[+T](run: ParseState^ => ParseResult[T]^)

def failP(msg: String^): Parser[Nothing]^{cap, msg} =
  def fn(st: ParseState^): ParseResult[Nothing]^{} = #unsafeAsPure(Err[Nothing](msg))  // limitation, should be a bug
  Parser(fn)

def eofP: Parser[Unit]^ =
  def fn(st: ParseState^): ParseResult[Unit]^ =
    if st.isAtEnd then Ok(st.clone, ())
    else Err[Unit]("Expecting end-of-file")
  Parser(fn)

def predP(p: char => bool): Parser[char]^{cap, p} =
  def fn(state: ParseState^): ParseResult[char]^ =
    if state.isAtEnd then Err[char]("unexpected end-of-file")
    else
      val ch = state.peek
      if p(ch) then Ok(state.advance, ch)
      else Err[char]("predicate not satisfied")
  Parser[char](fn)

def charP(ch: char): Parser[char]^{cap} =
  predP(ch1 => ch1 == ch)

def pureP[T](value: T): Parser[T]^ =
  def fn(state: ParseState^): ParseResult[T]^ =
    Ok(state.clone, value)
  Parser(fn)

def mapP[T, U](p: Parser[T]^, f: T => U): Parser[U]^{cap, p, f} =
  def fn(st: ParseState^): ParseResult[U]^ =
    p.run(st) match
      case Ok(st1, value) => Ok(st1, f(value))
      case Err(msg) => Err[U](msg)
  Parser(fn)

def appP[A, B](pf: Parser[A => B]^, px: Parser[A]^): Parser[B]^{cap, pf, px} =
  def fn(st: ParseState^): ParseResult[B]^ =
    pf.run(st) match
      case Ok(st1, f) =>
        px.run(st1) match
          case Ok(st2, x) => Ok(st2, f(x))
          case Err(msg) => Err[B](msg)
      case Err(msg) => Err[B](msg)
  Parser(#unsafeAsPure(fn))  // limitation on pattern matching

def alternativeP[T](pa: Parser[T]^, pb: Parser[T]^): Parser[T]^{cap, pa, pb} =
  def fn(st: ParseState^): ParseResult[T]^ =
    pa.run(st) match
      case ok @ Ok(_, _) => ok
      case Err(_) => pb.run(st)
  Parser(fn)

struct P[T, U](fst: T, snd: U)
def pairP[A, B](pa: Parser[A]^, pb: Parser[B]^): Parser[P[A, B]^]^ =
  def fn(st: ParseState^): ParseResult[P[A, B]^]^ =
    pa.run(st) match
      case Ok(st1, a) =>
        pb.run(st1) match
          case Ok(st2, b) => 
            val t = Ok(st2, #box(P(a, b)))
            t
          case Err(msg) => 
            val t = Err[Nothing](msg)
            ???
      case Err(msg) => 
        //Err[Nothing](msg)
        ???
  Parser(fn)

def stringP(str: String^): Parser[Unit]^ =
  def go(cur: i32): Parser[Unit]^ =
    if cur >= str.size then 
      val t = pureP[Unit](())
      t
    else 
      val t = mapP(pairP(charP(str(cur)), go(cur+1)), res => ())
      t
  go(0)

def someP[T](px: Parser[T]^): Parser[List[T]^]^ =
  def fn(st: ParseState^): ParseResult[List[T]]^ = ???
  ???

def manyP[T](px: Parser[T]^): Parser[List[T]^]^ =
  def fn(st: ParseState^): ParseResult[List[T]]^ = ???
  ???

def main(): Unit =
  putStrLn("hello, world")
  val state1 = ParseState("hello", 0)
  val state2 = ParseState("world", 0)
  val state3 = ParseState("1+1", 0)
  val p1 = predP(ch => ch == 'h')
  val p2 = predP(ch => ch == '-')
  val p3 = predP(ch => ch >= '0' && ch <= '9')
  val p4 = alternativeP(p2, p3)
  val p5 = stringP("wor")
  p4.run(state3).print()
  p5.run(state2).print()
