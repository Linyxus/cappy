package cappy.tokenizing
import cappy.io.SourceFile
import cappy.core.*
import scala.compiletime.ops.double

class Tokenizer(source: SourceFile):
  import Tokenizer.*
  var currentPos: Int = 0
  val lineLengths: List[Int] = source.content.linesWithSeparators.map(_.length).toList
  var indentStack: List[Int] = List(0)

  /** Get the current indentation level */
  def lastIndent: Int = indentStack.head

  /** Get the current indentation level */
  def currentIndent: Int = locateColumn(currentPos)

  /** Push a new indentation level onto the stack */
  def indent(nextLevel: Int): Unit =
    indentStack = nextLevel :: indentStack

  /** Pop the current indentation level from the stack */
  def dedent(): Unit =
    indentStack = indentStack.tail

  /** Find out the column number of a position */
  def locateColumn(pos: Int): Int =
    var remain = pos
    var lineIdx = 0
    
    // Find which line contains the position
    while lineIdx < lineLengths.size && remain >= lineLengths(lineIdx) do
      remain -= lineLengths(lineIdx)
      lineIdx += 1
    
    // remain is now the offset within the line
    remain

  /** Check if the current position is at the end of the source file */
  def isAtEnd: Boolean = currentPos >= source.content.length

  /** Peek at the next character without consuming it */
  def peek: Char = source.content(currentPos)

  /** Check if the next character is the expected character */
  def expectChar(ch: Char): Boolean =
    if peek == ch then
      advance()
      true
    else
      false

  /** Advance the current position by one character */
  def advance(): Unit = currentPos += 1

  /** Advance the current position by one character and return the character */
  def forward(): Char =
    val ch = peek
    advance()
    ch

  def withPosition[A <: Positioned](op: => A): A =
    val startPos = currentPos
    val result = op
    val endPos = currentPos
    result.withPos(SourcePos(source, Span(startPos, endPos)))

  extension[A <: Positioned] (self: A)
    def withCurrentPos: self.type = withPosition(self)

  def skipWhitespaces(): Boolean =
    var skippedNewline = false
    while !isAtEnd && peek.isWhitespace do
      if peek == '\n' || peek == '\r' then
        skippedNewline = true
      advance()
    skippedNewline

  def nextToken(): Token | Error =
    val hasNewLine = skipWhitespaces()

    if isAtEnd then Token.EOF().withCurrentPos
    else if hasNewLine then
      if currentIndent > lastIndent then
        indent(currentIndent)
        Token.INDENT().withCurrentPos
      else if currentIndent < lastIndent then
        dedent()
        if currentIndent == lastIndent then
          Token.NEWLINE().withCurrentPos
        else
          Error(s"Invalid indentation level: $currentIndent, expected $lastIndent")
      else
        Token.NEWLINE().withCurrentPos
    else withPosition:
      forward() match
        case '"' =>
          val startPos = currentPos - 1
          while !isAtEnd && peek != '"' && peek != '\n' && peek != '\r' do
            advance()
          if !isAtEnd && peek == '"' then
            advance()
            val content = source.content.substring(startPos, currentPos)
            Token.STR(content)
          else
            val what = if isAtEnd then "end of file" else "new line"
            Error(s"Unclosed string literal, unexpected $what")
        case '=' if expectChar('>') => Token.FAT_ARROW()
        case '=' => Token.EQUAL()
        case '(' => Token.LPAREN()
        case ')' => Token.RPAREN()
        case '[' => Token.LBRACK()
        case ']' => Token.RBRACK()
        case ':' => Token.COLON()
        case ',' => Token.COMMA()
        case '{' => Token.LBRACE()
        case '}' => Token.RBRACE()
        case '^' => Token.HAT()
        case '-' if expectChar('>') => Token.ARROW()
        case '<' if expectChar(':') => Token.LESSCOLON()
        case '_' => Token.IDENT("_")
        case ch if ch.isLetter =>
          val startPos = currentPos - 1
          while !isAtEnd && peek.isLetterOrDigit do
            advance()
          val content = source.content.substring(startPos, currentPos)
          Token.IDENT(content)
        case ch => Error(f"Unrecognised character: $ch")

object Tokenizer:
  case class Error(msg: String) extends Positioned

  def tokenize(source: SourceFile): List[Token | Error] =
    val tokenizer = Tokenizer(source)
    var tokens: List[Token | Error] = List()
    var hasError = false
    while !tokenizer.isAtEnd && !hasError do
      val thisToken = tokenizer.nextToken()
      if thisToken.isInstanceOf[Error] then
        hasError = true
      tokens = thisToken :: tokens
    tokens.reverse
