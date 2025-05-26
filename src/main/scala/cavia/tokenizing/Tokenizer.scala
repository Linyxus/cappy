package cavia
package tokenizing
import io.SourceFile
import core.*

class Tokenizer(source: SourceFile):
  import Tokenizer.*
  var currentPos: Int = 0
  val lineLengths: List[Int] = source.content.linesWithSeparators.map(_.length).toList
  var indentStack: List[Int] = List(0)

  /** Get the current indentation level */
  def lastIndent: Int = indentStack.head

  /** Check if the given indentation level is valid */
  def validIndent(indent: Int): Boolean =
    indentStack.exists(_ == indent)

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

  /** Try to peek at the next character without consuming it; return None if at end */
  def maybePeek: Option[Char] =
    if isAtEnd then None
    else Some(peek)

  /** Check if the next character is the expected character */
  def expectChar(ch: Char): Boolean =
    if !isAtEnd && peek == ch then
      advance()
      true
    else
      false

  def expectString(str: String): Boolean =
    if currentPos + str.length > source.content.length then
      false
    else if source.content.substring(currentPos, currentPos + str.length) == str then
      currentPos += str.length
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

  val SPECIAL_CHARS = Set('_', '#', '?')

  def skipWhitespaces(): Boolean =
    var skippedNewline = false
    var isInComment = false
    @annotation.tailrec
    def loop(): Boolean =
      if isAtEnd then
        skippedNewline
      else if peek.isWhitespace || isInComment then
        if peek == '\n' || peek == '\r' then
          skippedNewline = true
          isInComment = false
        advance()
        loop()
      else if expectString("//") then
        isInComment = true
        loop()
      else skippedNewline
    loop()

  var needNewLine = false
  var needDedent = false

  /** Consume an integer or a double literal. Return true if it's a double literal. */
  def consumeIntOrDouble(): Boolean =
    var isDouble = false
    while !isAtEnd && peek.isDigit do
      advance()
    if expectChar('.') then
      if !isAtEnd && peek.isDigit then
        isDouble = true
        while !isAtEnd && peek.isDigit do
          advance()
      else
        currentPos -= 1
    isDouble

  def nextToken(): Token | Error =
    if needNewLine then
      needNewLine = false
      Token.NEWLINE().withCurrentPos
    else
      val hasNewLine = skipWhitespaces()
      if isAtEnd then Token.EOF().withCurrentPos
      else if hasNewLine || needDedent then
        if currentIndent > lastIndent then
          indent(currentIndent)
          Token.INDENT().withCurrentPos
        else if currentIndent < lastIndent then
          dedent()
          if currentIndent == lastIndent then
            needDedent = false
            needNewLine = true
            Token.DEDENT().withCurrentPos
          else if validIndent(currentIndent) then
            needDedent = true
            Token.DEDENT().withCurrentPos
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
          case '\'' =>
            maybePeek match
              case Some('\\') =>
                advance()
                val token: Token | Error =
                  if expectChar('n') then
                    Token.CHAR('\n')
                  else if expectChar('r') then
                    Token.CHAR('\r')
                  else if expectChar('t') then
                    Token.CHAR('\t')
                  else if expectChar('\\') then
                    Token.CHAR('\\')
                  else if expectChar('0') then
                    Token.CHAR('\u0000')
                  else
                    Error(s"Unrecognised escape character")
                if token.isInstanceOf[Error] then
                  token
                else
                  if expectChar('\'') then
                    token
                  else
                    Error(s"Unenclosed character literal")
              case Some(ch) =>
                advance()
                if expectChar('\'') then
                  Token.CHAR(ch)
                else
                  Error(s"Unenclosed character literal")
              case None => Error(s"Unenclosed character literal")
          case '=' if expectChar('>') => Token.FAT_ARROW()
          case '=' if expectChar('=') => Token.DOUBLE_EQUAL()
          case '=' => Token.EQUAL()
          case '(' => Token.LPAREN()
          case ')' => Token.RPAREN()
          case '[' => Token.LBRACK()
          case ']' => Token.RBRACK()
          case ':' => Token.COLON()
          case ',' => Token.COMMA()
          case '.' => Token.DOT()
          case '@' => Token.AT()
          case '{' => Token.LBRACE()
          case '}' => Token.RBRACE()
          case '^' => Token.HAT()
          case '-' if expectChar('>') => Token.ARROW()
          case '-' => Token.MINUS()
          case '<' if expectChar(':') => Token.LESSCOLON()
          case '<' if expectChar('=') => Token.LESS_EQUAL()
          case '<' => Token.LESS()
          case '>' if expectChar('=') => Token.GREATER_EQUAL()
          case '>' => Token.GREATER()
          case '!' if expectChar('=') => Token.BANG_EQUAL()
          case '!' => Token.BANG()
          case '+' if expectChar('+') => Token.DOUBLE_PLUS()
          case '+' => Token.PLUS()
          case '*' => Token.STAR()
          case '/' => Token.SLASH()
          case '%' => Token.PERCENT()
          case '&' if expectChar('&') => Token.DOUBLE_AMP()
          case '|' if expectChar('|') => Token.DOUBLE_BAR()
          case ch if ch.isDigit =>
            var startPos = currentPos - 1
            val isDouble = consumeIntOrDouble()
            if isDouble then
              Token.FLOAT(source.content.substring(startPos, currentPos))
            else
              Token.INT(source.content.substring(startPos, currentPos))
          case ch if ch.isLetter || SPECIAL_CHARS.contains(ch) =>
            val startPos = currentPos - 1
            def isValidChar(ch: Char): Boolean = ch.isLetterOrDigit || SPECIAL_CHARS.contains(ch)
            while !isAtEnd && isValidChar(peek) do
              advance()
            val content = source.content.substring(startPos, currentPos)
            Token.IDENT(content)
          case ch => Error(f"Unrecognised character: $ch")

object Tokenizer:
  import reporting.Message
  case class Error(msg: String) extends Positioned:
    def asMessage: Message = Message.simple(s"ERROR(tokenization): $msg", pos)

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
