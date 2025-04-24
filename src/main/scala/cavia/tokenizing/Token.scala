package cavia.tokenizing
import cavia.core.Positioned

enum Token extends Positioned:
  case IDENT(name: String)  // Variable or function name
  case STR(content: String)  // string literal
  case INT(content: String)  // integer literal
  case EQUAL()  // =
  case LPAREN()  // (
  case RPAREN()  // )
  case LBRACK()  // [
  case RBRACK()  // ]
  case COLON()  // :
  case LESSCOLON()  // <:
  case COMMA()  // ,
  case LBRACE() // {
  case RBRACE() // }
  case DOT()  // .
  case ARROW()  // ->
  case FAT_ARROW()  // =>
  case HAT()  // ^
  case PLUS()  // +
  case MINUS()  // -
  case STAR()  // *
  case SLASH()  // /
  case PERCENT() //  %
  case BANG()  // !
  case DOUBLE_AMP() // &&
  case DOUBLE_BAR()  // ||
  case DOUBLE_EQUAL() // ==
  case BANG_EQUAL() // !=
  case LESS() // <
  case GREATER() // >
  case LESS_EQUAL() // <=
  case GREATER_EQUAL()  // >=
  case DOUBLE_PLUS()  // ++
  case EOF()  // End of file
  case INDENT()  // >>> indent
  case DEDENT()  // <<< dedent
  case NEWLINE()  // new line with same indentation

  def isWhitespaceOrEOF: Boolean = this match
    case INDENT() | DEDENT() | NEWLINE() | EOF() => true
    case _ => false
