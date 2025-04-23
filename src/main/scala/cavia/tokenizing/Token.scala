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
  case EOF()  // End of file
  case INDENT()  // >>> indent
  case DEDENT()  // <<< dedent
  case NEWLINE()  // new line with same indentation

  def isWhitespaceOrEOF: Boolean = this match
    case INDENT() | DEDENT() | NEWLINE() | EOF() => true
    case _ => false
