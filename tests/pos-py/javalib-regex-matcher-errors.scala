import java.lang.StringBuilder
import java.util.regex.Pattern

@main def javalibRegexMatcherErrors(): Unit =
  // group() before any match attempt -> IllegalStateException
  val m0 = Pattern.compile("a+").matcher("abc")
  try
    m0.group()
    println("before-match:no-throw")
  catch
    case e: IllegalStateException => println("before-match:ISE")

  // group(999) out of range -> IndexOutOfBoundsException from numberedGroup
  val m1 = Pattern.compile("(a)(b)").matcher("abc")
  m1.find()
  try
    m1.group(999)
    println("out-of-range:no-throw")
  catch
    case _: IndexOutOfBoundsException => println("out-of-range:IOOBE")

  // appendReplacement: bare $ at end of replacement -> IllegalArgumentException
  val m3 = Pattern.compile("a").matcher("a")
  m3.find()
  try
    m3.appendReplacement(new StringBuilder(), "$")
    println("dangling-dollar:no-throw")
  catch
    case _: IllegalArgumentException => println("dangling-dollar:IAE")

  // appendReplacement: $ followed by non-digit, non-{ -> IllegalArgumentException
  val m4 = Pattern.compile("a").matcher("a")
  m4.find()
  try
    m4.appendReplacement(new StringBuilder(), "$x")
    println("bad-dollar:no-throw")
  catch
    case _: IllegalArgumentException => println("bad-dollar:IAE")

  // appendReplacement: ${ with no closing } -> IllegalArgumentException
  val m5 = Pattern.compile("a").matcher("a")
  m5.find()
  try
    m5.appendReplacement(new StringBuilder(), "${name")
    println("unterminated-name:no-throw")
  catch
    case _: IllegalArgumentException => println("unterminated-name:IAE")

  // appendReplacement: trailing backslash with no escaped char -> IllegalArgumentException
  val m6 = Pattern.compile("a").matcher("a")
  m6.find()
  try
    m6.appendReplacement(new StringBuilder(), "\\")
    println("dangling-backslash:no-throw")
  catch
    case _: IllegalArgumentException => println("dangling-backslash:IAE")
