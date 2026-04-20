import java.nio.charset.{CoderResult, CodingErrorAction, MalformedInputException, UnmappableCharacterException}

@main def javalibCharsetCoderResult(): Unit =
  val malformed1a = CoderResult.malformedForLength(1)
  val malformed1b = CoderResult.malformedForLength(1)
  val malformed5a = CoderResult.malformedForLength(5)
  val malformed5b = CoderResult.malformedForLength(5)
  val unmappable2a = CoderResult.unmappableForLength(2)
  val unmappable2b = CoderResult.unmappableForLength(2)
  println("identity:" + (CoderResult.UNDERFLOW eq CoderResult.UNDERFLOW) + ":" + (CoderResult.OVERFLOW eq CoderResult.OVERFLOW))
  println("cache:" + (malformed1a eq malformed1b) + ":" + (malformed5a eq malformed5b) + ":" + (unmappable2a eq unmappable2b))
  println("predicates:" + CoderResult.UNDERFLOW.isUnderflow() + ":" + CoderResult.OVERFLOW.isOverflow() + ":" + malformed1a.isMalformed() + ":" + unmappable2a.isUnmappable() + ":" + malformed1a.isError())
  val malformedThrow =
    try
      malformed5a.throwException()
      "no-throw"
    catch
      case e: MalformedInputException => "malformed:" + e.getInputLength()
  val unmappableThrow =
    try
      unmappable2a.throwException()
      "no-throw"
    catch
      case e: UnmappableCharacterException => "unmappable:" + e.getInputLength()
  println("throws:" + malformedThrow + ":" + unmappableThrow)
  println("actions:" + CodingErrorAction.REPLACE + ":" + CodingErrorAction.REPORT + ":" + CodingErrorAction.IGNORE)
