import java.util.regex.{Pattern, PatternSyntaxException}

@main def javalibRegexBadPattern(): Unit =
  try
    Pattern.compile("[")
    println("bad:no-throw")
  catch
    case e: PatternSyntaxException =>
      println("bad:" + e.getDescription() + ":" + e.getIndex())
