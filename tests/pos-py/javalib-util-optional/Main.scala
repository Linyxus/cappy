import java.util.Optional

@main def javalibUtilOptional(): Unit =
  val present = Optional.of("value")
  val empty = Optional.empty[String]()
  println("present:" + present.isPresent() + ":" + present.map(_.length).get())
  println("empty:" + empty.isEmpty() + ":" + empty.orElse("fallback"))
