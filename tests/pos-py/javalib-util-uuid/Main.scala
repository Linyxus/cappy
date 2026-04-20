import java.util.UUID

@main def javalibUtilUuid(): Unit =
  val parsed = UUID.fromString("123e4567-e89b-12d3-a456-426614174000")
  println("fromstring:" + parsed.toString() + ":" + parsed.version() + ":" + parsed.variant())

  val reparsed = UUID.fromString(parsed.toString())
  println("compare:" + (parsed.compareTo(reparsed) == 0) + ":" + parsed.compareTo(reparsed))

  val random = UUID.randomUUID()
  println("random:" + random.toString().length() + ":" + random.version() + ":" + random.variant())
