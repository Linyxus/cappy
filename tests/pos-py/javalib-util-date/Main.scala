import java.util.Date

@main def javalibUtilDate(): Unit =
  val date = new Date(123456789L)
  val cloned = date.clone().asInstanceOf[Date]
  println("millis:" + date.getTime() + ":" + cloned.before(new Date(123456790L)) + ":" + new Date("123456789").toString())
