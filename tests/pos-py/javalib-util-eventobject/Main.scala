import java.util.EventObject

@main def javalibUtilEventObject(): Unit =
  val source = "payload"
  val event = new EventObject(source)
  println("source:" + event.getSource() + ":" + event.toString().contains("payload"))
