import java.util.Properties

@main def javalibUtilProperties(): Unit =
  val defaults = new Properties()
  defaults.setProperty("fallback", "base")

  val props = new Properties(defaults)
  props.setProperty("name", "scala-py")
  println("get:" + props.getProperty("name") + ":" + props.getProperty("fallback"))
  println("names:" + props.stringPropertyNames().contains("name") + ":" + props.stringPropertyNames().contains("fallback"))
