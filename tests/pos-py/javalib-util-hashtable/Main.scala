import java.util.{Enumeration, Hashtable}

private def joinEnumeration[A](enumeration: Enumeration[A]): String =
  var first = true
  var result = ""
  while enumeration.hasMoreElements() do
    if !first then
      result += ","
    result += String.valueOf(enumeration.nextElement())
    first = false
  result

private def joinValues(enumeration: Enumeration[String]): String =
  var first = true
  var result = ""
  while enumeration.hasMoreElements() do
    if !first then
      result += ","
    result += enumeration.nextElement()
    first = false
  result

private def catchesNullPointer(body: => Unit): Boolean =
  try
    body
    false
  catch
    case _: NullPointerException => true

@main def javalibUtilHashtable(): Unit =
  val table = new Hashtable[String, String]()
  table.put("one", "first")
  table.put("two", "second")

  println("basic:" + table.size() + ":" + table.get("one") + ":" + table.containsKey("two") + ":" + table.containsValue("second"))
  println("enumeration:" + joinEnumeration(table.keys()) + ":" + joinValues(table.elements()))
  println("nulls:" +
    catchesNullPointer(table.put(null.asInstanceOf[String], "third")) + ":" +
    catchesNullPointer(table.put("three", null.asInstanceOf[String]))
  )
