package java.util

class Properties(protected val defaults: Properties) extends Hashtable[AnyRef, AnyRef] {
  def this() = this(null.asInstanceOf[Properties])

  def setProperty(key: String, value: String): AnyRef =
    put(key, value)

  def getProperty(key: String): String =
    getProperty(key, null.asInstanceOf[String])

  def getProperty(key: String, defaultValue: String): String =
    get(key) match
      case value: String => value
      case _ =>
        if defaults != null then defaults.getProperty(key, defaultValue)
        else defaultValue

  def propertyNames(): java.util.Enumeration[?] = {
    val names = new HashSet[String]
    var ancestor = this
    while ancestor != null do
      val keys = ancestor.keySet().iterator()
      while keys.hasNext() do
        names.add(keys.next().asInstanceOf[String])
      ancestor = ancestor.defaults
    enumerationOf(names)
  }

  def stringPropertyNames(): java.util.Set[String] = {
    val names = new HashSet[String]
    var ancestor = this
    while ancestor != null do
      val entries = ancestor.entrySet().iterator()
      while entries.hasNext() do
        val entry = entries.next()
        val key = entry.getKey()
        val value = entry.getValue()
        if key.isInstanceOf[String] && value.isInstanceOf[String] then
          names.add(key.asInstanceOf[String])
      ancestor = ancestor.defaults
    names
  }

  private def enumerationOf[A](set: java.util.Set[A]): java.util.Enumeration[A] = {
    val iter = set.iterator()
    new java.util.Enumeration[A] {
      def hasMoreElements(): Boolean = iter.hasNext()
      def nextElement(): A = iter.next()
    }
  }
}
