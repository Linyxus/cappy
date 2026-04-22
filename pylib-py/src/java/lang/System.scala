package java.lang

import java.io.PrintStream

import scala.python.{PyAny, extern, name, native}
import scala.python.runtime.{PyOs, PySys, PyTime}

object System:
  @extern("builtins")
  private object builtins extends PyAny:
    @name("id")
    def pyId(value: Any): Long = native

  final class _SystemProperties private[lang] ():
    def getProperty(key: String): String | Null =
      System.getProperty(key)

    def getProperty(key: String, defaultValue: String): String =
      System.getProperty(key, defaultValue)

    def setProperty(key: String, value: String): String | Null =
      System.setProperty(key, value)

    def clearProperty(key: String): String | Null =
      System.clearProperty(key)

    def stringPropertyNames(): Array[String] =
      System.copyPropertyNames()

  final class _SystemEnv private[lang] ():
    def get(key: String): String | Null =
      System.getenv(key)

    def containsKey(key: String): scala.Boolean =
      get(key) != null

  private var inRef: Object | Null = null
  private var outRef = PrintStream.stdout()
  private var errRef = PrintStream.stderr()
  private val propertiesView = new _SystemProperties()
  private val envView = new _SystemEnv()

  private var propertyKeys = new Array[String](16)
  private var propertyValues = new Array[String](16)
  private var propertySize = 0
  private var propertiesInitialized = false

  def in: Object | Null = inRef

  def out: PrintStream = outRef

  def err: PrintStream = errRef

  def setIn(in: Object | Null): Unit =
    inRef = in

  def setOut(out: PrintStream): Unit =
    outRef = ThrowablesSupport.requireNonNull(out)

  def setErr(err: PrintStream): Unit =
    errRef = ThrowablesSupport.requireNonNull(err)

  def currentTimeMillis(): scala.Long =
    PyTime.time_ns() / 1000000L

  def nanoTime(): scala.Long =
    PyTime.perf_counter_ns()

  def arraycopy(src: Object, srcPos: scala.Int, dest: Object, destPos: scala.Int, length: scala.Int): Unit =
    if src == null || dest == null then
      throw new NullPointerException()

    val srcArray = src.asInstanceOf[Array[Object]]
    val destArray = dest.asInstanceOf[Array[Object]]

    if srcPos < 0 || destPos < 0 || length < 0 then
      throw new ArrayIndexOutOfBoundsException(length)
    if srcPos > srcArray.length - length then
      throw new ArrayIndexOutOfBoundsException(srcPos + length)
    if destPos > destArray.length - length then
      throw new ArrayIndexOutOfBoundsException(destPos + length)

    if (srcArray.asInstanceOf[AnyRef] ne destArray.asInstanceOf[AnyRef]) || destPos < srcPos || srcPos + length < destPos then
      var i = 0
      while i < length do
        destArray(destPos + i) = srcArray(srcPos + i)
        i += 1
    else
      var i = length - 1
      while i >= 0 do
        destArray(destPos + i) = srcArray(srcPos + i)
        i -= 1

  def identityHashCode(x: Any): scala.Int =
    if x == null then 0
    else builtins.pyId(x).asInstanceOf[scala.Long].toInt & 0x7FFFFFFF

  def getProperty(key: String): String | Null =
    ensurePropertiesInitialized()
    propertyValue(propertyIndex(ThrowablesSupport.requireNonNull(key)))

  def getProperty(key: String, defaultValue: String): String =
    getProperty(key) match
      case null  => defaultValue
      case value => value

  def setProperty(key: String, value: String): String | Null =
    ensurePropertiesInitialized()
    val nnKey = ThrowablesSupport.requireNonNull(key)
    val nnValue = ThrowablesSupport.requireNonNull(value)
    val idx = propertyIndex(nnKey)
    if idx >= 0 then
      val previous = propertyValues(idx)
      propertyValues(idx) = nnValue
      previous
    else
      ensurePropertyCapacity(propertySize + 1)
      propertyKeys(propertySize) = nnKey
      propertyValues(propertySize) = nnValue
      propertySize += 1
      null

  def clearProperty(key: String): String | Null =
    ensurePropertiesInitialized()
    val idx = propertyIndex(ThrowablesSupport.requireNonNull(key))
    if idx < 0 then null
    else
      val previous = propertyValues(idx)
      var i = idx
      while i < propertySize - 1 do
        propertyKeys(i) = propertyKeys(i + 1)
        propertyValues(i) = propertyValues(i + 1)
        i += 1
      propertySize -= 1
      previous

  def getProperties(): _SystemProperties =
    ensurePropertiesInitialized()
    propertiesView

  def getenv(name: String): String | Null =
    PyOs.getenv(ThrowablesSupport.requireNonNull(name))

  def getenv(): _SystemEnv =
    envView

  def lineSeparator(): String =
    "\n"

  def gc(): Unit =
    Runtime.getRuntime().gc()

  private def userName(): String | Null =
    val user = PyOs.getenv("USER")
    if user != null then user else PyOs.getenv("USERNAME")

  private def propertyIndex(key: String): Int =
    var i = 0
    while i < propertySize do
      if propertyKeys(i) == key then return i
      i += 1
    -1

  private def propertyValue(index: Int): String | Null =
    if index < 0 then null else propertyValues(index)

  private def seedProperty(key: String, value: String | Null, fallback: String): Unit =
    ensurePropertyCapacity(propertySize + 1)
    propertyKeys(propertySize) = key
    propertyValues(propertySize) = if value == null then fallback else value
    propertySize += 1

  private def ensurePropertyCapacity(required: Int): Unit =
    if required <= propertyKeys.length then
      ()
    else
      val newSize = if propertyKeys.length < required then required * 2 else propertyKeys.length * 2
      val newKeys = new Array[String](newSize)
      val newValues = new Array[String](newSize)
      var i = 0
      while i < propertySize do
        newKeys(i) = propertyKeys(i)
        newValues(i) = propertyValues(i)
        i += 1
      propertyKeys = newKeys
      propertyValues = newValues

  private[lang] def copyPropertyNames(): Array[String] =
    ensurePropertiesInitialized()
    val names = new Array[String](propertySize)
    var i = 0
    while i < propertySize do
      names(i) = propertyKeys(i)
      i += 1
    names

  private def ensurePropertiesInitialized(): Unit =
    if !propertiesInitialized then
      seedProperty("os.name", PyOs.os_name(), "posix")
      seedProperty("os.arch", PyOs.getenv("HOSTTYPE"), "python")
      seedProperty("os.version", PyOs.getenv("OSTYPE"), "")
      seedProperty("java.version", "scala-py", "scala-py")
      seedProperty("java.vendor", "scala-py", "scala-py")
      seedProperty("file.separator", "/", "/")
      seedProperty("path.separator", ":", ":")
      seedProperty("line.separator", "\n", "\n")
      seedProperty("user.home", PyOs.getenv("HOME"), "")
      seedProperty("user.dir", PyOs.getcwd(), "")
      seedProperty("user.name", userName(), "")
      propertiesInitialized = true
