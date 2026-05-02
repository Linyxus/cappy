// Smoke test for Wave 5 priority #1: every JVM-reflection method that the
// Python backend does not support raises UnsupportedOperationException with
// the documented message prefix, so users can `catch` it and fixtures show
// up as a clear error rather than a silent empty-array result.
//
// The full policy lives in
// `notes/wave5-worklist/01-reflection-unsupported-and-blacklist.md`.

@main def reflectionUnsupported(): Unit =
  val cls = classOf[String]

  // Intrinsic ops still work — these MUST NOT throw.
  println("getName:" + cls.getName)
  println("getSimpleName:" + cls.getSimpleName)
  println("isArray:" + cls.isArray)
  println("isPrimitive:" + cls.isPrimitive)
  println("isInterface:" + cls.isInterface)

  // Unsupported ops throw UnsupportedOperationException with the
  // documented prefix. Verify a representative selection.
  def expectUOE(name: String)(body: => Unit): Unit =
    try
      body
      println("MISSING_UOE:" + name)
    catch
      case e: UnsupportedOperationException =>
        val msg = e.getMessage()
        val ok = msg != null && msg.startsWith("JVM reflection is not supported")
        println("uoe:" + name + ":" + ok)

  expectUOE("getDeclaredFields"):
    cls.getDeclaredFields
  expectUOE("getDeclaredMethods"):
    cls.getDeclaredMethods
  expectUOE("getMethods"):
    cls.getMethods
  expectUOE("getFields"):
    cls.getFields
  expectUOE("getDeclaredField"):
    cls.getDeclaredField("foo")
  expectUOE("getDeclaredMethod"):
    cls.getDeclaredMethod("foo")
  expectUOE("getEnumConstants"):
    cls.getEnumConstants
  expectUOE("getEnclosingClass"):
    cls.getEnclosingClass
