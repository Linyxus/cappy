@main def javalibLangStringSubsequence(): Unit =
  // Direct String subSequence
  val s: String = "scalapy"
  println("string-sub:" + s.subSequence(0, 5))
  println("string-sub:" + s.subSequence(2, 7))
  println("string-sub-empty:[" + s.subSequence(3, 3) + "]")
  println("string-sub-full:" + s.subSequence(0, s.length()))

  // String accessed through CharSequence interface — runtime is Python str
  val cs: CharSequence = s
  println("cs-len:" + cs.length())
  println("cs-charAt:" + cs.charAt(0).toInt)
  println("cs-sub:" + cs.subSequence(1, 4))
  println("cs-sub-toString:" + cs.subSequence(0, 5).toString())

  // Bounds checking matches JVM contract
  try
    s.subSequence(-1, 3)
    println("missed-iobe")
  catch case e: StringIndexOutOfBoundsException => println("iobe:negative-begin")

  try
    s.subSequence(2, 100)
    println("missed-iobe")
  catch case e: StringIndexOutOfBoundsException => println("iobe:end-too-large")

  try
    s.subSequence(4, 2)
    println("missed-iobe")
  catch case e: StringIndexOutOfBoundsException => println("iobe:begin-greater-than-end")
