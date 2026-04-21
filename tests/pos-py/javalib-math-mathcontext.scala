import java.math.{MathContext, RoundingMode}

@main def javalibMathMathcontext(): Unit =
  val parsed = new MathContext("precision=9 roundingMode=HALF_DOWN")
  val custom = new MathContext(5, RoundingMode.CEILING)
  println("decimal32:" + MathContext.DECIMAL32.getPrecision() + ":" + MathContext.DECIMAL32.getRoundingMode().pythonName)
  println("unlimited:" + MathContext.UNLIMITED.getPrecision() + ":" + MathContext.UNLIMITED.getRoundingMode())
  println("custom:" + parsed.getPrecision() + ":" + parsed.getRoundingMode() + ":" + custom.toString())
  println("equals-hashcode:" + (new MathContext(5, RoundingMode.CEILING) == custom) + ":" + (new MathContext(5, RoundingMode.CEILING).hashCode() == custom.hashCode()))
