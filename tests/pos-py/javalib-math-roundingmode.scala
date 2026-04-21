import java.math.{BigDecimal, RoundingMode}

@main def javalibMathRoundingmode(): Unit =
  def joinValues(values: Array[RoundingMode]): String =
    val out = new java.lang.StringBuilder()
    var i = 0
    while i < values.length do
      if i != 0 then out.append(',')
      out.append(values(i).toString())
      i += 1
    out.toString()

  println("values:" + joinValues(RoundingMode.values))
  println("valueof-name:" + RoundingMode.valueOf("HALF_UP").ordinal + ":" + RoundingMode.valueOf("HALF_UP").pythonName)
  println("valueof-ordinal:" + RoundingMode.valueOf(BigDecimal.ROUND_HALF_EVEN) + ":" + RoundingMode.valueOf(BigDecimal.ROUND_FLOOR).pythonName)
  println("python-name-map:" + RoundingMode.HALF_UP.pythonName + ":" + RoundingMode.UNNECESSARY.pythonName)
