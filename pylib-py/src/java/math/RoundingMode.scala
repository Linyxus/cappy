package java.math

final class RoundingMode private (
    val ordinal: Int,
    private val name0: String,
    val pythonName: String
) extends java.io.Serializable:
  override def toString(): String =
    name0

object RoundingMode:
  val UP: RoundingMode = new RoundingMode(0, "UP", "ROUND_UP")
  val DOWN: RoundingMode = new RoundingMode(1, "DOWN", "ROUND_DOWN")
  val CEILING: RoundingMode = new RoundingMode(2, "CEILING", "ROUND_CEILING")
  val FLOOR: RoundingMode = new RoundingMode(3, "FLOOR", "ROUND_FLOOR")
  val HALF_UP: RoundingMode = new RoundingMode(4, "HALF_UP", "ROUND_HALF_UP")
  val HALF_DOWN: RoundingMode = new RoundingMode(5, "HALF_DOWN", "ROUND_HALF_DOWN")
  val HALF_EVEN: RoundingMode = new RoundingMode(6, "HALF_EVEN", "ROUND_HALF_EVEN")
  val UNNECESSARY: RoundingMode = new RoundingMode(7, "UNNECESSARY", "ROUND_UNNECESSARY")

  private val allValues = Array(
    UP,
    DOWN,
    CEILING,
    FLOOR,
    HALF_UP,
    HALF_DOWN,
    HALF_EVEN,
    UNNECESSARY
  )

  def values: Array[RoundingMode] =
    allValues.clone()

  def valueOf(name: String): RoundingMode =
    if name == null then
      throw new NullPointerException()

    var i = 0
    while i < allValues.length do
      val mode = allValues(i)
      if mode.toString() == name then
        return mode
      i += 1

    throw new IllegalArgumentException("No enum constant java.math.RoundingMode." + name)

  def valueOf(mode: Int): RoundingMode =
    mode match
      case BigDecimal.ROUND_UP          => UP
      case BigDecimal.ROUND_DOWN        => DOWN
      case BigDecimal.ROUND_CEILING     => CEILING
      case BigDecimal.ROUND_FLOOR       => FLOOR
      case BigDecimal.ROUND_HALF_UP     => HALF_UP
      case BigDecimal.ROUND_HALF_DOWN   => HALF_DOWN
      case BigDecimal.ROUND_HALF_EVEN   => HALF_EVEN
      case BigDecimal.ROUND_UNNECESSARY => UNNECESSARY
      case _                            => throw new IllegalArgumentException("Invalid rounding mode")
