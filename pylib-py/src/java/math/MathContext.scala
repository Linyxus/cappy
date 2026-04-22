package java.math

object MathContext:
  private final class ParsedContext(val precision: Int, val roundingMode: RoundingMode)

  // Held as `val`s so the static-field forwarder replicates them on
  // `class MathContext`, matching stdlib's `MathContext.DECIMAL128`
  // field-access pattern. The lazy-module-init runtime
  // (`_scpy_lazy_module` in PyIRRuntime) defers MathContext's `__init__`
  // until first attribute access, so the `RoundingMode` accesses below
  // safely resolve through their own lazy proxy.
  val UNLIMITED: MathContext = new MathContext(0, RoundingMode.HALF_UP)
  val DECIMAL32: MathContext = new MathContext(7, RoundingMode.HALF_EVEN)
  val DECIMAL64: MathContext = new MathContext(16, RoundingMode.HALF_EVEN)
  val DECIMAL128: MathContext = new MathContext(34, RoundingMode.HALF_EVEN)

  private def parse(text: String): ParsedContext =
    if text == null then
      throw new NullPointerException()

    val precisionPrefix = "precision="
    val roundingPrefix = "roundingMode="
    val split = text.indexOf(' ', precisionPrefix.length)
    if !text.startsWith(precisionPrefix) || split < 0 then
      throw new IllegalArgumentException("Missing precision: " + text)

    val precisionText = text.substring(precisionPrefix.length, split)
    val precision =
      try java.lang.Integer.parseInt(precisionText)
      catch case _: NumberFormatException => throw new IllegalArgumentException("Bad precision: " + text)

    val roundingStart = split + 1
    if !text.regionMatches(roundingStart, roundingPrefix, 0, roundingPrefix.length) then
      throw new IllegalArgumentException("Missing rounding mode: " + text)

    val rounding = RoundingMode.valueOf(text.substring(roundingStart + roundingPrefix.length))
    new ParsedContext(precision, rounding)

final class MathContext(setPrecision: Int, setRoundingMode: RoundingMode):
  if setPrecision < 0 then
    throw new IllegalArgumentException("Negative precision: " + setPrecision)
  if setRoundingMode == null.asInstanceOf[RoundingMode] then
    throw new NullPointerException()

  private val precision0 = setPrecision
  private val roundingMode0 = setRoundingMode

  def this(setPrecision: Int) =
    this(setPrecision, RoundingMode.HALF_UP)

  private def this(args: MathContext.ParsedContext) =
    this(args.precision, args.roundingMode)

  def this(text: String) =
    this(MathContext.parse(text))

  def getPrecision(): Int =
    precision0

  def getRoundingMode(): RoundingMode =
    roundingMode0

  override def equals(other: Any): Boolean =
    other match
      case that: MathContext =>
        precision0 == that.precision0 &&
          roundingMode0 == that.roundingMode0
      case _ =>
        false

  override def hashCode(): Int =
    (precision0 << 3) | roundingMode0.ordinal

  override def toString(): String =
    "precision=" + precision0 + " roundingMode=" + roundingMode0
