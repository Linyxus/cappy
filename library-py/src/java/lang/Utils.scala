package java.lang

private[java] object Utils:
  /** Round up to a power of 2; if overflow, returns the given number. */
  @inline def roundUpToPowerOfTwo(i: Int): Int =
    if i > (1 << 30) then i
    else ((1 << 31) >>> (Integer.numberOfLeadingZeros(i - 1)) - 1)
