@main def javalibLangFloatBits(): Unit =
  println("float:one-bits:" + java.lang.Float.floatToRawIntBits(1.0f))
  println("float:negzero-bits:" + (java.lang.Float.floatToRawIntBits(-0.0f) == 0x80000000))
  println("float:subnormal-rt:" + (java.lang.Float.floatToRawIntBits(java.lang.Float.intBitsToFloat(1)) == 1))
  println("float:nan:" + java.lang.Float.isNaN(java.lang.Float.intBitsToFloat(0x7fc00000)))

  println("double:one-bits:" + java.lang.Double.doubleToRawLongBits(1.0))
  println("double:negzero-bits:" + (java.lang.Double.doubleToRawLongBits(-0.0) == 0x8000000000000000L))
  println("double:subnormal-rt:" + (java.lang.Double.doubleToRawLongBits(java.lang.Double.longBitsToDouble(1L)) == 1L))
  println("double:nan:" + java.lang.Double.isNaN(java.lang.Double.longBitsToDouble(0x7ff8000000000000L)))
