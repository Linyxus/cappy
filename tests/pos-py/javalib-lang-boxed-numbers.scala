@main def javalibLangBoxedNumbers(): Unit =
  println("boolean:parse:" + java.lang.Boolean.parseBoolean("TrUe"))
  println("boolean:compare:" + java.lang.Boolean.compare(false, true))
  println("boolean:identity:" + (java.lang.Boolean.TRUE == java.lang.Boolean.valueOf(true)))

  println("byte:parse:" + java.lang.Byte.parseByte("7f", 16))
  println("byte:compare:" + java.lang.Byte.compare((-1).toByte, 1.toByte))
  println("byte:unsigned:" + java.lang.Byte.toUnsignedInt((-1).toByte))

  println("short:parse:" + java.lang.Short.toString(java.lang.Short.parseShort("7fff", 16)))
  println("short:compare:" + java.lang.Short.compare(java.lang.Short.parseShort("-1"), java.lang.Short.parseShort("1")))
  println("short:unsigned:" + java.lang.Short.toUnsignedInt(java.lang.Short.parseShort("-1")))

  println("integer:parse:" + java.lang.Integer.parseInt("ff", 16))
  println("integer:hex:" + java.lang.Integer.toHexString(255))

  println("long:parse:" + java.lang.Long.parseLong("7fffffffffffffff", 16))
  println("long:compare:" + java.lang.Long.compare(-5L, 5L))
  println("long:hash:" + java.lang.Long.hashCode(0x1122334455667788L))
  println("long:hex:" + java.lang.Long.toHexString(-1L))
  println("long:bitcount:" + java.lang.Long.bitCount(-1L))
  println("long:unsigned:" + java.lang.Long.toUnsignedString(-1L))

  println("float:parse:" + java.lang.Float.parseFloat("0x1.0p2"))
  println("float:hex:" + java.lang.Float.toHexString(3.5f))
  println("float:nan-ne:" + (java.lang.Float.NaN != java.lang.Float.NaN))
  println("float:compare:" + java.lang.Float.compare(-0.0f, 0.0f))
  println("float:hash-nan:" + java.lang.Float.hashCode(java.lang.Float.NaN))

  println("double:parse:" + java.lang.Double.parseDouble("0x1.8p1"))
  println("double:hex:" + java.lang.Double.toHexString(3.5))
  println("double:nan-ne:" + (java.lang.Double.NaN != java.lang.Double.NaN))
  println("double:compare:" + java.lang.Double.compare(-0.0, 0.0))
  println("double:hash-nan:" + java.lang.Double.hashCode(java.lang.Double.NaN))

  println("void:type:" + java.lang.Void.TYPE)
  println("void:tostring:()")
