import java.util.BitSet

@main def javalibUtilBitSet(): Unit =
  val bits = new BitSet()
  bits.set(1)
  bits.set(3)
  bits.flip(3)
  bits.set(5, 8)
  bits.clear(6)
  println("bitops:" + bits.get(1) + ":" + bits.get(3) + ":" + bits.get(5) + ":" + bits.nextSetBit(0) + ":" + bits.nextClearBit(0) + ":" + bits)

  val grown = new BitSet()
  grown.set(70)
  grown.set(130)
  println("grow:" + grown.length() + ":" + grown.nextSetBit(64) + ":" + grown.previousSetBit(140))

  val counts = new BitSet()
  counts.set(1)
  counts.set(2)
  counts.set(65)
  counts.flip(2)
  println("cardinality:" + counts.cardinality() + ":" + counts.toLongArray().length + ":" + counts.toByteArray().length)
