import java.util.IdentityHashMap

final class KeyBox(val id: Int):
  override def equals(other: Any): Boolean =
    other match
      case that: KeyBox => id == that.id
      case _            => false

  override def hashCode(): Int =
    id

final class ValueBox(val label: String):
  override def equals(other: Any): Boolean =
    other match
      case that: ValueBox => label == that.label
      case _              => false

  override def hashCode(): Int =
    label.hashCode()

@main def javalibUtilIdentityHashMap(): Unit =
  val map = new IdentityHashMap[KeyBox, ValueBox]()
  val leftKey = new KeyBox(1)
  val rightKey = new KeyBox(1)
  val missingKey = new KeyBox(1)

  val leftValue = new ValueBox("left")
  val rightValue = new ValueBox("right")
  val nullValue = new ValueBox("null-slot")

  map.put(leftKey, leftValue)
  map.put(rightKey, rightValue)
  map.put(null.asInstanceOf[KeyBox], nullValue)

  println("size:" + map.size())
  println("lookup:" + map.get(leftKey).label + ":" + map.get(rightKey).label + ":" + map.get(null).label)
  println("identity:" + map.containsKey(leftKey) + ":" + map.containsKey(missingKey) + ":" + map.containsValue(leftValue) + ":" + map.containsValue(new ValueBox("left")))
  println("remove:" + (map.remove(missingKey) == null) + ":" + map.size())
