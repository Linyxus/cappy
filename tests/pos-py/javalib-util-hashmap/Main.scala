import java.util.HashMap

final class CollisionKey(val id: Int):
  override def equals(other: Any): Boolean =
    other match
      case that: CollisionKey => id == that.id
      case _                  => false

  override def hashCode(): Int =
    7

@main def javalibUtilHashMap(): Unit =
  val basic = new HashMap[String, String]()
  basic.put("a", "one")
  basic.put("b", "two")
  println("basic:" + basic.get("a") + ":" + basic.get("b") + ":" + basic.size())
  println("contains:" + basic.containsKey("a") + ":" + basic.containsValue("two") + ":" + basic.containsKey("missing"))

  val existing = basic.putIfAbsent("a", "ignored")
  val inserted = basic.putIfAbsent("c", "three")
  println(
    "putIfAbsent:" +
      String.valueOf(existing) + ":" +
      String.valueOf(inserted) + ":" +
      String.valueOf(basic.get("c"))
  )

  basic.computeIfPresent(
    "a",
    (_: String, value: String) => value + "!"
  )
  basic.computeIfAbsent(
    "d",
    (_: String) => "four"
  )
  basic.merge(
    "b",
    "+",
    (left: String, right: String) => left + right
  )
  println("compute-merge:" + basic.get("a") + ":" + basic.get("b") + ":" + basic.get("d"))

  basic.put(null.asInstanceOf[String], "null-key")
  basic.put("nullv", null.asInstanceOf[String])
  println(
    "nulls:" +
      String.valueOf(basic.get(null)) + ":" +
      basic.containsKey(null) + ":" +
      basic.containsKey("nullv") + ":" +
      (basic.get("nullv") == null)
  )

  val collision = new HashMap[CollisionKey, String]()
  val left = new CollisionKey(1)
  val right = new CollisionKey(2)
  collision.put(left, "left")
  collision.put(right, "right")
  println("collision:" + collision.get(left) + ":" + collision.get(right) + ":" + collision.remove(left) + ":" + collision.get(right))
