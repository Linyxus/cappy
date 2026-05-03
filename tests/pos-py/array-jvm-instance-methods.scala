// Wave 6 item 01a — JVM-shape instance methods on `_scpy_Array`.
// `arr.toString` / `arr.hashCode` / `arr.equals(_)` go through the
// post-erasure `Object`/`AnyRef` surface and were missing from the
// `_scpy_Array(list)` runtime wrapper. This fixture exercises all
// three methods on both a primitive array (`Array[Int]`) and a
// reference array (`Array[String]`) and validates the JVM contract:
//
//   - `toString` produces a string of shape `<descriptor>@<hex>` where
//     descriptor is the Java-style array descriptor (`[I`, `[Ljava.lang.String;`).
//     The trailing `@<hex>` is identity-hash-dependent, so we slice it
//     off and only assert on the descriptor prefix.
//   - `hashCode` returns a signed 32-bit Int. We assert the bound:
//     `Int.MinValue <= hash <= Int.MaxValue`.
//   - `equals` is identity equality: an array is `.equals` to itself
//     but not to a structurally-equal copy.
//
// The receiver static type stays `Array[T]` (no `(arr: AnyRef)` upcast)
// so the call lowers to a direct `arr.toString__Ljava_dlang_dString()`
// against `_scpy_Array`. `(arr: AnyRef).hashCode` lowers through
// `_scpy_any_hash_code` instead, which has its own dispatch table —
// that's a separate concern from "the methods exist on _scpy_Array".
@main def Test: Unit =
  val ints: Array[Int]      = Array(1, 2, 3)
  val intsAlias: Array[Int] = ints
  val intsCopy: Array[Int]  = Array(1, 2, 3)
  val strs: Array[String]   = Array("a", "b")
  val strsAlias: Array[String] = strs
  val strsCopy: Array[String] = Array("a", "b")

  // toString prefix only (drop the `@<hex>` tail).
  def descriptor(s: String): String =
    val at = s.indexOf("@")
    if at >= 0 then s.substring(0, at) else s

  println(descriptor(ints.toString))
  println(descriptor(strs.toString))

  // hashCode within signed 32-bit range.
  val ih = ints.hashCode
  val sh = strs.hashCode
  println(ih >= Int.MinValue && ih <= Int.MaxValue)
  println(sh >= Int.MinValue && sh <= Int.MaxValue)

  // equals is identity: same reference yes, structural copy no.
  println(ints.equals(intsAlias))
  println(ints.equals(intsCopy))
  println(strs.equals(strsAlias))
  println(strs.equals(strsCopy))
