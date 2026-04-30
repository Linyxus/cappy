// Layer 5.1.d regression guard.
// `Seq.hashCode` is mapped to `__hash__` by `specialMethodNameOf`. When a
// concrete `Seq` impl (`WrappedString`, `List`) doesn't override `hashCode`,
// it inherits from `Seq`. The reachability analyzer's dunder-keep rule used
// to walk only the instantiated class's local methods, so the inherited
// `__hash__` got pruned and `maybeRebindInheritedHash` rebound to the
// identity hash on `_scpy_Object`. Result: same logical sequence got
// different hashes depending on backing type. The fix walks ancestors when
// the local class doesn't define its own dunder.
@main def run(): Unit =
  val sw: Seq[Char]  = "ab"
  val sw2: Seq[Char] = Array('a', 'b').toIndexedSeq
  val sw3            = Seq('a', 'b')
  val sw4            = "ab".toList
  val all            = List(sw, sw2, sw3, sw4)
  for (s1 <- all; s2 <- all)
    assert(s1 == s2,         s"$s1 != $s2")
    assert(s1.## == s2.##,   s"$s1.## != $s2.##")
  println("ok")
