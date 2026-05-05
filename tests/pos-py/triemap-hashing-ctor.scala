// Exercises `scala.collection.concurrent.TrieMap`'s `(Hashing, Equiv)`
// constructor and the per-key wrapper that routes hashCode/equals
// through the user-supplied functions. The Python backend's TrieMap
// is a single-threaded shim over `mutable.HashMap`, but it boxes
// every key in a `KeyBox` so custom Hashing/Equiv is HONOURED at
// lookup time — the same contract upstream `TrieMap` provides.

import scala.collection.concurrent
import scala.util.hashing.Hashing

@main def triemapHashingCtor(): Unit =
  // Default no-arg ctor uses `Hashing.default` / `Equiv.universal`,
  // which round-trip through `##` / `==` — basic put/get works.
  val tmDefault = new concurrent.TrieMap[String, String]
  tmDefault.put("a", "alpha")
  tmDefault.put("bb", "bravo")
  println(tmDefault.size)              // 2
  println(tmDefault("a"))               // alpha
  println(tmDefault("bb"))              // bravo

  // Custom Hashing only — `Equiv.universal` keeps standard `==`.
  // Distinct keys with the same custom hash collide in the same
  // bucket but stay distinct under equality.
  val tmH = new concurrent.TrieMap[String, String](
    Hashing.fromFunction(x => x.length),
    Equiv.universal
  )
  tmH.put("a", "alpha")
  tmH.put("b", "bravo")                  // same hash as "a", distinct key
  tmH.put("aa", "long-a")
  println(tmH.size)                      // 3
  println(tmH("a"))                      // alpha
  println(tmH("b"))                      // bravo
  println(tmH("aa"))                     // long-a

  // Custom Equiv: two strings are equal iff their first chars match.
  // Putting "a" then "a1" must overwrite — first chars both 'a'.
  // This is the failing case from `tests/run/triemap-hash.scala`.
  val tmE = new concurrent.TrieMap[String, String](
    Hashing.fromFunction(x => x(0).toInt),
    Equiv.fromFunction(_(0) == _(0))
  )
  tmE.put("a",  "first")
  tmE.put("a1", "second")                // first-char-equal to "a" → overwrite
  tmE.put("b",  "bee")
  println(tmE.size)                      // 2
  println(tmE("a"))                      // second
  println(tmE("ab"))                     // second  (lookup with different but first-char-equal key)
  println(tmE("b"))                      // bee
