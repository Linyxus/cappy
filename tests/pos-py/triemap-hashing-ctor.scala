// Exercises `scala.collection.concurrent.TrieMap`'s `(Hashing, Equiv)`
// constructor. The Python backend's TrieMap is a single-threaded shim
// over `mutable.HashMap`; the custom Hashing/Equiv arguments are
// accepted for link-time compatibility but not consulted at lookup
// time (see TrieMap.scala's class doc). This fixture verifies that
// the constructor LINKS and that basic put/get works under default
// hashing semantics.

import scala.collection.concurrent
import scala.util.hashing.Hashing

@main def triemapHashingCtor(): Unit =
  val tm = new concurrent.TrieMap[String, String](
    Hashing.fromFunction(x => x.length),
    Equiv.universal
  )
  tm.put("a", "alpha")
  tm.put("bb", "bravo")
  tm.put("ccc", "charlie")

  // Default `==` / `##` semantics — distinct keys preserved.
  println(tm.size)            // 3
  println(tm("a"))            // alpha
  println(tm("bb"))           // bravo
  println(tm("ccc"))          // charlie

  // Iteration through the wrapped HashMap.
  val keys = tm.keysIterator.toList.sorted
  println(keys.mkString(","))  // a,bb,ccc
