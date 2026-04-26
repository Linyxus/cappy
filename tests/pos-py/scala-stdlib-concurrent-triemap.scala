// Broad coverage of scala.collection.concurrent.TrieMap (single-threaded).
//
// Under -scalapy, `scala.collection.concurrent.TrieMap` is overridden in
// `library-py/` to delegate to `mutable.HashMap` because the upstream
// implementation depends on Java helper classes (`INodeBase`, `MainNode`,
// `CNodeBase`, `BasicNode`) and `AtomicReferenceFieldUpdater`, none of
// which are ported under `pylib-py/`. The Python backend is single-threaded,
// so plain map delegation is correct for every concurrent-map operation.

import scala.collection.concurrent.TrieMap

@main def scalaStdlibConcurrentTrieMap(): Unit =
  // 1. construction (varargs + empty + from)
  val m = TrieMap("a" -> 1, "b" -> 2, "c" -> 3)
  val e = TrieMap.empty[String, Int]
  val fromList = TrieMap.from(List("x" -> 10, "y" -> 20))
  println("build:" + m.size + ":" + e.size + ":" + fromList.size)

  // 2. accessors (apply / get / getOrElse / contains)
  println("access:" + m("a") + ":" + m.get("b") + ":" + m.getOrElse("z", -1) + ":" + m.contains("c"))

  // 3. transform (sorted keys / values / entries — sorted for determinism)
  println("transform:" + m.keys.toList.sorted.mkString(",") + "|" +
    m.values.toList.sorted.mkString(",") + "|" +
    m.toList.sortBy(_._1).mkString(","))

  // 4. higher-order (filter / exists / count / find by key)
  println("higherOrder:" +
    m.filter((_, v) => v > 1).size + ":" +
    m.exists((_, v) => v == 1) + ":" +
    m.count((_, v) => v > 0) + ":" +
    m.find((_, v) => v == 2).map(_._1))

  // 5. aggregate (foldLeft over values / values.sum)
  println("aggregate:" + m.foldLeft(0)((acc, kv) => acc + kv._2) + ":" + m.values.sum)

  // 6. mutate (put / update / += / remove / -=)
  val mut = TrieMap("a" -> 1, "b" -> 2)
  mut.put("c", 3)
  mut.update("d", 4)
  mut += ("e" -> 5)
  val removed = mut.remove("a")
  mut -= "b"
  println("mutate:" + mut.size + ":" + removed + ":" + mut.contains("a") + ":" + mut.contains("b"))

  // 7. concurrent ops (putIfAbsent / replace / remove(k,v) / replace(k, old, new))
  val c = TrieMap("k" -> 1)
  val absent1 = c.putIfAbsent("k", 99)  // should NOT replace; returns Some(1)
  val absent2 = c.putIfAbsent("z", 7)   // inserts; returns None
  val rep1 = c.replace("k", 5)          // replace if present; returns Some(1)
  val rep2 = c.replace("k", 5, 50)      // CAS-style: replace if value matches
  val rep3 = c.replace("k", 999, 1000)  // mismatch; returns false
  val rem1 = c.remove("z", 7)           // value matches; removes
  val rem2 = c.remove("k", 999)         // value mismatch; doesn't remove
  println("concurrentOps:" + absent1 + ":" + absent2 + ":" + rep1 + ":" + rep2 + ":" + rep3 + ":" + rem1 + ":" + rem2 + ":" + c.size)

  // 8. convert (toList / toMap / toSeq sizes — sorted-list representation for determinism)
  val cv = TrieMap("a" -> 1, "b" -> 2, "c" -> 3)
  println("convert:" + cv.toList.size + ":" + cv.toMap.size + ":" + cv.toSeq.size)
