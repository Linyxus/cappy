package dotty.tools.benchmarks.py.dispatch

/** Multiple-trait mixin dispatch: the same receiver is dispatched through
 *  different supertype interfaces. Mixed arrays are bimorphic; the mono array
 *  dispatches via the concrete class. `allThreeMixed` adds a mid-loop
 *  `asInstanceOf` interface downcast. */
trait Hashable:
  def hash: Int

trait Comparable:
  def compareKey: Int

trait Printable:
  def label: String

class Item(val id: Int, val weight: Int) extends Hashable with Comparable with Printable:
  def hash: Int = id * 31 + weight
  def compareKey: Int = weight * 1000 + id
  def label: String = "item-" + id

class Packet(val seq: Int, val sz: Int) extends Hashable with Comparable:
  def hash: Int = seq ^ (sz << 8)
  def compareKey: Int = seq

class InterfaceMixinBench:
  var size: Int = 0
  var hashables: Array[Hashable] = Array.empty
  var comparables: Array[Comparable] = Array.empty
  var items: Array[Item] = Array.empty

  def setup(size: Int): Unit =
    this.size = size
    hashables = Array.tabulate(size) { i =>
      if i % 2 == 0 then Item(i, i % 5) else Packet(i, i % 7)
    }
    comparables = Array.tabulate(size) { i =>
      if i % 2 == 0 then Item(i, i % 5) else Packet(i, i % 7)
    }
    items = Array.tabulate(size)(i => Item(i, i % 5))

  val operations: Map[String, () => Any] = Map(
    "hashMixed" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += hashables(i).hash
        i += 1
      s
    },
    "compareMixed" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += comparables(i).compareKey
        i += 1
      s
    },
    "hashMono" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += items(i).hash
        i += 1
      s
    },
    "allThreeMixed" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        val h = hashables(i)
        s += h.hash + h.asInstanceOf[Comparable].compareKey
        i += 1
      s
    },
  )

@main def main(): Unit = ()
