package dotty.tools.benchmarks.py.hasheq

/** `groupBy` / `groupMap` / `groupMapReduce` on records keyed by a String
 *  field. The String key drives `h * 31 + char` hashing on every insert. */
case class Event(kind: String, value: Int)

class GroupByRecordBench:
  var size: Int = 0
  var events: Vector[Event] = Vector.empty
  private val kinds = Vector("click", "view", "scroll", "hover")

  def setup(size: Int): Unit =
    this.size = size
    events = (0 until size).map(i => Event(kinds(i % 4), i)).toVector

  val operations: Map[String, () => Any] = Map(
    "groupByKind"     -> (() => events.groupBy(_.kind)),
    "groupByKindSize" -> (() => events.groupBy(_.kind).view.mapValues(_.size).toMap),
    "groupMap"        -> (() => events.groupMap(_.kind)(_.value)),
    "groupMapReduce"  -> (() => events.groupMapReduce(_.kind)(_.value)(_ + _)),
  )

@main def main(): Unit = ()
