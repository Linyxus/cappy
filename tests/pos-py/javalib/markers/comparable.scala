final class MarkerComparable(val value: Int) extends java.lang.Comparable[MarkerComparable]:
  def compareTo(other: MarkerComparable): Int =
    if value < other.value then -1
    else if value > other.value then 1
    else 0

@main def markersComparable(): Unit =
  println("comparable:" + (new MarkerComparable(5)).compareTo(new MarkerComparable(3)))
