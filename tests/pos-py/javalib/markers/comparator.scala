final class MarkerComparator extends java.util.Comparator[Int]:
  def compare(o1: Int, o2: Int): Int =
    if o1 < o2 then -1
    else if o1 > o2 then 1
    else 0

@main def markersComparator(): Unit =
  println("comparator:" + (new MarkerComparator).compare(5, 3))
