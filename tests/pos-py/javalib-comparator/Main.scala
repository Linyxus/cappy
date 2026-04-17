import java.util.Comparator
import java.util.function.*

final class TestRank(val value: Int) extends Comparable[TestRank]:
  def compareTo(other: TestRank): Int =
    if value < other.value then -1
    else if value > other.value then 1
    else 0

final class CompareRecord(
  val name: String,
  val rank: TestRank,
  val points: Int,
  val createdAt: Long,
  val ratio: Double
)

@main def javalibComparator(): Unit =
  val rankKey: Function[CompareRecord, TestRank] = record => record.rank
  val nameLengthKey: ToIntFunction[CompareRecord] = record => record.name.length
  val pointsKey: ToIntFunction[CompareRecord] = record => record.points
  val createdAtKey: ToLongFunction[CompareRecord] = record => record.createdAt
  val ratioKey: ToDoubleFunction[CompareRecord] = record => record.ratio
  val descendingRank: Comparator[TestRank] = new Comparator[TestRank]:
    def compare(left: TestRank, right: TestRank): Int =
      right.compareTo(left)
  val byRank: Comparator[CompareRecord] = Comparator.comparing[CompareRecord, TestRank](rankKey)

  val rankLow = new TestRank(1)
  val rankHigh = new TestRank(3)

  val low = new CompareRecord("ax", rankLow, 2, 10L, 1.5)
  val high = new CompareRecord("beta", rankHigh, 9, 30L, 4.5)

  val tieShort = new CompareRecord("a", new TestRank(2), 5, 20L, 1.0)
  val tieLong = new CompareRecord("tool", new TestRank(2), 5, 20L, 1.0)

  val tieIntLow = new CompareRecord("i-low", new TestRank(4), 1, 40L, 2.0)
  val tieIntHigh = new CompareRecord("i-high", new TestRank(4), 8, 40L, 2.0)

  val tieLongLow = new CompareRecord("l-low", new TestRank(5), 3, 100L, 2.0)
  val tieLongHigh = new CompareRecord("l-high", new TestRank(5), 3, 400L, 2.0)

  val tieDoubleLow = new CompareRecord("d-low", new TestRank(6), 3, 100L, 1.25)
  val tieDoubleHigh = new CompareRecord("d-high", new TestRank(6), 3, 100L, 3.75)

  val missing: CompareRecord = null

  println("compare-to:" + rankLow.compareTo(rankHigh))
  println("natural-order:" + Comparator.naturalOrder[TestRank]().compare(rankHigh, rankLow))
  println("reverse-order:" + Comparator.reverseOrder[TestRank]().compare(rankLow, rankHigh))
  println("reversed:" + byRank.reversed().compare(low, high))
  println(
    "then-comparing:" +
      byRank
        .thenComparing(Comparator.comparingInt[CompareRecord](nameLengthKey))
        .compare(tieShort, tieLong)
  )
  println(
    "then-comparing-int:" +
      byRank.thenComparingInt(pointsKey).compare(tieIntLow, tieIntHigh)
  )
  println(
    "then-comparing-long:" +
      byRank.thenComparingLong(createdAtKey).compare(tieLongLow, tieLongHigh)
  )
  println(
    "then-comparing-double:" +
      byRank.thenComparingDouble(ratioKey).compare(tieDoubleLow, tieDoubleHigh)
  )
  println(
    "nulls-first:" +
      Comparator.nullsFirst[CompareRecord](byRank).compare(missing, low) + ":" +
      Comparator.nullsFirst[CompareRecord](byRank).compare(missing, missing)
  )
  println(
    "nulls-last:" +
      Comparator.nullsLast[CompareRecord](byRank).compare(low, missing) + ":" +
      Comparator.nullsLast[CompareRecord](byRank).compare(missing, missing)
  )
  println("comparing:" + Comparator.comparing[CompareRecord, TestRank](rankKey).compare(low, high))
  println(
    "comparing-with:" +
      Comparator.comparing[CompareRecord, TestRank](rankKey, descendingRank)
        .compare(low, high)
  )
  println("comparing-int:" + Comparator.comparingInt[CompareRecord](pointsKey).compare(low, high))
  println("comparing-long:" + Comparator.comparingLong[CompareRecord](createdAtKey).compare(low, high))
  println("comparing-double:" + Comparator.comparingDouble[CompareRecord](ratioKey).compare(low, high))
