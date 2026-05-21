package dotty.tools.benchmarks.py.hasheq

/** Sort a `Vector` of records by a multi-key `given Ordering` and by single
 *  derived keys. Drives Ordering virtual dispatch + per-pair field reads. */
case class Employee(dept: Int, salary: Int, name: String)

given Ordering[Employee] with
  def compare(a: Employee, b: Employee): Int =
    val d = Integer.compare(a.dept, b.dept)
    if d != 0 then d
    else
      val s = Integer.compare(b.salary, a.salary) // salary descending
      if s != 0 then s
      else a.name.compareTo(b.name)

class SortByOrderingBench:
  var size: Int = 0
  var employees: Vector[Employee] = Vector.empty

  def setup(size: Int): Unit =
    this.size = size
    employees = (0 until size).map(i => Employee(i % 8, (size - i) * 100, s"emp$i")).toVector

  val operations: Map[String, () => Any] = Map(
    "sortMultiKey" -> (() => employees.sorted),
    "sortByDept"   -> (() => employees.sortBy(_.dept)),
    "minByDept"    -> (() => employees.minBy(_.dept)),
    "maxBySalary"  -> (() => employees.maxBy(_.salary)),
  )

@main def main(): Unit = ()
