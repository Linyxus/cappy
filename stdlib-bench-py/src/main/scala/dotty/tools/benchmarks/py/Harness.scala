package dotty.tools.benchmarks.py

import scala.python.runtime.PyTime

trait BenchmarkSuite:
  def sizes: List[Int] = List(16, 256, 4096)
  def setup(size: Int): Unit
  def operations: Map[String, () => Any]
  var sink: Any = null

object Harness:
  private val DefaultWarmup   = 5
  private val DefaultMeasure  = 5
  private val DefaultTargetNs = 100_000_000L

  def calibrate(op: () => Any, sinkSet: Any => Unit, targetNs: Long = DefaultTargetNs): Int =
    var k = 1
    var done = false
    var result = -1
    while !done do
      val s = PyTime.perf_counter_ns()
      var i = 0
      while i < k do
        sinkSet(op())
        i += 1
      val e = PyTime.perf_counter_ns() - s
      if e >= targetNs then
        result = k
        done = true
      else
        k =
          if e <= 0L then k * 2
          else
            val projected = ((targetNs.toDouble / e.toDouble) * k).toInt + 1
            math.max(k * 2, projected)
    result

  def measure(
      bench:  String,
      op:     String,
      size:   Int,
      k:      Int,
      body:   () => Any,
      sinkSet: Any => Unit,
      warmup: Int = DefaultWarmup,
      meas:   Int = DefaultMeasure
  ): Unit =
    var w = 0
    while w < warmup do
      val s = PyTime.perf_counter_ns()
      var i = 0
      while i < k do
        sinkSet(body())
        i += 1
      val e = PyTime.perf_counter_ns() - s
      println(s"W $bench $op $size $k $e")
      w += 1
    var m = 0
    while m < meas do
      val s = PyTime.perf_counter_ns()
      var i = 0
      while i < k do
        sinkSet(body())
        i += 1
      val e = PyTime.perf_counter_ns() - s
      println(s"M $bench $op $size $k $e")
      m += 1

  def runFromArgs(suite: BenchmarkSuite, benchName: String, args: Array[String]): Unit =
    val op = args(0)
    val size = args(1).toInt
    val mode = args(2)
    suite.setup(size)
    val body = suite.operations.getOrElse(
      op,
      throw new RuntimeException(s"unknown op '$op' in $benchName"))
    val sinkSet: Any => Unit = v => { suite.sink = v }
    mode match
      case "calibrate" =>
        val k = calibrate(body, sinkSet)
        println(s"K $benchName $op $size $k")
      case "measure" =>
        val k      = args(3).toInt
        val warmup = if args.length > 4 then args(4).toInt else DefaultWarmup
        val meas   = if args.length > 5 then args(5).toInt else DefaultMeasure
        measure(benchName, op, size, k, body, sinkSet, warmup, meas)
        val sinkHash = if suite.sink == null then 0 else suite.sink.hashCode & 0x3ff
        println(s"S $benchName $sinkHash")
      case other =>
        throw new RuntimeException(s"unknown mode '$other'")
