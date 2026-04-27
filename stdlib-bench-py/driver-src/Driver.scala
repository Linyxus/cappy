package dotty.tools.benchmarks.py

import java.io.File
import scala.collection.mutable
import scala.sys.process.*

object Driver:

  final case class Sample(elapsedNs: Long, k: Int)

  def main(args: Array[String]): Unit =
    val repoRoot =
      if args.isEmpty then new File(".").getCanonicalFile
      else new File(args(0)).getCanonicalFile
    val forks   = sys.env.getOrElse("BENCH_FORKS",   "3").toInt
    val warmup  = sys.env.getOrElse("BENCH_WARMUP",  "5").toInt
    val measure = sys.env.getOrElse("BENCH_MEASURE", "5").toInt
    val filter  = sys.env.get("BENCH_FILTER")

    val outDir = new File(repoRoot, "stdlib-bench-py/target/py-bundles")
    outDir.mkdirs()
    val scpyc   = new File(repoRoot, "bin/scpyc")
    val harness = new File(repoRoot,
      "stdlib-bench-py/src/main/scala/dotty/tools/benchmarks/py/Harness.scala")

    val selected = filter match
      case Some(pat) => Catalog.entries.filter(_.benchId.contains(pat))
      case None      => Catalog.entries

    if selected.isEmpty then sys.error(s"no benches match filter '${filter.getOrElse("")}'")

    // 1. Compile each bench bundle. The Python-backend linker does soft
    //    validation (per notes/python-backend-adhoc-audit.md finding #8):
    //    "Unresolved class" can be reported with non-zero exit while the
    //    bundle is still emitted and runnable. Treat bundle presence — not
    //    the compiler exit code — as the success signal.
    for entry <- selected do
      val src       = new File(repoRoot, entry.source)
      val benchOut  = new File(outDir, entry.benchId.replace('.', '-'))
      benchOut.mkdirs()
      val bundle    = new File(benchOut, entry.bundleName)
      if bundle.isFile then bundle.delete()
      println(s"[compile] ${entry.benchId}")
      val rc = Process(
        Seq(scpyc.getAbsolutePath, "-d", benchOut.getAbsolutePath,
            harness.getAbsolutePath, src.getAbsolutePath),
        repoRoot
      ).!
      if !bundle.isFile then
        sys.error(s"compile produced no bundle for ${entry.benchId} (rc=$rc, expected $bundle)")
      if rc != 0 then
        println(s"[compile] ${entry.benchId}: rc=$rc but bundle present (soft-link warning)")

    // 2. Calibrate K once per (bench, op, size), then run `forks` measurement
    //    subprocesses, each yielding `measure` samples.
    val results = mutable.LinkedHashMap.empty[(String, String, Int), List[Sample]]
    for entry <- selected do
      val benchOut = new File(outDir, entry.benchId.replace('.', '-'))
      val bundle   = new File(benchOut, entry.bundleName)
      if !bundle.isFile then sys.error(s"bundle missing: $bundle")
      for op <- entry.ops; size <- entry.sizes do
        print(s"[calibrate] ${entry.benchId}.$op@$size … ")
        val k = calibrateK(repoRoot, bundle, op, size)
        println(s"k=$k")
        var samples = List.empty[Sample]
        var f = 0
        while f < forks do
          print(s"[measure ${f + 1}/$forks] ${entry.benchId}.$op@$size … ")
          val ns = runMeasure(repoRoot, bundle, op, size, k, warmup, measure)
          println(s"${ns.size} samples")
          samples = samples ++ ns.map(t => Sample(t, k))
          f += 1
        results((entry.benchId, op, size)) = samples

    printTable(results.toMap)

  private def calibrateK(repoRoot: File, bundle: File, op: String, size: Int): Int =
    val out = uvRun(repoRoot, bundle, Seq(op, size.toString, "calibrate"))
    out.linesIterator.collectFirst {
      case s if s.startsWith("K ") => s.split(" ").last.toInt
    }.getOrElse(sys.error(s"calibrate gave no K line:\n$out"))

  private def runMeasure(
      repoRoot: File, bundle: File, op: String, size: Int, k: Int,
      warmup:   Int, meas: Int
  ): List[Long] =
    val out = uvRun(repoRoot, bundle,
      Seq(op, size.toString, "measure", k.toString, warmup.toString, meas.toString))
    out.linesIterator.collect {
      case s if s.startsWith("M ") => s.split(" ").last.toLong
    }.toList

  private def uvRun(repoRoot: File, bundle: File, args: Seq[String]): String =
    val cmd = Seq("uv", "run", "--project", repoRoot.getAbsolutePath, "--no-sync",
                  "python", "-W", "ignore", bundle.getAbsolutePath) ++ args
    val buf = new StringBuilder
    val rc  = Process(cmd, repoRoot).!(
      ProcessLogger(l => { buf.append(l); buf.append('\n') },
                    l => { buf.append(l); buf.append('\n') }))
    if rc != 0 then sys.error(s"uv run failed (rc=$rc): ${cmd.mkString(" ")}\n$buf")
    buf.toString

  private def printTable(results: Map[(String, String, Int), List[Sample]]): Unit =
    println()
    println(f"${"Benchmark"}%-60s ${"(size)"}%6s ${"Mode"}%5s ${"Cnt"}%4s ${"Score"}%18s ${"Error"}%16s ${"Units"}%-8s")
    val rows = results.toList.sortBy { case ((b, o, s), _) => (b, o, s) }
    for ((bench, op, size), samples) <- rows do
      val rates = samples.map(s => s.k.toDouble * 1e9 / s.elapsedNs.toDouble)
      val n     = rates.size
      val mean  = rates.sum / n
      val variance =
        if n > 1 then rates.map(x => (x - mean) * (x - mean)).sum / (n - 1)
        else 0.0
      val stddev = math.sqrt(variance)
      val label  = s"$bench.$op"
      println(f"$label%-60s $size%6d ${"thrpt"}%5s $n%4d $mean%18.2f ±$stddev%15.2f ${"ops/s"}%-8s")
