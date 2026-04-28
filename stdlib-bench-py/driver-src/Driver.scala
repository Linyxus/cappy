package dotty.tools.benchmarks.py

import java.io.File
import scala.sys.process.*

/** Compile each bench source through `bin/scpyc`, then invoke the
 *  pyperf shim once per bundle. pyperf handles calibration, multi-process
 *  fan-out, warmup, JSON output, and stats. The shim is at
 *  `stdlib-bench-py/python-shim/run_bench.py`; the aggregator that
 *  consolidates results into `notes/benchmark.md` is
 *  `stdlib-bench-py/python-shim/results_to_md.py`.
 *
 *  Configuration env vars (all optional; defaults in [[BenchEnv]] match
 *  the DEV preset documented in `notes/benchmark.md`):
 *
 *    BENCH_PROCESSES   pyperf --processes
 *    BENCH_WARMUPS     pyperf --warmups
 *    BENCH_VALUES      pyperf --values
 *    BENCH_MIN_TIME    pyperf --min-time (seconds, fractional)
 *    BENCH_INNER_LOOPS shim's manual unroll factor (forwarded as env)
 *    BENCH_FILTER      substring match against `Catalog.entries.benchId`
 *    BENCH_QUIET       any non-empty value adds --quiet to pyperf
 *    JMH_RESULTS_JSON  optional path to a JMH `-rf json -rff …` file;
 *                      if set, results_to_md joins ratios into the table
 */
object Driver:

  private final case class BenchEnv(
      processes:  Int,
      warmups:    Int,
      values:     Int,
      minTime:    String,   // string so "0.1" round-trips cleanly
      innerLoops: Int,
      quiet:      Boolean,
      filter:     Option[String],
      jmhJson:    Option[String]
  )

  private def loadEnv(): BenchEnv =
    BenchEnv(
      processes  = sys.env.getOrElse("BENCH_PROCESSES",   "1").toInt,
      warmups    = sys.env.getOrElse("BENCH_WARMUPS",     "3").toInt,
      values     = sys.env.getOrElse("BENCH_VALUES",      "5").toInt,
      minTime    = sys.env.getOrElse("BENCH_MIN_TIME",    "1"),
      innerLoops = sys.env.getOrElse("BENCH_INNER_LOOPS", "10").toInt,
      quiet      = sys.env.get("BENCH_QUIET").exists(_.nonEmpty),
      filter     = sys.env.get("BENCH_FILTER"),
      jmhJson    = sys.env.get("JMH_RESULTS_JSON"),
    )

  def main(args: Array[String]): Unit =
    val repoRoot =
      if args.isEmpty then new File(".").getCanonicalFile
      else new File(args(0)).getCanonicalFile
    val env = loadEnv()

    val bundlesDir = new File(repoRoot, "stdlib-bench-py/target/py-bundles")
    bundlesDir.mkdirs()
    val resultsDir = new File(repoRoot, "stdlib-bench-py/target/pyperf-results")
    resultsDir.mkdirs()
    val resultsJson = new File(resultsDir, "all.json")
    if resultsJson.isFile then resultsJson.delete()

    val scpyc   = new File(repoRoot, "bin/scpyc")
    val shim    = new File(repoRoot, "stdlib-bench-py/python-shim/run_bench.py")
    val aggregator = new File(repoRoot, "stdlib-bench-py/python-shim/results_to_md.py")

    val selected = env.filter match
      case Some(pat) => Catalog.entries.filter(_.benchId.contains(pat))
      case None      => Catalog.entries
    if selected.isEmpty then sys.error(s"no benches match filter '${env.filter.getOrElse("")}'")

    println(s"[config] processes=${env.processes} warmups=${env.warmups} " +
            s"values=${env.values} min-time=${env.minTime}s " +
            s"inner-loops=${env.innerLoops}")

    // 1. Compile each bench source. The Python-backend linker can report
    //    "Unresolved class" with rc != 0 while still emitting a runnable
    //    bundle (per notes/python-backend-adhoc-audit.md finding #8); we
    //    treat bundle presence as the success signal.
    for entry <- selected do
      val src      = new File(repoRoot, entry.source)
      val benchOut = new File(bundlesDir, entry.benchId.replace('.', '-'))
      benchOut.mkdirs()
      val bundle   = new File(benchOut, entry.bundleName)
      if bundle.isFile then bundle.delete()
      println(s"[compile] ${entry.benchId}")
      val rc = Process(
        Seq(scpyc.getAbsolutePath, "-d", benchOut.getAbsolutePath,
            src.getAbsolutePath),
        repoRoot
      ).!
      if !bundle.isFile then
        sys.error(s"compile produced no bundle for ${entry.benchId} (rc=$rc, expected $bundle)")
      if rc != 0 then
        println(s"[compile] ${entry.benchId}: rc=$rc but bundle present (soft-link warning)")

    // 2. Per bundle, invoke the pyperf shim once and let it fan out across
    //    (op, size). All results append into the single JSON suite that the
    //    aggregator reads.
    for entry <- selected do
      val benchOut = new File(bundlesDir, entry.benchId.replace('.', '-'))
      val bundle   = new File(benchOut, entry.bundleName)
      if !bundle.isFile then sys.error(s"bundle missing: $bundle")
      val qual = scalaQualName(entry, bundle)
      println(s"[bench] ${entry.benchId} (sizes=${entry.sizes.mkString(",")} ops=${entry.ops.mkString(",")})")
      val pyperfArgs = Seq(
        "--processes", env.processes.toString,
        "--warmups",   env.warmups.toString,
        "--values",    env.values.toString,
        "--min-time",  env.minTime,
        "--append",    resultsJson.getAbsolutePath,
      ) ++ (if env.quiet then Seq("--quiet") else Seq.empty)
      val cmd = Seq(
        "uv", "run", "--project", repoRoot.getAbsolutePath, "--no-sync",
        "python", "-W", "ignore", shim.getAbsolutePath
      ) ++ pyperfArgs
      val benchEnv = Map(
        "BENCH_BUNDLE"      -> bundle.getAbsolutePath,
        "BENCH_QUAL"        -> qual,
        "BENCH_SIZES"       -> entry.sizes.mkString(","),
        "BENCH_OPS"         -> entry.ops.mkString(","),
        "BENCH_INNER_LOOPS" -> env.innerLoops.toString,
      )
      val rc = Process(cmd, repoRoot, benchEnv.toSeq*).!
      if rc != 0 then sys.error(s"pyperf run failed (rc=$rc): ${cmd.mkString(" ")}")

    // 3. Hand off to the aggregator. Markdown table + results.jsonl land
    //    in the repo root if the JMH JSON is also available.
    val aggArgs = Seq(
      "uv", "run", "--project", repoRoot.getAbsolutePath, "--no-sync",
      "python", aggregator.getAbsolutePath,
      "--pyperf-json", resultsJson.getAbsolutePath,
      "--out-md",      new File(repoRoot, "notes/benchmark.md").getAbsolutePath,
      "--out-jsonl",   new File(repoRoot, "results.jsonl").getAbsolutePath,
    ) ++ env.jmhJson.toSeq.flatMap(p => Seq("--jmh-json", p))
    println(s"[aggregate] ${aggArgs.takeRight(6).mkString(" ")}")
    val aggRc = Process(aggArgs, repoRoot).!
    if aggRc != 0 then sys.error(s"aggregator failed (rc=$aggRc)")

  /** scpyc emits the Python class name from `<package>.<bench-class>`. We
   *  reconstruct the Scala-qualified name from the catalog entry; the
   *  package is fixed by the bench-py source layout. */
  private def scalaQualName(entry: Catalog.Entry, bundle: File): String =
    val parts = entry.benchId.split("\\.").toList   // e.g. "mutable" :: "ArrayBufferBench" :: Nil
    s"dotty.tools.benchmarks.py.${parts.mkString(".")}"
