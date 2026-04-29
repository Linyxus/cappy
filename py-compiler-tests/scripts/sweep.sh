#!/usr/bin/env bash
# Run each PyStockRunTests bucket in a separate sbt invocation so each gets
# a fresh test fork JVM. Avoids the Layer 0.3 cross-bucket state accumulation
# that causes OOM and slowdowns when all 16 @Test methods run in one sbt
# invocation.
#
# Usage: py-compiler-tests/scripts/sweep.sh [LOG_DIR]
#
# LOG_DIR defaults to /tmp/pyrun-analysis. Per-bucket logs land at
# $LOG_DIR/bucket-NN.log; combined log at $LOG_DIR/perbucket-combined.log.
set -u
script_dir=$(cd "$(dirname "$0")" && pwd)
repo_root=$(cd "$script_dir/../.." && pwd)
cd "$repo_root"

log_dir=${1:-/tmp/pyrun-analysis}
mkdir -p "$log_dir"
combined_log="$log_dir/perbucket-combined.log"
: > "$combined_log"

for i in 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15; do
  bucket_log="$log_dir/bucket-$i.log"
  echo "=== bucket $i starting at $(date +%T) ===" | tee -a "$combined_log"
  start=$(date +%s)
  sbt --client "pyCompilerTests/testOnly dotty.tools.dotc.PyStockRunTests -- --tests=runPyTests_$i" > "$bucket_log" 2>&1
  rc=$?
  end=$(date +%s)
  elapsed=$((end - start))
  status="ok"
  [ $rc -ne 0 ] && status="failed(rc=$rc)"
  echo "=== bucket $i $status in ${elapsed}s at $(date +%T) ===" | tee -a "$combined_log"
  cat "$bucket_log" >> "$combined_log"
done

echo "=== sweep done at $(date +%T) ===" | tee -a "$combined_log"
