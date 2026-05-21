#!/usr/bin/env bash
# Authoritative compile sweep for bench-py sources.
# Compiles every bench .scala with bin/spc to a per-file output dir,
# continuing past failures, and reports PASS (bundle produced) / FAIL.
# Usage: bench-py/sweep_compile.sh [category-substring-filter]
set -u
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." >/dev/null && pwd)"
cd "$ROOT" || exit 1
FILTER="${1:-}"
SRCROOT="bench-py/src/main/scala/dotty/tools/benchmarks/py"
OUT="/tmp/benchsweep"
LOGS="$OUT/logs"
rm -rf "$OUT"; mkdir -p "$LOGS"

pass=0; fail=0
fails=()
while IFS= read -r src; do
  rel="${src#$SRCROOT/}"            # e.g. mathalg/SieveBench.scala
  cat="${rel%%/*}"                  # mathalg
  name="$(basename "$src" .scala)"  # SieveBench
  [ -n "$FILTER" ] && [[ "$cat" != *"$FILTER"* ]] && continue
  outdir="$OUT/$cat-$name"
  mkdir -p "$outdir"
  log="$LOGS/$cat-$name.log"
  ./bin/spc -d "$outdir" "$src" >"$log" 2>&1
  rc=$?
  if [ -f "$outdir/$name.py" ]; then
    pass=$((pass+1))
    printf 'PASS  %-32s (rc=%s)\n' "$cat.$name" "$rc"
  else
    fail=$((fail+1))
    fails+=("$cat.$name")
    printf 'FAIL  %-32s (rc=%s) log=%s\n' "$cat.$name" "$rc" "$log"
  fi
done < <(find "$SRCROOT" -name '*.scala' | sort)

echo "============================================================"
echo "compiled: $pass   failed: $fail"
if [ "${#fails[@]}" -gt 0 ]; then
  echo "FAILURES:"
  for f in "${fails[@]}"; do echo "  - $f"; done
fi
