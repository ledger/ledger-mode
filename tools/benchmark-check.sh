#!/usr/bin/env bash
# Compare bench/current.txt against bench/baseline.txt.  By default, regressions
# are reported but informational — the same pattern this repo uses for lint and
# checkdoc, since shared CI runners can vary 20-50% on microbenchmarks.  Set
# `LEDGER_MODE_STRICT_BENCH=1` (lefthook does this for local commits) to make
# regressions over `BENCH_THRESHOLD` percent fatal.  If there is no baseline
# yet, copy the current run into place and exit 0 (first-run bootstrap).

set -euo pipefail

root="${LEDGER_MODE_ROOT:-$(git rev-parse --show-toplevel 2>/dev/null || pwd)}"
current="${BENCH_OUT:-$root/bench/current.txt}"
baseline="${BENCH_BASELINE:-$root/bench/baseline.txt}"
threshold="${BENCH_THRESHOLD:-5}"   # percent
strict="${LEDGER_MODE_STRICT_BENCH:-}"

if [ ! -s "$current" ]; then
  echo "benchmark-check: $current is empty or missing" >&2
  exit 1
fi

if [ ! -f "$baseline" ]; then
  echo "benchmark-check: no baseline at $baseline; recording current run."
  cp "$current" "$baseline"
  exit 0
fi

awk -v threshold="$threshold" -v strict="$strict" '
  FNR == NR { base[$1] = $2; next }
  {
    name = $1; cur = $2 + 0
    b = base[name]
    if (b == "" || b + 0 == 0) { next }
    ratio = (cur / b) * 100.0
    delta = ratio - 100.0
    printf("%-20s baseline=%.4fs current=%.4fs (%+.1f%%)\n",
           name, b + 0, cur, delta)
    if (delta > threshold) { fails++ }
  }
  END {
    if (fails > 0) {
      mode = (strict == "" ? "informational" : "fatal")
      printf("benchmark-check: %d benchmark(s) slower than baseline by more than %.1f%% (%s)\n",
             fails, threshold + 0, mode) > "/dev/stderr"
      if (strict != "") exit 1
    }
  }
' "$baseline" "$current"
