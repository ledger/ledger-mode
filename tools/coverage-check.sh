#!/usr/bin/env bash
# Compute line-coverage percentage from coverage/lcov.info and fail if it
# regressed against bench/coverage-baseline (a single percentage like "73.4").
#
# Tolerance: a 0.1 percentage-point dip is allowed (rounding noise).

set -euo pipefail

root="${LEDGER_MODE_ROOT:-$(git rev-parse --show-toplevel 2>/dev/null || pwd)}"
lcov_file="${COVERAGE_OUT:-$root/coverage/lcov.info}"
baseline_file="${COVERAGE_BASELINE:-$root/bench/coverage-baseline}"

if [ ! -f "$lcov_file" ]; then
  echo "coverage-check: $lcov_file not found; run 'nix build .#checks.\$system.coverage' first" >&2
  exit 1
fi

# LCOV "DA:" lines are <line>,<count>[,<checksum>].  Some producers (undercover
# included) emit DA: data without the LF/LH summary lines, so compute the
# totals ourselves: LF is the count of DA: lines, LH is the count of those
# whose count field is non-zero.
total_lf=$(grep -c '^DA:' "$lcov_file" || true)
total_lh=$(awk -F'[:,]' '/^DA:/ && $3+0 > 0 { c++ } END { print c+0 }' "$lcov_file")

if [ "$total_lf" -eq 0 ]; then
  echo "coverage-check: no instrumented lines in $lcov_file" >&2
  exit 1
fi

current=$(awk -v lh="$total_lh" -v lf="$total_lf" \
              'BEGIN { printf "%.2f", (lh / lf) * 100 }')
echo "Line coverage: $current% ($total_lh / $total_lf)"

if [ ! -f "$baseline_file" ]; then
  echo "No baseline at $baseline_file; recording current as baseline."
  echo "$current" > "$baseline_file"
  exit 0
fi

baseline=$(<"$baseline_file")
verdict=$(awk -v c="$current" -v b="$baseline" \
              'BEGIN { print (c + 0.1 >= b) ? "ok" : "regressed" }')
case "$verdict" in
  ok)        echo "coverage-check: ok (baseline $baseline%)"; exit 0 ;;
  regressed) echo "coverage-check: dropped from $baseline% to $current%" >&2; exit 1 ;;
esac
