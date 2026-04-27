#!/usr/bin/env bash
# Verify that every *.el file in the project is already formatted.
#
# Usage: tools/format-check.sh [files...]
#
# Copies the project (or the listed files) into a scratch directory, runs the
# formatter, and diffs against the originals.  Exits non-zero on any diff.

set -euo pipefail

root="${LEDGER_MODE_ROOT:-$(git rev-parse --show-toplevel 2>/dev/null || pwd)}"
emacs="${EMACS:-emacs}"

if [ "$#" -eq 0 ]; then
  mapfile -t files < <(find "$root" \
    -path "$root/.git" -prune -o \
    -path "$root/result" -prune -o \
    -path "$root/bench" -prune -o \
    -name '*.el' -print)
else
  files=("$@")
fi

scratch="$(mktemp -d)"
trap 'rm -rf "$scratch"' EXIT

# Stage copies of each file and remember the original path.
declare -a stage_paths=()
declare -a orig_paths=()
for f in "${files[@]}"; do
  rel="${f#"$root"/}"
  dest="$scratch/$rel"
  mkdir -p "$(dirname "$dest")"
  cp "$f" "$dest"
  stage_paths+=("$dest")
  orig_paths+=("$f")
done

LEDGER_MODE_ROOT="$scratch" "$emacs" -Q --batch \
  --load "$root/tools/format.el" "${stage_paths[@]}"

failed=0
for i in "${!stage_paths[@]}"; do
  if ! diff -q "${orig_paths[$i]}" "${stage_paths[$i]}" >/dev/null; then
    echo "::error file=${orig_paths[$i]}::not formatted; run 'nix run .#format'"
    diff -u "${orig_paths[$i]}" "${stage_paths[$i]}" || true
    failed=1
  fi
done
exit "$failed"
