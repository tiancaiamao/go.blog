#!/bin/bash
# md baseline generator: render each listed post with cora's md4c
# (process-file) and write the sxml->xml output to
# src-ta/test/baseline/md/<name>.html. Byte-identical reference for
# the ta md module (spec A2).
#
# POSTS    — content/ posts (the delivered baseline set md_test.ta asserts
#            against): code blocks + language class, task lists, tables,
#            images, nested blockquotes, and ordered lists.
# FIXTURES — small hand-written inputs living next to their baselines, for
#            constructs no real post contains (e.g. ordered lists whose
#            start != 1, rendered with a start attribute).
set -e
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR/../../.."   # go.blog root

CORA_BIN="${CORA_BIN:-/Users/genius/project/cora/cora}"
OUTDIR=src-ta/test/baseline/md
mkdir -p "$OUTDIR"

POSTS=(
  200-lines-gc.md
  2018.md
  2022.md
  7900xtx-local-ai.md
  chibi-scheme1.md
  klambda.md
  raft-2F1A.md
)
FIXTURES=(
  ol-start0.md
  ol-start2.md
)

render() {  # $1 = input path, $2 = basename for the progress line
  local in="$1" name="$2" out
  out="$OUTDIR/$name.html"
  tmp=$(mktemp /tmp/md_baseline.XXXXXX.cora)
  trap 'rm -f "$tmp"' EXIT
  sed -e "s|__IN__|$in|g" -e "s|__OUT__|$out|g" \
      "$SCRIPT_DIR/gen_md_baseline.cora" > "$tmp"
  "$CORA_BIN" "$tmp" >/dev/null
  rm -f "$tmp"
  echo "  $name -> $out"
}

for name in "${POSTS[@]}"; do
  render "./content/$name" "$name"
done
for name in "${FIXTURES[@]}"; do
  render "./$OUTDIR/$name" "$name"
done
echo "md baseline done"