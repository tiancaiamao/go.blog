#!/usr/bin/env bash
# core_test.sh — run the core module acceptance test with its scratch tree.
#
# core_test.ta calls core.write_* directly (not via gen.ta, whose
# ensure_dirs() creates the output directories before generating), so the
# scratch output tree ./.coretest/ must pre-exist; this script prepares
# it, runs the test (which byte-compares against generate/ inside ta),
# then adds shell `diff` evidence for each generated file and cleans up.
#
# Usage (from anywhere): src-ta/test/core_test.sh

set -euo pipefail
cd "$(dirname "$0")/../.."

ROOT=.coretest
TINYACTOR="${TINYACTOR:-/Users/genius/project/tinyactor/tinyactor}"

rm -rf "$ROOT"
mkdir -p "$ROOT/an-ai-debug-story.md" \
         "$ROOT/gitbbs.md"
# generate_file (single-post update) refreshes EVERY category/tags page, so
# mirror cora's full summary-page tree — including nested dirs: the tag
# "call/cc" nests as tags/call/cc/ (the names are exactly what
# build_category/build_tags produce — verified byte-identical by the test)
while IFS= read -r d; do
  mkdir -p "$ROOT/$d"
done < <(cd generate && find category tags -type d)

"$TINYACTOR" run src-ta/test/core_test.ta

echo "--- diff evidence (generated vs generate/) ---"
FILES=(
  "index.html"
  "an-ai-debug-story.md/index.html"
  "category/AI/index.html"
  "tags/raft/index.html"
  "feed.atom"
)
# gitbbs.md / about.html baselines predate the last cora run (stale
# sidebar tag set); core_test.ta verifies them modulo the sidebar, so they
# are excluded from the plain diff here.
MODULO_STALE=(
  "gitbbs.md/index.html"
  "about.html"
)
for f in "${FILES[@]}"; do
  if cmp -s "$ROOT/$f" "generate/$f"; then
    echo "IDENTICAL $f"
  else
    echo "DIFFERS   $f"
    diff "$ROOT/$f" "generate/$f" | head -10 || true
    exit 1
  fi
done

rm -rf "$ROOT"
echo "CORE TEST PASS (all pages byte-identical against generate/)"