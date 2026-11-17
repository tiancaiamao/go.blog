#!/bin/bash
# tools/build-dsl.sh — compile the go.blog template DSL + a TA entry into
# one self-contained KL image.
#
# Usage: tools/build-dsl.sh [entry.ta] [out.kl]
#   entry.ta  default: src-ta/gen.ta
#   out.kl    default: build/blog-gen.kl
#
# Environment overrides (used by the DSL feature test):
#   DSL_SRC   template DSL source (default dsl/templates.dsl)
#   STUB_OUT  generated TA stub path (default src-ta/lib/template.ta)
#   TPL_KL    generated template KL path (default build/templates.kl)
#
# Steps:
#   1. build the DSL compiler (TA) to build/dslc.tabc
#   2. run it on dsl/templates.dsl -> build/templates.kl + TA external stubs
#      (the stub replaces src-ta/lib/template.ta)
#   3. emit KL for the TA entry (templates are external there)
#   4. concatenate: templates first (define-before-use), entry last
#
# `TINYACTOR` overrides the CLI, otherwise the vendored pinned toolchain is
# used. The resulting .kl can be run with `tinyactor run <out.kl> <args...>`.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$SCRIPT_DIR"

TINYACTOR="${TINYACTOR:-$SCRIPT_DIR/vendor/tinyactor/tinyactor}"
if [ ! -x "$TINYACTOR" ]; then
    echo "Error: TinyActor CLI not found: $TINYACTOR" >&2
    exit 1
fi

ENTRY="${1:-src-ta/gen.ta}"
OUT="${2:-build/blog-gen.kl}"
DSL_SRC="${DSL_SRC:-dsl/templates.dsl}"
STUB_OUT="${STUB_OUT:-src-ta/lib/template.ta}"
TPL_KL="${TPL_KL:-build/templates.kl}"

mkdir -p build
COMPILER=build/dslc.tabc
ENTRY_KL=build/entry.kl

"$TINYACTOR" build dsl/main.ta "$COMPILER"
"$TINYACTOR" run "$COMPILER" "$DSL_SRC" "$TPL_KL" "$STUB_OUT"
"$TINYACTOR" build --emit-kl "$ENTRY" "$ENTRY_KL"
cat "$TPL_KL" "$ENTRY_KL" > "$OUT"

echo "built $OUT ($(wc -l < "$OUT") lines)" >&2