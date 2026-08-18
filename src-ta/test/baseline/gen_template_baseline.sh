#!/bin/bash
# template baseline generator: run the five template.cora constructors with
# the FIXED argument set (see gen_template_baseline.cora's header) and write
# each sxml->xml result to src-ta/test/baseline/template/<name>.html.
# Byte-identical reference for the ta template module.
set -e
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR/../../.."   # go.blog root

CORA_BIN="${CORA_BIN:-/Users/genius/project/cora/cora}"
OUTDIR=src-ta/test/baseline/template
mkdir -p "$OUTDIR"

"$CORA_BIN" "$SCRIPT_DIR/gen_template_baseline.cora" >/dev/null
echo "template baselines written to $OUTDIR"