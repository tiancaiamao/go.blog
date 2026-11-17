#!/usr/bin/env bash
# dsl_test.sh — build and run the DSL sugar regression test.

set -euo pipefail
cd "$(dirname "$0")/../.."

TINYACTOR="${TINYACTOR:-$PWD/vendor/tinyactor/tinyactor}"
[ -x "$TINYACTOR" ] || { echo "DSL TEST FAIL: TinyActor CLI not found: $TINYACTOR" >&2; exit 1; }

DSL_SRC=dsl/features.dsl \
STUB_OUT=build/features_stub.ta \
TPL_KL=build/features.templates.kl \
TINYACTOR="$TINYACTOR" \
  ./tools/build-dsl.sh src-ta/test/dsl_test.ta build/dsl-test.kl >&2

"$TINYACTOR" run build/dsl-test.kl