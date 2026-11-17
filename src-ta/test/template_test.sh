#!/usr/bin/env bash
# template_test.sh — run the template DSL acceptance test.
#
# template_test.ta imports the generated external stubs, so it must run as
# one KL image with dsl/templates.dsl's compiled output spliced in front. This
# wrapper does that build, then runs the image.

set -euo pipefail
cd "$(dirname "$0")/../.."

TINYACTOR="${TINYACTOR:-$PWD/vendor/tinyactor/tinyactor}"
[ -x "$TINYACTOR" ] || { echo "TEMPLATE TEST FAIL: TinyActor CLI not found: $TINYACTOR" >&2; exit 1; }

TINYACTOR="$TINYACTOR" ./tools/build-dsl.sh src-ta/test/template_test.ta build/template-test.kl >&2
"$TINYACTOR" run build/template-test.kl