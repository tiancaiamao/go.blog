#!/usr/bin/env bash
# wrapper_test.sh — blog-gen-ta wrapper failure-handling regression tests.
#
# Scope (go.blog fix/blog-generator-errors):
#   1. the single tinyactor run (generate) fails -> wrapper exits nonzero,
#      static sync NOT run
#   2. the single tinyactor run (file) fails -> wrapper exits nonzero,
#      static sync NOT run, and tinyactor is invoked EXACTLY ONCE (the
#      wrapper no longer runs a separate `dirs` preparation pass)
#   3. `file <absent-name>` -> nonzero exit (core.ta co_die signal), no
#      static sync, and the generated tree is left untouched
#
# The wrapper under test is the repo's blog-gen-ta. Tests 1-2 use a stub
# tinyactor (so no real generation happens); test 3 exercises the real
# toolchain and honors the TINYACTOR override, e.g.:
#   TINYACTOR=/path/to/tinyactor src-ta/test/wrapper_test.sh
#
# No git operations are used. The only repo writes come from the wrapper's
# own generation run (gen.ta creates its output directories idempotently
# via file.mkdir_p) and the static sync; test 3 pins that down by
# asserting the generated directory tree is unchanged before/after.
#
# Usage (from anywhere): src-ta/test/wrapper_test.sh

set -euo pipefail
cd "$(dirname "$0")/../.."

ROOT="$PWD"
BLOG_GEN_TA="${BLOG_GEN_TA:-$ROOT/blog-gen-ta}"
if [ -n "${TINYACTOR:-}" ]; then
    TINYACTOR="$TINYACTOR"
elif command -v tinyactor >/dev/null 2>&1; then
    TINYACTOR="$(command -v tinyactor)"
else
    TINYACTOR="$ROOT/../tinyactor/tinyactor"
fi

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

fails=0
ok()   { echo "ok $*"; }
fail() { echo "WRAPPER TEST FAIL: $*" >&2; fails=$((fails + 1)); }

# run_capture: run "$@" capturing combined output into $CAPTURED and the
# exit status into $RC, without letting a nonzero status abort this script.
run_capture() {
    set +e
    CAPTURED="$("$@" 2>&1)"
    RC=$?
    set -e
}

[ -x "$BLOG_GEN_TA" ] || { echo "WRAPPER TEST FAIL: $BLOG_GEN_TA not executable" >&2; exit 1; }

# ---- Test 1: tinyactor run failure (generate) propagates, no static sync ----
cat > "$WORK/stub_fail_generate" <<'EOF'
#!/usr/bin/env bash
echo "stub tinyactor: generation failed as expected" >&2
exit 9
EOF
chmod +x "$WORK/stub_fail_generate"

run_capture env TINYACTOR="$WORK/stub_fail_generate" "$BLOG_GEN_TA" generate
[ "$RC" -eq 9 ] || fail "generate failure: expected exit 9, got $RC (out: $CAPTURED)"
if printf '%s' "$CAPTURED" | grep -q "Syncing static assets"; then
    fail "generate failure: static sync ran despite failure"
fi
ok "tinyactor run failure (generate) -> nonzero exit, static sync skipped"

# ---- Test 2: tinyactor run failure (file) propagates, invoked exactly once ----
cat > "$WORK/stub_fail_file" <<'EOF'
#!/usr/bin/env bash
echo "$*" >> "$CALLS"
echo "stub tinyactor: generation failed as expected" >&2
exit 7
EOF
chmod +x "$WORK/stub_fail_file"

run_capture env TINYACTOR="$WORK/stub_fail_file" CALLS="$WORK/calls.log" \
    "$BLOG_GEN_TA" file some-post.md
[ "$RC" -eq 7 ] || fail "file failure: expected exit 7, got $RC (out: $CAPTURED)"
if printf '%s' "$CAPTURED" | grep -q "Syncing static assets"; then
    fail "file failure: static sync ran despite failure"
fi
if [ "$(wc -l < "$WORK/calls.log")" -ne 1 ]; then
    fail "file failure: expected exactly 1 tinyactor invocation, got: $(cat "$WORK/calls.log")"
else
    grep -qx "run src-ta/gen.ta file some-post.md" "$WORK/calls.log" \
        || fail "file failure: unexpected invocation args: $(cat "$WORK/calls.log")"
fi
ok "tinyactor run failure (file) -> nonzero exit, exactly-once invocation, no static sync"

# ---- Test 3: `file <absent>` fails with the real toolchain ----
[ -x "$TINYACTOR" ] || { echo "WRAPPER TEST FAIL: TINYACTOR not executable: $TINYACTOR" >&2; exit 1; }

find -L generate -mindepth 1 -type d | sort > "$WORK/dirs.before"

run_capture "$BLOG_GEN_TA" file "definitely-not-a-real-post-xyz.md"
[ "$RC" -ne 0 ] || fail "missing file: expected nonzero exit, got 0"
printf '%s' "$CAPTURED" | grep -q "File not found in index" \
    || fail "missing file: expected 'File not found in index' message (out: $CAPTURED)"
if printf '%s' "$CAPTURED" | grep -q "Syncing static assets"; then
    fail "missing file: static sync ran despite missing-file failure"
fi
find -L generate -mindepth 1 -type d | sort > "$WORK/dirs.after"
if ! diff -q "$WORK/dirs.before" "$WORK/dirs.after" >/dev/null; then
    fail "missing file: generated directory tree changed"
fi
ok "file <absent> -> nonzero exit, no static sync, generated tree untouched"

if [ "$fails" -gt 0 ]; then
    echo "WRAPPER TEST FAIL ($fails failure(s))" >&2
    exit 1
fi
echo "WRAPPER TEST PASS"