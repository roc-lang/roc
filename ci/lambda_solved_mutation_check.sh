#!/usr/bin/env bash
# Mutation acceptance test for the direct Lambda Solved set-invariant tests.
#
# The Lambda Mono differential harness consumes the same solved program on both
# sides, so it structurally cannot see a bug inside src/postcheck/lambda_solved
# (a mutated set corrupts both sides identically, and set-coarsening is usually
# behavior-preserving). Those bugs are instead caught by the direct tests in
# src/postcheck/lambda_solved/solve.zig's test section (reunify.md section
# 12.6). Each patch in ci/lambda_solved_mutations/ seeds one representative
# solver bug (union member dropped, capture-type unify skipped, callable slot
# sealed to a singleton instead of empty, the iterator-backing erasure
# exemption ignored, occurrence cloning reverted to a completed-graph memo).
# For each mutation this script rebuilds and runs the postcheck module tests and
# requires them to FAIL — a mutant that survives means the direct tests are not
# actually protecting that invariant.
#
# Run from anywhere; operates on the repo containing this script. The working
# tree must be clean enough that `git apply` / `git apply -R` of the patches
# succeeds. If a patch no longer applies after solve.zig changes, regenerate it
# at the site described by its file name.

set -u
cd "$(dirname "$0")/.."

log_dir=$(mktemp -d)
failed=0

for patch in ci/lambda_solved_mutations/*.patch; do
    name=$(basename "$patch" .patch)
    echo "=== mutation: $name ==="

    if ! git apply "$patch"; then
        echo "FAILED: $patch no longer applies; regenerate it against the current sources"
        failed=1
        continue
    fi

    run_log="$log_dir/$name.run.log"

    if zig build run-test-zig-module-postcheck >"$run_log" 2>&1; then
        echo "FAILED: MUTANT SURVIVED ($name) — the direct tests did not catch the seeded bug"
        tail -30 "$run_log"
        failed=1
    else
        echo "caught: $name"
        grep -E "error:|panic|FAIL|expected" "$run_log" | head -3
    fi

    git apply -R "$patch"
done

echo
if [ "$failed" -ne 0 ]; then
    echo "mutation check FAILED (logs in $log_dir)"
    exit 1
fi
echo "mutation check passed: all seeded mutations were caught"
