#!/usr/bin/env bash
# Mutation acceptance test for the Lambda Mono differential harness.
#
# Each patch in ci/lambda_mono_mutations/ seeds one representative
# body-lowering bug into src/postcheck/solved_lir_lower.zig (swapped capture
# pack order, off-by-one callable variant target, reordered match branches,
# off-by-one list-rest length, reversed argument evaluation order). For each
# mutation this script rebuilds the harness, runs the generated sweep corpus,
# and requires the harness to FAIL — a mutant that survives means the harness
# is not actually protecting the direct solved-to-LIR body lowering.
#
# Run from anywhere; operates on the repo containing this script. The working
# tree must be clean enough that `git apply` / `git apply -R` of the patches
# succeeds. If a patch no longer applies after solved_lir_lower.zig changes,
# regenerate it at the site described by its file name.

set -u
cd "$(dirname "$0")/.."

log_dir=$(mktemp -d)
failed=0

for patch in ci/lambda_mono_mutations/*.patch; do
    name=$(basename "$patch" .patch)
    echo "=== mutation: $name ==="

    if ! git apply "$patch"; then
        echo "FAILED: $patch no longer applies; regenerate it against the current sources"
        failed=1
        continue
    fi

    build_log="$log_dir/$name.build.log"
    run_log="$log_dir/$name.run.log"

    if ! zig build build-test-lambda-mono-differential >"$build_log" 2>&1; then
        echo "FAILED: mutated compiler did not build ($name)"
        tail -20 "$build_log"
        git apply -R "$patch"
        failed=1
        continue
    fi

    if zig-out/bin/lambda-mono-differential-runner generated-only fail-fast >"$run_log" 2>&1; then
        echo "FAILED: MUTANT SURVIVED ($name) — the harness did not catch the seeded bug"
        tail -30 "$run_log"
        failed=1
    else
        echo "caught: $name"
        grep -E "DIVERGED|FAILED" "$run_log" | head -3
    fi

    git apply -R "$patch"
done

echo
if [ "$failed" -ne 0 ]; then
    echo "mutation check FAILED (logs in $log_dir)"
    exit 1
fi
echo "mutation check passed: all seeded mutations were caught"
