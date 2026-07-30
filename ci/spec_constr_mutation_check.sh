#!/usr/bin/env bash
# Mutation acceptance test for the SpecConstr correctness guards.
#
# Each patch in ci/spec_constr_mutations/ seeds one representative SpecConstr
# bug into src/postcheck/monotype_lifted/spec_constr.zig, and a named
# lir_inline test must catch it. This script applies each patch, rebuilds the
# in-process lir_inline suite, and requires the suite to FAIL — a mutant that
# survives means the named guard is not actually protecting that behavior.
#
#   m1  resolvePending always pins (effect-windowed delegation never fires):
#       the structural fusion tests (e.g. "issue 10301 for-loop over
#       effect-produced list scalarizes") assert the fused shape — raw list
#       accesses live in the root, no iterator-step proc remains reachable — and
#       fail when nothing fuses.
#   m3  the loop-carried binder-wide install is skipped (#10317): a carried
#       variable reassigned inside the loop resolves to its vanished pre-loop
#       binding, which the Debug capture-gain validator and the #10317
#       regression tests catch.
#
# Why every target is lir_inline (and none is the lambda-mono differential
# runner): that runner lowers every case with inline mode `.none`, which skips
# SpecConstr entirely, so both of its oracles execute a program SpecConstr
# never touched and a SpecConstr bug cannot diverge them. Only a comparison
# against a lowering that ran SpecConstr can see these bugs; lir_inline's
# .none-vs-.wrappers differential (SpecConstr runs only for .wrappers) and its
# single-mode structural/validator assertions are that comparison.
#
# A chain-check mutation (delegating an effectful pending across another effect)
# is deliberately not seeded here: its only observable symptom is reordered
# effects, and no oracle in the suite distinguishes it. The differential runner
# is blind for the reason above, and the .none-vs-.wrappers transcript check
# does not separate it either, because the delegated-pending flush is
# oldest-first and reproduces source order for the shapes the suite exercises.
# The effect-order corpus cases and the expectSameObservationsAcrossInlineModes
# tests still guard effect ordering behaviorally; they just are not sensitive to
# this particular seeded regression.
#
# Recompiling the compiler per mutation is expensive, so this is a manual /
# minici-optional check, not part of the default CI path; run it when
# spec_constr.zig itself changes.
#
# Run from anywhere; operates on the repo containing this script. The working
# tree must be clean enough that `git apply` / `git apply -R` of the patches
# succeeds. If a patch no longer applies after spec_constr.zig changes,
# regenerate it at the site described by its file name.

set -u
cd "$(dirname "$0")/.."

log_dir=$(mktemp -d)
failed=0

for patch in ci/spec_constr_mutations/*.patch; do
    name=$(basename "$patch" .patch)
    echo "=== mutation: $name ==="

    if ! git apply "$patch"; then
        echo "FAILED: $patch no longer applies; regenerate it against the current sources"
        failed=1
        continue
    fi

    run_log="$log_dir/$name.log"
    if zig build run-test-zig-lir-inline >"$run_log" 2>&1; then
        echo "FAILED: MUTANT SURVIVED ($name) — the lir_inline suite passed with the seeded bug"
        failed=1
    else
        echo "caught: $name"
        grep -E "terminated with signal|failed:|tests passed" "$run_log" | head -4
    fi

    git apply -R "$patch"
done

echo
if [ "$failed" -ne 0 ]; then
    echo "mutation check FAILED (logs in $log_dir)"
    exit 1
fi
echo "mutation check passed: all seeded mutations were caught"
