# CoreCIR Architectural Plan

## Status

Complete.

## Rules Followed

- `zig` was not run at any point while carrying out this plan.
- No fallbacks, heuristics, workarounds, rescue paths, compatibility shims, or transitional stopgaps were introduced.
- Invariant failures were handled with debug panics and `unreachable`, not silent recovery.

## Completed End State

- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/corecir/Pipeline.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/corecir/Pipeline.zig) is orchestration-only.
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/corecir/TemplateCatalog.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/corecir/TemplateCatalog.zig) owns callable-template extraction and root inventory.
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/corecir/ContextMono.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/corecir/ContextMono.zig) owns contextual monotype facts and monotype construction/remapping logic.
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/corecir/DispatchSolved.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/corecir/DispatchSolved.zig) owns normalized dispatch facts.
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/corecir/Lambdasolved.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/corecir/Lambdasolved.zig) owns solved semantic scanning, root analysis, and callable realization.
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/corecir/Lambdamono.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/corecir/Lambdamono.zig) owns executable-program construction.
- `Lambdamono.Expr` is a strict executable tagged union; invalid executable states are unrepresentable in the IR type.
- Assembly consumes already-produced solved facts and no longer launches fresh semantic rescue work from `Pipeline` or `Lambdamono`.
- No direct stage-specific execution helpers remain in `Pipeline`.

## Final Audit Result

The final artifact audit was clean for the tracked architectural remnants:

- no semantic scan/realization engine remains in `Pipeline`
- no fat role-based executable expr overlay remains
- no late scan-then-assemble rescue path remains in `Pipeline`
- no direct mutable cross-stage state mutation remains outside owning stages
- no remaining trace of the old integrated/transitional CoreCIR design remains in the targeted Zig compiler pipeline files

## Next Phase

This plan is finished. The next work, if desired, is a separate post-cutover phase using the compiler again to fix any breakage exposed by the architectural transition.
