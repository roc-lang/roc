# Iterative Unify Design

## Goal

Convert `src/check/unify.zig` from recursive type unification to an explicit iterative work loop without changing the checked type semantics, diagnostics policy, or static-dispatch behavior.

## Reference

PR #9901 converted the occurs check from recursive calls to explicit work frames. This change applies the same long-term direction to unification: semantic state is represented directly in typed frames, child work is scheduled explicitly, and post-child merges run only after all required child unifications have succeeded.

## Architecture

The public `unify` API remains unchanged. `unify` still resets `Scratch`, constructs a per-call `Unifier`, and returns `.ok`, `.problem`, or `.mismatch`.

Inside `Unifier`, `unifyGuarded(a, b)` becomes a scheduling operation. It pushes an explicit guarded-pair frame onto `Scratch.unify_work_stack`, and a new `runWorkLoop` method processes frames until the stack is empty or a mismatch/OOM occurs.

Frames encode the same phases that recursion currently encodes on the native call stack:

- guarded pair entry with recursion-pair guard setup
- pair guard cleanup
- direct merge
- child unification sequences for tuples, functions, aliases, nominals, records, tag unions, and static-dispatch constraints
- post-child operations that perform merges or mismatch aggregation only after child frames finish

The implementation must not use recovery, guessing, fallback paths, or heuristic matching. Every frame carries the explicit data needed for the next semantic step.

## Behavior

Unification order remains semantically equivalent to the current recursive implementation. Where the old implementation unified child A then child B then merged, the iterative stack schedules frames in reverse order so execution observes the same order.

Mismatch aggregation is preserved for cases that intentionally check all children before failing, including same-identity aliases and shared record fields. These use frame-local result counters or ranges in scratch memory instead of native stack locals.

Deferred static-dispatch origin tracking remains tied to the second operand of the active guarded pair. The explicit frame state saves and restores `unresolved_b` around a guarded pair exactly as the recursive implementation does today.

The existing pair recursion guard and constraint recursion guard are preserved. Guard entry and guard cleanup are separate frames so cleanup runs after all child frames scheduled by that guarded pair.

## Testing

Targeted tests must cover:

- deeply nested tuple unification that would stress recursive call depth
- behavior already covered by existing alias, function, record, tag union, nominal, static-dispatch, and write-no-report tests
- full `zig build test-check` or the repository equivalent targeted check test step
- `zig build minici`, using failing-section retry discipline if any section fails

## Out of Scope

This change does not alter occurs checking, type inference policy, diagnostics text, row-polymorphism semantics, nominal opacity rules, or backend behavior.
