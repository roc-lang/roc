# Problems: Late Guessing And Recovery Suspects

This file tracks places that look similar to the callable/lambda issue we just fixed: a later compiler stage may be guessing, searching, or reconstructing information that an earlier stage knew directly.

The question for each problem is:

> What is the most long-term ideal thing we should do here?

## 1. Dev Backend `Num.abs_diff` Signedness Guess

Location:

- `src/backend/dev/LirCodeGen.zig`

Suspicion:

The dev backend has an `inferAbsDiffInputLayout` helper. It receives a return layout and optional argument layout, then guesses the input signedness from the return type and whether either argument is a negative immediate.

Risk:

Signedness is semantic information. A backend should not infer it from a return type or literal shape.

## 2. Checked-Type Payload Lookup From Type Keys During Dependency Finalization

Location:

- `src/eval/compile_time_finalization.zig`
- `src/check/checked_artifact.zig`

Suspicion:

Finalization can receive a canonical requested type key and then search local/imported checked type stores to find a checked payload root.

Risk:

If an earlier phase had the checked payload root, finalization should consume it explicitly instead of rediscovering it by key.

## 3. Capture And Const Planning Recover Checked Payloads From Source Type Keys

Location:

- `src/lir/checked_pipeline.zig`

Suspicion:

Capture-slot planning and const-graph planning call `CheckedTypeStore.rootForKey` from source type keys while building reification plans.

Risk:

This can recreate the same class of bug as callable descriptor member payload recovery: the producer knew the payload root, but the consumer finds it later by matching a key.

## 4. Private Capture Finalization Recovers Checked Payloads

Location:

- `src/eval/compile_time_finalization.zig`

Suspicion:

Private capture publication calls `rootForKey` for source types while producing const instances and capture refs.

Risk:

Private capture plans may be missing explicit checked payload references that should have been published with the plan.

## 5. `named_fn` Layout Sentinel And Backend Symbol Lookup

Location:

- `src/layout/layout.zig`
- `src/interpreter_layout/layout.zig`
- likely backend call lowering paths

Suspicion:

`layout.Idx.named_fn` is a fake layout sentinel for call expressions whose function is resolved later by name.

Risk:

Resolved callable identity should travel as a procedure/function reference plus calling convention, not as a layout sentinel that implies later name lookup.

## 6. Callable-Set Member Selection By Runtime Discriminant

Location:

- `src/eval/compile_time_finalization.zig`

Suspicion:

Compile-time finalization selects a finite callable-set member by reading a runtime tag discriminant and mapping it to a member id.

Risk:

This is probably legitimate value decoding, but only if the producer explicitly promised that representation order and member ids match. If that promise is implicit, it is another hidden recovery contract.
