# List Append/Reserve Correctness Plan

Status: complete

## Goal

Make list append/reserve work in the long-term ideal way:

- `list_append_unsafe` remains a genuinely memory-unsafe compiler primitive
- the compiler knows only about irreducible list primitives, not about `List.append` as a special builtin
- `List.append` is ordinary Roc code in `Builtin.roc`
- `List.reserve` is publicly exposed ordinary Roc code in `Builtin.roc`
- ordinary public builtins establish safety preconditions before calling unsafe primitives
- interpreter and backends execute `list_append_unsafe` as actually unsafe, with no semantic softening

This plan explicitly prioritizes the ideal long-term architecture over implementation speed or patch size.

## Required Architecture

The correct boundary is:

1. `Builtin.roc` defines public list APIs in ordinary Roc
2. the compiler only recognizes irreducible primitives such as:
   - `list_append_unsafe`
   - `list_get_unsafe`
   - `list_reserve`
   - `list_with_capacity`
  - `list_release_excess_capacity`
3. later stages lower those primitives directly without changing their meaning
4. interpreter and backends implement the same primitive contract

The compiler should not know that `List.append` exists in any special way.

## Current Bugs

Today the contract is broken in multiple places:

- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/build/roc/Builtin.roc`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/build/roc/Builtin.roc)
  - `List.append` directly calls `list_append_unsafe` without first establishing spare capacity
  - `List.reserve` is not publicly exposed even though the primitive exists

- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/build/builtin_compiler/main.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/build/builtin_compiler/main.zig)
  - `list_append_unsafe` is mapped, but the top-level primitives `list_reserve` and `list_release_excess_capacity` were not

- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/interpreter.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/interpreter.zig)
  - `list_append_unsafe` is intentionally implemented with the safe append helper

- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig)
  - non-ZST `list_append_unsafe` is intentionally compiled to the safe append helper

This is an explicit semantic contract violation.

## Phase 1: Expose The Right Public Builtins

Fix `Builtin.roc` so the public API exposes the correct ordinary Roc functions.

Required changes:

- add public `List.reserve : List(item), U64 -> List(item)`
- add public `List.release_excess_capacity : List(item) -> List(item)` if we intend to expose that primitive too
- rewrite `List.append` to establish spare capacity before calling the unsafe primitive

Target shape:

```roc
reserve = |list, spare| list_reserve(list, spare)

append = |list, item| {
    reserved = List.reserve(list, 1)
    list_append_unsafe(reserved, item)
}
```

Success condition:

- `List.append` is ordinary Roc code
- `List.reserve` is ordinary Roc code
- no public builtin exposes memory unsafety directly

## Phase 2: Audit Every Builtin.roc Use Of list_append_unsafe

Audit every direct use of `list_append_unsafe` in `Builtin.roc`.

Each call site must satisfy one of these:

- it is preceded by an explicit `List.reserve(..., needed_spare)` proof
- it originates from `List.with_capacity(...)` with a loop whose max append count is explicitly bounded by that capacity
- it is otherwise accompanied by an explicit static proof of sufficient capacity

Expected current call sites:

- `List.append`
- `List.rev`
- `List.map`
- any local loops that grow a list by repeated appends

For each call site:

- keep it if the proof is explicit and correct
- rewrite it if the proof is missing
- do not add compiler heuristics or backend workarounds to compensate

Success condition:

- every direct `list_append_unsafe` in `Builtin.roc` is justified locally and explicitly

## Phase 3: Wire Public Reserve/Release Builtins Through The Builtin Compiler

Update the builtin compiler so the public Builtin module can lower the new public functions directly to primitives.

Required changes:

- map `list_reserve` to `.list_reserve`
- map `list_release_excess_capacity` to `.list_release_excess_capacity` if exposed publicly

Likely affected file:

- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/build/builtin_compiler/main.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/build/builtin_compiler/main.zig)

Success condition:

- `Builtin.roc` can express the correct public surface using only primitive replacements

## Phase 4: Make list_append_unsafe Actually Unsafe Everywhere

Interpreter and backends must stop changing the meaning of the primitive.

Required changes:

- interpreter `list_append_unsafe` must call the actual unsafe primitive implementation
- dev backend `list_append_unsafe` must always call the actual unsafe wrapper, not the safe wrapper for non-ZST
- wasm path must be checked to ensure its host/import implementation matches the unsafe contract exactly
- remove comments and code that intentionally “match” the previous cheating behavior

Likely affected files:

- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/interpreter.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/interpreter.zig)
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig)
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/repl/wasm_runner.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/repl/wasm_runner.zig)
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig)

Success condition:

- every implementation of `list_append_unsafe` is actually unsafe and assumes capacity is already available

## Phase 5: Add Tests That Prove The Boundary Is Correct

We need tests for both semantics and architecture.

### A. Public Builtin Behavior Tests

Add end-to-end tests showing:

- `List.append([], x)` works
- `List.append([..], x)` works
- repeated `List.append` works through ordinary public API usage
- `List.reserve(list, n)` followed by appends works
- `List.release_excess_capacity` preserves contents

### B. Primitive Contract Tests

Add lower-level tests that prove:

- `list_append_unsafe` works correctly when spare capacity is already available
- `list_with_capacity(n)` followed by exactly `n` unsafe appends works
- `list_reserve(list, spare)` followed by `spare` unsafe appends works

These tests should exercise:

- non-ZST element lists
- ZST element lists
- refcounted element lists

### C. Contract-Separation Tests

Add tests that verify the public/safe layer is the thing establishing the precondition, not the primitive executor.

Examples:

- compile and run Roc code using `List.append` and verify behavior stays correct after removing backend/interpreter safe-append cheating
- add backend/interpreter tests that directly execute low-level `list_append_unsafe` only in proven-capacity situations

If practical, add representation/allocation-sensitive tests to show:

- `List.reserve(list, 1)` avoids reallocation before a following append when capacity was insufficient previously
- repeated appends after a single reserve do not perform extra growth until spare capacity is exhausted

### D. Builtin Surface Tests

Add tests proving:

- `List.reserve` is publicly available from Roc code
- `List.release_excess_capacity` is publicly available from Roc code
- `List.append` is still just ordinary Roc code built from the primitive layer

## Phase 6: Final Audit

After implementation:

- audit the full pipeline for `list_append_unsafe`
- verify no stage treats `List.append` specially
- verify no backend/interpreter path softens the unsafe contract
- verify every direct `list_append_unsafe` call in `Builtin.roc` has an explicit local capacity proof

## Completion Criteria

This plan is complete only when all of these are true:

- `Builtin.roc` publicly exposes `List.reserve`
- `Builtin.roc` publicly exposes `List.release_excess_capacity`
- `List.append` in `Builtin.roc` establishes capacity explicitly before unsafe append
- builtin compiler maps `list_reserve` and `list_release_excess_capacity` correctly
- interpreter uses the actual unsafe append primitive for `list_append_unsafe`
- dev backend uses the actual unsafe append primitive for `list_append_unsafe`
- wasm import/host path matches the same unsafe contract
- tests cover both public behavior and primitive contract boundaries
- no compiler stage knows about `List.append` specially
