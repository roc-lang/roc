# Boxed Lambda Erasure Plan

Status: complete

## Goal

Make `Box.box` / `Box.unbox` keep exactly the same user-visible Roc types as today:

- `Box.box : a -> Box(a)`
- `Box.unbox : Box(a) -> a`

but compile boxed functions as erased callables internally.

The end state must satisfy all of these:

- users never see the concept of erasure in source types, docs, or type errors
- boxing a function introduces an explicit internal callable-representation fact that says "erased"
- later stages consume that fact directly; no stage reconstructs or guesses it
- boxed lambdas lower through the erased callable path
- unboxed calls lower through indirect-call / erased-capture handling
- the runtime representation is explicit and consistent all the way through

`cor` is the design guide:

- keep surface typing simple
- hide erased-callable machinery from user-facing types
- make earlier stages produce one explicit fact
- make later stages consume that fact only

## Current State

These are the relevant current boundaries:

- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/build/roc/Builtin.roc`](\/Users\/rtfeldman\/.codex\/worktrees\/1d55\/roc\/src\/build\/roc\/Builtin.roc)
  `Box.box` / `Box.unbox` are still ordinary polymorphic builtins.
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdamono/lower_type.zig`](\/Users\/rtfeldman\/.codex\/worktrees\/1d55\/roc\/src\/lambdamono\/lower_type.zig)
  executable function repr is split into `.lset` vs `.erased`.
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdamono/lower.zig`](\/Users\/rtfeldman\/.codex\/worktrees\/1d55\/roc\/src\/lambdamono\/lower.zig)
  `.erased` function values already lower as `packed_fn`, and `.erased` calls already lower as `call_indirect`.
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdamono/layout_facts.zig`](\/Users\/rtfeldman\/.codex\/worktrees\/1d55\/roc\/src\/lambdamono\/layout_facts.zig)
  currently maps primitive `erased` to `opaque_ptr`.

That last point is the representation mismatch we must remove before boxed lambdas can use erased-callable lowering soundly.

## Non-Goals

- Do not change the published user-facing type of `Box.box` or `Box.unbox`.
- Do not introduce `Erased` into diagnostics, pretty-printing, docs, or source syntax.
- Do not add fallbacks, heuristics, or "if this looks like a function later" recovery.
- Do not implement this as a monotype-only rewrite.

## Phase 1: Define The Hidden Callable-Repr Fact

Add one explicit internal callable-representation fact at the solved-type boundary.

Requirements:

- This fact must be invisible to users.
- It must distinguish at least:
  - concrete lambda-set callable
  - erased callable
- It must survive the path from solved types into monotype/lambdamono.
- It must be explicit enough that no later stage needs to inspect function shape to rediscover whether a callable is erased.

Design preference:

- attach the fact to function typing in the same conceptual place where the lambda-set currently lives
- but separate "user-visible function type" from "internal callable repr"
- if necessary, add a dedicated internal enum/fact rather than overloading existing user-facing type printing

Files likely involved:

- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdasolved/type.zig`](\/Users\/rtfeldman\/.codex\/worktrees\/1d55\/roc\/src\/lambdasolved\/type.zig)
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdasolved/lower.zig`](\/Users\/rtfeldman\/.codex\/worktrees\/1d55\/roc\/src\/lambdasolved\/lower.zig)
- any user-facing type printer or error renderer touched by the representation change

Success condition:

- a solved function can be marked internally as "erased callable" without changing what the user sees as its type

## Phase 2: Make Box.box / Box.unbox Set That Fact

Teach the semantic handling of `Box.box` and `Box.unbox`:

- if `a` is not a function, nothing special happens
- if `a` is a function, then crossing the `Box` boundary must explicitly set internal callable repr to `erased`

Operational rule:

- `Box.box` on a function:
  - input callable repr may be concrete
  - boxed payload callable repr becomes erased
- `Box.unbox` on a function:
  - output callable repr remains erased

Important constraint:

- this must be done where builtin semantics are established, not later by monotype trying to notice "oh, this function went through Box"

Files likely involved:

- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/build/roc/Builtin.roc`](\/Users\/rtfeldman\/.codex\/worktrees\/1d55\/roc\/src\/build\/roc\/Builtin.roc)
- builtin semantic handling in checker / solved lowering
- any code that currently treats `Builtin.Box.box` as pure shape-preserving polymorphism

Success condition:

- after the solved stage, a function that crossed `Box.box` has one explicit erased-callable fact
- later stages do not need to infer this from the presence of `Box`

## Phase 3: Reconcile Erased Callable Type/Layout With Value Lowering

This is the main representation task.

Today:

- erased callable values lower as `packed_fn`
- erased callable types/layouts still collapse toward primitive `erased` / `opaque_ptr`

That is not acceptable for boxed lambdas.

Choose one explicit runtime representation and use it consistently.

Preferred design:

- erased callable runtime representation is the packed-function representation
- type/layout lowering publishes an explicit logical layout/runtime class for erased callables that matches `packed_fn`
- boxed erased callables are therefore `Box(packed_fn_repr)`, not `Box(opaque_ptr_guess)`

Alternative design only if strictly better:

- make erased callables truly be opaque pointers everywhere
- then change erased value lowering so `packed_fn` is materialized and boxed behind that pointer model

The first option appears more aligned with the existing executable AST.

Files likely involved:

- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdamono/lower_type.zig`](\/Users\/rtfeldman\/.codex\/worktrees\/1d55\/roc\/src\/lambdamono\/lower_type.zig)
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdamono/lower.zig`](\/Users\/rtfeldman\/.codex\/worktrees\/1d55\/roc\/src\/lambdamono\/lower.zig)
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdamono/layout_facts.zig`](\/Users\/rtfeldman\/.codex\/worktrees\/1d55\/roc\/src\/lambdamono\/layout_facts.zig)
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/ir/lower.zig`](\/Users\/rtfeldman\/.codex\/worktrees\/1d55\/roc\/src\/ir\/lower.zig)
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lir/FromIr.zig`](\/Users\/rtfeldman\/.codex\/worktrees\/1d55\/roc\/src\/lir\/FromIr.zig)

Success condition:

- erased callable type facts and erased callable value lowering describe the same runtime thing

## Phase 4: Make Boxed Function Values Follow The Erased Path

Once the fact and representation are correct, the lowering rule becomes simple:

- a function value outside `Box` may still lower through `.lset`
- a function value crossing `Box.box` lowers as erased callable
- `Box.unbox` returns the same erased callable representation
- calling the unboxed value uses `call_indirect`

This phase must remove any lingering concrete-lambda-set extraction along the boxed-function path.

Specifically:

- no `extractLsetFn(...)` after the function has been boxed/unboxed
- no concrete lambda tag construction for values already marked erased-by-box

Success condition:

- `Box.box(|...| ...)` produces boxed erased callable representation
- `Box.unbox(...)` returns erased callable representation
- calling it works through the existing erased-call path

## Phase 5: Preserve Surface-Typing And Diagnostics

After the internal change is in place, audit user-visible surfaces:

- type printing
- error messages
- snapshots / debug output intended for end users
- builtin docs

Requirements:

- the user should continue to see `Box(a)` and ordinary function types
- there should be no `Erased` leak in type errors, inference output, or help text

If internal debug printers still mention erased-callable details, that is acceptable only for compiler-internal debug output, not user-facing reports.

## Phase 6: Tests

Add tests only from valid Roc source code.

The test plan must cover both semantics and representation.

### A. Basic Boxed Lambda Round-Trip

Add source-level tests for:

- boxing a non-capturing lambda
- unboxing it
- calling it multiple times
- using the return value in further computation
- inspecting the final result

Example shape:

- `boxed = Box.box(|x| x + 1)`
- `f = Box.unbox(boxed)`
- `f(1) + f(2) + f(3)`

### B. Capturing Lambda Round-Trip

Add source-level tests for:

- boxing a lambda that captures multiple values
- unboxing it
- calling it multiple times
- ensuring captured values are respected

Example shape:

- `capture1 = 10`
- `capture2 = 20`
- `boxed = Box.box(|a, b| a + b + capture1 + capture2)`
- `f = Box.unbox(boxed)`
- check multiple calls and inspected output

### C. Pass-Around Behavior

Add tests where boxed erased lambdas are:

- stored in records
- returned from helper functions
- passed through multiple helper functions
- unboxed and called later

The goal is to verify the erased callable fact survives nontrivial data flow without any concrete-lambda recovery.

### D. Multiple Calls After Unboxing

Add tests where the same unboxed function value is called more than once, with results combined and inspected.

This matters because accidental one-shot capture consumption or broken boxed-call lowering would often hide in single-call tests.

### E. Polymorphic Argument Types

Add tests specifically for lambdas whose arguments are polymorphic before boxing.

Required cases:

- a boxed identity-like lambda used after unboxing at multiple types in valid Roc structure
- a boxed higher-order helper where the unboxed callable is instantiated at different argument types on different specializations

The tests should verify:

- specialization still works
- boxed-callable erasure does not collapse distinct valid instantiations incorrectly

If one single value cannot legally be called at multiple types in one specialization, structure the tests so the same source lambda is boxed/unboxed in multiple valid type contexts and each specialization is exercised explicitly.

### F. Closing Over Polymorphic Types

Add tests where the boxed lambda closes over values whose surrounding function is polymorphic.

Required cases:

- a polymorphic helper returns a boxed lambda that closes over its type parameter
- later unboxing and calling uses that closed-over value correctly
- multiple valid instantiations of the helper are exercised

The goal is to prove that hidden erasure of callable representation does not break capture specialization or closed-over polymorphic values.

### G. Allocation / Representation Verification

Add at least one representation-sensitive test proving boxed lambdas are using erased-callable representation, not concrete lambda-tag representation.

Possible verification strategies:

- instrument a test host and assert `roc_alloc` sizes / alignments / counts for boxed lambdas
- inspect emitted IR/LIR in a compiler test and assert the boxed lambda path contains erased-callable artifacts:
  - `packed_fn`
  - `call_indirect`
  - absence of concrete lambda tag transport through `Box`

Preferred approach:

- one semantic test at the Roc level
- one compiler-internal representation test that directly checks the lowered artifact shape

This avoids overfitting to platform-specific allocator details while still proving the representation change actually happened.

Candidate locations:

- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/test/eval_low_level_tests.zig`](\/Users\/rtfeldman\/.codex\/worktrees\/1d55\/roc\/src\/eval\/test\/eval_low_level_tests.zig)
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/test/snapshots`](\/Users\/rtfeldman\/.codex\/worktrees\/1d55\/roc\/test\/snapshots)
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/crates/compiler/test_gen/src/gen_primitives.rs`](\/Users\/rtfeldman\/.codex\/worktrees\/1d55\/roc\/crates\/compiler\/test_gen\/src\/gen_primitives.rs)
- dedicated lowering snapshot tests if they already exist for `lambdamono` / IR / LIR

## Phase 7: Audit And Cleanup

After the feature works:

- remove any temporary dual-path code
- remove any debug-only "boxed function special-case" probes that are no longer needed
- confirm no new fallback or heuristic paths were added
- re-audit the full pipeline for:
  - hidden callable-repr recovery
  - concrete-lambda extraction after boxing
  - runtime-shape shortcuts introduced just for boxed lambdas

## Success Criteria

This work is complete only when all of these are true:

- `Box.box` / `Box.unbox` user-visible types are unchanged
- the user never sees `Erased` in source-facing diagnostics
- boxed functions carry an explicit hidden erased-callable fact from early solving onward
- boxed functions lower through erased callable value/call paths
- the erased callable runtime representation is explicit and consistent
- valid Roc tests cover:
  - non-capturing boxed lambdas
  - capturing boxed lambdas
  - pass-around and repeated-call behavior
  - polymorphic argument cases
  - closed-over polymorphic value cases
  - representation-sensitive verification that boxing a lambda really compiled to erased callable form
