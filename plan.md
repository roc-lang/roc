# Hosted Proc Fact Plan

Status: complete

## Goal

Remove the remaining hosted-lambda shortcut from the active pipeline and replace it with the long-term ideal design:

- no compiler stage treats hosted functions as ordinary lambdas with fake bodies
- no compiler stage fabricates runtime errors or crash placeholders to stand in for missing hosted semantics
- hosted functions are represented as one explicit proc fact produced early and consumed directly by every later stage
- the resulting design is maximally correct and maximally efficient, even if it takes more implementation work

This plan explicitly prioritizes the ideal long-term shape of the compiler over short-term convenience or minimal patch size.

`cor` remains the guide:

- earlier stages decide semantics once
- later stages consume those explicit facts only
- no recovery, fallback, or heuristic reconstruction

## Current Problem

Today hosted functions are introduced in canonicalization as `e_hosted_lambda`, but they still look too much like ordinary lambdas:

- canonicalization fabricates a crash body
- monotype partially lowers them through the lambda path
- zero-arg hosted functions still fabricate a `runtime_error`
- direct expr lowering still has hosted-lambda TODOs

That is the wrong boundary. Hosted functions are top-level external procs, not closures.

## Target Shape

Hosted functions should become a first-class top-level proc concept.

The target pipeline shape is:

1. canonicalization/checking records explicit hosted-proc facts
2. monotype lowers hosted defs as hosted procs, not lambdas
3. lambda lifting preserves hosted proc defs directly
4. lambdasolved carries hosted proc defs without inference from fake bodies
5. lambdamono carries hosted proc defs without closure lowering
6. IR carries hosted proc defs explicitly
7. LIR proc specs receive hosted metadata directly and never need a fake Roc body to explain the proc
8. backend/interpreter hosted call paths consume that explicit proc metadata

## Phase 1: Introduce First-Class Hosted Proc Metadata

Add one shared hosted-proc metadata shape that is explicit and lossless:

- hosted symbol name
- stable hosted-table index

Then introduce first-class hosted top-level def variants instead of encoding hosted defs as ordinary function defs.

Likely affected files:

- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/ast.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/ast.zig)
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype_lifted/ast.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype_lifted/ast.zig)
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdasolved/ast.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdasolved/ast.zig)
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdamono/ast.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdamono/ast.zig)
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/ir/ast.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/ir/ast.zig)

Success condition:

- the compiler can represent “top-level hosted proc” explicitly without pretending it has a lambda body

## Phase 2: Make Monotype Lower Hosted Defs Directly

Replace the remaining hosted-lambda shortcut in monotype with explicit hosted-proc lowering.

Requirements:

- branch on hosted-proc facts before entering lambda lowering
- remove the hosted-specific runtime-error fabrication
- remove the hosted-lambda TODO arms from ordinary expr lowering
- preserve full explicit arg facts and full function type facts

Likely affected files:

- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig)

Success condition:

- monotype no longer treats hosted functions as lambdas at all

## Phase 3: Preserve Hosted Proc Facts Through Lambda Lifting

Hosted procs are already top-level and have no closure semantics.

So lambda lifting should:

- copy hosted-proc defs through untouched
- never try to lift them
- never try to infer free variables or captures for them

Likely affected files:

- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype_lifted/lower.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype_lifted/lower.zig)

Success condition:

- hosted-proc defs survive into `monotype_lifted` as explicit hosted procs

## Phase 4: Carry Hosted Proc Facts Through Lambdasolved

Hosted procs must not participate in ordinary function-body inference.

Requirements:

- instantiate hosted-proc defs explicitly
- add them to environments as already-known top-level proc facts
- do not infer them from fake bodies
- do not include them in recursive function-body SCC inference

Likely affected files:

- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdasolved/lower.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdasolved/lower.zig)

Success condition:

- lambdasolved carries hosted proc defs explicitly and never asks for a hosted body

## Phase 5: Carry Hosted Proc Facts Through Lambdamono

Hosted procs are executable top-level procs, not closures.

Requirements:

- preserve hosted metadata on defs
- keep hosted procs out of closure lowering
- keep hosted procs out of lambda-set specialization queues unless that is explicitly required and modeled
- if a hosted proc is referenced as a function value, treat it as a captureless top-level proc value, not as a closure

Likely affected files:

- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdamono/lower.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdamono/lower.zig)
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdamono/specializations.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdamono/specializations.zig)

Success condition:

- lambdamono lowers hosted procs as ordinary top-level executable proc definitions with explicit hosted metadata

## Phase 6: Carry Hosted Proc Facts Through IR And Into LIR

Thread the hosted fact into IR and then into LIR proc specs.

Requirements:

- IR defs carry hosted metadata explicitly
- LIR proc specs receive hosted metadata directly from IR
- hosted procs no longer require fake bodies to justify their existence
- if the LIR/IR container shape still requires a body field structurally, refactor that shape so hosted procs can be represented explicitly instead of by placeholder bodies

Likely affected files:

- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/ir/lower.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/ir/lower.zig)
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lir/FromIr.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lir/FromIr.zig)
- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lir/LIR.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lir/LIR.zig)

Success condition:

- hosted proc specs reach LIR through explicit proc facts, not through fake Roc code

## Phase 7: Tests

We need tests that prove both semantic correctness and the explicit-fact pipeline shape.

### A. Monotype / Lowering Unit Tests

Add unit tests showing that a hosted declaration:

- lowers to an explicit hosted-proc def
- does not produce a runtime-error body
- does not take the lambda-lowering path

### B. Cross-Stage Propagation Tests

Add unit tests showing that the hosted metadata survives through:

- monotype
- monotype_lifted
- lambdasolved
- lambdamono
- IR
- LIR

The test should verify:

- hosted index is preserved
- hosted symbol name is preserved
- no stage reconstructs the hosted fact from a fake body

### C. End-To-End Runtime Tests

Add end-to-end tests that compile and run Roc code using hosted functions and verify:

- the hosted function call reaches the host ABI callback
- the correct hosted-table index is used
- arguments are marshaled with the expected layouts
- return values are handled correctly

Coverage should include:

- zero-arg hosted function
- one-arg hosted function
- multiple-arg hosted function
- repeated hosted calls
- hosted calls inside blocks / branches / loops

### D. Default-App `echo!` Coverage

Because `echo!` is injected as a hosted function, add tests that confirm:

- `echo!` lowers as an explicit hosted proc
- calling `echo!` no longer depends on fake hosted lambda bodies

## Phase 8: Final Audit

After implementation:

- rerun the earlier hosted-lambda audit
- verify the old hosted shortcut sites are gone
- verify no new fallbacks or heuristics were introduced
- verify the remaining hosted behavior is entirely explicit-fact-driven

## Completion Criteria

This plan is complete only when all of these are true:

- [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig:991`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig:991) is gone
- hosted expr-lowering TODO arms are gone from monotype
- hosted defs are explicit proc facts through the full pipeline
- LIR hosted proc specs are produced from explicit upstream metadata
- tests verify both propagation and runtime behavior
- no fake hosted lambda body remains in the active pipeline
