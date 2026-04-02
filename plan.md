# CoreCIR Architectural Cutover Plan

## Absolute Rules

- Do not run `zig` for any reason at any time while executing this plan.
- Do not run `zig build`, `zig test`, `zig build minici`, eval tests, repl tests, or any compiler entrypoint driven by Zig.
- Do not introduce workarounds, hacks, rescue paths, compatibility shims, temporary placeholders, transitional stopgaps, or fallback behavior.
- Do not preserve old architecture for backwards compatibility.
- If an invariant is missing during the cutover, the only acceptable temporary behavior is:
  - `std.debug.panic(...)` in debug builds
  - `unreachable` in release builds

## Goal

Replace the remaining integrated mutable CoreCIR pipeline architecture with the long-term ideal architecture:

1. `TemplateCatalog` performs one-time source-to-solved extraction of callable templates and root inventory.
2. `ContextMono` owns exact contextual monotypes derived only from already-inferred checker types.
3. `DispatchSolved` owns exact static-dispatch sites and their normalized resolutions.
4. `Lambdasolved` owns solved callable-set membership and capture provenance.
5. `Lambdamono` is a real executable specialized IR, not a fat record with optional semantic slots.
6. `Lower` mechanically consumes `Lambdamono`; it does not search, recover, or reinterpret source semantics.

## Stage 1: Introduce Dedicated Stage Boundaries

### 1.1 Add `TemplateCatalog`

- Create a new `src/corecir/TemplateCatalog.zig`.
- Move one-time callable/root discovery into `TemplateCatalog`.
- `TemplateCatalog` must own:
  - callable template registration
  - root expression inventory
  - external callable-backed def registration
  - callable boundary extraction from CIR
- After this, later stages may not inspect raw CIR callable shape to rediscover template boundaries.

### 1.2 Add `DispatchSolved`

- Create a new `src/corecir/DispatchSolved.zig`.
- Move normalized dispatch-site ownership out of checker storage and out of `ContextMono`.
- `DispatchSolved` must own:
  - dispatch site identity
  - source-context-scoped dispatch sites
  - normalized resolution:
    - target def
    - intrinsic dispatch
- After this, later stages may not read checker dispatch constraints directly.

### 1.3 Reduce `Pipeline.zig` To Orchestration Only

- `Pipeline.zig` must stop implementing semantic work directly.
- It must only sequence stage execution:
  - `TemplateCatalog`
  - `ContextMono`
  - `DispatchSolved`
  - `Lambdasolved`
  - `Lambdamono`
- Delete semantic helper ownership from `Pipeline` as it moves into stage files.

## Stage 2: Make Checker Types The Only Monotype Source Of Truth

### 2.1 Strip Resolved Dispatch Metadata From Checker Storage

- Replace `types.StaticDispatchConstraint` with a checker-owned requirement-only form.
- Delete checker-owned:
  - `source_expr_idx`
  - `resolved_target`
- Rename it to a requirement-oriented shape if necessary.

### 2.2 Keep `ContextMono` Pure

- `ContextMono` must continue to derive monotypes only from inferred checker types.
- No monotype logic may inspect:
  - declared signatures
  - annotations
  - CIR argument shape
  - builtin call shape
  - lambda parameter counts
  - dispatch resolution artifacts

## Stage 3: Replace Shared Mutable Scan State With Stage-Local Worklists

### 3.1 Delete Shared `ExprScanState`

- Delete the shared scan/in-progress state currently hanging off `Pipeline.Pass`.
- Replace it with stage-local state:
  - `TemplateCatalog.Builder`
  - `ContextMono.Solver`
  - `DispatchSolved.Solver`
  - `Lambdasolved.Solver`
  - `Lambdamono.Builder`

### 3.2 Remove Mixed Scan/Assembly Flows

- Delete the current “scan root, then assemble expr graph, then assemble callable defs” flow.
- Each stage must either:
  - solve facts
  - or build the final output for that stage
- No stage may first scan mutable side tables and then separately assemble the same semantics later.

## Stage 4: Make `Lambdamono` A Real Executable AST

### 4.1 Delete The Fat Optional-Slot Expr Record

- Replace `Lambdamono.Expr` with a tagged union of executable node kinds.
- Make `monotype` structurally required.
- Delete optional semantic slots like:
  - `callable`
  - `call`
  - `origin`
  - `dispatch`
  - `lookup`

### 4.2 Introduce Explicit Executable Node Variants

- The specialized IR must have explicit executable nodes for at least:
  - local lookup
  - def lookup
  - direct callable
  - packed callable
  - closure introduction
  - direct call
  - indirect call
  - low-level call
  - dispatch call
  - projection
  - block
  - switch
  - return
  - literals/records/tags/tuples as needed

### 4.3 Make Invalid States Unrepresentable

- A node that is a direct call must not also have optional lookup semantics.
- A node that is a def lookup must not also have optional dispatch semantics.
- A node that is a packed callable must encode that directly, not via optional side slots.

## Stage 5: Make Dispatch First-Class In Specialized IR

### 5.1 Dispatch Sites Must Be Normalized Before `Lambdasolved`

- `DispatchSolved` must produce exact dispatch-site facts keyed by source context.
- `Lambdasolved` and `Lambdamono` must consume those exact facts, not checker constraints.

### 5.2 `Lambdamono` Must Encode Dispatch Explicitly

- Dispatch must appear as explicit executable IR, not as a side lookup against `ContextMono`.
- Later lowering must consume:
  - direct dispatch target
  - dispatch intrinsic

## Stage 6: Delete Legacy / Transitional Artifacts

- Delete all remaining legacy or transitional artifacts introduced by the current integrated pipeline design, including:
  - direct semantic work in `Pipeline.zig`
  - shared scan/in-progress state structs
  - late assembly flows that duplicate earlier semantic work
  - checker-owned resolved dispatch metadata
  - optional semantic-slot executable IR in `Lambdamono`
  - any remaining consumer-side rereads of raw CIR to rediscover already-recorded facts

## Stage 7: Mandatory Re-Audit Loop

When the cutover appears complete:

1. Audit `src/corecir`, `src/mir`, `src/lir`, `src/types`, `src/backend`, and `src/eval`.
2. For each of the following artifact classes, search the codebase and verify there is no remaining trace of them:
   - integrated semantic work in `Pipeline`
   - shared scan/in-progress state spanning multiple stages
   - late scan-then-assemble duplication
   - checker-owned resolved dispatch metadata
   - raw checker dispatch-constraint reads outside dispatch solving
   - fat optional-slot `Lambdamono.Expr` semantics
   - consumer-side CIR rereads for already-solved facts
3. If even one trace remains:
   - loop back to the beginning of this plan
   - delete it
   - continue until the audit is clean

## Success Criteria

This plan is complete only when all of the following are true:

- `zig` has not been run at any point while carrying out this plan.
- `Pipeline.zig` is orchestration-only.
- `TemplateCatalog`, `ContextMono`, `DispatchSolved`, `Lambdasolved`, and `Lambdamono` are physically separate stage owners.
- checker storage no longer owns resolved dispatch targets or source-expression dispatch metadata.
- `Lambdamono` is a strict executable AST with invalid states unrepresentable.
- later stages consume explicit earlier-stage outputs and do not search, guess, recover, or reinterpret.
- a final artifact-by-artifact audit finds no remaining trace of the old integrated/transitional design anywhere in the code base.
