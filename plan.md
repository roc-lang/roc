# Effect Propagation, Compile-Time Roots, And Static Const Plan

## Objective

Build one checked-stage system that owns Roc effect validation,
compile-time evaluation eligibility, compile-time diagnostics, maximal root
selection, and static constant output.

The target end state is:

- every module is considered for eligible compile-time evaluation
- unreachable top-level values still run eligible `crash`, `dbg`, and `expect`
  during `roc check`
- effectful calls never run at compile time
- `crash`, `dbg`, and `expect` are compile-time observables, not effectful calls
- static-dispatch effectfulness uses the same checked dataflow as direct calls
- root selection keeps maximal eligible roots
- rejected parents preserve eligible children
- named constants, closed locals, and equivalent inline expressions select
  equivalent roots
- reachable evaluated static data is stored once and shared where possible
- later stages consume checked roots and checked const data directly

The motivating integration target is Rocci Bird:

- sprite sheet list bytes are static data
- sprite sheet records are static data
- `Sprite.sub_or_crash(...)` cells inside animation constructors are evaluated
  at compile time when their inputs are compile-time-known
- equivalent named and inline animation data compile the same way
- `update` does not rebuild sprite/list data during ordinary gameplay
- the final `--opt=size` WASM-4 binary is measured and disassembled

## Non-Negotiable Invariants

- Checking is the last user-facing stage for type errors, effect errors,
  static-dispatch errors, compile-time `crash`, compile-time `dbg`, and
  compile-time `expect`.
- Later stages consume explicit checked output. They must not guess
  effectfulness, root eligibility, or static data ownership from source shape,
  CIR shape, generated wasm, object symbols, function bodies, or backend code.
- Effect dependencies are directed. A caller depending on a callee is not
  equality.
- Union-find is not the effect solver. Recursive groups may be condensed, but
  ordinary caller-to-callee dependencies remain one-way.
- Static dispatch contributes effectfulness through resolved checked method
  effects, not through `!` spelling or unresolved type variables.
- A negative effect answer is not final while dispatch watchers or callee slots
  can still change.
- Function creation is not effectful merely because the body is effectful.
  Calling the function propagates the body effect.
- Effectful functions do not run during compile-time evaluation.
- `crash`, `dbg`, and `expect` must run at compile time whenever their enclosing
  expression has no runtime dependency and no effectful call.
- Compile-time root selection depends only on runtime dependency and
  effectfulness.
- There are no root blockers for numbers, strings, empty lists, records,
  `return`, `break`, loops, `crash`, `dbg`, or `expect`.
- Parent root selection is the only reason to remove child roots.
- Rejecting a parent root preserves eligible children selected inside it.
- Ordinary nested expressions use stack-local summaries, not a permanent
  per-expression summary table.
- Local summaries are stored only when later local lookup can consume them.
- Checked module data must not contain unresolved function effects or unresolved
  compile-time root eligibility.
- Static storage and compile-time evaluation are separate outputs: unreachable
  values may be evaluated for diagnostics without forcing target data output.
- If an evaluated reachable value cannot yet be represented as target static
  data, that limitation is explicit in checked/static-data output.
- No backend may rediscover or guess root eligibility.

## Data Model

### Effect Slots

The checker owns sparse effect slots only where effectfulness is checked output:

```zig
const EffectSlotId = enum(u32) { _ };

const EffectSlotKind = union(enum) {
    function_body: CIR.Expr.Idx,
    top_level_value: CIR.Def.Idx,
    expect_body: Region,
    const_root_candidate: u32,
};

const EffectSlot = struct {
    kind: EffectSlotKind,
    direct_effect: bool,
    resolved_effectful: ?bool,
};

const EffectEdge = struct {
    from: EffectSlotId,
    to: EffectSlotId,
};
```

Slots become effectful when:

- their body directly calls a checked effectful function
- their body calls through an effectful function-typed value
- a watched static-dispatch call resolves to an effectful method
- a callee slot reachable through a directed edge is effectful

Slots must not exist for every expression. Delayed-effect root candidates need
slots because their final parent-versus-children decision can depend on static
dispatch after the expression returns.

### Expression Check Result

`checkExpr` returns a small transient result:

```zig
const RuntimeDep = enum {
    compile_time_known,
    runtime_dependent,
    poisoned,
};

const RootEffect = union(enum) {
    effect_free,
    effectful,
    delayed: EffectSlotId,
};

const ExprCheckResult = struct {
    runtime_dep: RuntimeDep,
    root_effect: RootEffect,
};
```

Ownership rules:

- `RuntimeDep` is computed bottom-up during the existing checker traversal.
- `RootEffect` is backed by effect slots; it is not a second effect solver.
- Parents combine child summaries immediately.
- Immutable local definitions store only the summaries later lookups need.
- Ordinary expression summaries are discarded after the parent consumes them.

### Root Candidates

The checker maintains a candidate stack. Every expression frame records the
candidate stack length at entry.

Candidate payloads identify:

- the checked expression to evaluate
- the checked local binding pattern when preserving local sharing requires it
- the body shape needed by compile-time evaluation
- the effect slot when parent selection is delayed
- the child candidate interval owned by the delayed parent

Frame exit rules:

- compile-time-known and effect-free: remove candidates added inside the frame
  and append the parent
- runtime-dependent or effectful: leave candidates added inside the frame
- delayed effectfulness: append a delayed parent tied to the effect slot and
  record the child candidate interval

Effect finalization resolves delayed parents:

- effect-free delayed parent: keep the parent and remove its child interval
- effectful delayed parent: discard the parent and keep finalized children
- nested delayed parents finalize from explicit intervals, not a second source
  walk

## Phase 1: Finish Effect Soundness

Goal: every function, top-level value, `expect`, import, and delayed root
candidate gets a finalized checked effect answer before checked output.

Implementation steps:

1. Audit every effect slot creation site:
   - function bodies
   - lambda bodies
   - top-level value right-hand sides
   - `expect` bodies
   - delayed root candidates
2. Add `const_root_candidate` slots for delayed root decisions.
3. Keep directed caller-to-callee edges for ordinary calls.
4. Keep dispatch watchers keyed by dispatch function variable.
5. Replace recursive ad hoc solving with directed SCC propagation:
   - build the slot graph from `effect_edges`
   - condense strongly connected groups
   - mark a group effectful if any slot in it has direct effect
   - propagate effectfulness to callers
   - write final answers back to slots
6. Finalize negative answers only after dispatch watchers have resolved for the
   checked boundary.
7. Ensure imported checked effect summaries feed direct calls and
   dispatch-selected methods exactly like local checked effects.

Required tests:

- direct effectful top-level call reports `effectful_top_level`
- delayed receiver method call at top level reports `effectful_top_level`
- delayed type-method call at top level reports `effectful_top_level`
- effectful binop dispatch at top level reports `effectful_top_level`
- effectful unary dispatch at top level reports `effectful_top_level`
- interpolation dispatch propagates effectfulness
- synthetic iterator dispatch propagates effectfulness
- imported nominal method dispatch propagates effectfulness
- pure annotation rejects direct effectful call
- pure annotation rejects delayed effectful method call
- effectful annotation accepts direct effectful call
- effectful annotation accepts delayed effectful method call
- pure where-clause accepts pure implementation
- pure where-clause rejects effectful implementation
- effectful where-clause accepts effectful implementation
- effectful where-clause call makes caller effectful
- direct effectful call in `expect` reports `effectful_expect`
- delayed effectful dispatch in `expect` reports `effectful_expect`
- local function alias preserves effectfulness
- imported function alias preserves effectfulness
- higher-order pure function parameter stays pure when called
- higher-order effectful function parameter makes caller effectful
- closure creation with effectful body is pure
- calling closure with effectful body is effectful
- self-recursive pure function stays pure
- self-recursive effectful function is effectful
- mutual recursion with one effectful member propagates to the group
- mutual recursion where every member is pure stays pure
- `dbg` around pure value is not effectful
- `dbg` around effectful call reports through the call
- `crash` in otherwise pure value is not effectful
- `expect` in otherwise pure value is not effectful

Success criteria:

- no checked function type is emitted before its effect slot is finalized
- no checked top-level value or `expect` diagnostic is emitted from an
  unresolved slot
- recursive effect groups solve without solver recursion
- imported checked effects behave the same as local checked effects

## Phase 2: Replace Expression `bool` With `ExprCheckResult`

Goal: expression checking returns runtime-dependency/root data instead of using
expression-level booleans or shape predicates for root eligibility.

Implementation steps:

1. Introduce `ExprCheckResult` and helper constructors.
2. Update `checkExpr` to return `ExprCheckResult`.
3. Update block and statement checking to combine child results.
4. Preserve effect-slot marking while the return type changes.
5. Store immutable local RHS summaries only when a later local lookup needs
   them.
6. On local lookup, return the stored summary for immutable
   compile-time-known locals.
7. Mark lambda arguments as runtime-dependent.
8. Mark match-bound values as runtime-dependent unless they are explicitly
   introduced by compile-time pattern extraction from a selected root.
9. Mark loop-bound values as runtime-dependent.
10. Mark mutable variables and reassigned variables as runtime-dependent.
11. Treat checked top-level value lookups as compile-time-known unless their
    checked summary says otherwise.
12. Treat imported checked value lookups as compile-time-known unless their
    imported checked summary says otherwise.
13. Remove old expression-level `does_fx` as a root eligibility input.

Required tests:

- closed top-level list literal returns compile-time-known
- closed top-level record containing a list returns compile-time-known
- immutable local independent of a lambda argument returns compile-time-known
- immutable local depending directly on a lambda argument is runtime-dependent
- immutable local depending indirectly on a lambda argument is runtime-dependent
- local alias of compile-time-known local stays compile-time-known
- match-bound value blocks a parent root
- loop-bound value blocks a parent root
- mutable local blocks a parent root
- reassignment blocks a parent root
- top-level checked value lookup stays compile-time-known
- imported checked value lookup stays compile-time-known
- erroneous child result poisons parent without duplicate diagnostics

Success criteria:

- no permanent table is added for all expression summaries
- local summary storage is scoped like the local binding
- effect propagation still goes through slots
- focused effect tests still pass after each conversion slice

## Phase 3: Implement Maximal Root Frames

Goal: root selection has one parent-child replacement rule and no shape-based
root blockers.

Implementation steps:

1. Add root candidate stack storage.
2. Add `beginRootFrame` and `finishRootFrame` helpers.
3. Record `candidate_start` for every expression frame.
4. On compile-time-known/effect-free expression exit:
   - delete candidates added since `candidate_start`
   - append the expression as the frame root
5. On runtime-dependent or effectful expression exit:
   - leave candidates added since `candidate_start`
6. On delayed-effect expression exit:
   - create a delayed parent candidate with the effect slot
   - record the child candidate interval it owns
   - defer parent-versus-children selection
7. Finalize delayed parent candidates after effect finalization.
8. Make nested delayed candidates stable through interval ownership.
9. Preserve root identity for local binding RHS roots when a later lookup uses
   that binding.
10. Preserve pattern extraction roots only for checked destructuring cases that
    need a compile-time value from a checked parent root.
11. Delete root blockers based on leaves, strings, numbers, empty lists,
    records, `return`, `break`, loops, `crash`, `dbg`, and `expect`.

Required tests:

- number literal can be a root when it is the maximal eligible expression
- string literal can be a root when it is the maximal eligible expression
- empty list can be a root when it is the maximal eligible expression
- empty record can be a root when it is the maximal eligible expression
- record containing list selects the record, not the list child
- list inside runtime-dependent record stays as child root
- nested closed block selects the block root
- runtime-dependent block preserves independent closed child roots
- closed `return` expression can be covered by its parent root
- closed `break` expression can be covered by its parent root
- closed `for` expression can be covered by its parent root
- `if` with runtime condition preserves eligible branch child roots
- `match` with runtime scrutinee preserves eligible branch child roots
- effectful parent preserves independent static child root
- direct effectful call blocks containing parent root
- delayed parent resolving pure replaces children
- delayed parent resolving effectful preserves children
- nested delayed parents finalize in stable order
- named top-level constant and equivalent inline expression select equivalent
  roots
- extracting from closed record destructure selects necessary root
- extracting from closed tuple destructure selects necessary root
- extracting from closed tag payload selects necessary root
- extracting from nested destructure selects necessary root

Success criteria:

- parent-child root replacement has one implementation
- child preservation when parents fail has one implementation
- leaf-root tests pass without flooding runtime-dependent parents
- later pruning is no longer needed for correctness

## Phase 4: Compile-Time Evaluation And Diagnostics

Goal: compile-time evaluation consumes selected roots and eligible top-level
values, and `roc check` reports compile-time observables without backend work.

Implementation steps:

1. Define checked output for:
   - selected root requests
   - evaluated root values
   - top-level evaluation diagnostics
   - top-level values evaluated only for diagnostics
2. Schedule eligible top-level values in every checked module.
3. Schedule unreachable eligible top-level values for diagnostics.
4. Schedule selected roots exactly once.
5. Do not schedule child roots removed by parent selection.
6. Ensure compile-time evaluation runs:
   - `crash`
   - `dbg`
   - `expect`
7. Reject effectful calls before the evaluator can execute them.
8. Report evaluation diagnostics through `roc check`.
9. Avoid duplicate diagnostics when a top-level value and a selected root share
   the same checked source.
10. Do not store successful unreachable top-level values in target static data.

Required tests:

- unreachable top-level `crash` reports during `roc check`
- unreachable top-level `dbg` reports during `roc check`
- unreachable failed `expect` reports during `roc check`
- reachable selected-root `crash` reports during `roc check`
- reachable selected-root `dbg` reports during `roc check`
- reachable selected-root failed `expect` reports during `roc check`
- `crash` in an otherwise eligible parent does not block parent root selection
- `dbg` in an otherwise eligible parent does not block parent root selection
- `expect` in an otherwise eligible parent does not block parent root selection
- effectful call inside otherwise compile-time-known expression is not
  evaluated
- successful unreachable top-level value does not appear in target static data
- all modules in an import graph run eligible top-level diagnostics

Success criteria:

- selected roots are the evaluator input
- the evaluator does not decide root eligibility
- unreachable diagnostics work without storing unreachable target data
- compile-time observable diagnostics are visible from `roc check`

## Phase 5: Static Data Output

Goal: reachable evaluated values are emitted as target static data exactly when
their representation is explicit.

Implementation steps:

1. Define explicit static-storable categories:
   - scalar values
   - strings
   - lists with static element bytes
   - records whose fields are static-storable
   - tuples whose elements are static-storable
   - tag values whose payloads are static-storable
   - opaque values whose backing value is static-storable and allowed by checked
     output
2. Define explicit non-storable cases.
3. Store reachable evaluated values only.
4. Share repeated static list bytes when checked values identify the same bytes.
5. Store records that contain lists as records pointing at shared list data.
6. Store tuples and tag payloads that contain lists as values pointing at shared
   list data.
7. Ensure equivalent named and inline values produce equivalent static data.
8. Ensure removed child roots do not produce duplicate static data.
9. Ensure target static-data emission consumes evaluated checked values, not
   source/CIR shape.

Required tests:

- static list bytes are emitted once when shared
- static record points at static list bytes
- repeated records sharing a list share the list bytes
- tuple containing list points at static list bytes
- tag payload containing list points at static list bytes
- repeated sprite sheets share bytes
- sub-sprite records point at sprite sheet bytes
- inline animation cells and named animation cells emit equivalent data
- child roots removed by parent root do not emit duplicate data
- effectful parent does not prevent independent static child data
- unreachable successfully evaluated value is not emitted as target data
- non-storable reachable evaluated value is represented explicitly

Success criteria:

- target data size reflects reachable selected roots, not source arrangement
- no backend scans source/CIR to decide whether a value is static
- Rocci-shaped sprite data is verified by compiler tests before integration

## Phase 6: Delete Old Hoist Machinery

Goal: remove every path that can disagree with the checked root-frame system.

Remove or replace:

- expression-level `does_fx` as root eligibility data
- observable-effect root blockers
- later-hoist suppression rules
- leaf/root pruning rules
- source-shape root predicates
- duplicate dependency verification walks used to repair selection
- root selection paths that can disagree with the maximal-root frame rule
- comments that describe old behavior as the intended design

Required tests:

- static search shows no root blocker for `dbg`, `expect`, or `crash`
- static search shows no root blocker for leaves
- static search shows no root blocker for `return`, `break`, or loops
- focused hoist/root tests pass
- checked module tests pass
- compile-time evaluation tests pass

Success criteria:

- checking has one owner for root selection
- selected roots are the only checked input to compile-time root scheduling
- old helper names may remain only when they are thin names over the new rule
  and cannot disagree with it

## Phase 7: Rocci Bird Integration

Goal: prove the compiler behavior on the original WASM-4 program.

Preparation:

1. Build the local compiler:

   ```sh
   zig build
   ```

2. Build the roc-wasm4 platform host in size mode:

   ```sh
   cd /home/rtfeldman/code/roc-wasm4
   zig build -Doptimize=ReleaseSmall
   ```

3. Format Rocci Bird with the local compiler:

   ```sh
   /home/rtfeldman/code/worktrees/roc/vivid-canyon/roc/zig-out/bin/roc fmt /home/rtfeldman/code/roc-wasm4/examples/rocci-bird.roc
   ```

4. Build Rocci Bird with Roc size optimization:

   ```sh
   cd /home/rtfeldman/code/roc-wasm4
   /home/rtfeldman/code/worktrees/roc/vivid-canyon/roc/zig-out/bin/roc build examples/rocci-bird.roc --opt=size --output=rocci-bird.wasm
   ```

5. Measure the wasm:

   ```sh
   wc -c rocci-bird.wasm
   ```

6. Disassemble the wasm with local tooling:

   ```sh
   wasm-objdump -x -d rocci-bird.wasm > rocci-bird.disasm.txt
   ```

Disassembly checks:

- sprite sheet byte arrays appear in data/static sections, not rebuilt inside
  `update`
- sprite sheet records point at shared byte arrays
- `Sprite.sub_or_crash(rocci_sprite_sheet, ...)` cells are precomputed when
  their inputs are compile-time-known
- animation records are not rebuilt inline in ordinary gameplay paths
- `update` no longer contains repeated byte-by-byte construction of sprite/list
  values
- remaining allocations in ordinary gameplay are counted separately from game
  over paths

Runtime checks:

- optimized Rocci Bird starts and plays correctly
- dev Rocci Bird starts and plays correctly
- equivalent inline and named animation data do not change behavior
- no game logic changes are required for static data improvements

Recorded output:

- final wasm byte count
- code section byte count
- data section byte count
- largest function bodies
- normal-gameplay allocation sites
- comparison to the Rust WASM-4 port

Success criteria:

- Rocci Bird builds with `--opt=size`
- Rocci Bird runs
- final disassembly proves sprite/list data is static
- final disassembly proves animation records are not rebuilt in `update`
- equivalent named and inline constants are no-ops for size and disassembly

## Verification Commands

Focused check tests:

```sh
zig build run-test-zig-module-check -- --test-filter "effect propagation"
zig build run-test-zig-module-check -- --test-filter "hoist roots"
```

Focused compile tests:

```sh
zig build run-test-zig-module-compile -- --test-filter "hoisted"
```

Full checked-module tests:

```sh
zig build run-test-zig-module-check
zig build run-test-zig-module-compile
```

Broader compiler tests at major phase boundaries:

```sh
zig build test
```

Rocci Bird integration:

```sh
cd /home/rtfeldman/code/roc-wasm4
zig build -Doptimize=ReleaseSmall
/home/rtfeldman/code/worktrees/roc/vivid-canyon/roc/zig-out/bin/roc fmt examples/rocci-bird.roc
/home/rtfeldman/code/worktrees/roc/vivid-canyon/roc/zig-out/bin/roc build examples/rocci-bird.roc --opt=size --output=rocci-bird.wasm
wc -c rocci-bird.wasm
wasm-objdump -x -d rocci-bird.wasm > rocci-bird.disasm.txt
```

## Completion Checklist

### Effect Tests

- [x] Direct top-level effectful call test exists.
- [x] Static-dispatch top-level method effect test exists.
- [x] Type-method top-level effect test exists.
- [x] Binop dispatch effect test exists.
- [x] Unary dispatch effect test exists.
- [x] Interpolation dispatch effect test exists.
- [x] Iterator dispatch effect test exists.
- [x] Imported nominal method effect test exists.
- [x] Pure annotation rejects direct effect test exists.
- [x] Pure annotation rejects delayed dispatch effect test exists.
- [x] Effectful annotation accepts direct effect test exists.
- [x] Effectful annotation accepts delayed dispatch effect test exists.
- [x] Pure where-clause accepts pure implementation test exists.
- [x] Pure where-clause rejects effectful implementation test exists.
- [x] Effectful where-clause accepts effectful implementation test exists.
- [x] Effectful where-clause propagates to caller test exists.
- [x] Direct effectful `expect` test exists.
- [x] Delayed dispatch effectful `expect` test exists.
- [x] Function alias effect tests exist.
- [x] Higher-order effect tests exist.
- [x] Closure creation/call effect tests exist.
- [x] Recursive effect tests exist.
- [x] `crash`/`dbg`/`expect` non-effect tests exist.

### Effect Implementation

- [x] Effect slots exist for function bodies.
- [x] Effect slots exist for top-level value right-hand sides.
- [x] Effect slots exist for `expect` bodies.
- [x] Effect slots exist for delayed-effect root candidates.
- [x] Active effect-slot stack exists.
- [x] Direct effectful calls mark active slots.
- [x] Calls to known local functions add directed effect edges.
- [x] Calls through function parameters use checked function effect kinds.
- [x] Closure creation does not mark the active slot effectful.
- [x] Dispatch function variables record active-slot watchers.
- [x] Dispatch watchers cover receiver method calls.
- [x] Dispatch watchers cover type-method calls.
- [x] Dispatch watchers cover binop and unary dispatch.
- [x] Dispatch watchers cover interpolation dispatch.
- [x] Dispatch watchers cover iterator dispatch.
- [x] Dispatch watchers cover where-clause dispatch.
- [x] Dispatch watchers cover imported nominal method dispatch.
- [x] Dispatch resolution marks watcher slots when selected methods are
      effectful.
- [x] Pure where-clause implementation checking rejects effectful methods.
- [x] Effectful where-clause calls propagate to callers.
- [x] Function effect kinds finalize before checked function output.
- [x] Recursive effect groups solve with directed SCC propagation.
- [x] Top-level effect errors use finalized effect slots.
- [x] `expect` effect errors use finalized effect slots.
- [x] Checked modules export/import explicit effect summaries.

### Runtime Dependency And Root Selection

- [x] `checkExpr` returns `ExprCheckResult`.
- [x] Blocks and statements combine `ExprCheckResult`.
- [x] Expression effect propagation no longer depends on old `does_fx`.
- [x] Immutable local binding summaries are stored only when later lookups need
      them.
- [x] Runtime dependency from lambda arguments is detected by the new summary.
- [x] Runtime dependency from match-bound values is detected by the new summary.
- [x] Runtime dependency from loop-bound values is detected by the new summary.
- [x] Runtime dependency from mutable variables and reassignment is detected by
      the new summary.
- [x] Top-level checked value lookups are compile-time-known.
- [x] Imported checked value lookups are compile-time-known.
- [x] Root-candidate stack exists.
- [x] Every expression uses root frames.
- [x] Parent root selection removes child roots in the same frame.
- [x] Rejected parent roots preserve eligible child roots.
- [x] Delayed-effect parent roots finalize correctly.
- [x] Nested delayed-effect parents finalize correctly.
- [x] Leaf/root pruning rules are removed.
- [x] Observable-effect root blockers are removed.
- [x] `return`/`break`/loop syntax root blockers are removed.

### Compile-Time Evaluation

- [x] Compile-time evaluation consumes final selected roots from checking.
- [ ] Eligible top-level expressions are evaluated in every module.
- [ ] Eligible top-level expressions are evaluated even when unreachable.
- [x] Eligible selected roots are evaluated.
- [x] Removed child roots are not evaluated separately.
- [x] `crash` runs during compile-time evaluation.
- [ ] `dbg` runs during compile-time evaluation.
- [ ] `expect` runs during compile-time evaluation.
- [ ] `roc check` reports compile-time `crash`, `dbg`, and failed `expect`
      diagnostics.
- [ ] Duplicate diagnostics from shared top-level/root sources are avoided.
- [ ] Effectful calls are rejected before compile-time evaluation can run them.

### Static Data

- [ ] Checked output separates evaluated values from target static data.
- [x] Checked module output stores only reachable evaluated values.
- [ ] Static-storable value categories are explicit.
- [ ] Non-storable reachable evaluated values are explicit.
- [x] Static list bytes are shared.
- [x] Static records point at static list data.
- [ ] Static tuple/tag payloads point at static list data where applicable.
- [ ] Equivalent named and inline constants produce equivalent static data.
- [x] Removed child roots do not emit duplicate static data.

### Cleanup

- [ ] Old hoist selection machinery is deleted or fully replaced.
- [ ] No post-check stage infers root eligibility from source/CIR shape.
- [x] No root blocker remains for `dbg`, `expect`, or `crash`.
- [x] No root blocker remains for leaf expressions.
- [x] No root blocker remains for `return`, `break`, or loops.
- [x] Focused effect tests pass.
- [x] Focused root tests pass.
- [x] Compile-time evaluation/static data tests pass.
- [x] Full `zig build run-test-zig-module-check` passes.
- [x] Full `zig build run-test-zig-module-compile` passes.
- [ ] Broader compiler tests pass at major phase boundaries.

### Rocci Bird

- [ ] Rocci Bird formats successfully.
- [ ] roc-wasm4 host builds with `zig build -Doptimize=ReleaseSmall`.
- [ ] Rocci Bird builds with `--opt=size`.
- [ ] Rocci Bird runs.
- [ ] Rocci Bird disassembly confirms sprite/list data is static.
- [ ] Rocci Bird disassembly confirms animation records are not rebuilt in
      `update`.
- [ ] Rocci Bird disassembly confirms equivalent inline/top-level constants are
      equivalent.
- [ ] Rocci Bird optimized wasm size is recorded.
- [ ] Rocci Bird remaining normal-gameplay allocations are recorded.
- [ ] Final pass over this plan confirms every checklist item is genuinely
      complete before marking the goal complete.
