# Effect Propagation And Compile-Time Root Plan

## Objective

Build one checked-stage system that decides Roc effectfulness, compile-time
evaluation roots, compile-time diagnostics, and static constant output without
later recovery passes.

The compiler outcome must satisfy these requirements:

- every module is checked for eligible compile-time evaluation
- unreachable top-level values still run eligible `crash`, `dbg`, and `expect`
  during `roc check`
- effectful calls never run at compile time
- `crash`, `dbg`, and `expect` are never treated as effectful calls
- static-dispatch effectfulness is handled with the same checked dataflow as
  direct calls
- root selection keeps maximal eligible roots and preserves eligible children
  when a parent is runtime-dependent or effectful
- named constants and equivalent inline constants produce equivalent selected
  roots
- reachable evaluated static data is stored once and shared where possible
- backends consume checked roots and checked constant data directly

The integration target is Rocci Bird:

- sprite sheet lists should be static data
- sprite sheet records should be static data
- `Sprite.sub_or_crash(...)` cells inside animation constructors should be
  evaluated at compile time when their inputs are compile-time-known
- equivalent named and inline animation data should compile the same way
- `update` should not rebuild those sprites or allocate their list bytes during
  ordinary gameplay
- the final `--opt=size` WASM-4 binary should be measured and disassembled

## Current Branch Baseline

The branch already has meaningful effect work:

- effect slots exist for function bodies, top-level value right-hand sides, and
  `expect` bodies
- direct effectful calls mark active slots
- known local function calls add directed caller-to-callee edges
- calls through function-typed values use their checked effect kind
- dispatch watchers cover receiver methods, type methods, binops, unary ops,
  interpolation, iterators, where-clause dispatch, and imported nominal methods
- top-level and `expect` diagnostics are emitted from finalized slots
- `dbg`, `expect`, and `crash` no longer block roots as "observable effects"
- `return`, `break`, and loop syntax are no longer root blockers

The branch still has old root-selection machinery:

- `checkExpr` still returns the old expression-level `bool`
- root selection still depends on shape predicates and later pruning
- leaf roots cannot be enabled safely yet, because the old selector floods
  runtime-dependent parents with child candidates
- delayed-effect root candidates do not have their own effect slots
- selected roots are not yet produced by a single maximal-root frame rule
- compile-time evaluation and static data still need to consume the final root
  output and prove the Rocci Bird static-data behavior

## Non-Negotiable Invariants

- Checking is the last user-facing stage for type errors, effect errors,
  static-dispatch errors, compile-time `crash`, compile-time `dbg`, and
  compile-time `expect`.
- Every later stage consumes explicit checked output. Later stages must not
  guess effectfulness, root eligibility, or static data ownership by scanning
  CIR, source names, function bodies, generated wasm, object symbols, or
  backend code.
- Effect dependencies are directed. A caller depending on a callee is not
  equality.
- Union-find is not the effect solver. Recursive groups may be condensed, but
  ordinary caller-to-callee dependencies must remain one-way.
- Static dispatch contributes effectfulness through resolved checked method
  effects, not through source spelling, `!` names, or unresolved type variables.
- An unresolved pure answer is not final while dispatch watchers or callee
  slots can still change.
- Function creation is not effectful merely because the body is effectful.
  Calling the function propagates the body's effectfulness.
- Effectful functions do not run during compile-time evaluation.
- `crash`, `dbg`, and `expect` are compile-time observables, not effectful
  calls.
- Compile-time root selection is based on runtime dependency and effectfulness,
  not expression shape.
- There are no root blockers for numbers, strings, empty lists, records,
  `return`, `break`, loops, blocks, `crash`, `dbg`, or `expect`.
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
  data, that limitation must be represented explicitly in checked/static-data
  output. It must not be hidden behind backend cleanup.

## Data Model

### Effect Slots

The checker owns sparse effect slots for boundaries whose effectfulness is
checked output:

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
slots because their final root decision can depend on dispatch resolution after
their expression returns.

### Expression Check Result

Replace expression-level `bool` propagation with a small transient result:

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

The exact names can change during implementation, but the ownership must not:

- `RuntimeDep` is computed bottom-up during the existing expression walk
- `RootEffect` is derived from active effect slots and direct function-typed
  call information
- parents combine child summaries immediately
- immutable local definitions store only the summary later local lookups need
- ordinary expression summaries are not stored after the parent consumes them

### Root Candidates

The checker maintains a stack of candidate roots selected so far. Every
expression frame records the candidate stack length at entry.

Candidate payloads must identify:

- the checked expression to evaluate
- the checked local binding pattern, when preserving local sharing requires it
- the body shape needed by compile-time evaluation
- the effect slot when parent selection is delayed
- the child candidate interval owned by the delayed parent

When an expression exits:

- if it is compile-time-known and effect-free, remove candidates added inside
  the frame and append the parent
- if it is runtime-dependent or effectful, leave candidates added inside the
  frame alone
- if it has delayed effectfulness, store a delayed parent tied to its effect
  slot and child interval

After effect finalization:

- delayed parents that resolve effect-free replace their child interval
- delayed parents that resolve effectful are discarded and their children stay
- no other rule removes child roots

## Phase 1: Finish Effect Soundness

The current branch has most of this implemented. This phase finishes the parts
that must be stable before root selection can depend on effects.

Implementation steps:

1. Audit every effect slot creation site:
   - function bodies
   - lambda bodies
   - top-level value right-hand sides
   - `expect` bodies
   - delayed root candidates
2. Add `const_root_candidate` slots when a root candidate's effect is delayed.
3. Replace recursive ad hoc effect solving with directed SCC propagation:
   - build slot graph from `effect_edges`
   - condense strongly connected groups
   - mark a group effectful if any slot in it has `direct_effect`
   - propagate group effectfulness along caller-to-callee edges
   - write final answers back to slots
4. Ensure finalized negative answers are written only after all relevant
   dispatch watchers have resolved.
5. Ensure checked module data records explicit effect summaries for exported
   values and imports.
6. Ensure imported effect summaries feed direct calls and dispatch-selected
   methods exactly like local checked effects.

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
- directed recursive effect groups solve without recursion in the solver
- imported checked effects behave the same as local checked effects

## Phase 2: Replace Expression `bool` With `ExprCheckResult`

This phase changes expression checking to return runtime-dependency/root data
instead of using expression-level `bool` effect propagation.

Implementation steps:

1. Introduce `ExprCheckResult` and helper constructors:
   - compile-time-known/effect-free
   - compile-time-known/effectful
   - compile-time-known/delayed
   - runtime-dependent
   - poisoned
2. Update `checkExpr` to return `ExprCheckResult`.
3. Update block and statement checking to combine child results.
4. Preserve existing effect-slot marking while return types change.
5. Store immutable local RHS summaries only when a later local lookup can read
   them.
6. On local lookup, return the stored summary for immutable compile-time-known
   locals.
7. Mark lookup of lambda arguments as runtime-dependent.
8. Mark lookup of match-bound values as runtime-dependent unless they are
   explicitly introduced by a compile-time pattern extraction root.
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
- immutable local depending indirectly on a lambda argument is
  runtime-dependent
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

This phase makes root selection a single frame rule and removes shape-based
selection decisions.

Implementation steps:

1. Add root candidate stack storage.
2. Add `beginRootFrame` and `finishRootFrame` helpers.
3. Have every expression frame record `candidate_start`.
4. On compile-time-known/effect-free expression exit:
   - delete candidates added since `candidate_start`
   - append the expression as the frame's root
5. On runtime-dependent or effectful expression exit:
   - leave candidates added since `candidate_start` in place
6. On delayed-effect expression exit:
   - create a delayed parent candidate with the effect slot
   - record the child candidate interval it owns
   - do not decide parent versus children yet
7. Finalize delayed parent candidates after effect finalization.
8. Make finalization stable when delayed candidates are nested.
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
- the old selector's later pruning is no longer needed for correctness

## Phase 4: Compile-Time Evaluation

This phase consumes the final selected roots and evaluates all eligible
top-level values needed for checking diagnostics.

Implementation steps:

1. Define checked output for:
   - selected root requests
   - evaluated root values
   - top-level evaluation diagnostics
   - top-level values evaluated only for diagnostics
2. Ensure every module schedules eligible top-level values for evaluation,
   including unreachable ones.
3. Ensure selected roots are scheduled exactly once.
4. Ensure child roots removed by parent selection are not scheduled.
5. Ensure compile-time evaluation runs:
   - `crash`
   - `dbg`
   - `expect`
6. Ensure effectful calls are rejected before the evaluator can execute them.
7. Ensure evaluation diagnostics are reported by `roc check`.
8. Ensure duplicate diagnostics are not emitted when a top-level value and a
   selected root share the same checked source.
9. Ensure successful unreachable top-level values do not force target static
   data output.

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

- `roc check` reports compile-time observables without building backend code
- selected roots are the evaluator's input; the evaluator does not decide root
  eligibility
- unreachable diagnostics work without storing unreachable target data

## Phase 5: Static Data Output

This phase stores reachable evaluated values as target static data when their
representation is known.

Implementation steps:

1. Define explicit static-storable categories:
   - scalar values
   - strings
   - lists with static element bytes
   - records whose fields are static-storable
   - tuples whose elements are static-storable
   - tag values whose payloads are static-storable
   - opaque values whose backing value is static-storable and allowed by the
     checked output
2. Define explicit non-storable cases instead of backend guessing.
3. Store reachable evaluated values only.
4. Share repeated static list bytes when the checked values identify the same
   bytes.
5. Store records that contain lists as records pointing at shared list data.
6. Ensure equivalent named and inline values produce equivalent static data.
7. Ensure removed child roots do not produce duplicate static data.
8. Ensure target static-data emission consumes evaluated checked values, not
   source/CIR shape.

Required tests:

- static list bytes are emitted once when shared
- static record points at static list bytes
- repeated records sharing a list share the list bytes
- repeated sprite sheets share bytes
- sub-sprite records point at the sprite sheet bytes
- inline animation cells and named animation cells emit equivalent data
- child roots removed by parent root do not emit duplicate data
- effectful parent does not prevent independent static child data
- unreachable successfully evaluated value is not emitted as target data
- non-storable reachable evaluated value is represented explicitly

Success criteria:

- target data size reflects reachable selected roots, not source arrangement
- no backend scans source/CIR to decide whether a value is static
- Rocci-shaped sprite data can be checked with unit tests before integration

## Phase 6: Delete Old Hoist Machinery

This phase removes the old system after the new effect/root/evaluation path is
covered by tests.

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
- focused hoist/root tests still pass
- checked module tests still pass
- compile-time evaluation tests still pass

Success criteria:

- checking has one owner for root selection
- selected roots are the only checked input to compile-time root scheduling
- old helper names may remain only if they are thin names over the new behavior
  and cannot disagree with it

## Phase 7: Rocci Bird Integration

This phase proves the compiler behavior on the original motivating program.

Preparation:

1. Build the local compiler in debug mode:

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

6. Disassemble the wasm with the available local tool:

   ```sh
   wasm-objdump -x -d rocci-bird.wasm > rocci-bird.disasm.txt
   ```

7. If `wasm-objdump` is unavailable, use the repo's existing wasm inspection
   path or the bundled LLVM wasm object tooling. Do not add a compiler
   dependency on a PATH lookup.

Disassembly checks:

- sprite sheet byte arrays appear in data/static sections, not rebuilt inside
  `update`
- sprite sheet records point at the shared byte arrays
- `Sprite.sub_or_crash(rocci_sprite_sheet, ...)` cells are precomputed when
  their inputs are compile-time-known
- animation records are not rebuilt inline in ordinary gameplay paths
- the `update` body no longer contains repeated byte-by-byte construction of
  sprite/list values
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

## Full Test Matrix

### Effect Edge Cases

- direct call to known effectful function
- call through local function alias
- call through imported function alias
- call through pure function parameter
- call through effectful function parameter
- closure creation with effectful body
- closure call with effectful body
- receiver static dispatch
- type-method static dispatch
- binop static dispatch
- unary static dispatch
- interpolation static dispatch
- synthetic iterator static dispatch
- pure where-clause with pure implementation
- pure where-clause with effectful implementation
- effectful where-clause with effectful implementation
- imported nominal method dispatch
- direct effectful call in `expect`
- delayed effectful dispatch in `expect`
- self-recursive pure function
- self-recursive effectful function
- mutual recursion with one effectful member
- mutual recursion with all pure members
- `dbg` around pure expression
- `dbg` around effectful expression
- `crash` in pure expression
- `expect` in pure expression

### Runtime Dependency Edge Cases

- lambda argument lookup
- nested lambda argument lookup
- immutable local depending directly on runtime value
- immutable local depending indirectly on runtime value
- immutable local independent of runtime value
- local alias chain
- top-level value lookup
- imported value lookup
- match-bound value lookup
- loop-bound value lookup
- mutable local lookup
- reassigned local lookup
- branch-local binding lookup
- destructured binding lookup
- pattern extraction from selected parent root
- erroneous binding lookup

### Root Selection Edge Cases

- parent root replaces child roots
- rejected parent preserves child roots
- delayed parent resolving pure replaces child roots
- delayed parent resolving effectful preserves child roots
- nested delayed parents finalize in stable order
- leaf number root
- leaf string root
- empty list root
- empty record root
- record containing static list
- tuple containing static list
- tag containing static payload
- block containing internal locals
- `if` with compile-time-known condition
- `if` with runtime condition
- `match` with compile-time-known scrutinee
- `match` with runtime scrutinee
- `return` inside eligible parent
- `break` inside eligible parent
- `for` expression inside eligible parent
- direct effectful call in parent
- delayed effectful call in parent
- function value with effectful body
- function call with effectful body
- named top-level constant
- equivalent inline constant
- unused top-level constant
- unused local constant

### Compile-Time Observable Edge Cases

- reachable compile-time `crash`
- unreachable top-level `crash`
- reachable compile-time `dbg`
- unreachable top-level `dbg`
- reachable compile-time failed `expect`
- unreachable top-level failed `expect`
- compile-time `expect` success
- compile-time observable inside selected parent root
- compile-time observable inside child root preserved by rejected parent
- duplicate observable source reached by top-level and selected root

### Static Data Edge Cases

- scalar constant
- string constant
- list constant
- record of scalars
- record containing list
- tuple containing list
- tag containing list
- repeated list bytes
- repeated records sharing one list
- opaque value backed by storable data
- non-storable reachable value
- unreachable storable value
- removed child root
- imported static constant
- Rocci sprite sheet
- Rocci sub-sprite
- Rocci animation cell
- Rocci animation record

## Verification Commands

Run focused tests while developing:

```sh
zig build run-test-zig-module-check -- --test-filter "effect propagation"
zig build run-test-zig-module-check -- --test-filter "hoist roots"
```

Run the full check module before committing compiler changes:

```sh
zig build run-test-zig-module-check
```

Run broader compiler tests after major phase boundaries:

```sh
zig build test
```

Run roc-wasm4 integration after compiler tests pass:

```sh
cd /home/rtfeldman/code/roc-wasm4
zig build -Doptimize=ReleaseSmall
/home/rtfeldman/code/worktrees/roc/vivid-canyon/roc/zig-out/bin/roc fmt examples/rocci-bird.roc
/home/rtfeldman/code/worktrees/roc/vivid-canyon/roc/zig-out/bin/roc build examples/rocci-bird.roc --opt=size --output=rocci-bird.wasm
wc -c rocci-bird.wasm
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
- [ ] Effect slots exist for delayed-effect root candidates.
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

### Root Selection

- [x] `checkExpr` returns `ExprCheckResult`.
- [x] Blocks and statements combine `ExprCheckResult`.
- [x] Expression effect propagation no longer depends on old `does_fx`.
- [x] Immutable local binding summaries are stored only when later lookups need
  them.
- [x] Runtime dependency from lambda arguments is detected by the new summary.
- [x] Runtime dependency from match-bound values is detected by the new
  summary.
- [x] Runtime dependency from loop-bound values is detected by the new summary.
- [x] Runtime dependency from mutable variables and reassignment is detected by
  the new summary.
- [x] Top-level checked value lookups are compile-time-known.
- [x] Imported checked value lookups are compile-time-known.
- [x] Root-candidate stack exists.
- [x] Every expression uses root frames.
- [x] Parent root selection removes child roots in the same frame.
- [x] Rejected parent roots preserve eligible child roots.
- [ ] Delayed-effect parent roots finalize correctly.
- [ ] Nested delayed-effect parents finalize correctly.
- [x] Leaf/root pruning rules are removed.
- [x] Observable-effect root blockers are removed.
- [x] `return`/`break`/loop syntax root blockers are removed.

### Compile-Time Evaluation

- [ ] Compile-time evaluation consumes final selected roots from checking.
- [ ] Eligible top-level expressions are evaluated in every module.
- [ ] Eligible top-level expressions are evaluated even when unreachable.
- [ ] Eligible selected roots are evaluated.
- [ ] Removed child roots are not evaluated separately.
- [ ] `crash` runs during compile-time evaluation.
- [ ] `dbg` runs during compile-time evaluation.
- [ ] `expect` runs during compile-time evaluation.
- [ ] `roc check` reports compile-time `crash`, `dbg`, and failed `expect`
      diagnostics.
- [ ] Duplicate diagnostics from shared top-level/root sources are avoided.
- [ ] Effectful calls are rejected before compile-time evaluation can run them.

### Static Data

- [ ] Checked output separates evaluated values from target static data.
- [ ] Checked module output stores only reachable evaluated values.
- [ ] Static-storable value categories are explicit.
- [ ] Non-storable reachable evaluated values are explicit.
- [ ] Static list bytes are shared.
- [ ] Static records point at static list data.
- [ ] Static tuple/tag payloads point at static list data where applicable.
- [ ] Equivalent named and inline constants produce equivalent static data.
- [ ] Removed child roots do not emit duplicate static data.

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
