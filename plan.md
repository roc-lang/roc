# Effect Propagation, Compile-Time Roots, And Static Data Plan

## Objective

Replace the current hoisting/root-selection behavior with one checked-stage
system that owns:

- Roc effect validation
- compile-time root eligibility
- maximal compile-time root selection
- compile-time `crash`, `dbg`, and `expect` diagnostics
- evaluated constant output
- reachable static-data output

The target result is that every module is considered for compile-time
evaluation, effectful calls never run at compile time, `crash`/`dbg`/`expect`
run at compile time whenever their enclosing expression is eligible, and named
top-level constants compile the same as equivalent closed inline expressions.

The motivating integration proof is Rocci Bird: sprite sheets, sprite records,
`Sprite.sub_or_crash(...)` cells, and animation records must end up in static
data when their inputs are compile-time-known, and ordinary gameplay must not
rebuild those values inside `update`.

## Invariants

- Checking is the last user-facing compiler stage for type errors, effect
  errors, static-dispatch errors, compile-time `crash`, compile-time `dbg`, and
  compile-time `expect`.
- Later stages consume explicit checked outputs. They must not infer
  effectfulness, root eligibility, static-data ownership, or constant identity
  from source shape, CIR shape, function bodies, wasm bytes, object symbols, or
  backend code.
- Compile-time root eligibility depends only on runtime data dependency,
  checked control reachability, and effectfulness.
- Effectful calls do not run at compile time.
- `crash`, `dbg`, and `expect` are compile-time observables, not effectful
  calls. They must not block root selection.
- Function creation is not an effectful call, even when the function body is
  effectful. Calling the function propagates the body effect.
- Static dispatch contributes effectfulness through resolved checked method
  effects, not through source spelling.
- Negative effect answers are not final while any callee slot or static-dispatch
  watcher can still change.
- Effect dependencies are directed. A caller depending on a callee is not
  equality. Recursive groups may be condensed, but ordinary caller-to-callee
  edges remain one-way.
- Union-find is not the effect solver.
- Root selection has one parent-child replacement rule: an eligible parent
  replaces child candidates in its own expression frame.
- Rejecting a parent for runtime dependency or effectfulness preserves eligible
  unconditionally reached child candidates.
- Runtime-controlled branch bodies, match guards, and match branch values do
  not publish independent candidates. They contribute summaries to the
  enclosing control expression.
- Module-level lookup expressions are compile-time-known references to checked
  module-level bindings. They do not inherit the initializer's transient
  expression summary at each use site.
- Checking a module-level definition as a dependency while another expression
  frame is active must detach the transient hoist-frame and candidate stacks.
  The dependency definition still writes shared checked outputs, selected
  roots, delayed roots, effect slots, and known bindings, but it must not bubble
  runtime dependency or child candidates into the lookup that forced it.
- `return` and `break` are not standalone stored-value roots without an
  explicit checked continuation representation. Their payloads may still
  contribute through checked control data.
- Leaves, strings, numbers, empty lists, records, loops, `crash`, `dbg`, and
  `expect` are not special root blockers.
- Ordinary expression summaries are stack-local. Store a summary only when a
  later local lookup, effect finalization, delayed root candidate, or checked
  output needs it.
- Compile-time evaluation and static storage are separate outputs. Unreachable
  eligible top-level values are evaluated for diagnostics, but successfully
  evaluated unreachable data is not forced into target static data.
- Backends do not rediscover hoisting or static-data eligibility.

## Architecture

### Effect Slots

The checker owns sparse effect slots only for boundaries that need checked
effect answers:

- function bodies
- lambda bodies
- top-level value right-hand sides
- `expect` bodies
- delayed compile-time root candidates

Slots are updated by explicit events:

- direct call to a known effectful function marks the active slot effectful
- direct call to a local function with a slot adds a caller-to-callee edge
- call through a function-typed value consumes the checked function effect kind
- unresolved static dispatch records a watcher from the dispatch variable to
  the active slot
- static-dispatch resolution marks or connects watcher slots from the selected
  checked method effect

Effect finalization builds the directed slot graph, condenses SCCs only for
recursive groups, propagates effectfulness to callers, and writes final checked
effect kinds before checked module output.

### Expression Results

`checkExpr` returns a transient result to its parent:

```zig
const ExprCheckResult = struct {
    runtime_dep: RuntimeDep,
    control_dep: ControlDep,
    root_effect: RootEffect,
};
```

`RuntimeDep` tells whether the expression can be known without runtime data.
`ControlDep` tells whether the expression may publish an independent candidate
or is under a runtime-controlled branch/guard/branch-value. `RootEffect` is a
view of effect-slot state; it is not a second effect solver.

### Root Candidate Frames

Every expression frame records the root-candidate stack length at entry.

Frame exit rules:

- compile-time-known, unconditionally reached, effect-free: remove candidates
  added inside the frame and append the parent candidate
- runtime-dependent or effectful: keep eligible candidates added inside the
  frame
- runtime-controlled branch/guard/branch-value: suppress independent candidate
  publication inside the conditional region and return summaries to the
  enclosing `if` or `match`
- delayed effectfulness: append a tentative parent candidate tied to the effect
  slot and record the child candidate interval it owns

After effect finalization:

- delayed parent resolves effect-free: keep the parent and remove its child
  interval
- delayed parent resolves effectful: discard the parent and keep finalized
  children

This interval rule is the only subsumption rule.

## Phase 0: Baseline Audit

Tasks:

- [x] Inventory current effect-slot storage, dispatch watchers, and finalization
  points.
- [x] Inventory current hoist/root selection helpers.
- [x] Identify every current source-shape filter for roots.
- [x] Identify every current observable-effect filter for roots.
- [x] Identify every later pass that repairs, prunes, or reinterprets selected
  roots.
- [ ] Record current Rocci Bird `--opt=size` byte size and disassembly with
  named constants.
- [ ] Record current Rocci Bird `--opt=size` byte size and disassembly with
  equivalent inline constants.

Success criteria:

- [x] There is a written map from old implementation paths to replacement
  phases.
- [x] Every old path has an owner phase for deletion or conversion.

Current implementation map:

- Effect storage lives in `src/check/Check.zig`: `EffectSlotId`,
  `EffectSlot`, `EffectEdge`, `DispatchEffectWatch`, `effect_slots`,
  `effect_edges`, `dispatch_effect_watches`, `active_effect_slots`, and
  `function_effect_slots_by_pattern`.
- Effect finalization lives in `resolveEffectSlots`, which builds directed
  caller-to-callee edges, condenses SCCs, and propagates effectfulness back to
  callers. `effectSlotIsEffectful` consumes that result; mutations invalidate
  cached resolutions.
- Dispatch watcher ownership lives in `recordDispatchEffectWatch`,
  `markExprResultDelayedOnDispatch`, and `reportEffectfulDispatch`.
  Watcher resolution marks the owning slot effectful and reports top-level or
  `expect` errors from that slot.
- Root selection state lives in `ExprCheckResult`, `HoistFrame`,
  `DelayedHoistRoot`, `HoistSelectionTransaction`, `hoist_expr_candidates`,
  `hoist_delayed_roots`, `hoist_known_values`, `hoist_selected_exprs`,
  `hoist_selected_bindings`, and `selected_hoisted_roots`.
- Current source-shape filters are concentrated in
  `exprCanBeStandaloneConstRoot`, `exprCanCoverConstRootChildren`, and
  `exprCanBeBindingConstRoot`. Phase 3 owns replacing these with expression
  result and frame policy; Phase 6 owns deleting any leftover blockers.
- Current observable-effect audit: `crash`, `dbg`, and `expect` are allowed by
  the root helpers and tested semantically. `expect_err` is still treated
  specially in child-covering policy, so Phase 6 must keep auditing it while
  deleting old source-shape filters.
- Later root repair/pruning currently lives in
  `finalizeDelayedHoistedRoots`, `finalizeSelectedHoistedRootsAfterSolving`,
  `stageExprDependenciesInternal`, and dependency/concreteness checks on
  selected roots. Phase 4 owns checked output/evaluation scheduling, Phase 5
  owns static-data output, and Phase 6 owns deleting any duplicate selection
  logic that can disagree with root frames.

## Phase 1: Effect Soundness

Tasks:

- [x] Introduce or normalize `EffectSlotId`, `EffectSlot`, `EffectEdge`, and
  dispatch watcher storage.
- [x] Create effect slots for function bodies, lambdas, top-level values,
  `expect` bodies, and delayed root candidates.
- [x] Track the active effect slot while checking each boundary.
- [x] Mark active slots for direct calls to checked effectful functions.
- [x] Add directed caller-to-callee edges for calls to local functions with
  slots.
- [x] Consume checked function effect kinds for calls through function-typed
  values.
- [x] Register dispatch watchers for unresolved static-dispatch calls.
- [x] Resolve watcher slots from selected checked method effects.
- [x] Implement directed SCC finalization for recursive effect groups.
- [x] Finalize negative answers only after callees and dispatch watchers settle.
- [x] Emit checked effect summaries for local and imported checked modules.
- [x] Report top-level and `expect` effect errors from finalized slots.

Implementation evidence:

- `beginEffectSlot` and `endEffectSlot` maintain the active slot stack.
  Top-level values, `expect` bodies, and lambda/function bodies create slots;
  delayed root candidates allocate `const_root_candidate` slots.
- `markActiveEffectSlotEffectful` records direct effects, and calls through
  function-typed values consume the checked `fn_pure`/`fn_unbound`/`fn_effectful`
  function kind at the call site.
- `recordDirectCalleeEffectDependency` adds caller-to-callee edges for local
  function calls with slots; `resolveEffectSlots` keeps those dependencies
  directed and uses SCC condensation only to solve recursive groups.
- Static dispatch constraints call `recordDispatchEffectWatch` when the
  dispatch function variable is created. `reportEffectfulDispatch` resolves the
  watched slot from the selected checked method effect and reports from the
  owning top-level or `expect` slot.
- Function effect summaries are represented by the checked function type kind
  selected after slot finalization, and expression summaries are retained only
  for checked top-level values and checked function/lambda bodies.

Tests:

- [x] direct effectful top-level call reports `effectful_top_level`
- [x] delayed receiver method call at top level reports `effectful_top_level`
- [x] delayed type-method call at top level reports `effectful_top_level`
- [x] effectful binop dispatch at top level reports `effectful_top_level`
- [x] effectful unary dispatch at top level reports `effectful_top_level`
- [x] interpolation dispatch propagates effectfulness
- [x] synthetic iterator dispatch propagates effectfulness
- [x] imported nominal method dispatch propagates effectfulness
- [x] pure annotation rejects direct effectful call
- [x] pure annotation rejects delayed effectful method call
- [x] effectful annotation accepts direct effectful call
- [x] effectful annotation accepts delayed effectful method call
- [x] pure where-clause accepts pure implementation
- [x] pure where-clause rejects effectful implementation
- [x] effectful where-clause accepts effectful implementation
- [x] effectful where-clause call makes caller effectful
- [x] direct effectful call in `expect` reports `effectful_expect`
- [x] delayed effectful dispatch in `expect` reports `effectful_expect`
- [x] local function alias preserves effectfulness
- [x] imported function alias preserves effectfulness
- [x] higher-order pure function parameter stays pure when called
- [x] higher-order effectful function parameter makes caller effectful
- [x] closure creation with effectful body is pure
- [x] calling closure with effectful body is effectful
- [x] boxed lambda creation with effectful body is pure
- [x] calling boxed lambda with effectful body is effectful
- [x] self-recursive pure function stays pure
- [x] self-recursive effectful function is effectful
- [x] mutual recursion with one effectful member propagates to the group
- [x] mutual recursion where every member is pure stays pure
- [x] `dbg` around pure value is not effectful
- [x] `dbg` around effectful call reports through the call
- [x] `crash` in otherwise pure value is not effectful
- [x] `expect` in otherwise pure value is not effectful

## Phase 2: Expression Results

Tasks:

- [x] Introduce `ExprCheckResult` and helper constructors.
- [x] Convert `checkExpr` to return `ExprCheckResult`.
- [x] Convert block and statement checking to combine expression results.
- [x] Preserve effect-slot updates while changing return types.
- [x] Store immutable local RHS summaries only when later local lookup needs
  them.
- [x] Return stored summaries for immutable compile-time-known local lookups.
- [x] Mark lambda parameters runtime-dependent.
- [x] Mark match-bound values runtime-dependent unless introduced by checked
  compile-time pattern extraction.
- [x] Mark loop-bound values runtime-dependent.
- [x] Mark mutable and reassigned locals runtime-dependent.
- [ ] Treat checked top-level value lookups as compile-time-known unless their
  checked summaries say otherwise.
- [ ] Treat imported checked value lookups as compile-time-known unless their
  imported summaries say otherwise.
- [x] Treat module-level lookups as checked binding identities instead of
  including the initializer's transient expression summary at each use site.
- [x] Detach hoist-frame and candidate stacks when a forward module-level
  lookup checks another module-level definition as a dependency.
- [x] Remove old expression-level `does_fx` as a root eligibility input.

Implementation evidence:

- `ExprCheckResult` carries runtime dependency, root-effect state, and
  runtime-source data; `checkExpr`, `checkBlockStatements`, `checkIfElseExpr`,
  `checkMatchExpr`, unary/binop helpers, and iterator checking return and
  combine it.
- Local immutable summaries are scoped in `local_check_summaries`; local lookup
  consumes them before deciding whether the lookup is compile-time-known or
  runtime-dependent.
- Lambda parameters, loop-bound values, mutable bindings, and reassignments are
  recorded as runtime-dependent summaries. Match branch binders get contextual
  binding summaries, with pattern extraction roots used only for checked
  compile-time extraction cases.
- Effect state still flows through effect slots; `ExprCheckResult.root_effect`
  is only the root-selection view of the finalized/delayed effect state.
- Static-dispatch failures now return an explicit problem result from
  `checkStaticDispatchConstraints`; `checkExpr` turns that into a poisoned
  expression summary so erroneous children cannot make parent roots eligible.
- There is no remaining `does_fx` root-eligibility input in the checker.

Tests:

- [x] closed top-level list literal returns compile-time-known
- [x] closed top-level record containing a list returns compile-time-known
- [x] immutable local independent of a lambda argument is compile-time-known
- [x] immutable local depending directly on a lambda argument is runtime-dependent
- [x] immutable local depending indirectly on a lambda argument is runtime-dependent
- [x] local alias of compile-time-known local stays compile-time-known
- [x] match-bound value blocks a containing parent root
- [x] loop-bound value blocks a containing parent root
- [x] mutable local blocks a containing parent root
- [x] reassignment blocks a containing parent root
- [x] top-level checked value lookup stays compile-time-known
- [x] imported checked value lookup stays compile-time-known
- [x] forward top-level constant lookup does not poison the forcing expression
  with the initializer's transient runtime dependency
- [x] order of first and later top-level constant lookups does not change
  compile-time root selection
- [x] erroneous child result poisons the parent without duplicate diagnostics

## Phase 3: Maximal Root Selection

Tasks:

- [x] Add root-candidate stack storage.
- [x] Add expression root frames with `candidate_start`.
- [x] Add checked control-reachability state to expression frames.
- [x] On eligible frame exit, replace child candidates with the parent.
- [x] On runtime-dependent or effectful frame exit, preserve child candidates.
- [x] Suppress independent publication from runtime-controlled branch bodies.
- [x] Suppress independent publication from runtime-controlled match guards.
- [x] Suppress independent publication from runtime-controlled match branch
  values.
- [x] Allow enclosing compile-time-known `if` and `match` expressions to become
  roots.
- [x] Handle `return` and `break` through explicit control-transfer policy.
- [x] Add delayed parent candidates tied to effect slots.
- [x] Finalize delayed parents from effect-slot results.
- [x] Make nested delayed parents stable by explicit candidate intervals.
- [x] Preserve local binding root identity when later lookup uses the binding.
- [x] Preserve checked pattern extraction roots only when needed.
- [ ] Delete leaf, string, number, empty-list, record, loop,
  `crash`/`dbg`/`expect`, and source-shape root blockers.
- [x] Delete old branch-child preservation rules that can select untaken
  runtime branches independently.

Implementation evidence:

- `hoist_expr_candidates` is the candidate stack. Each `HoistFrame` stores
  `candidate_start`, delayed-root interval bounds, suppression state, binding
  identity, and runtime/effect flags.
- `finishHoistFrame` is the parent-child replacement point: eligible parents
  cover children, runtime-dependent or effectful parents flush/preserve
  children, and suppressed branch regions do not publish independent roots.
- `checkIfElseExpr` and `checkMatchExpr` check runtime-controlled branch bodies,
  guards, and branch values through `checkExprWithHoistSelectionSuppressed`.
- `DelayedHoistRoot` records an effect slot and child intervals; delayed roots
  finalize from `effectSlotIsEffectful` in reverse interval order.
- Local binding identity is preserved through `hoist_known_values`,
  `hoist_selected_bindings`, and `HoistSelectionTransaction`.
- Checked pattern extraction roots are represented explicitly by
  `HoistPatternExtraction` and tested for record, tuple, tag, nested, rest, and
  match extraction cases.

Tests:

- [x] number literal can be a maximal root
- [x] string literal can be a maximal root
- [x] empty list can be a maximal root
- [x] empty record can be a maximal root
- [x] record containing list selects the record, not the list child
- [x] list inside runtime-dependent record stays as child root
- [x] nested closed block selects the block root
- [x] runtime-dependent block preserves independent closed child roots
- [x] closed `return` payload can be selected, but `return` is not a root
- [x] closed values in functions containing `break` can be selected, but
  `break` is not a root
- [x] closed `for` expression can be covered by a parent root
- [x] runtime-condition `if` does not publish independent branch-body roots
- [x] runtime-scrutinee `match` does not publish independent branch-body roots
- [x] compile-time-known `if` selects the enclosing `if` root
- [x] compile-time-known `match` selects the enclosing `match` root
- [x] untaken branch `crash` is not selected independently
- [x] `crash` in selected compile-time branch reports at compile time
- [x] `dbg` in selected compile-time branch reports at compile time
- [x] failed `expect` in selected compile-time branch reports at compile time
- [x] effectful parent preserves independent static child root
- [x] direct effectful call blocks containing parent root
- [x] delayed parent resolving pure replaces children
- [x] delayed parent resolving effectful preserves children
- [x] nested delayed parents finalize in stable order
- [x] named top-level constant and equivalent inline expression select
  equivalent roots
- [x] closed local constant and equivalent inline expression select equivalent
  roots
- [x] inline `sub_or_crash` animation cells inside a runtime-dependent record
  select the cells list as a compile-time root
- [x] inline imported opaque `sub_or_crash` cells through a boxed hosted model
  select static cells data even when the first use forces a forward top-level
  sprite-sheet definition
- [x] record destructure extracts necessary compile-time root
- [x] tuple destructure extracts necessary compile-time root
- [x] tag payload destructure extracts necessary compile-time root
- [x] nested destructure extracts necessary compile-time root

## Phase 4: Compile-Time Evaluation And Diagnostics

Tasks:

- [ ] Define checked output for selected root requests.
- [ ] Define checked output for evaluated root values.
- [ ] Define checked output for top-level values evaluated only for diagnostics.
- [ ] Schedule eligible top-level values in every checked module.
- [ ] Schedule unreachable eligible top-level values for diagnostics.
- [ ] Schedule selected roots exactly once.
- [ ] Avoid scheduling child roots removed by parent selection.
- [ ] Run `crash`, `dbg`, and `expect` during compile-time evaluation.
- [ ] Reject effectful calls before evaluation can execute them.
- [ ] Report evaluation diagnostics through `roc check`.
- [ ] Deduplicate diagnostics shared by a top-level value and selected root.
- [ ] Keep successful unreachable evaluated values out of target static data.

Tests:

- [x] unreachable top-level `crash` reports during `roc check`
- [x] unreachable top-level `dbg` reports during `roc check`
- [x] unreachable failed `expect` reports during `roc check`
- [x] reachable selected-root `crash` reports during `roc check`
- [x] reachable selected-root `dbg` reports during `roc check`
- [x] reachable selected-root failed `expect` reports during `roc check`
- [x] effectful call inside compile-time-known expression is not evaluated
- [x] all modules in an import graph run eligible top-level diagnostics
- [x] duplicate diagnostics are not emitted for shared top-level/root sources
- [x] successful unreachable top-level value is not emitted as target data

## Phase 5: Static Data Output

Tasks:

- [ ] Define explicit static-storable categories for scalars, strings, lists,
  records, tuples, tags, and allowed opaque values.
- [ ] Define explicit non-storable categories.
- [ ] Store reachable evaluated values only.
- [ ] Share repeated static list bytes.
- [ ] Store records that point at shared static list bytes.
- [ ] Store tuples and tag payloads that point at shared static list bytes.
- [x] Ensure opaque static data uses checked backing values only when allowed.
- [x] Ensure removed child roots do not emit duplicate static data.
- [ ] Ensure target static-data emission consumes evaluated checked values, not
  source/CIR shape.
- [ ] Ensure lowering knows exactly when it is lowering a root's own entry
  wrapper so it does not recursively restore that root from static data.

Tests:

- [x] static list bytes are emitted once when shared
- [x] static record points at static list bytes
- [x] repeated records sharing a list share the list bytes
- [x] tuple containing list points at static list bytes
- [x] tag payload containing list points at static list bytes
- [x] opaque backed by static-storable data emits only through allowed checked
  output
- [x] repeated sprite sheets share bytes
- [x] sub-sprite records point at sprite sheet bytes
- [x] inline `sub_or_crash` animation cells point at shared sprite sheet bytes
- [x] inline imported opaque animation cells through a boxed hosted model are
  emitted as reachable static data
- [x] inline animation cells and named animation cells emit equivalent data
- [x] child roots removed by parent root do not emit duplicate data
- [x] effectful parent does not prevent independent static child data
- [x] unreachable successfully evaluated value is not emitted as target data
- [ ] non-storable reachable evaluated value is represented explicitly

## Phase 6: Cleanup Old Machinery

Tasks:

- [ ] Delete or replace all old root-selection paths that can disagree with
  root frames.
- [ ] Delete root blockers for `dbg`, `expect`, and `crash`.
- [ ] Delete leaf/root pruning rules.
- [ ] Delete loop/data-shape root blockers.
- [ ] Delete duplicate dependency verification walks used to repair selection.
- [ ] Delete comments that describe old behavior as intended.
- [ ] Add static searches that prevent reintroducing forbidden blockers where
  practical.

Tests:

- [ ] static search finds no root blocker for `dbg`, `expect`, or `crash`
- [ ] static search finds no root blocker for leaves
- [ ] static search finds no data-shape root blocker for loops
- [ ] static search shows `return` and `break` use explicit control-transfer
  policy
- [x] focused effect tests pass
- [x] focused root tests pass
- [x] compile-time evaluation tests pass
- [x] static-data tests pass

Current audit:

- `exprCanBeStandaloneConstRoot`, `exprCanCoverConstRootChildren`, and
  `exprCanBeBindingConstRoot` currently allow `crash`, `dbg`, `expect`,
  numbers, strings, empty lists, empty records, records, tuples, tags, calls,
  dispatch calls, field access, and `for`.
- `return` and `break` are excluded only as standalone stored roots; the
  checker marks those expressions runtime-dependent and includes the returned
  payload expression where appropriate.
- Runtime-controlled branch bodies are suppressed by
  `checkExprWithHoistSelectionSuppressed`, with a comment explaining the
  compile-time observable behavior being preserved.
- There is no repo-local static-search test harness yet. Do not replace the
  focused semantic tests with source-text tests; if source searches are added,
  they should be CI-side guardrails for forbidden blocker patterns only.

## Phase 7: Rocci Bird Integration

Tasks:

- [ ] Build the local compiler with `zig build`.
- [ ] Build the roc-wasm4 host with `zig build -Doptimize=ReleaseSmall`.
- [ ] Run `roc fmt` on `/home/rtfeldman/code/roc-wasm4/examples/rocci-bird.roc`.
- [ ] Build Rocci Bird with `roc build examples/rocci-bird.roc --opt=size`.
- [ ] Measure final wasm byte size.
- [ ] Disassemble final wasm.
- [ ] Compare named top-level animation data against equivalent inline
  animation data.
- [ ] Verify optimized Rocci Bird starts and plays.
- [ ] Verify dev Rocci Bird starts and plays.
- [ ] Record remaining normal-gameplay allocation sites, excluding game-over
  paths.

Disassembly checks:

- [ ] sprite sheet byte arrays appear in static data, not rebuilt inside
  `update`
- [ ] sprite sheet records point at shared byte arrays
- [ ] `Sprite.sub_or_crash(rocci_sprite_sheet, ...)` cells are precomputed when
  inputs are compile-time-known
- [ ] animation records are not rebuilt inline in ordinary gameplay paths
- [ ] equivalent inline and named animation data produce equivalent static data
- [ ] `update` no longer contains repeated byte-by-byte construction of
  sprite/list values

Recorded output:

- [ ] final wasm byte count
- [ ] code section byte count
- [ ] data section byte count
- [ ] largest function bodies
- [ ] normal-gameplay allocation sites
- [ ] comparison to the Rust WASM-4 port

## Verification Commands

Focused check tests:

```sh
zig build run-test-zig-module-check -- --test-filter "effect"
zig build run-test-zig-module-check -- --test-filter "hoist"
```

Focused compile/static-data tests:

```sh
zig build run-test-zig-module-compile -- --test-filter "hoisted"
zig build run-test-zig-module-compile -- --test-filter "static"
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
/home/rtfeldman/code/worktrees/roc/vivid-canyon/roc/zig-out/bin/roc build \
  examples/rocci-bird.roc \
  --opt=size \
  --output=rocci-bird.wasm
wc -c rocci-bird.wasm
wasm-objdump -x -d rocci-bird.wasm > rocci-bird.disasm.txt
```

## Final Checklist

- [ ] Effect propagation is directed and finalized before checked output.
- [ ] Static dispatch can no longer hide effectful calls from `roc check`.
- [ ] `checkExpr` returns root-relevant data without a permanent per-expression
  summary table.
- [ ] Root selection has one implementation and one parent-child replacement
  rule.
- [ ] `crash`, `dbg`, and `expect` run at compile time whenever their enclosing
  expression is eligible.
- [ ] Runtime-controlled branch bodies do not run compile-time observables
  independently.
- [ ] Every module evaluates eligible top-level values for diagnostics.
- [ ] Reachable static data is emitted once and shared.
- [ ] Unreachable successful constants are not forced into target data.
- [ ] Named constants, closed locals, and equivalent inline expressions select
  equivalent roots and produce equivalent static data.
- [ ] Rocci Bird builds with `--opt=size`.
- [ ] Rocci Bird disassembly proves sprite/list/animation data is static.
- [ ] Rocci Bird runs in optimized and dev WASM-4 builds.
- [ ] Full relevant Zig test suites pass.
