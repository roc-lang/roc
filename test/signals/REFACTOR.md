# Signals Engine Refactor Plan

This plan is for the post-current-work cleanup of the Signals engine subsystem —
primarily `test/signals/src/engine.zig`, and also `test/signals/src/native_host.zig`,
which the audit below shows is the second source of bloat.
The goal is not to make the Signals engine "small"; the target design in
`DESIGN.md` requires one host-agnostic engine to own the mutable node table,
scheduler, dirty set, scope forest, keyed diff, identity tables, structural
splice/apply path, and effect-source lifecycle. Some complexity is inherent.

The goal is to make that complexity locally understandable and harder to misuse:
extract cohesive modules around invariants, reduce repeated descriptor/route code,
and use Zig's type system where it can prevent wrong-table/wrong-id/wrong-owner
mistakes without weakening the explicit runtime checks that catch producer bugs.

## Verified findings (independent codebase audit)

The priorities below are grounded in a direct audit of the tree, not only in the
brainstormed ideas. The load-bearing facts:

- **Two files are bloated, not one.** `engine.zig` is ~8.7k lines, but
  `native_host.zig` is ~6.8k lines — the second-largest file in the subsystem and
  larger than every other module combined. Any plan that only refactors
  `engine.zig` leaves roughly 40% of the subsystem's bulk untouched. The user's
  complaint is about the whole thing being "very large and difficult to work
  with," so this plan covers both. See the dedicated native-host phase below.
- **`engine.zig` has zero `test` blocks.** Engine behavior is validated only
  transitively through native app specs (`run-test-signals`). Extraction is
  therefore also the opportunity to make core invariants unit-testable for the
  first time.
- **`keyed_rows.zig` is dead code with dead tests.** Nothing imports it
  (`grep` confirms no `@import("keyed_rows.zig")` anywhere). Its 3 tests never
  run. The real keyed-row logic lives inline in `engine.zig`. This must be an
  explicit decision in the each-runtime phase, not a silent assumption.
- **Signals unit tests are rooted only through `native_host.zig`.** `build.zig`
  wires the signals Zig tests with a single `b.addTest` whose root is
  `test/signals/src/native_host.zig` (the `signals_host` test). A module's tests
  run under `zig build run-test-zig` *only if that module is reachable by import*
  from `native_host.zig` (or, for browser-only code, `wasm_host.zig`).
- **CI does not guard signals test wiring.** `ci/check_test_wiring.zig` walks
  only `src/`, never `test/signals/src/`. There is no automated protection
  against orphaned signals test files — which is exactly how `keyed_rows.zig`'s
  tests rot silently. Every new module in this plan must be deliberately imported
  into the rooted graph, and that fact verified, because nothing will warn you.
- **The native allocation ledger currently violates the design's own complexity
  budget.** `freeRocAllocation`/`rocReallocFn` resolve the ledger entry with
  `findExactRocAllocationIndex` / `findRocAllocationIndex`, which linearly scan
  `roc_allocations` on every free/realloc. `DESIGN.md` mandates O(1) free with
  the ledger index stored in the allocation header and explicitly warns that an
  O(live) scan makes session cost O(allocs²) and poisons allocation telemetry.
  This is a concrete, sourced defect the refactor should fix, not just a
  cleanliness nicety. (Uncertainty: it may have been tolerated as a debug-only
  cost; the design text is unambiguous that it should not be.)

## Critical assessment

The earlier ideas are directionally right: `engine.zig` is carrying too many
subsystems, and there are clear opportunities for Zig to encode more invariants.
However, the safest route is **not** a broad rewrite or a split by line count. The
engine is large because it is currently the meeting point for several stateful
algorithms:

- descriptor ingestion and dense lookup indexes;
- retained `HostValue` / capability ownership;
- persistent signal records and cached signal values;
- the mounted active signal graph and route tables;
- scope, node identity, DOM identity, and keyed-row site indexes;
- dirty propagation and `is_eq` pruning;
- structural collection/splice/apply/removal;
- render cache updates and sink command emission;
- task/interval/cleanup effect-source lifecycle;
- native/WASM host contract verification and metrics.

The implementation should be refactored by **invariant ownership**. Each new
module should own a small set of data structures plus the operations that keep
those structures valid. `engine.zig` should become the orchestration layer that
connects those modules, not a bag of unrelated helper functions.

Important constraints to preserve:

- Keep **one shared engine**. No *reactive or structural* logic should move into
  `native_host.zig` or `wasm_host.zig`. But note the inverse is also true and is
  often missed: "thin host" describes the host's *role in the reactive
  architecture*, not its current size. `native_host.zig` is ~6.8k lines and
  carries large non-reactive subsystems (a spec-test DSL parser, a spec runner, a
  benchmark harness, the simulated DOM, crash handlers, and an allocation ledger)
  that are legitimate, low-risk extraction targets in their own right. Refactoring
  the host out of these does not violate the one-engine rule; it strengthens it by
  leaving the host as genuine glue.
- Keep **complexity discipline**. Do not replace O(1)/O(changed) indexes with
  scans hidden behind nicer APIs.
- Keep **confined erasure**. The host remains app-type blind and invokes the
  app-compiled capabilities it was handed; it must not infer Roc value layout.
- Keep **runtime invariant assertions** for producer/consumer wiring defects.
  Some `@panic` sites are appropriate assertions, not bloat.
- Prefer small behavior-preserving moves first. Only introduce generic helpers
  after tests pin the concrete behavior.

## Desired end state

`engine.zig` should primarily expose and coordinate:

- `Engine(Ctx)` lifecycle;
- descriptor collection from Roc `Elem` values;
- dirty propagation entrypoints;
- structural update orchestration;
- public host-facing methods used by native/WASM;
- metrics folding and high-level dispatch.

Most concrete tables and low-level invariants should live in focused modules:

- `engine_metrics.zig`
- `engine_contract.zig`
- `render_cache.zig`
- `descriptor_stream.zig`
- `retained_values.zig`
- `signal_records.zig`
- `active_signal_graph.zig`
- `route_table.zig` or route helpers inside `active_signal_graph.zig`
- `scope_runtime.zig` / `mounted_scopes.zig`
- `each_runtime.zig`
- `structural_splice.zig`
- `effects_runtime.zig`
- `engine_scratch.zig`

And, separately, `native_host.zig` should shed its non-reactive subsystems into
focused modules so it becomes genuine host glue (`NativeCtx`, the sink, the ABI
exports, and wiring):

- `spec/spec_parser.zig` (the spec-test DSL parser)
- `spec/spec_runner.zig` (locator resolution, action dispatch, assertions)
- `bench/benchmark.zig` (iteration/sample harness and stats)
- `sim_dom.zig` (the simulated `DomElement` table and its sink application)
- `roc_alloc_ledger.zig` (allocation tracking + leak/double-free diagnostics)
- `crash_handlers.zig` (stack-overflow / access-violation / arithmetic handlers)

These names are suggestions; prefer names that match the final ownership
boundary discovered while extracting.

## Prioritized implementation phases

### Phase 1 — Extract metrics and compile-time host contracts

**Priority:** very high  
**Risk:** low  
**Candidate modules:** `engine_metrics.zig`, `engine_contract.zig`

Move the low-risk top-level support code first:

- `RuntimeMetrics`
- `NoMetrics`
- `DispatchMetrics`
- `zeroRuntimeMetrics`
- `verifyRegistryOps`
- `verifySink`
- `verifyMetrics`
- `verifyCtx`
- related `verifyDeclFn` / `verifyTypeDecl` helpers

Why first:

- This immediately shrinks `engine.zig` without changing engine behavior.
- Future modules can import metrics/contract types without importing the entire
  engine.
- It preserves an existing good pattern: Zig comptime checks make the host seam
  explicit.

Critical notes:

- Do not remove contract checks just because the duck-typed `Ctx` compiles today.
- If `RuntimeMetrics` remains all scalar counters, consider replacing the verbose
  `zeroRuntimeMetrics()` implementation with `std.mem.zeroes(RuntimeMetrics)`;
  only do this if it stays obvious and tested.

Acceptance criteria:

- Native and WASM hosts still instantiate `engine.Engine(Ctx)`.
- Compile-time errors for missing `Ctx`/`Sink`/metrics methods remain clear.
- `zig build run-test-zig` and `zig build run-test-signals` pass.

### Phase 2 — Extract render cache and sink application

**Priority:** very high  
**Risk:** low to medium  
**Candidate module:** `render_cache.zig`

Move render-cache state and operations out of `engine.zig`:

- `RenderScalarNodeCache`
- `RenderCustomTextAttrCache`
- `RenderNamedEventCache`
- render tree cache lifecycle;
- `ensureRenderCacheNode`
- `appendRenderNode`
- `ensureRenderNode`
- `removeRenderNode`
- `replaceRenderChildren`
- `replaceRenderChildrenForMoves`
- text/bool/custom attr application;
- event/named-event binding application.

Why this is high value:

- It is a coherent seam: maintain host-side render cache and emit sink commands.
- It should not need signal graph evaluation knowledge.
- It keeps native/WASM behavior shared because the module remains parameterized by
  the same `Ctx.sink()` contract.

Design shape:

```zig
pub fn Cache(comptime Ctx: type) type { ... }
```

or, if easier during migration, expose pure functions that accept:

- allocator;
- sink;
- `render.Counts`;
- scratch needed for move counting.

Prefer an owned cache object eventually, but avoid forcing a large API redesign in
the first extraction.

Type-system opportunities here:

- Replace the repeated `bound_click_event`, `bound_input_event`, ... fields with
  enum-indexed event slots once behavior is tested.
- Replace text/bool field switch boilerplate with small enum-indexed storage only
  after extraction.

Acceptance criteria:

- Render command counts and semantic app specs do not change.
- Moving children still counts append vs move correctly.
- Duplicate application remains idempotent where currently idempotent.
- `zig build run-test-signals` passes.

Suggested new unit tests:

- creating and removing render nodes keeps dense identity invariants;
- applying unchanged text/bool/event fields emits no duplicate command;
- clearing a missing optional field is a no-op where expected;
- reordering children counts only displaced moves;
- named-event replacement frees copied names exactly once.

### Phase 3 — Extract descriptor stream and descriptor indexes

**Priority:** highest bloat-reduction value  
**Risk:** medium  
**Candidate module:** `descriptor_stream.zig`

Move the retained descriptor stream as a module with its indexes:

- all `HostNode*Desc` descriptor structs;
- `HostRenderNode` and render-node descriptor metadata;
- `HostTextFieldDescriptorIndexes`;
- `HostBoolFieldDescriptorIndexes`;
- `HostEventDescriptorIndexes`;
- `HostScopeSiteDescriptorIndexes`;
- `HostElemDescriptorIndex`;
- `HostNodeDescriptorIndex`;
- `HostNodeDescriptorStream` append/deinit/index methods;
- stream reader helpers such as `findElementDesc`, `streamElemTag`,
  `streamDirectChildren`, `elemScopeId`, and render parent/scope lookup.

Why it matters:

- This is one of the largest cohesive blocks in `engine.zig`.
- It directly protects the design constraint that descriptor lookup by `elem_id`
  and `node_id` is O(1), not rediscovered by scans.
- It creates a natural unit-test home for swap-remove/index-update bugs.

Suggested extraction order:

1. Move pure descriptor structs and index structs.
2. Move render child/sibling metadata helpers.
3. Move append methods for static descriptors.
4. Move append/deinit methods that own retained signal/capability resources.
5. Move descriptor-use tracking for signal records only after signal-record
   ownership dependencies are clear.

Do not over-generalize immediately. The repeated `record*Index`, `update*Index`,
and `clear*Index` methods are obvious candidates for a generic helper, but first
move them as-is and pin behavior with tests. Then introduce a small helper such as
`IndexedSlot` or `DenseDescriptorIndexes` if the abstraction stays simple.

Critical warning:

- `customTextAttrDescriptorExists` and `namedEventDescriptorExists` currently scan
  per element/name. That may be acceptable because custom names are less common,
  but the plan should not accidentally expand scan-based lookup into hot paths.
  If named/custom attributes become hot, add explicit per-elem/name indexes rather
  than hiding the scan.

Acceptance criteria:

- `Engine(Ctx)` can still collect descriptors without a circular dependency on
  the full engine type.
- Descriptor indexes remain dense and validated.
- Removing/replacing descriptor ranges still refreshes moved indexes correctly.
- `zig build run-test-zig` and `zig build run-test-signals` pass.

Suggested new unit tests:

- duplicate descriptor index insertion panics/asserts in debug tests;
- element/text/signal-text indexes round-trip by `elem_id`;
- static/signal text and bool attr indexes round-trip by field;
- event indexes round-trip by event kind;
- scope-site/state/when/each indexes round-trip by `node_id`;
- replacing a render range updates moved render indexes;
- render child/sibling metadata remains valid after insert/remove/replace.

### Phase 4 — Extract retained resources and capability-call helpers

**Priority:** high  
**Risk:** medium  
**Candidate module:** `retained_values.zig`

Centralize the operations that retain, release, compare, clone, and call through
capability-owned erased values:

- `HostValueCell`
- retained handle helpers for:
  - `HostTextRead`
  - `HostBoolRead`
  - `HostEventReducer`
  - `HostEachOps`
  - task request reads if needed
- `retainHostCallable`
- `retainHostValueCapability` / `releaseHostValueCapability`
- `assertHostValueCapabilitiesMatch`
- capability push/pop call wrappers.

Why it matters:

- Retained ownership is the highest-risk class of bug in this subsystem.
- The current code already improved capability coherence; this phase makes that
  discipline easier to audit.
- It reduces repeated push/defer-pop/call patterns.

Recommended type-system improvement:

Introduce a tiny capability-frame guard so missing pops become structurally
harder:

```zig
pub fn CapabilityFrame(comptime Ctx: type) type {
    return struct {
        ctx: Ctx.Handle,

        pub fn init(ctx: Ctx.Handle, caps: []const HostValueCapability) @This() {
            Ctx.pushHostValueCapabilities(ctx, caps);
            return .{ .ctx = ctx };
        }

        pub fn deinit(self: @This()) void {
            Ctx.popHostValueCapabilities(self.ctx);
        }
    };
}
```

Use with `defer frame.deinit()`.

Critical warning:

- Zig cannot statically know the original Roc type behind a `HostValue`. Do not
  promise to eliminate all runtime capability assertions. The invariant is that
  the host passes values to the capability that owns them; runtime checks should
  remain where they catch wiring defects.

Acceptance criteria:

- Capability retain/release metrics remain balanced.
- Task payload consuming ownership still works.
- Host-value registry tests still pass.
- `zig build run-test-signals` passes.

Suggested new unit tests:

- `HostValueCell.replaceRetained` drops the old value/capability once and retains
  the new capability once;
- clone/deinit balance capability refcounts;
- capability-frame guard pops on all return paths;
- retained `HostEachOps` releases every callable/capability exactly once.

### Phase 5 — Extract signal records and cached signal values

**Priority:** high  
**Risk:** medium  
**Candidate module:** `signal_records.zig`

Move persistent signal-record data and lifecycle out of `engine.zig`:

- `HostSignalCacheSlot`
- `HostSignalConstRecord`
- `HostSignalMapRecord`
- `HostSignalMap2Record`
- `HostSignalCombineRecord`
- `HostSignalTaskSourceRecord`
- `HostSignalIntervalSourceRecord`
- `HostSignalRecordPayload`
- `HostSignalRecord`
- `HostSignalBinding`
- `validateExistingSignalRecord`
- `appendSignalRecordSourceNodeIds`
- signal-token retain/release helpers.

Keep dirty evaluation in `engine.zig` initially. Move data and lifecycle before
moving algorithms.

Why it matters:

- Signal records are persistent graph descriptors; the active graph is a mounted
  view over them. Mixing those two concepts increases cognitive load.
- Refcount/cached-value ownership should be locally auditable.

Acceptance criteria:

- Active graph still retains records while mounted and releases them on clear.
- Descriptor streams still retain records referenced by active descriptors.
- Cached values are deinitialized exactly once.
- `zig build run-test-signals` passes.

Suggested new unit tests:

- retaining/releasing a map/map2/combine record recursively releases children;
- active graph cannot hold the last signal-record reference silently;
- cached-value absent/present transitions are balanced;
- `appendSignalRecordSourceNodeIds` de-duplicates source ids.

### Phase 6 — Extract active signal graph and route tables

**Priority:** high, after phases 3-5  
**Risk:** medium to high  
**Candidate modules:** `active_signal_graph.zig`, possibly `route_table.zig`

Move the mounted graph state and route maintenance:

- `HostActiveSignalGraphNode` usage;
- active source routes;
- active text/bool/change/structural signal routes;
- active graph id assignment and validation;
- dependent edge insertion/removal/replacement;
- dirty root collection and rank sorting wrappers;
- route-table swap-remove fixups.

Why it matters:

- This is load-bearing for O(changed) propagation and glitch-free ordering.
- Text/bool/change/structural routes are similar enough that a small generic route
  helper may reduce boilerplate once tests exist.

Critical warning:

- Do not introduce a generic route abstraction that assumes all route keys are the
  same. Text and structural sinks may match by kind/index, while bool/change
  routes have different shapes. Use a route-specific match key or callback.
- Do not rebuild the full active graph for local structural changes unless that
  path is already intentionally doing so and covered by metrics.

Acceptance criteria:

- Dirty propagation order remains rank-sorted.
- Moved active records update their dense ids and all route references.
- Structural sink routes survive descriptor removal/replacement correctly.
- Metrics such as `active_graph_records_rebuilt`, `nodes_recomputed`, and
  `propagation_prunes` do not regress unexpectedly.

Suggested new unit tests:

- append/remove/replace dependent edges;
- removing a graph record updates moved record ids;
- source route removal does not leave stale ids;
- sink route removal updates moved route indexes;
- dirty roots collect reachable dependents once and sort by rank.

### Phase 7 — Extract scope, identity, and mounted row-site runtime

**Priority:** high  
**Risk:** medium to high  
**Candidate modules:** `scope_runtime.zig`, `each_runtime.zig`

The current `scope_tree.zig` and `identity_table.zig` are useful low-level seam
modules, but `engine.zig` still owns the mounted runtime around them:

- node identity activation/deactivation;
- DOM identity activation/deactivation;
- scope disposal and cleanup event collection;
- each-row site indexes;
- each-row memberships by scope id;
- key hash heads/links;
- row reuse/create/remove metrics;
- row key/item retained cells.

Extract this as runtime state around the existing pure helpers.

Important finding:

- `keyed_rows.zig` already contains a generic diff planner with tests, but the
  engine currently has a richer in-place row-site implementation. Do not blindly
  replace `syncEachRowScopes` with `keyed_rows.buildPlan`; the real path also owns
  active site indexes, scope membership, retained key/item cells, disposal, and
  metrics. Either evolve `keyed_rows.zig` into the real runtime module or retire
  it once the engine's implementation has been extracted and tested.

Type-system opportunities:

- Give `ScopeId`, `NodeId`, and row-site identifiers distinct types at this seam.
- Introduce a row-site object whose methods are the only way to mutate
  `scope_ids`, `hash_heads`, `hash_links`, and `memberships_by_scope_id`.

Acceptance criteria:

- Keyed rows still reuse scopes by key hash plus typed equality.
- Duplicate next keys are rejected, not silently aliased.
- Row reorders preserve per-row local state.
- Removed row scopes run cleanup and cancel scoped tasks/intervals.
- Membership indexes remain valid after create/reuse/remove/reorder.

Suggested new unit tests:

- no-collision reorder reuses all scopes and updates order;
- hash collision resolves with typed equality;
- duplicate next keys fail;
- removed rows are returned for disposal;
- replacing item with equal value avoids unnecessary row descriptor rebuild;
- membership lookup by scope id updates after reorder/removal;
- comparison/hash metric counts stay bounded for representative cases.

### Phase 8 — Extract effects runtime

**Priority:** medium  
**Risk:** medium  
**Candidate module:** `effects_runtime.zig`

Move effect-source lifecycle once signal records and active graph are clearer:

- pending tasks;
- task request ids;
- active intervals;
- interval tokens;
- task start/cancel command emission;
- interval start/cancel command emission;
- cleanup event storage and draining;
- task/interval source lookup helpers.

Why not earlier:

- Task and interval sources are signal records, so this extraction depends on a
  clean signal-record module.
- Interval synchronization depends on active graph contents.

Acceptance criteria:

- Scope disposal still cancels pending tasks in the removed subtree.
- Active intervals start/cancel exactly when mounted interval sources change.
- Task result payloads remain consuming and capability-checked.
- Async effect app specs remain green.

Suggested new unit tests:

- disposing a scope cancels only tasks in that scope subtree;
- interval source remount does not duplicate active interval commands;
- task resolve after cancellation is ignored or rejected according to current
  semantics;
- cleanup events drain in the same order as before.

### Phase 9 — Extract structural splice/removal last

**Priority:** high final payoff, but late  
**Risk:** high  
**Candidate module:** `structural_splice.zig`

Structural splice/removal is likely the most tempting extraction because it is
large, but it should wait until the dependencies above have stable APIs.

It depends on:

- descriptor stream;
- render cache;
- active signal graph and sink routes;
- scopes and identities;
- each-row runtime;
- dirty structural signals;
- effects cleanup/cancellation;
- metrics;
- sink command emission.

Extracting it too early would either create a circular module graph or produce a
"misc engine internals" parameter object that is no easier to reason about than
`engine.zig`.

Desired final shape:

- `StructuralSplice.applyWhenBranch(...)`
- `StructuralSplice.applyEachRows(...)`
- `StructuralSplice.removeScopeSubtree(...)`
- `StructuralSplice.replaceDescriptorRange(...)`
- `StructuralSplice.rebindRoutesAfterReplacement(...)`

These should orchestrate other modules rather than directly mutate every table.

Acceptance criteria:

- Branch flips remove only the inactive branch subtree.
- Row reorder emits moves rather than rebuilds where current semantics do.
- Local structural updates do not rebuild unrelated scopes.
- Descriptor removal updates route indexes and render indexes.
- Cleanup/task/interval disposal still happens for removed scopes.
- Work counters do not hide new full-tree scans.

## Cross-cutting type-system improvements

These are valuable, but most should happen **after** the relevant module is
extracted and covered by tests.

### Typed ids

Introduce lightweight distinct id types gradually:

- `ElemId`
- `NodeId`
- `ScopeId`
- `EventId`
- `ActiveSignalId`
- `TaskRequestId`
- `IntervalToken`
- `RowIndex` / `RowSiteId`

This would prevent many wrong-table/wrong-id mistakes currently defended by
runtime assertions.

Suggested pattern:

```zig
pub fn Id(comptime Tag: type) type {
    return enum(u64) {
        _,

        pub fn fromInt(value: u64) @This() {
            return @enumFromInt(value);
        }

        pub fn toInt(self: @This()) u64 {
            return @intFromEnum(self);
        }

        pub fn toIndex(self: @This()) usize {
            return @intCast(@intFromEnum(self));
        }
    };
}
```

Do not mass-convert the whole engine in one patch. Start at module boundaries:
render cache can own `ElemId`, scope runtime can own `ScopeId`, active graph can
own `ActiveSignalId`, and descriptor stream can bridge ABI `u64` into typed ids.

### Dense slot/table wrappers

Repeated "grow array to id, then return slot" logic is a good generic target:

- `DenseSlots(Id, T)`
- `DenseOptionalSlots(Id, T)`
- `DenseIndexById(Id, Index)`

Use these for descriptor indexes, state indexes by node id, render cache nodes,
active route arrays, and memberships by scope id.

Keep the abstraction simple and explicit. It must not hide scans or make swap-
remove index repair harder to see.

### Enum-indexed field storage

Repeated text/bool/event slot structs can become enum-indexed arrays after
render cache and descriptor stream have tests:

- text fields: `text`, `role`, `label`, `test_id`, `value`, `class`;
- bool fields: `checked`, `disabled`;
- event kinds: `click`, `input`, `check`, pointer events.

This should reduce switch boilerplate and make adding a new field/event a single
comptime-visible change.

### Generic descriptor/index helpers

After `descriptor_stream.zig` exists, consider a small helper for the repeated
record/update/clear pattern:

```zig
fn setFresh(slot: *?usize, index: usize) void
fn updateExisting(slot: *?usize, index: usize) void
fn clearExpected(slot: *?usize, expected: usize) void
```

The current code already has these primitives; the opportunity is to remove the
large family of near-identical forwarding methods without hiding which index is
being updated.

### Generic route tables

A route-table helper is plausible, but only after active graph route behavior is
tested. The abstraction must support different route keys and moved-index fixups.
Do not force text, bool, change, and structural routes into one shape if their
invariants differ.

### Ownership wrappers

Zig does not provide RAII, but small explicit owned wrappers can reduce mistakes:

- `OwnedCapability`
- `OwnedTextRead`
- `OwnedBoolRead`
- `OwnedEventReducer`
- `OwnedEachOps`
- `CachedHostValue`
- `CapabilityFrame(Ctx)`

Each should have clear `retain`, `deinit`, `cloneRetained`, and `replace` names.
Avoid clever ownership transfer conventions that are not obvious at call sites.

### Domain-specific scratch

`EngineScratch` currently collects scratch for several domains. As modules move
out, split scratch into the module that owns it:

- render move scratch in `render_cache.zig`;
- keyed-row hash/match scratch in `each_runtime.zig`;
- descriptor replacement scratch in `descriptor_stream.zig` or
  `structural_splice.zig`;
- binder stack scratch near descriptor collection.

Scratch extraction should follow module extraction, not precede it.

## Parallel track — decompose `native_host.zig`

This track is independent of the engine extraction track above and can run
concurrently (ideally by a different person or in separate PRs) because its write
scope is disjoint: it touches `native_host.zig` and new sibling modules, not the
engine internals. It is sequenced here after the engine cross-cutting notes only
for readability, not dependency.

**Why this belongs in the plan:** `native_host.zig` is the second-largest file in
the subsystem (~6.8k lines). The brainstormed ideas assumed the host was already
thin and focused only on `engine.zig`; the audit shows the host carries several
cohesive, non-reactive subsystems that are easier and safer to extract than
anything in the engine. Doing these first also de-risks the engine work: a
slimmer host with isolated spec/bench/DOM/ledger modules makes engine behavior
changes easier to validate.

### Native Phase A — Extract the spec-test DSL

**Priority:** high (low risk, high clarity payoff)  
**Candidate modules:** `spec/spec_parser.zig`, `spec/spec_runner.zig`

Move `SpecCommand`, `SpecCommandType`, `Locator`, `LocatorKind`, `ParseError`,
`parseTestSpec*`, `parseLocator`, `parseQuotedValue`, and the action/assertion
driver (`runActionCommand*`, locator resolution, `expect_*` handling) out of the
host. The parser is pure (`[]const u8 -> []SpecCommand`) and trivially
unit-testable in isolation — today it has no direct tests at all.

Why first in this track:

- It is pure data-in/data-out with no engine coupling.
- It is a large, self-contained block.
- It immediately gives the spec format real unit tests (malformed lines,
  quoting, locator variants, metric-delta parsing).

Acceptance criteria:

- Every existing app spec still parses and runs identically.
- `run-test-signals` is unchanged.
- New parser unit tests are reachable from the rooted test graph.

### Native Phase B — Extract the benchmark harness

**Priority:** medium  
**Candidate module:** `bench/benchmark.zig`

Move `BenchmarkStats`, `runBenchmarkIteration`, `runAppBenchmarks`,
`runActionCommandMeasured`, `dispatchRocEventMeasured`, and the ns-timing helpers.
This depends on the spec types from Phase A, so do it after A.

Acceptance criteria:

- `run-signals-bench` produces the same CSV columns and comparable numbers.
- No timing logic remains inline in the host entry point.

### Native Phase C — Extract the simulated DOM

**Priority:** medium  
**Candidate module:** `sim_dom.zig`

Move `DomElement`, `DomTextAttr`, `DomNamedEvent`, child-index helpers, and the
sink application that mutates the simulated DOM. This is the native realization of
the shared render-command vocabulary; keep it behind the existing `render_sink`
seam so the engine stays sink-agnostic.

Acceptance criteria:

- Render-command application semantics are identical (the native host must keep
  asserting the same semantics the browser executes).
- Locator resolution (Phase A) reads the simulated DOM through a stable API.

### Native Phase D — Extract the allocation ledger and fix its budget

**Priority:** high (this fixes a real, sourced defect)  
**Candidate module:** `roc_alloc_ledger.zig`

Move the ledger (`RocAllocation`, `recordRocAllocation`, `freeRocAllocation`,
`rocReallocFn`, the recently-freed ring buffer, and the leak/double-free
diagnostics) into its own module. While extracting, **fix the complexity-budget
violation**: today `freeRocAllocation`/`rocReallocFn` find the ledger entry with a
linear scan (`findExactRocAllocationIndex`/`findRocAllocationIndex`) over all live
allocations. `DESIGN.md` mandates O(1) free with the ledger index stored in the
allocation header, and warns that an O(live) scan makes a session O(allocs²) and
poisons the very allocation telemetry it feeds.

Why fix it here:

- The defect is most safely addressed when the ledger is isolated and unit-tested.
- The fix is a known shape: prefix each allocation with a header carrying its
  ledger slot (or use a swap-remove free-list keyed by an address map) so free is
  O(1). Preserve the existing interior-pointer / double-free / alignment
  diagnostics; they are valuable and must survive the rewrite.

Uncertainty to resolve before changing behavior:

- Confirm whether the linear scan is intentionally tolerated for the debug host.
  Even if so, isolating it makes the cost explicit and the budget assertable.

Acceptance criteria:

- All existing alloc/dealloc/realloc diagnostics still fire on the same inputs
  (interior pointer, already-freed, unknown pointer, alignment mismatch).
- Free/realloc no longer scan the live-allocation list.
- `retained_alloc_delta` and per-event allocation counters are unchanged for the
  app suite.

### Native Phase E — Extract crash/signal handlers

**Priority:** low (pure mechanical move)  
**Candidate module:** `crash_handlers.zig`

Move the Windows/POSIX stack-overflow, access-violation, and arithmetic handlers
and their fixed messages. These are large, OS-specific, and entirely unrelated to
reactivity; moving them is a clean win with near-zero risk.

## Things to avoid

- Do not start with a top-down `Engine(Ctx)` rewrite.
- Do not split by arbitrary line ranges.
- Do not move reactive/structural behavior into native or WASM host files (but do
  extract the host's *non-reactive* subsystems out of them — see the native track).
- Do not assume "thin host" means "small host"; verify size before deciding a file
  needs no work.
- Do not preserve the O(live) allocation-ledger scan when isolating it; fix it to
  O(1) per the design budget while keeping every diagnostic.
- Do not weaken runtime invariant assertions just to make APIs prettier.
- Do not introduce host-side Roc value decoding or layout knowledge.
- Do not replace explicit indexes with hidden scans.
- Do not change structural patch semantics while extracting modules.
- Do not rely on benchmarks as the main correctness gate.
- Do not delete useful tests or snapshots added to validate behavior.

## Validation strategy

After each extraction slice, run from repository root:

```sh
zig build run-test-zig
zig build run-test-signals
```

Know what each gate actually covers, because they catch different regressions:

- `run-test-zig` runs the Zig unit tests, including the signals `signals_host`
  test target rooted at `native_host.zig`. This is what catches a broken
  extraction *only if the new module is in the rooted import graph* (see the
  test-rooting rule below).
- `run-test-signals` builds and runs the native spec/app suite. This is the real
  semantic + work-budget gate: it asserts `expect_metric_delta` invariants
  (`active_graph_records_rebuilt`, `stream_nodes_scanned`, `each_key_compares`,
  per-event allocation deltas, render counters). It is the primary guard for any
  engine change, since `engine.zig` has no unit tests of its own today.

Use targeted filters only when the test name is known, for example:

```sh
zig build run-test-zig -- --test-filter "scope tree"
zig build run-test-zig -- --test-filter "signal graph"
zig build run-test-zig -- --test-filter "host value registry"
```

Do not use plain `zig test`; it does not match this project.

For browser-boundary changes, also run the relevant JS contract tests and/or
`serve.py` flow described in `README.md`/`NEXT_STEPS.md`. Engine refactors should
usually be validated by the native spec runner first because it owns semantic and
work-budget assertions.

Benchmarks:

```sh
zig build run-signals-bench
```

Use this as trend evidence after behavior is green. If a known benchmark blocker
appears, keep it visible rather than silently skipping the case.

Important test-rooting rule (verified, and unguarded by CI):

- The signals Zig tests are rooted through a single `b.addTest` in `build.zig`
  whose root source file is `test/signals/src/native_host.zig` (the `signals_host`
  test target). A module's `test` blocks run under `zig build run-test-zig` **only
  if that module is reachable by `@import` from `native_host.zig`** (or, for
  browser-only code, from `wasm_host.zig`, which is built as the wasm object).
- `ci/check_test_wiring.zig` walks only `src/`, **not** `test/signals/src/`, so
  nothing will warn you about an orphaned signals test file. `keyed_rows.zig` is
  the existing proof: it is imported by nothing and its tests never run.
- Therefore, for every new module created by this plan: add the `@import` into the
  rooted graph in the same PR, and **verify the new test names actually appear**
  in the run (e.g. `zig build run-test-zig -- --test-filter "<new test name>"`
  should match and run, not report zero matches).
- Resolve `keyed_rows.zig` explicitly in the each-runtime phase: either wire it
  into the rooted graph and adopt it, or remove it after lifting the real
  implementation out of `engine.zig`. Do not leave dead tests behind.

## Review checklist for each PR/slice

- Does this change preserve the one-engine/two-thin-host architecture?
- Did any O(1) lookup become a scan?
- Did any scan move into a hot path without a metric or explicit justification?
- Are ownership transfers obvious at call sites?
- Are capability retain/release metrics still balanced?
- Are moved dense indexes repaired in the same operation that moves entries?
- Are runtime assertions still catching producer/wiring defects?
- Did new module tests actually run under `zig build run-test-zig`? (Confirm the
  module is imported into the `native_host.zig`/`wasm_host.zig` rooted graph and
  that a `--test-filter` on a new test name matches — CI will not warn you.)
- For native-track PRs: did the spec parser/runner/DOM/ledger extraction keep all
  existing diagnostics and `expect_metric_delta` behavior identical?
- Did `zig build run-test-signals` pass?

## Recommended order summary

1. Guardrails and baseline validation.
2. Metrics and compile-time host contracts.
3. Render cache and sink application.
4. Descriptor stream and descriptor indexes.
5. Retained values and capability-call helpers.
6. Signal records and cached signal values.
7. Active signal graph and route tables.
8. Scope/identity mounted runtime and keyed rows.
9. Effects runtime.
10. Structural splice/removal.
11. Follow-up type-system cleanup: typed ids, dense slots, enum-indexed fields,
    generic descriptor/route helpers, and module-local scratch.

In **parallel** (disjoint write scope, can be a separate person/PR stream),
decompose `native_host.zig`:

A. Spec-test DSL parser + runner (`spec/`).
B. Benchmark harness (`bench/`).
C. Simulated DOM (`sim_dom.zig`).
D. Allocation ledger — extract **and** fix the O(live) scan to O(1) (`roc_alloc_ledger.zig`).
E. Crash/signal handlers (`crash_handlers.zig`).

The native track is mostly lower-risk than the engine track (no reactive logic),
so it is a good place to start building momentum and unit-test coverage while the
engine seams are still being studied. The one item worth front-loading regardless
of track is Native Phase D, because it fixes a real, design-documented complexity
budget violation rather than only moving code.

This order intentionally extracts low-risk, cohesive modules before touching the
highest-coupling structural and active-graph code. It should reduce cognitive
load quickly while keeping behavior stable enough that each step can be reviewed
and validated independently.
