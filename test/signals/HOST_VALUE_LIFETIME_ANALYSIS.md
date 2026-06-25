# Signals HostValue Lifetime and Ownership Analysis

This memo records the current proof state for the Signals `HostValue` lifetime
bug. It is intentionally about ownership design, not about making traps nicer.

## Short Version

The original task-payload ownership bug is real and the `take_tagged` direction
is sound: task result payloads are host-owned `HostValue` cells, and the
`done`/`failed` callbacks consume them. The host must not drop the payload after
the callback.

That is not the whole bug.

After the payload handoff is fixed, the ops dashboard still double-frees the
heap backing for the real `/api/ops/dashboard` response. Refined wasm phase
diagnostics show this shape:

```text
roc_dealloc received a pointer that was already freed
requested_size=1144 allocated_size=1144
freed_phase=455 current_phase=409
```

The deterministic response body is 1140 bytes; on wasm32, the heap `RocStr`
allocation is 1144 bytes. Phase `455` is set immediately before cloning the
newly cached dirty map result, and phase `409` is a later cached-signal clone.
Because the map input is dropped by a Zig `defer` after the return expression is
evaluated, the first release is consistent with dropping the temporary
`TaskStatus.Done(Str)` input after the derived `fold_task` result has been
cached. The later release happens when the host clones/reads cached state.

The critical conclusion is:

> A host-owned erased registry is only sound if every retained `HostValue` has a
> complete, coherent, monomorphized ownership capability. The current
> source-level `TypeTag.split : Box(a) -> { keep, out }` is not proven sufficient
> for nested refcounted values across the erased host boundary.

This does not yet prove "the host-owned graph architecture is wrong." It does
prove that the current capability model is incomplete or too fragile.

## Design Constraints

The repo rules matter here because the tempting fixes are host-side layout
workarounds.

- Later compiler stages must not use workarounds, fallbacks, or heuristics:
  [design.md](/Users/luke/Documents/GitHub/roc/design.md:45).
- Later stages must consume explicit data produced by earlier stages:
  [design.md](/Users/luke/Documents/GitHub/roc/design.md:48).
- Backends must only lower explicit LIR RC statements:
  [design.md](/Users/luke/Documents/GitHub/roc/design.md:77).
- Static ownership reasoning belongs in ARC insertion:
  [design.md](/Users/luke/Documents/GitHub/roc/design.md:138).
- Hosted functions receive ownership of refcounted arguments and must decref,
  store, return, or explicitly retain them:
  [host_abi.zig](/Users/luke/Documents/GitHub/roc/src/builtins/host_abi.zig:31).

Therefore the Signals host must not infer `Str`, `List`, tag-union, or nested
`Box` layout from erased pointers. If it needs clone/split/drop/equality/read
semantics, those must be explicit typed Roc/generated capabilities.

## Roc Box and ARC Model

`Box(a)` is a refcounted heap allocation. The ABI value is the payload pointer;
the refcount lives immediately before that payload. There are two ownership
levels:

1. the outer box allocation;
2. nested refcounted values inside the payload, such as `Str` or `List`.

Relevant source points:

- Box ABI records payload layout, size, alignment, and nested-refcounted status:
  [store.zig](/Users/luke/Documents/GitHub/roc/src/layout/store.zig:1681).
- Box RC helper planning adds a child drop helper only when the payload contains
  refcounted fields:
  [rc_helper.zig](/Users/luke/Documents/GitHub/roc/src/layout/rc_helper.zig:226).
- `Box.box` is modeled as allocating while retaining its argument:
  [LowLevel.zig](/Users/luke/Documents/GitHub/roc/src/base/LowLevel.zig:727).
- `Box.unbox` is modeled as retaining the result while borrowing the box:
  [LowLevel.zig](/Users/luke/Documents/GitHub/roc/src/base/LowLevel.zig:729).

The host can safely retain or release only the outer `RocBox` if it has explicit
box helpers. It cannot recursively retain nested payload fields without layout
metadata or generated helpers.

## Current Signals Boundary

The current platform tag is:

[HostValue.roc](/Users/luke/Documents/GitHub/roc/test/signals/platform/HostValue.roc:2)

```roc
TypeTag(a) := [
    TypeTag(
        {
            id : U64,
            split : Box((Box(a) -> { keep : Box(a), out : Box(a) })),
        },
    ),
]
```

The current split thunk is source-level Roc:

[HostValue.roc](/Users/luke/Documents/GitHub/roc/test/signals/platform/HostValue.roc:11)

```roc
split_box = |boxed| {
    value = Box.unbox(boxed)
    { keep: Box.box(value), out: Box.box(value) }
}
```

The host registry operations are:

- `store` consumes an owned box from Roc and stores it;
- `take` removes a registry entry and transfers its owned box back to Roc;
- `get` must keep one owned value and return another independently owned value.

Registry `get` does exactly that mechanically:

[host_value_registry.zig](/Users/luke/Documents/GitHub/roc/test/signals/src/host_value_registry.zig:145)

```zig
const split = ops.splitBox(occupied.box, tag_value);
occupied.box = split.@"keep";
break :blk split.@"out";
```

That is only sound if `splitBox` satisfies the split law below.

## The Split Law

For a stored owned `Box(a)`:

```roc
{ keep, out } = split(box)
```

must satisfy:

1. `split` consumes the input `box`.
2. `keep` and `out` are both independently owned `Box(a)` values.
3. Dropping `out` must not invalidate `keep`.
4. Dropping `keep` must not invalidate `out`.
5. Repeated `get_tagged` calls must behave like cloning the stored value each
   time.
6. Nested refcounted payloads (`Str`, `List`, boxed closures, records, tag
   unions) must have correct ownership in both returned boxes.
7. The host must not repair, inspect, or special-case nested payload layout.

The current failure means this law is not established for the actual
`HostValue` path used by the dashboard.

## What Was Proven

### 1. The task payload transfer fix is still right

`Signal.task_source` now consumes task payloads:

- success path:
  [Signal.roc](/Users/luke/Documents/GitHub/roc/test/signals/platform/Signal.roc:70);
- failure path:
  [Signal.roc](/Users/luke/Documents/GitHub/roc/test/signals/platform/Signal.roc:79).

The wasm host records the payload take epoch, calls the callback, and asserts
that the payload was taken:

[wasm_host.zig](/Users/luke/Documents/GitHub/roc/test/signals/src/wasm_host.zig:457)

```zig
const payload = hostValueStr(payload_text);
setHostValueTypeTag(payload, task_payload.payload_tag);
const payload_take_epoch = hostValueTakeEpoch();
...
assertHostValueTakenAfter(payload, payload_take_epoch);
```

The remaining double-free happens after this, during source dispatch, so the
payload callback is no longer the whole explanation.

### 2. The first free is on the derived map dirty-eval path

The diagnostic phase markers are strict trap labels only. They do not change
ownership behavior.

Important phases:

- `421`: before `callErasedHostValueToHostValue` for a dirty `.map` transform:
  [engine.zig](/Users/luke/Documents/GitHub/roc/test/signals/src/engine.zig:5023).
- `455`: immediately before cloning the newly cached dirty map value:
  [engine.zig](/Users/luke/Documents/GitHub/roc/test/signals/src/engine.zig:2813).
- `409`: cached signal clone:
  [engine.zig](/Users/luke/Documents/GitHub/roc/test/signals/src/engine.zig:2784).

The ops harness reports:

```text
freed_phase=455 current_phase=409
```

So the first release happens after the `Signal.fold_task` map has produced and
cached the derived result, when the temporary map input is being cleaned up
under the latest phase label. The later release happens when the host clones a
cached value. This still implicates the split law: the temporary map input and
the retained source/cache path are not independent owners of the response
string backing.

### 3. Pure Roc controls are not enough to reproduce it

The pure control app exercises:

- shared `Box(Str)` aliases;
- generic `split_box`;
- `TaskStatus(Str)` split;
- `Box.unbox` followed by `Str.to_utf8`.

It builds and mounts successfully in the browser runtime. That means ordinary
Roc code can survive the simplified shape; the failure needs the hosted
`HostValue`/erased-callable path.

### 4. Replacing deep split with outer-box aliasing did not fix it

I tested a split shape equivalent to:

```roc
split = |boxed| { keep: boxed, out: boxed }
```

That delegates sharing to the outer `Box` refcount and avoids nested payload
copying. The ops dashboard still failed with the same `421 -> 409` double-free
shape in that run; the final current-tree run refined the location to
`455 -> 409`. So merely retaining/sharing the outer box is not sufficient.

### 5. Making derived transforms consume their temporary inputs did not fix it

I tested changing `Signal.map`/`map2`/`combine` transform callbacks to use
`take_tagged` and changed the engine to stop dropping inputs after transform
callbacks. The ops dashboard still failed with the same 1144-byte double-free
shape. That experiment was reverted.

This means the problem is deeper than "the map transform should consume the
temporary HostValue."

### 6. A reduced HostValue app reproduces a related boundary failure

`tmp_signal_str_to_utf8_repro.roc` stores a heap-backed `Str` in
`Signal.const`, maps it through `Str.to_utf8`, then renders the original signal.
It renders but traps on unmount under the strict allocator ledger with an
interior-pointer dealloc. That is a separate allocator/backend invariant, not
the same exact double-free shape, but it confirms that `Signal.map` over a
heap-backed `Str` at the HostValue boundary is enough to expose ownership
problems without HTTP or dashboard parsing.

## Current Root-Cause Assessment

The immediate root cause is not the HTTP task response transport. The response
enters Roc, is stored as `TaskStatus.Done(Str)`, and then the successful derived
path releases the response backing while cleaning up a temporary map input. A
later cached clone tries to release the same backing again.

The likely ownership failure is:

```text
HostValue cache owns Box(TaskStatus.Done(Str))
dirty eval clones/gets a HostValue for Signal.map
Signal.map reads it through HostValue.get_tagged
Dashboard.decode consumes the Str via Str.to_utf8/parse
the map input cleanup releases the response backing
the source/cache path still contains a value pointing at that same backing
later cached clone/drop releases the backing again
```

The host does not know enough to fix this. It cannot increment the nested `Str`
refcount without violating the design constraints. The typed split capability
was supposed to provide that ownership, but the actual erased boundary path has
not satisfied the split law.

## Architecture Assessment

Hypothesis C should be treated as a design risk, not a proven conclusion that
the architecture must be thrown away.

A host-owned graph is sound if the host owns only:

- graph topology;
- scheduling;
- dirty propagation order;
- effect/DOM execution;
- opaque handles;

and every typed operation on erased values comes from a complete monomorphized
capability.

The current design is fragile because those capabilities are fragmented:

- `TypeTag(a)` carries `split`;
- signal descriptors carry equality/drop/read/transform thunks;
- event descriptors carry payload tags/drops/transforms;
- row descriptors carry key/item equality/drop/hash/read behavior;
- cleanup paths sometimes use untagged `take`.

That is not automatically unsound, but it makes the host-owned erased registry
lifetime-critical in too many places.

## Sound Solution Directions

### Direction 1: Generated typed HostValue capabilities

Keep the host-owned graph, but replace source-level generic `split` as the
ownership foundation with generated, monomorphized capabilities. A complete
capability should likely be closer to:

```roc
HostValueOps(a) := {
    id : U64,
    split : Box(a) -> { keep : Box(a), out : Box(a) },
    drop : HostValue -> {},
    eq : HostValue, HostValue -> Bool,
    # maybe hash/read/accessor capabilities depending on use site
}
```

The exact shape may differ. The key is that a retained erased value must carry
one coherent ownership dictionary, not scattered partial thunks.

If generated glue can produce a real box clone/split helper from layout metadata,
this is the strongest medium-term continuation of the current architecture. It
preserves:

- host-owned graph;
- pure descriptor style;
- explicit typed operations;
- no host-side layout inference.

### Direction 2: Move typed cache/state transitions into Roc platform runtime

If generated typed capabilities become too broad or cannot be made auditable,
move more runtime graph state into Roc platform code. The app can still provide
a pure graph description, but Roc platform entrypoints would manage:

- task result resolution;
- event reducer application;
- signal recomputation;
- equality/cache updates;
- cleanup.

The host would then schedule effects and execute DOM commands.

This reduces host/Roc ownership crossings, but it does not magically eliminate
erasure. A heterogeneous runtime graph still needs existential state, generated
specialization, or capability dictionaries. This direction is justified if the
capability model keeps proliferating or cannot satisfy the split law.

### Direction 3: Do not add host-side Roc layout knowledge

Rejected approaches:

- increment nested `Str` refcounts in the host;
- accept alternate ARC deallocation pointer shapes in the allocator ledger;
- identify `TaskStatus.Done(Str)` payloads by pointer shape;
- clone strings with app-specific tricks before storing.

Those are workarounds. They either violate the explicit-data rule or hide the
bug behind host layout inference.

## Allocator Ledger Status

The wasm allocator ledger now tracks exact pointers and alignments and rejects:

- interior pointers:
  [wasm_host.zig](/Users/luke/Documents/GitHub/roc/test/signals/src/wasm_host.zig:728);
- already-freed pointers:
  [wasm_host.zig](/Users/luke/Documents/GitHub/roc/test/signals/src/wasm_host.zig:732);
- alignment mismatches:
  [wasm_host.zig](/Users/luke/Documents/GitHub/roc/test/signals/src/wasm_host.zig:740).

This is intentionally strict. It should not learn ARC pointer shapes. If Roc
calls `roc_dealloc` with an interior pointer, that is a compiler/runtime
invariant to investigate, not a host fallback case to accept.

## Recommended Decision Path

1. Keep the task payload `take_tagged` fix and host consume assertion.
2. Treat `HostValue.get/get_tagged` as split-and-replace clone, never as borrow.
3. Stop relying on source-level generic `TypeTag.split` as a proven capability
   for nested refcounted erased values.
4. Prototype or design generated typed HostValue capabilities from glue/layout
   metadata.
5. If generated capabilities are not enough or become unmanageable, move typed
   cache/state transitions into Roc platform entrypoints and leave the host as
   scheduler/effect/DOM executor.

## Validation Snapshot

Current checks run after the failed transform experiment was reverted:

```text
./zig-out/bin/roc check test/signals/apps/ops_dashboard.roc
zig test test/signals/src/native_host.zig
python3 test/signals/serve.py --example ops_dashboard --no-server --skip-tailwind
node /private/tmp/signals_ops_dashboard_harness.mjs
```

Results:

- Roc check: passed.
- Native host tests: passed.
- Ops wasm build: passed.
- Ops harness: still fails with the `1144` byte double-free, now localized to
  `freed_phase=455 current_phase=409`.

This is a useful failure: it proves the remaining bug is a HostValue split/clone
capability problem on the successful derived path, not a backend/server problem
and not just the task payload callback ownership bug.
