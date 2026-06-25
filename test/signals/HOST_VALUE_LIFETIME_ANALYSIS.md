# Signals HostValue Lifetime and Ownership Analysis

This memo records the current proof state for the Signals `HostValue` lifetime
bug. It is intentionally about ownership design, not about making traps nicer.

## Short Version

The original task-payload ownership bug is real and the `take_tagged` direction
is sound: task result payloads are host-owned `HostValue` cells, and the
`done`/`failed` callbacks consume them. The host must not drop the payload after
the callback.

That is not the whole bug.

After the payload handoff was fixed, the ops dashboard still double-freed the
heap backing for the real `/api/ops/dashboard` response. Refined wasm phase
diagnostics showed this shape:

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

The reduced wasm repros proved the missing retain was in wasm `Str.to_utf8` for
non-small strings. Native/dev `Str.to_utf8` documents and implements that the
returned `List(U8)` is a seamless slice sharing the string allocation, so heap
strings must be incref'd before returning the list:
[str.zig](/Users/luke/Documents/GitHub/roc/src/builtins/str.zig:1402).
The wasm implementation copied the heap `RocStr` fields into a `RocList`
without that retain:
[WasmCodeGen.zig](/Users/luke/Documents/GitHub/roc/src/backend/wasm/WasmCodeGen.zig:13852).
Dropping the derived `List(U8)` could therefore release the task response
backing while the task-source cache still retained `TaskStatus.Done(Str)`.
Later cleanup released the cached task body again.

The architectural conclusion is narrower:

> A host-owned erased registry is only sound if every retained `HostValue` has a
> complete, coherent, monomorphized ownership capability. The current
> `TypeTag.split : Box(a) -> { keep, out }` is generated at app compile time, but
> split/eq/drop/read are still scattered across descriptor fields instead of
> bundled as one coherent per-edge capability.

The immediate dashboard trap is not proof that the host-owned graph architecture
is wrong. The reduced `HostValue` split repros now pass, and the dashboard fix
belongs to wasm builtin lowering parity for `Str.to_utf8`. The capability
coherence issue remains a design risk and audit target.

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

The later double-free happened after this, during source dispatch, so the
payload callback was no longer the whole explanation.

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

### 5. A task-status-only monomorphic split did not fix it

I tested two stronger split paths specifically for `TaskStatus(Str, Str)`:

1. a direct Roc platform entrypoint:

   ```roc
   split_task_status_str :
       Box(TaskStatus(Str, Str))
       -> { keep : Box(TaskStatus(Str, Str)), out : Box(TaskStatus(Str, Str)) }
   ```

2. a Zig prototype that used the generated ABI `TaskStatus` layout plus the
   generated recursive `increfTaskStatus` helper from the temporary glue output.

Both experiments were wired through a dedicated tag id and instrumented so the
host trapped if that branch was reached. The branch was reached, which proves
the dashboard's task-status source split was using the monomorphic path. The
ops dashboard still failed with:

```text
freed_phase=455 current_phase=409
```

That result is important but narrow:

- it does not prove generated/monomorphized capabilities are unsound;
- it proves that generating a capability for only `TaskStatus(Str, Str)` is
  insufficient;
- the failing value is still alive through downstream cached graph state after
  `fold_task`, decode, and ready-branch evaluation.

The dashboard creates app-level values derived from the same response backing:
decoded dashboard state, branch state, derived text, and repeated render inputs.
Those values are not platform ABI-visible types, so they cannot have helpers
baked into the prebuilt host or ahead-of-time platform ABI. They can still have
typed capabilities, because platform Roc modules are compiled into the app
artifact and monomorphized against app types. The mechanism is already visible
in `Signal.map`: `wrapped`, `eq`, `drop`, and `HostValue.new_tag({})`'s `split`
are boxed as `RocErasedCallable` values after instantiation at concrete app
types.

`DebugGlue` being platform-shaped is expected. It confirms only that
`roc_platform_abi.zig` is not the app-type capability generator. It does not
prove that app-level capabilities are impossible.

So the proof moved from:

```text
maybe TaskStatus(Str, Str) split is wrong
```

to:

```text
the host-owned graph needs complete typed capabilities for every cached
HostValue type, not only the task-result type
```

That also lines up with the Signals design note that erasure must be confined to
"one generated set of thunks per edge," pinned to monomorphized types:
[DESIGN.md](/Users/luke/Documents/GitHub/roc/test/signals/DESIGN.md:195). The
current implementation has `HostValue.new_tag({})` returning `id = 0` plus a
source-level split closure for generic app-level cells:
[HostValue.roc](/Users/luke/Documents/GitHub/roc/test/signals/platform/HostValue.roc:17).
That closure is monomorphized by the app build, but it is still only one
operation in a scattered edge contract, not a complete ownership dictionary.

### 6. Making derived transforms consume their temporary inputs did not fix it

I tested changing `Signal.map`/`map2`/`combine` transform callbacks to use
`take_tagged` and changed the engine to stop dropping inputs after transform
callbacks. The ops dashboard still failed with the same 1144-byte double-free
shape. That experiment was reverted.

This means the problem is deeper than "the map transform should consume the
temporary HostValue."

### 7. A reduced HostValue app exposed a generated box-alignment bug

`tmp_signal_str_to_utf8_repro.roc` stores a heap-backed `Str` in
`Signal.const`, maps it through `Str.to_utf8`, then renders the original signal.
It renders but traps on unmount under the strict allocator ledger with an
interior-pointer dealloc.

A smaller `Status.Done(Str)` HostValue repro also rendered successfully but
trapped on unmount. The improved ledger showed:

```text
roc_dealloc received an interior pointer
offset=4 requested_size=16 allocated_size=16 tracked_align=8
```

That was not the 1144-byte dashboard double-free. It was a generated Zig glue
bug for boxed payload alignment: known non-refcounted boxed payloads such as
`Box(U64)` were released through pointer-aligned `decrefBox`, which is invalid
on wasm32 when the payload alignment is 8. `ZigGlue.roc` now emits
`decrefBoxWith(..., @alignOf(payload), null, ...)` for known non-refcounted box
payloads, and `roc_platform_abi.zig` was regenerated with `roc glue`.

After that fix, both reduced HostValue cleanup repros mounted and unmounted
cleanly. This is evidence that the monomorphized `HostValue.new_tag` split
closure can satisfy the split law in the reduced `Status.Done(Str)` case. At
that point the ops dashboard still failed with the original 1144-byte
double-free, which led to the `Str.to_utf8` reductions below.

### 8. Decode-only localized the remaining double-free to `Str.to_utf8`

`tmp_repro_decode_only.roc` keeps the HTTP task, `Signal.fold_task`, and
`Dashboard.decode`, but renders only `"ready <version>"`. It does not build the
full dashboard view. With the real deterministic payload it:

- resolves and renders successfully when the harness skips unmount;
- used to double-free the same 1144-byte response allocation on unmount.

So the successful async update can complete in a minimal decode-only graph. The
remaining double-free was tied to cleanup of retained state after a successful
decode.

Two smaller reductions separated `Str.to_utf8` from the list-slice parser:

| Repro | Body operation | Before wasm fix | After wasm fix |
|---|---|---|---|
| `tmp_repro_decode_no_utf8.roc` | `Str.count_utf8_bytes(body)` | pass | pass |
| `tmp_repro_decode_to_utf8_only.roc` | `List.len(Str.to_utf8(body))` | double-free on unmount | pass |

This proves `List.drop_first`, parser records, and dashboard view construction
are not required. The minimal trigger was:

```text
source cache retains TaskStatus.Done(Str)
fold_task split provides a cloned input Str to a decode map
decode calls Str.to_utf8(body)
the returned List(U8) aliases the same heap backing without an incref
dropping the List(U8) frees the body backing
later source/cache cleanup drops TaskStatus.Done(Str) and frees it again
```

The compiler model already said this builtin can retain or release sharing
arguments:
[LowLevel.zig](/Users/luke/Documents/GitHub/roc/src/base/LowLevel.zig:653).
The native builtin already retained the heap string. The wasm backend was the
outlier.

The fix is in the wasm non-SSO branch:
[WasmCodeGen.zig](/Users/luke/Documents/GitHub/roc/src/backend/wasm/WasmCodeGen.zig:13938).
It now increfs the shared data pointer before constructing the `RocList`.

## Current Root-Cause Assessment

The immediate root cause was not the HTTP task response transport and not a
server/backend issue. The response entered Roc, was stored as
`TaskStatus.Done(Str)`, and then the successful derived path called
`Str.to_utf8` while decoding the body. The wasm implementation returned a
`List(U8)` alias to the body backing without retaining that backing. A later
temporary cleanup released the list, which released the body allocation while
the source cache still owned the same body through `TaskStatus.Done(Str)`.
Source/cache cleanup then released the body again.

The fixed ownership flow is:

```text
HostValue cache owns Box(TaskStatus.Done(Str))
dirty eval clones/gets a HostValue for Signal.map
Signal.map reads it through HostValue.get_tagged
Dashboard.decode consumes the Str via Str.to_utf8/parse
Str.to_utf8 returns List(U8) sharing the Str allocation and increfs the backing
parse/list cleanup decrefs the List(U8) owner
source/cache cleanup later decrefs the TaskStatus.Done(Str) owner
the final owner releases the allocation exactly once
```

The host still does not know enough to fix this by incrementing nested `Str`
refcounts itself; that would violate the design constraints. The correct layer
was the typed builtin/codegen implementation that creates the aliased
`List(U8)`.

The reduced repro now suggests the generic monomorphized split body is not
automatically unsound. The remaining design risk is coherence and sequencing of
the scattered capabilities on successful async paths: split, transform,
equality, drop, render reads, and cache replacement are owned by separate
descriptor fields and host steps.

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

## Proof Conclusion

The prebuilt host constraint is real: the Zig host and `roc_platform_abi.zig`
cannot know app-defined types. That does not block generated capabilities,
because platform Roc code is compiled into the app artifact and monomorphized at
app compile time. `RocErasedCallable` is already the stable ABI for passing
those app-generated capabilities to the type-blind host.

The reduced HostValue split repro now passes, so a compiler ARC bug in the
generic `split = Box.unbox; Box.box; Box.box` body is not the leading conclusion.
The `decode_no_utf8` / `decode_to_utf8_only` pair proved the immediate
1144-byte bug was wasm `Str.to_utf8` lowering, not `TaskStatus` splitting,
`List.drop_first`, or dashboard-specific record construction.

The generated/monomorphized capability design remains sound if it is coherent:
each retained HostValue edge should carry the exact typed ownership operations
for that edge's concrete value type, generated by monomorphized platform Roc
closures rather than host-side layout knowledge.

The next sound implementation step is not "add a TaskStatus helper" and not
"increment Str in the host." For the immediate bug, the right fix is the wasm
`Str.to_utf8` retain. For the broader design, bundle the per-edge operations
that already exist as app-compiled closures into a coherent capability object,
then audit host sequencing against that object. If that remains unmanageable,
move more typed cache/state transitions into Roc platform code.

## Sound Solution Directions

### Direction 1: Complete app-compiled HostValue capabilities

Keep the host-owned graph, but bundle the monomorphized platform Roc closures
that already exist into one per-edge capability. A complete capability should
likely be closer to:

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

This is the strongest medium-term continuation of the current architecture. It
preserves:

- host-owned graph;
- pure descriptor style;
- explicit typed operations generated at app compile time;
- no host-side layout inference.

The task-status-only experiment shows the required scope. The capability set
must cover every host-retained cached value type in the graph, including
app-level types, not just task-result types. The app-compile-time platform Roc
path can express those capabilities; the missing piece is bundling and using
them coherently.

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
3. Keep the strict allocator ledger; it exposed the generated boxed-payload
   alignment bug without accepting interior dealloc pointers.
4. Bundle the app-compiled per-edge closures (`split`, `drop`, `eq`, reads,
   transforms) into one coherent HostValue capability object.
5. Keep the wasm `Str.to_utf8` implementation aligned with the native builtin:
   heap strings return an aliased list and must retain the shared backing.
6. Audit the dirty async success path against a coherent capability object,
   especially cache replacement and post-transform cleanup ordering.
7. If bundled capabilities are not enough or become unmanageable, move typed
   cache/state transitions into Roc platform entrypoints and leave the host as
   scheduler/effect/DOM executor.

## Validation Snapshot

Current checks run after reverting the temporary TaskStatus entrypoint,
regenerating `roc_platform_abi.zig` with `roc glue`, fixing generated
boxed-payload alignment, and fixing wasm `Str.to_utf8` heap-string retain:

```text
./zig-out/bin/roc check test/signals/apps/ops_dashboard.roc
zig test test/signals/src/native_host.zig
node --test test/signals/browser/runtime_contract.test.mjs
python3 test/signals/serve.py test/signals/apps/tmp_hostvalue_split_status_get_repro.roc --output /private/tmp/tmp_hostvalue_split_status_get_repro.wasm --no-server --skip-tailwind
node /private/tmp/signals_mount_harness.mjs /private/tmp/tmp_hostvalue_split_status_get_repro.wasm "len"
python3 test/signals/serve.py test/signals/apps/tmp_signal_str_to_utf8_repro.roc --output /private/tmp/tmp_signal_str_to_utf8_repro.wasm --no-server --skip-tailwind
node /private/tmp/signals_mount_harness.mjs /private/tmp/tmp_signal_str_to_utf8_repro.wasm "len"
python3 test/signals/serve.py --example ops_dashboard --no-server --skip-tailwind
node /private/tmp/signals_ops_dashboard_harness.mjs
python3 test/signals/serve.py test/signals/apps/tmp_repro_decode_only.roc --output /private/tmp/tmp_repro_decode_only.wasm --no-server --skip-tailwind
node /private/tmp/signals_task_harness.mjs /private/tmp/tmp_repro_decode_only.wasm "ready" --no-unmount
node /private/tmp/signals_task_harness.mjs /private/tmp/tmp_repro_decode_only.wasm "ready"
python3 test/signals/serve.py test/signals/apps/tmp_repro_decode_no_utf8.roc --output /private/tmp/tmp_repro_decode_no_utf8.wasm --no-server --skip-tailwind
node /private/tmp/signals_task_harness.mjs /private/tmp/tmp_repro_decode_no_utf8.wasm "bytes"
python3 test/signals/serve.py test/signals/apps/tmp_repro_decode_to_utf8_only.roc --output /private/tmp/tmp_repro_decode_to_utf8_only.wasm --no-server --skip-tailwind
node /private/tmp/signals_task_harness.mjs /private/tmp/tmp_repro_decode_to_utf8_only.wasm "bytes"
./zig-out/bin/roc check test/signals/apps/task_to_utf8_lifetime.roc
python3 test/signals/serve.py test/signals/apps/task_to_utf8_lifetime.roc --output /private/tmp/task_to_utf8_lifetime.wasm --no-server --skip-tailwind
node test/signals/browser/task_lifetime_harness.mjs /private/tmp/task_to_utf8_lifetime.wasm success
node test/signals/browser/task_lifetime_harness.mjs /private/tmp/task_to_utf8_lifetime.wasm failure
zig build roc
zig build run-test-zig -- --test-filter "signals host task result callbacks consume heap string payloads"
zig build run-test-zig -- --test-filter "render command"
zig build run-test-signals
zig build run-test-cli -- --suite glue --filter "ZigGlue"
```

Results:

- Roc check: passed.
- Native host tests: passed, including the heap string task-payload consume
  regression, through `zig build run-test-zig`.
- Render command tests: passed through `zig build run-test-zig`.
- Runtime contract tests: passed, including async resolve trap reporting.
- Reduced `Status.Done(Str)` repeated HostValue split/read/unmount repro:
  passed after the generated boxed-payload alignment fix.
- Reduced `Signal.const(Str) -> Str.to_utf8` HostValue repro: passed after the
  generated boxed-payload alignment fix.
- Decode no-utf8 task repro: passed before and after the wasm fix.
- Decode to-utf8-only task repro: failed before the wasm fix and passes after
  the wasm fix.
- Decode-only task repro: passes async resolve and unmount after the wasm fix.
- Checked-in task-to-UTF-8 wasm regression: passed for both `Done(Str)` and
  `Failed(Str)` task results. The harness asserts ready/failed text appears
  while mounted and all HostValues are released after unmount.
- Ops wasm build: passed.
- Deterministic ops wasm harness: passed after removing the stale pre-unmount
  zero-live-HostValues assertion; it now checks render text while mounted and
  zero live HostValues after unmount.
- `zig build roc`: passed.
- `zig build run-test-signals`: initially exposed a stale
  `async_effects.txt` metric expectation. Closing the async panel now releases
  six more retained allocations, so the expected `retained_alloc_delta` changed
  from `-7` to `-13`; after that fixture update, the full Signals suite passed.
- Filtered ZigGlue CLI integration: 3 of 4 filtered cases passed; the `fx`
  ZigGlue case failed on an unrelated generated `Padded` struct layout assertion
  before exercising this boxed-payload change.

The latest ops wasm harness no longer traps in `roc_ui_resolve` or on unmount.
The old harness still had a stale pre-unmount assertion that expected
`runtime.liveHostValues() == 0` while the app was mounted; after successful
render that value is non-zero by design. The meaningful cleanup assertion is
after `runtime.unmount()`.
