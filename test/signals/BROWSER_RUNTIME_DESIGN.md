# Signals Browser Runtime — JS/WASM Architecture Design

> Status: design plus the G-B1/G-B2 browser spikes, the shared render-command
> buffer, a non-structural wasm runtime (`wasm_host.zig`), and automated
> executor + end-to-end counter guards (`browser/executor.test.mjs`,
> `browser/counter_app.test.mjs`) driven through a dependency-free DOM double.
> Structural `Ui.each`/`Ui.when`, async, and the shared-engine extraction are
> still ahead. This document proposes how a JavaScript runtime loads,
> instantiates, mounts, and drives a Signals Roc app compiled to `wasm32`. It is
> written against the *actual* code
> in this directory: `src/native_host.zig`, `src/wasm_host.zig`,
> `src/render_commands.zig`, `src/signal_graph.zig`, `src/scope_tree.zig`,
> `src/identity_table.zig`, `src/keyed_rows.zig`,
> `src/host_value_registry.zig`, `src/roc_platform_abi.zig`,
> `platform/main.roc`, and the authoritative `DESIGN.md`.

## 0. The framing that decides everything

The single most important observation is structural, not stylistic:

**The reactive runtime already exists, and it lives in the host, not in JS.**

`DESIGN.md` and `native_host.zig` define a complete runtime: a mutable node
table, topological-rank scheduler, dirty set, `is_eq` value pruning, scope
forest, keyed `Ui.each` diff with a hash index, event routing, and a fixed,
host-independent patch command set (`ResetDom`, `CreateElement`, `AppendChild`,
`SetText`, `SetValue`, `SetChecked`, `SetDisabled`, `SetRole`, `SetLabel`,
`SetTestId`, `BindClick`, `BindInput`, `BindCheck`). The native host applies
those patches to a *simulated* DOM; `DESIGN.md` states explicitly that "a
browser host implements the same commands against the real DOM."

So the browser runtime is **not** a fresh reactive architecture. It is:

1. A **dedicated browser host** (`wasm_host.zig`, today carrying only allocation
   + `HostValue` cells) that **reuses the reactive engine logic where that makes
   sense** and provides its own browser/DOM boundary. The engine logic
   (node table, scheduler, dirty set, scopes, keyed `Ui.each` diff, propagation,
   refcount discipline) is *not* native-host-specific and should be shared rather
   than reimplemented; the *boundary* (how patches reach the renderer, how events
   arrive, timers/`fetch`) is host-specific and is written fresh for the browser.
   The native and browser hosts are two distinct hosts behind one
   render-command/engine contract — not one host recompiled.
2. A **thin JavaScript DOM executor** that does nothing but apply the host's
   already-computed patch commands to real DOM nodes and forward real DOM events
   back as integer event ids.

**Sharing boundary (what is reused vs. host-specific):**

- *Shared, host-agnostic (reuse, do not duplicate):* the ABI primitives and
  refcount helpers already factored into `roc_platform_abi.zig`
  (`RocErasedCallable`, `incref/decrefErasedCallable`, `RocStr`/`RocList`, the
  descriptor structs), plus the reactive engine logic that `native_host.zig`
  currently holds inline — ingestion, node table, rank scheduler, dirty
  propagation with `is_eq` pruning, scope forest, keyed-row diff. Factoring this
  engine into a shared module (consumed by both hosts) is the preferred path; see
  Open Question O9.
- *Browser-host-specific (written fresh in `wasm_host.zig`):* the patch-op
  serialization into the JS command buffer, the `roc_ui_*` control exports, the
  browser timer/`fetch` bridge, `roc_dbg`/`roc_crashed` routing to `console`, and
  `memory.grow` coordination. The native host's *simulated DOM* and spec runner
  are equally native-specific and are **not** part of the browser host.

This framing keeps the design compatible with `AGENTS.md`: the engine that owns
identity, ownership, dirtiness, and scaling stays in one place (a shared engine
the host drives), JS never reconstructs meaning, and "where you built it is your
identity" is preserved end-to-end. **Every alternative that moves reactive logic
into JS violates the existing design and is rejected on that basis, not merely on
performance.**

The research notes (vdom platform) describe a *different* architecture — JS owns
a vdom-diff trampoline and Roc returns a `List Patch` per event. That design's
*tactics* (integer node ids, handler ids, accessor paths, in-place marshalling,
bit-packed returns) are excellent and we adopt them. Its *topology* (per-event
`roc_dispatch_event` FFI crossing, diff deciding patches) is superseded here:
this platform's host computes patches in-process with zero per-event Roc
entrypoint crossings, which is strictly cheaper than the vdom's per-event
crossing.

---

## 1. Recommended architecture: "Host owns logical identity, JS is a patch executor"

```
  Roc app (wasm)         Signals host (Zig, in wasm)        JS runtime (browser)
  --------------         ---------------------------        --------------------
  main : {} -> Elem      node table (mutable)               nodes[]   : Node[]
  pure descriptor   ──▶  scheduler / dirty set / scopes ──▶ listeners[]: Fn[]
  (roc_ui_init, once)    reducer + transform thunks         applyCmd(op, args...)
                         keyed each diff, ranks              forward event(id,payload)
  retained closures ◀──  host calls them in-process
  (no per-event FFI)     emits patch ops ─────────────────▶ exactly one DOM call per op
```

### Boundary contract (answers Q1)

- **WASM does not export a high-level render/update API, and JS never calls
  `roc_main`/`roc_ui_init` directly.** The only thing JS imports from Roc-side
  WASM is the *host's* small C-ABI surface (below). Roc's entrypoint is invoked
  *by the Zig host inside WASM*, exactly as `native_host.zig` does today. JS
  asks the host to "init"; the host calls `roc_ui_init`.
- **WASM owns logical identity; JS owns DOM identity; they are kept in lockstep
  by integer ids.** This is research-note Tactic 3, and it is also what the
  native host already does with its flat `DomElement` array. JS holds
  `nodes: (Node|null)[]`; the host holds dense node ids. The host emits
  `CreateElement(node_id, tag)` and JS stores `nodes[node_id] =
  document.createElement(tag)`. DOM nodes never cross the boundary.
- **The crossing is a patch-op stream, host→JS, plus an event call, JS→host.**
  Not a serialized tree (JS would have to interpret it), not a pull-based
  inspection API (JS would have to walk WASM memory and reconstruct meaning —
  forbidden by `AGENTS.md`), not a JS-side vdom diff (duplicates the engine).

### Why this is the recommendation

It is the *only* option that satisfies all of: no reactive logic in JS, JS never
inspects Roc memory for meaning, ownership stays host-owned, identity stays
host-owned, and it reuses the engine logic and patch set the native host already
proved out (sharing the code where it is host-agnostic, see O9) rather than
re-deriving the reactive model. The browser host and the simulated host become
two distinct hosts implementing one render-command/engine contract — which is
precisely the contract `DESIGN.md` already promised.

---

## 2. The JS/WASM ABI surface

Two layers cross the boundary. Keep them separate.

### Layer A — host C-ABI exports (already partly present in `wasm_host.zig`)

The host exports a tiny, stable, integer-only control surface. JS calls these:

```
// lifecycle
roc_ui_mount() -> void          // host runs roc_ui_init, ingests, emits initial patch stream
roc_ui_event(event_id: u32, payload_ptr: u32, payload_len: u32) -> u32  // returns packed flags
roc_ui_timer(token: u32) -> void                 // drive interval/timer source
roc_ui_resolve(request_id: u32, ptr: u32, len: u32, ok: u32) -> void   // async result
roc_ui_unmount() -> void        // dispose all scopes, drop descriptor, free retained closures

// memory (already exist for marshalling)
roc_alloc(len, align) -> ptr    // wasm_host.zig:71
roc_dealloc(ptr, align)         // wasm_host.zig:75
roc_realloc(...)                // wasm_host.zig:79

// plus the exported linear `memory`
```

`roc_ui_event` returns a **bit-packed integer** for `preventDefault` /
`stopPropagation` (research-note Tactic 6, `host.zig:108` in the vdom design).
This avoids returning a struct across the boundary for the common case.

The host *drives the engine entirely inside WASM*. There is no per-event Roc
entrypoint crossing — `roc_ui_event` enters the Zig host, which routes the event
id to its source node and calls the retained reducer thunk via
`RocErasedCallable` in-process (DESIGN.md §"Entry point"). This is the key
divergence from the vdom design and the reason the boundary is cheap.

### Layer B — the patch-op stream (host→JS)

When `roc_ui_mount`/`roc_ui_event`/etc. run, the host produces patch ops. The
shared command vocabulary and fixed-width record shape live in
`src/render_commands.zig`; `wasm_host.zig` exposes the command-buffer pointer,
length, record width, and clear operation. We must choose *how* JS receives
them. **Recommended: a single shared command buffer in linear memory that JS
drains synchronously after each host call**, rather than one host→JS function
call per op.

```
// JS imports exactly ONE function into WASM for DOM work:
env.host_flush()   // host signals "the command buffer is ready to drain"
```

The host appends fixed-width command records to a ring/append buffer in linear
memory (op-code + up to N integer operands + optional (ptr,len) for strings),
then calls `host_flush()` once. JS reads `cmdBuf` via `Int32Array` and dispatches
a `switch` over op-codes — exactly the 13-or-so DOM operations the patch set
already enumerates. This is the synthesis of two research-note tactics: minimize
*number* of crossings (one `host_flush` per host call, not one call per patch —
fixing the vdom's acknowledged "each patch is its own call" limitation) and make
each crossing cheap (in-place integer reads, `encodeInto`/`TextDecoder` for the
few strings).

Op-code table (mirrors the existing Zig render command set 1:1):

| op | operands | DOM effect |
|---|---|---|
| `CreateElement` | node_id, tag_ref | `nodes[id]=document.createElement(tag)` |
| `CreateText` | node_id, (ptr,len) | `nodes[id]=document.createTextNode(s)` |
| `AppendChild` | parent_id, child_id | `nodes[parent].appendChild(nodes[child])` |
| `SetText` | node_id, (ptr,len) | `nodes[id].nodeValue = s` |
| `SetValue` | node_id, (ptr,len) | guarded controlled-input value set |
| `SetChecked` | node_id, 0/1 | checkbox |
| `SetDisabled` | node_id, 0/1 | disabled attr |
| `SetRole`/`SetLabel`/`SetTestId` | node_id, (ptr,len) | a11y/test attrs |
| `BindClick`/`BindInput`/`BindCheck` | node_id, event_id, accessor_ref | `addEventListener` |
| `RemoveNode` | node_id | detach + `nodes[id]=null` |
| `MoveBefore` | node_id, ref_id | reorder (DESIGN.md reorder budget) |

`tag_ref` and `accessor_ref` are **integer enum indices into a JS string/array
table**, not transcoded strings — this is research-note Tactic 9 ("integer enums
for commonly-used strings"), adopted from day one for tag/attr/event names
because the cost is trivial and it removes the dominant per-crossing transcode.
Free-form strings (text content, input values) still cross as (ptr,len) UTF-8.

> Note: G-B3 made `RemoveNode` and `MoveBefore` first-class native-host render
> counters so the simulated host can assert the same detach/move budget a live
> DOM needs. The browser host still needs the command-buffer serialization and JS
> executor cases for these ops. They remain **host-emitted render commands**, not
> JS-side reconstruction — see Open Question O1.

The G-B1 controlled-input spike (`browser/controlled_input_policy.mjs`) rejects
blind `input.value = next` as the executor rule. `SetValue` is a guarded op:
equal values are no-ops; differing values are deferred while the target input is
focused or while composition is active; the latest deferred value is applied
after blur unless a later input echo already matched it. This keeps the command
set unchanged for the first browser milestone, but it means focused
normalization/masking needs an explicit future input-reconciliation design
instead of being smuggled into the thin executor.

---

## 3. Initial mount lifecycle (answers Q2)

1. **Fetch + instantiate.** `WebAssembly.instantiateStreaming(fetch(wasmUrl),
   imports)`. Falls back to `instantiate(arrayBuffer)` only for environments
   without streaming (this is a *loader* fallback, not a compiler-stage one — it
   is allowed).
2. **Imports.** The import object provides: `env.host_flush` (drain command
   buffer), and a small set of host capability imports the WASM host needs but a
   browser provides — `now()`, `setTimeout`/`clearTimeout` shims for timers,
   `fetch` bridge for async tasks, and `console`/`abort` for `roc_dbg` /
   `roc_crashed` (today these are no-ops/`@trap` in `wasm_host.zig:95-101`; the
   browser build routes them to `console`).
3. **Memory access model.** JS holds typed-array views over
   `instance.exports.memory.buffer` (`memory8 = Uint8Array`, `memory32 =
   Int32Array`). **Views must be re-created after every host export call that may
   allocate**, before JS reads command buffers, strings, or event payload memory.
   The G-B2 spike (`browser/wasm_memory_views.mjs`) chooses this
   rebuild-after-host-call rule instead of adding a host-bumped memory generation
   export for the initial runtime. The refresh checks whether `memory.buffer`
   changed and rebuilds cached views only when needed; the rule is that the
   refresh happens after each allocating host call. JS reads the command buffer
   and string bytes through these views; it **never interprets Roc heap layout
   for meaning** — only the explicit command records and explicit (ptr,len)
   string slices the host hands it.
4. **Call entrypoint.** JS calls `roc_ui_mount()`. Inside WASM the Zig host calls
   `roc_ui_init() -> Box(Elem)` (`platform/main.roc:30`), ingests the descriptor
   tree, mints ids, builds adjacency/ranks, computes initial values by calling
   transform thunks in rank order, and emits the initial patch stream into the
   command buffer, then calls `host_flush()`.
5. **Translate to DOM.** JS drains the buffer: a sequence of
   `CreateElement`/`CreateText`/`AppendChild`/`Set*`/`Bind*` that builds the
   subtree rooted at the mount node and appends it to `root`.

JS does **zero** UI interpretation during mount. It executes a flat op list.

---

## 4. Updates and events (answers Q3)

### Event registration and identity

- `BindClick(node_id, event_id, accessor_ref)` tells JS to attach a listener.
  `event_id` is the host-minted integer routing key (DESIGN.md §"Event
  routing", dense `event_id -> source_node_id` table). `accessor_ref` selects a
  pre-registered **CyclicStructureAccessor** describing which event fields the
  reducer needs (research-note Tactic 5) — e.g. `target.value` for `on_input`.
- **Event delegation, not per-node listeners (recommended).** Instead of
  `addEventListener` per node, attach one delegated listener per event *type* at
  the mount root and resolve `event_id` from a `data-roc-evt` attribute or a
  `WeakMap<Node, eventId>`. This avoids listener churn on every structural
  splice and keeps the `DESIGN.md` reorder/dispose budgets clean (no
  add/removeEventListener storm). Per-node listeners remain a valid simpler
  fallback for the first milestone.

### Dispatch

On a real DOM event, JS:
1. resolves `event_id`,
2. walks the accessor path against the live `Event` and serializes *only* the
   requested leaves into a freshly `roc_alloc`'d buffer (Tactic 5 + Tactic 6 —
   `Event` objects are cyclic and not serializable; never copy the whole event),
3. calls `roc_ui_event(event_id, payload_ptr, payload_len)`,
4. inside WASM the host routes to the source, calls the reducer thunk,
   propagates in rank order with `is_eq` pruning, emits minimal patches,
5. `host_flush()` fires; JS drains the (usually tiny) patch list,
6. JS reads the packed return flags and calls `preventDefault`/`stopPropagation`
   as requested.

This is **linear-with-changes** because the engine is the existing signals
engine; JS sees exactly the patches the host decided are minimal.

### Scheduling: synchronous dispatch, batched flush

Run `roc_ui_event` **synchronously** inside the DOM event handler (so
`preventDefault` and controlled-input semantics work — these are
microtask-order sensitive). Apply the resulting patch buffer synchronously too,
since it is already minimal. **Do not** introduce a rAF-coalescing layer in the
first milestone; add it later only for high-frequency sources (timers,
animation — `DESIGN.md` Open Questions flag this). Async results
(`roc_ui_resolve`) and timer ticks (`roc_ui_timer`) enter the *same* host
propagation path (DESIGN.md: "one ordering authority"), so JS scheduling stays a
single queue with no second code path.

### Re-rendering strategy

**Host-managed retained graph + host-produced patch stream.** Not full-tree
re-render (Elm-style, rejected by DESIGN.md), not JS vdom diff (duplicates
engine), not JS dirty-tracking (JS has no graph). The host already retains the
graph and emits only changed-node patches.

---

## 5. Data crossing the boundary and ownership (answers Q4)

The throughline from the research notes — *keep rich data on the Roc/host side,
let only integers and minimal byte fragments cross* — is the rule.

| Data | How it crosses | Owner |
|---|---|---|
| Node identity | integer `node_id` | host logical / JS DOM, lockstep |
| Event handlers | integer `event_id`; reducer closure stays in host | host (`RocErasedCallable`) |
| Tag/attr/event names | integer enum index into JS string table | static table |
| Text / input values | (ptr,len) UTF-8 in linear memory | transient, see below |
| Event payload | minimal JSON/bytes via accessor path | transient |
| Roc state, signals, boxed values | **never cross**; held in host node table | host |
| Function/callback handles | **never cross**; called in-process via `RocErasedCallable` | host |

### Refcount ownership rules (first-class, per `AGENTS.md`)

- The host holds **exactly one refcount per live retained closure/value** and
  zero for disposed ones (DESIGN.md "Leak invariant"). This is already true in
  `native_host.zig` via `increfErasedCallable`/`decrefErasedCallable` and the
  `HostValueCell` retain/drop discipline (`roc_platform_abi.zig`).
- **JS never owns Roc refcounts.** JS holds *DOM* nodes and *integer* ids only.
  When the host emits `RemoveNode`, JS detaches the DOM node and clears
  `nodes[id]`; the *refcount* drop for that scope's closures happens inside the
  host's scope-dispose path, not in JS. This cleanly separates the two ownership
  domains: DOM lifetime (JS) and Roc value lifetime (host).
- **String buffers JS receives are borrowed for the duration of the drain.** JS
  copies bytes into JS strings during `host_flush` drain; the host owns and
  frees the underlying allocation. Buffers JS *produces* for event payloads are
  `roc_alloc`'d by JS and ownership transfers to the host on the
  `roc_ui_event` call (matching the ABI rule in `roc_platform_abi.zig:6-9`:
  "Roc transfers ownership of refcounted arguments to the hosted function").
- **JS inspects Roc memory only through explicit, host-defined records.** It
  reads the command buffer (a host-defined wire format) and explicit (ptr,len)
  slices. It never decodes a `RocStr` header, a tag union, or a list layout to
  *infer* meaning. (Contrast: the vdom `host.js` decodes `RocStr` small-string
  bits inline. We deliberately avoid that here because the host can hand JS a
  plain (ptr,len) in the command record, removing JS's dependence on the
  `RocStr` ABI detail — see Open Question O3.)

---

## 6. DOM update model (answers Q5)

**Recommended: direct DOM mutation from JS, driven by the host's patch-op
stream, with the host retaining the logical graph.** JS is a "thin executor."

This is the right balance because:
- It puts *zero* reactive logic in JS.
- It reuses the existing host engine and patch set unchanged.
- It honors "explicit data over reconstructed meaning."
- The op-stream-in-a-buffer form fixes the vdom design's per-patch-call cost.

Rejected models and why:
- **JS vdom diff:** duplicates the host engine, reintroduces the O(view-size)
  per-event cost signals exist to remove, and makes JS reconstruct meaning.
- **WASM-retained DOM graph with JS as pure executor of a node-pointer protocol:**
  this is essentially what we have, but if it required JS to hold *no* `nodes[]`
  array and instead the host to address DOM nodes directly, it can't — WASM
  cannot hold DOM references. The integer-indexed `nodes[]` array is mandatory.
- **Hybrid (JS owns some retained subtrees):** splits ownership of DOM identity,
  reintroducing exactly the lockstep-desync hazard the integer-id scheme avoids.

---

## 7. Async effects model (answers Q6)

`DESIGN.md` already makes effects **sources**: results re-enter the graph through
the same propagation path as a click. The browser runtime preserves this.

- **Timers / `Signal.interval`.** The `IntervalSource` descriptor
  (`roc_platform_abi.zig:__AnonStruct39`, carries `period_ms` and a `token`) is
  ingested at init. The host asks JS (via an import) to `setInterval(period_ms)`
  keyed by `token`; on each tick JS calls `roc_ui_timer(token)`, the host bumps
  the interval source, propagates, flushes patches.
- **Fetch / tasks.** `TaskSource` (`__AnonStruct45`, carries `name`, `token`,
  and `done`/`failed`/`initial` thunks) declares the request. The host emits a
  "start request" command carrying a host-assigned `request_id` (DESIGN.md:
  "`Cmd` requests carry a host-assigned request id tied to the owning scope and
  node — never an app string"). JS performs the `fetch`, and on
  settle/reject calls `roc_ui_resolve(request_id, ptr, len, ok)`. The host
  folds it into `[Loading, Done(a), Failed(err)]` and propagates.
- **Browser events.** Already covered in §4.
- **Promise completion.** Maps onto `roc_ui_resolve`. The promise lives in JS;
  only the resolved bytes + `request_id` cross.
- **Cancellation / lifecycle.** A scope dispose (DESIGN.md scope path) cancels
  in-flight requests for that scope: the host emits a "cancel request_id"
  command; JS aborts the `fetch` (`AbortController`) / `clearInterval`. The host
  runs `Ui.on_cleanup` and drops the scope's retained closures. JS clears the
  corresponding `nodes[]`/listeners entries. No app string keys anywhere.

---

## 8. User-facing runtime API (answers Q7)

```js
import { mount } from "./roc-ui-runtime.js";

const app = await mount({
  wasmUrl: "/app.wasm",
  root: document.getElementById("app"),
  flags: {},                       // reserved; passed to a future init-with-flags entrypoint
  // optional capability overrides for testing:
  fetch: window.fetch.bind(window),
  now: () => performance.now(),
});

// app.dispatch(...) is intentionally NOT exposed: events flow through real DOM.
app.unmount();   // calls roc_ui_unmount(); host disposes all scopes, drops descriptor,
                 // frees retained closures; JS removes listeners + clears nodes[].
```

`mount` returns once the initial patch stream is applied. The object is a thin
handle: `{ unmount(), wasmInstance, root }`. Everything else is driven by the
DOM and the host. This shape is browser-first and has no Node-only assumptions;
capability injection (`fetch`, `now`, timer shims) is how tests and SSR
environments substitute behavior without a second code path.

---

## 9. Major tradeoffs (answers Q8)

| Dimension | **Recommended: host-engine + JS executor** | Alt A: JS vdom diff (the research-note design) | Alt B: JS holds reactive graph |
|---|---|---|---|
| Simplicity (JS side) | High — JS is a `switch` over ops | Medium — JS owns diff + patch | Low — full engine in JS |
| Simplicity (overall) | High — one engine, reused | Medium — engine split Roc/JS | Low — two engines |
| Runtime perf | Best — minimal patches, batched flush, no per-event FFI crossing | Good, but per-event `roc_dispatch_event` crossing + per-patch call | Poor — JS GC pressure, boundary chatter for state reads |
| Memory safety | High — JS touches only explicit records | Medium — JS decodes `RocStr` ABI inline | Low — state straddles boundary |
| Refcount correctness | High — single owner (host), proven by native host | Medium — handler free-list in JS+Roc | Low — two refcount domains |
| Debuggability | High — patch stream is inspectable/loggable; reuses native-host metrics | Medium | Low |
| Fit with Roc ownership | Exact — mirrors `native_host.zig` | Good | Poor |
| Browser-API fit | High — JS does all DOM/`fetch`/timers | High | High but tangled |
| Logic in JS vs WASM | ~5% JS / 95% WASM | ~40% JS | ~90% JS |
| Dependence on generated Zig ABI | Low — boundary is host-defined command records; ABI structs stay inside WASM | High — JS reads Roc layouts | Very high |

The recommended option is strongest on every dimension that `AGENTS.md` and
`DESIGN.md` make non-negotiable (ownership, no reconstruction, single engine),
and the cheapest at runtime. Alt A is the honest second place — it is a *known
working* shape (the vdom platform shipped it) and its tactics are largely
adopted; it loses only because this platform already removed the per-event FFI
crossing and already has the engine in the host. Alt B is included only to be
explicit about why "just write it in JS" is wrong here.

A genuine cost of the recommendation: the dedicated browser host
(`wasm_host.zig`, today only alloc + `HostValue`) must gain the reactive engine —
ideally by consuming a shared engine module factored out of `native_host.zig`
(O9) rather than copy-pasting it — and it requires command-buffer serialization
for the shared render command set, including `RemoveNode` and `MoveBefore`.
That is real work, but it is *additive* and keeps both hosts behind one
engine/command contract.

---

## 10. Minimal first milestone (answers Q9)

Smallest useful runtime that proves the boundary end-to-end on a real DOM, using
the `counter` app from `DESIGN.md` (button + text + button):

1. **Instantiate** the module with the import object (`host_flush`, `console`,
   `abort`, timer/`fetch` stubs that throw if used).
2. **Grow `wasm_host.zig`** to contain the minimal engine path: ingest
   `roc_ui_init`, build the node table for *non-structural* signals only
   (`source`, `map`, `map2`), and the `event_id -> source` table — reusing the
   shared engine logic for the subset the counter exercises rather than
   reimplementing it (O9). No `Ui.each`, no async yet.
3. **Define the command-buffer wire format** and emit the initial patch stream:
   `CreateElement`, `CreateText`, `AppendChild`, `SetText`, `BindClick`.
4. **JS executor**: drain the buffer, build DOM under `root`, attach one
   delegated `click` listener at root.
5. **Dispatch one event**: click `+`, JS resolves `event_id`, calls
   `roc_ui_event`, host runs the reducer thunk, propagates, emits a single
   `SetText`, JS applies it. Prove the displayed count changes.
6. **Update one signal**: the `count` map signal recomputes; assert exactly one
   `SetText` patch crosses (mirrors the native-host `nodes_recomputed` /
   `patches_emitted` discipline).
7. **Clean unmount**: `roc_ui_unmount` disposes the root scope, drops the
   descriptor and all retained closures (assert `closure_retains ==
   closure_releases`), JS removes the listener and clears `nodes[]`.

Milestone exit criterion: counter increments/decrements in a real browser, and
the same metric assertions the native host uses (one recompute, one patch per
click; zero retained-closure leak on unmount) hold.

> Status: steps 1–7 are implemented and guarded by `browser/counter_app.test.mjs`
> — the real `counter.wasm` mounts, `Increment`/`Decrement` change the count with
> exactly one `set_text` per click, and `roc_ui_unmount` drops every retained
> host value (`roc_ui_live_host_values()` returns to 0). The automated guard
> drives a DOM double, not a real browser; running `counter.html` in a real
> browser remains the manual QA surface. The one-recompute budget is proven for
> the single-sink counter (`set_text == 1`); the general multi-sink
> `nodes_recomputed == 1` pruning is deferred to the shared-engine extraction
> (O9) rather than added as bespoke wasm-host logic.

Deliberately **out of milestone 1**: `Ui.each`/`Ui.when` structural splicing,
focused-input normalization beyond the guarded `SetValue` rule, async tasks,
intervals, event delegation sophistication, and high-pressure allocation tuning.
G-B2 already pins the required `memory.grow` view-refresh rule; the milestone
runtime must use that helper, but does not need to stress repeated grows. These
line up with the `DESIGN.md` "Definition of Done" being *simulated-host* scoped
— the browser host is explicitly the next stage.

---

## 11. Risks and open questions (answers Q10)

These require inspecting compiler behavior, generated ABI, layout rules, or
browser constraints, and should be settled before milestone 2.

- **O1 — New render commands.** Settled for the native command set and the
  browser executor: `RemoveNode` and `MoveBefore` are shared host-emitted render
  commands, the native host's metrics assert move-only reorder plus explicit
  detach, and `browser/runtime.mjs` implements both ops against the DOM.
  `browser/executor.test.mjs` asserts a `MoveBefore`/`RemoveNode` response
  reorders and detaches live nodes. Remaining: exercise them from a real
  structural app (`Ui.each`/`Ui.when`) once the browser host grows row scopes
  (G-B6).

- **O2 — Command-buffer wire format vs. the simulated host's direct apply.** The
  native host applies patches by direct Zig calls into its `DomElement` array.
  The browser host must serialize patches into a buffer for JS. Decide whether to
  refactor the native host to also go through a buffer (one code path, better
  parity) or keep two emit paths behind one logical command enum.

- **O3 — Does JS ever need to read `RocStr` directly?** The recommendation says
  no: the host hands plain (ptr,len) in command records. Confirm the host can
  always materialize a contiguous UTF-8 slice for every string it needs to send
  (it can, via `RocStr.asSlice`, `roc_platform_abi.zig:225`) so JS never depends
  on small-string bit-twiddling. If any path can't, that path must add an
  explicit accessor, not a JS-side decode.

- **O4 — `memory.grow` and cached views.** G-B2 has a repo-local spike and guard
  in `browser/wasm_memory_views.mjs`. Finding: do not add a memory generation
  export for the initial runtime. Instead, every JS call into a host export that
  may allocate must refresh cached `Uint8Array`/`Int32Array`/`DataView` objects
  immediately after the call and before reading command buffers or string/payload
  bytes. The refresh compares `memory.buffer` identity and rebuilds only when it
  changed. The guard instantiates a tiny Wasm module, forces `memory.grow` via
  `roc_alloc`, proves the stale view detaches, then reads a known byte pattern
  across the old/new page boundary through refreshed views.

- **O5 — Controlled inputs / focus / IME / selection.** G-B1 has a repo-local
  spike and guard in `browser/controlled_input_policy.mjs` plus a manual browser
  harness in `browser/controlled_input_spike.html`. Finding: `SetValue` must not
  be applied unconditionally. Equal values are no-ops, and differing values are
  deferred while the input is focused or composing. This is enough for the G-B4
  counter milestone because it has no text input. It is not the full answer for
  focused masking, validation, or selection-preserving normalization; those need
  an explicit future input-reconciliation design before text-input-heavy browser
  apps are declared complete.

- **O6 — Event payload accessor serialization format.** The native host's event
  payloads are typed (`EventPayloadKind` unit/str/bool, `native_host.zig:55`).
  The browser must map a `CyclicStructureAccessor` walk to that exact typed
  payload. Confirm the accessor descriptor is carried in the descriptor tree (or
  add it to the `OnEvent` attr struct, `__AnonStruct56`) rather than
  reconstructed in JS.

- **O7 — Async ABI surface.** `roc_ui_resolve`/`roc_ui_timer` and the
  request-id/cancel commands are *new* host exports/commands. They must be
  defined against the existing `TaskSource`/`IntervalSource` descriptor fields
  (`__AnonStruct45`/`__AnonStruct39`) and routed through the one propagation
  queue. Needs the host's scheduler to accept externally-injected source
  updates, which `native_host.zig` already supports for its fake-result
  injection.

- **O8 — Multiple instances per page.** All host state in `wasm_host.zig` is
  module-global (`host_values`, etc.). Two mounts on one page need either two
  WASM instances or an explicit per-mount handle. Recommend one WASM instance per
  `mount()` for milestone simplicity; revisit for shared-module efficiency.

- **O9 — Factoring the shared engine.** The reactive engine logic
  (ingestion, node table, rank scheduler, dirty propagation with `is_eq`
  pruning, scope forest, keyed-row diff) currently lives inline in
  `native_host.zig`, interleaved with simulated-DOM and spec-runner code. To let
  the dedicated `wasm_host.zig` reuse it without copy-paste, this logic should be
  factored into a shared, host-agnostic engine module (alongside the primitives
  already in `roc_platform_abi.zig`) that both hosts drive, with the
  patch-emission and renderer boundary kept behind an interface each host
  implements. Question: how much can be extracted cleanly given the native
  host's `DomElement`-array coupling, and is the seam a render-command sink the
  engine writes to (preferred) or a set of callbacks? This is the first
  structural decision before G-B4, so neither host diverges into its own engine.
