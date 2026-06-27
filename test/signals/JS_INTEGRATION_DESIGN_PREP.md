# JS Integration / Subscriptions Design Prep

Temporary working note for JavaScript integration in the Signals UI framework.
This is not an enduring design document yet. Its purpose is to capture the plan
shape, requirements, research notes, and open questions for `Sub`s, app-specific
JS interop, and browser mounting before the final design is folded into
`DESIGN.md`, `GUIDE.md`, and `NEXT_STEPS.md`.

## Why This Exists

`DESIGN.md` currently names `Sub(a)` as part of the intended app-facing API and
states the high-level rule that subscriptions are declared by structure and
started/stopped by the host. That is directionally correct, but it is not enough
for implementation or production-readiness.

Real web apps need long-lived browser and JS resources:

- WebSocket / EventSource streams;
- local/session storage events;
- browser online/offline status;
- visibility/focus/media-query state;
- third-party widgets and app-specific JS services;
- analytics or host-shell integration;
- multiple mounts or embedded widgets on one page.

These are not one-shot HTTP tasks. They need explicit subscription ownership,
lifecycle, test fakes, and boundary rules. Elm's ports are useful prior art for
this problem, but Signals should not copy Elm mechanically; it should adapt the
boundary discipline to the Signals effects-as-sources engine.

## Current Coverage in `DESIGN.md`

`DESIGN.md` covers `Sub`s only at a high level:

- `Core Concepts` says `Cmd / Sub` are typed effect requests and subscriptions;
  results re-enter the graph as source updates.
- `App-Facing API` lists `Sub(a)` as opaque to apps.
- `Glitch Freedom, Ordering, and Async` says `Sub`s are declared by structure and
  the host diffs the declared subscription set against the live set.
- `Async in the browser` covers timers and tasks, but not a general subscription
  descriptor or app-specific interop channel.
- `Open Questions` mentions multiple instances per page, but does not connect it
  to subscriptions or JS integration.

Missing details:

- no concrete `Sub(a)` constructors or descriptor shape;
- no place in `Elem` / scope structure where subscriptions are declared;
- no keyed identity for subscriptions;
- no start/stop diff algorithm beyond one sentence;
- no payload encoding format;
- no native fake model for inbound JS events;
- no app-specific interop channel comparable to Elm ports;
- no multiple-mount handle or per-mount JS registration model.

Conclusion: the enduring design has the right direction, but the plan is not yet
adequate. This prep doc should feed a future `DESIGN.md` section.

## Elm Ports Research Notes

Elm ports are an application-level interop boundary between Elm and JavaScript:

- outgoing ports produce `Cmd msg` values that JavaScript can subscribe to;
- incoming ports produce `Sub msg` values that JavaScript can send values into;
- payloads are limited to the types that can cross Elm's JS boundary, commonly
  JSON values for richer data;
- ports are deliberately coarse boundaries. Elm's guide recommends sending richer
  messages through a small number of ports rather than creating one port per JS
  function;
- ports are for applications, not packages, so the package ecosystem remains pure
  Elm.

The useful idea is the ownership boundary:

- Roc owns Roc app state;
- JS owns JS/browser resources that cannot or should not live in Roc;
- messages cross at a small explicit boundary;
- app-specific interop is allowed without making every JS API part of the core
  framework.

Signals should preserve that boundary discipline, but model integration using its
own primitives:

- outbound effects are explicit `Cmd`s;
- inbound events are host-owned sources/subscriptions;
- everything enters the one propagation queue;
- subscriptions are scope-owned and disposed deterministically;
- JS never decodes Roc layouts.

## Design Goals

1. **Make long-lived external resources first-class.** A subscription should be a
   retained source owned by a scope, not an ad hoc callback in JS.
2. **Keep JS thin.** JS owns browser resources and executes declared operations;
   it does not run reactive logic or reconstruct meaning.
3. **Avoid a generic unsafe FFI.** App-specific interop is useful, but it must
   cross through typed/declared channels with explicit payload formats.
4. **Keep packages portable.** Packages should not require arbitrary app ports;
   they should depend on platform capabilities or data types.
5. **Make native testing deterministic.** Every subscription type needs a native
   fake/injection path so behavior and work budgets are asserted without a real
   browser.
6. **Support multiple app instances.** JS resources, channels, listeners, and DOM
   ids must be scoped to a mount or WASM instance.

## Conceptual Model

### Subscription source

A `Sub(a)` represents a long-lived external source that can publish values of
`a`. The host starts it when its owning scope becomes active and stops it when the
scope is disposed or the declared subscription changes.

A subscription is different from a task:

- a task has one request and one success/failure result;
- a subscription may publish many values over time;
- subscription stop is normal lifecycle, not failure;
- subscription identity matters so the host can diff old vs new declarations.

### Scope ownership

Subscriptions should be declared inside explicit scopes, similar to state and
cleanup. The owner scope determines:

- when the subscription starts;
- when it is stopped;
- where retained callbacks/capabilities are released;
- which stale inbound messages must be ignored.

### Diffing declared subscriptions

On structural changes the engine should compare declared subscription descriptors
for each active scope:

- unchanged descriptor: keep existing external resource;
- changed descriptor with same construction site but different parameters: stop
  old resource and start new one;
- removed descriptor: stop old resource and release retained values;
- added descriptor: start new resource.

Diffing must not require a whole-app scan. It should follow the same structural
budget as other active-stream maintenance: O(affected scope), not O(total app).

### Inbound update path

External events enter as source updates:

1. JS/browser resource receives event/message.
2. JS encodes the declared payload into WASM-owned memory or another explicit
   boundary representation.
3. Host validates subscription id/generation and ignores stale events.
4. Host updates the subscription source value.
5. Propagation uses the same dirty queue as clicks, timers, and task results.

## Candidate App-Facing API Shapes

The exact API needs research. Possible concepts:

```roc
Sub(a)

Signal.from_sub : Sub(a), a -> Signal(a)
Ui.subscribe : Sub(a) -> Elem
```

or a component-style form that binds the subscription result to a signal source:

```roc
Ui.sub : Sub(a), (Signal(a) -> Elem) -> Elem
```

For app-specific interop, a ports-like channel could be introduced later:

```roc
Interop.Channel(a)
Interop.send : Channel(a), a -> Cmd
Interop.subscribe : Channel(a), a -> Sub(a)
```

These names are illustrative only. The design must account for Signals' current
`Elem`-driven app shape, scoped identity, retained callbacks, and confined
erasure.

## Built-In Subscription Candidates

Do not start by building every browser API. Use the smallest set that proves the
model:

- `Signal.interval` may be treated as the existing subscription-like canary;
- `Browser.online : Sub(Bool)`;
- `Browser.visibility : Sub(Visibility)`;
- `Browser.media_query : Str -> Sub(Bool)`;
- `WebSocket.messages : WebSocketConfig -> Sub(WebSocketEvent)` eventually;
- `Storage.changes : StorageArea -> Sub(StorageEvent)` eventually.

The first real milestone should include one browser-native subscription with
simple lifecycle and one app-specific interop channel, only if needed to prove the
escape-hatch shape.

## App-Specific JS Interop

A ports-like interop layer should be reserved for cases where a capability is
app-specific rather than platform-general. Examples:

- integrating a chart/map/editor widget;
- analytics or host-shell messages;
- app-specific storage/service-worker messages;
- temporary experiments before promoting a capability into the platform.

Requirements:

- channels are declared explicitly by the Roc app;
- JS handlers are registered at mount time, not discovered dynamically by Roc;
- payload format is platform-owned and layout-independent;
- inbound messages are tied to subscription ids and generations;
- outbound commands are batched through the host boundary, not direct JS calls
  from Roc;
- native specs can fake inbound and outbound messages;
- channels are scoped to a mount and cleaned up on unmount.

Non-goals:

- exposing arbitrary JS function calls to Roc;
- letting JS mutate engine state outside declared source updates;
- making framework packages depend on application interop channels;
- making the browser runtime a reactive runtime.

## Payload Boundary

Payloads need a representation that works for subscriptions and interop without
JS reading Roc layouts.

Candidates:

1. UTF-8 strings only.
   - too small for production interop, but useful for an initial spike.
2. JSON-like value format.
   - close to Elm ports and convenient for JS integration;
   - may allocate more and weakens static shape unless wrapped by typed Roc
     decoders/builders.
3. Small typed primitive-record format shared with generalized event payloads.
   - more consistent with Signals' descriptor discipline;
   - likely better long-term if the wire protocol grows dynamic records.
4. Bytes plus app-level codecs.
   - maximally simple boundary;
   - requires good library support to avoid every app hand-rolling codecs.

Research should align this with `WIRE_PROTOCOL_DESIGN_PREP.md` and
`ATTRIBUTE_EVENT_PAYLOAD_BOUNDARY_DESIGN_PREP.md`; event payloads, interop
payloads, and subscription payloads should not become three unrelated formats.

## Multiple Mounting

`DESIGN.md` currently lists multiple instances per page as an open question:
current host state is module-global, so two mounts need either two WASM instances
or an explicit per-mount handle.

This question is part of JS integration because subscriptions and interop need
mount-local ownership:

- DOM ids are per mount;
- event ids are per mount;
- subscription ids/generations are per mount;
- interop channels are per mount;
- JS cleanup on unmount must release listeners, intervals, sockets, and widget
  resources for only that mount.

Options:

1. **One WASM instance per mount.**
   - simplest engine model;
   - heavier memory/startup;
   - easy resource isolation.
2. **One WASM instance with explicit mount handles.**
   - better for many widgets;
   - requires every host export to take a handle;
   - forces all command buffers, ids, and JS registries to be mount-scoped.
3. **Single mount only for now, documented.**
   - acceptable for research;
   - poor production escape hatch and embedding story.

Recommendation for research: decide this before implementing app-specific interop
channels. If channels are global and mounting later becomes handle-based, the
migration will be costly.

## Browser Runtime Requirements

- Keep a per-mount subscription registry.
- Start/stop browser resources from explicit host commands or effect/subscription
  descriptors.
- Use generation/request tokens so late messages after stop are ignored.
- Encode inbound payloads without reading Roc layouts.
- Refresh WASM memory views after host calls that may allocate.
- Run cleanup on `roc_ui_unmount` for every live subscription and interop channel.
- Expose registration hooks for app-specific JS handlers without giving them
  direct access to engine internals.
- Record telemetry for subscribe/start, message, stop, ignored-late, handler
  error, and unmount cleanup.

## Native Host Requirements

- Deterministic fake subscriptions.
- Spec commands to send inbound subscription values by semantic name or declared
  test id.
- Assertions for start/stop counts and ignored-late events.
- Work metrics proving subscription diffing is bounded by changed scopes.
- Teardown assertions that retained callbacks/values and external-resource records
  are released.

## Relationship to HTTP / Effects

HTTP remains a first-class task capability using `roc-lang/http` types. It should
not wait for app-specific interop.

Shared concerns:

- request/subscription ids and generations;
- cancellation/stop semantics;
- payload boundary format;
- native fake infrastructure;
- browser telemetry;
- effect capability registry.

Different concerns:

- HTTP is one-shot and returns `Done`/`Failed`;
- subscriptions are long-lived and publish many values;
- ports-like interop is app-specific and should not shape package-level HTTP.

## Validation Plan

1. Add a small built-in subscription canary, such as online/visibility/media-query
   or promote interval into the same subscription infrastructure.
2. Add a native spec that proves:
   - subscription starts when scope enters;
   - unchanged descriptor does not restart;
   - parameter change restarts;
   - scope disposal stops;
   - late messages after stop are ignored;
   - retained values are released at teardown.
3. Add a browser contract test for:
   - subscribe/start command;
   - inbound payload encoding;
   - stop/unmount cleanup;
   - stale generation ignored.
4. Add a tiny app-specific interop spike only if needed:
   - one outbound command channel;
   - one inbound subscription channel;
   - JS registration at mount;
   - native fake coverage.
5. Decide the multiple-mount strategy before stabilizing JS handler registration.

## Outstanding Questions

- What is the concrete `Sub(a)` app-facing API and where is it declared in the
  `Elem` tree?
- Is `Signal.interval` a special signal source or should it be represented as a
  subscription internally?
- What identity key does a subscription use: construction site, explicit channel
  name, parameters, or a host-generated descriptor id?
- How are subscription parameter changes detected without app-authored equality
  footguns?
- What payload format should be shared by event payloads, subscription payloads,
  and app interop messages?
- Should app-specific interop be string/JSON-like for ergonomics or typed bytes
  for discipline?
- Are ports-like channels application-only, package-usable, or explicitly outside
  package contracts?
- Should outbound interop messages be `Cmd`s, command-buffer ops, or a separate
  effect channel?
- How do inbound interop messages identify their target source under multiple
  mounts?
- Should the production model allow multiple mounts in one WASM instance, or
  recommend one WASM instance per mount?
- What JS handler registration API should the browser runtime expose?
- How are handler errors surfaced: console telemetry, failed source value,
  diagnostics, or app-visible errors?

## Suggested First Milestone

Create a focused subscription/interoperability spike:

- introduce an internal subscription descriptor and route table;
- implement one simple built-in browser subscription or unify `Signal.interval`
  with the subscription lifecycle;
- add native fake injection and start/stop/late-message specs;
- add browser contract coverage for start/message/stop/unmount;
- document a provisional mount strategy;
- keep app-specific ports-like channels as a second slice unless a real example
  immediately needs them.

This milestone should produce enough evidence to fold the core `Sub` lifecycle
into `DESIGN.md` and add app-facing examples to `GUIDE.md`.
