# HTTP / Effects Capability Design Prep

Temporary working note for growing the Signals effect system from the current
research stub into a production-usable browser capability. This document captures
requirements and open questions before the final design is folded into
`DESIGN.md`, `GUIDE.md`, and `NEXT_STEPS.md`.

## Problem Statement

The core Signals model treats effects as sources: requests are started by `Cmd`,
results re-enter the signal graph as source updates, and scope disposal cancels
in-flight work. That foundation is strong.

The current public HTTP capability is intentionally tiny:

- `Http.get_text_task` constructs a task source by name;
- `Http.get_text` starts a request with a path string;
- the browser runtime routes a specific task-name prefix to `fetch`;
- policy is currently limited to documented Signals dev-server text endpoints.

This is not enough for production apps, which need real HTTP semantics,
structured request/response handling, cancellation, error classification, and a
clear story for adding non-HTTP effects without weakening the engine model.

## Current Context

Relevant current files:

- `test/signals/platform/Signal.roc`
  - `Task(a, err)`, `TaskStatus(a, err)`, `Signal.from_task`, `Signal.fold_task`,
    `Signal.start_str`, `Signal.task_source`, and `Signal.interval`.
- `test/signals/platform/Http.roc`
  - text GET wrapper around a task-name prefix.
- `test/signals/src/engine.zig`
  - pending task lifecycle, task result ingestion, source updates, scope disposal.
- `test/signals/browser/runtime.mjs`
  - JS fetch bridge and task telemetry.
- `test/signals/apps/async_effects.roc`
  - deterministic fake task, interval, cancellation, cleanup coverage.

Design constraints already established:

- effects are sources, not effects inside signal evaluation;
- result ordering enters the same propagation path as user events;
- request ids are host-assigned and tied to owning scope/node;
- disposing a scope cancels in-flight work;
- errors are values that apps render.

## Production Requirements

### HTTP request expressiveness

A production HTTP API should support at least:

- methods: `GET`, `POST`, `PUT`, `PATCH`, `DELETE`, maybe `HEAD`;
- URL or path construction with query parameters;
- request headers;
- request body variants:
  - none;
  - UTF-8 text;
  - JSON text or encoded value;
  - form URL encoded;
  - multipart/form-data eventually;
  - binary eventually;
- response access:
  - status code;
  - status text if useful;
  - headers;
  - body as text initially;
  - JSON decode path eventually;
  - binary/blob eventually;
- credentials / cookies policy;
- redirect policy if exposed;
- timeout and abort;
- retry/backoff policy as app-level composition or host-provided helper;
- cache policy, at least enough to avoid accidental browser defaults being
  surprising.

### Error model

Errors need to distinguish at least:

- network failure;
- timeout;
- cancellation due to scope disposal or explicit abort;
- non-2xx HTTP response if the API chooses to classify that as error;
- body decode failure;
- unsupported request shape;
- CORS/browser policy failure where observable.

Open design point: HTTP non-2xx can be either `Done(Response)` with status, or
`Failed(HttpErr)`. Production frameworks often need both styles. A good API may
provide low-level `send : Request -> Task(Response, TransportErr)` and higher
level helpers that map non-2xx into domain errors.

### Typed decoding

Roc apps should not receive untyped JS objects. Candidate staged approach:

1. support text response and app-authored parsing in Roc;
2. add JSON decode helpers when the Roc JSON story is ready for this platform;
3. add binary only when there is a concrete app need and a boundary format.

The host should not know the layout of `a` or `err`; typed conversion belongs in
Roc-provided callbacks/capabilities, consistent with confined erasure.

### Cancellation and lifecycle

- Every request must be owned by a scope or source node.
- Disposal cancels pending work and ensures late results are ignored.
- Explicit cancellation may be needed for user-driven abort without disposing the
  owning scope.
- Starting a new request from the same source needs defined behavior:
  - allow concurrent requests;
  - cancel previous request;
  - ignore stale results by generation;
  - keep previous value while loading or reset to loading.

The current `reset_on_start` flag is a useful seed but not the whole policy.

### Determinism and testing

Native specs need deterministic fake effects. Browser tests should cover only the
boundary contract and lifecycle, not duplicate app semantics.

Requirements:

- fake HTTP/task driver in native specs;
- ability to script success, failure, timeout, and cancellation;
- metrics for pending request count, started/canceled/resolved/ignored-late
  results;
- spec assertions for scope disposal cancellation and stale result behavior.

## Candidate API Directions

### Low-level typed request API

Possible Roc-facing data shape:

```roc
Request := {
    method : Method,
    url : Str,
    headers : List({ name : Str, value : Str }),
    body : Body,
    timeout_ms : [None, Some(U64)],
    credentials : Credentials,
}

Response := {
    status : U16,
    headers : List({ name : Str, value : Str }),
    body : Str,
}

Http.send_text : Request -> Task(Response, HttpError)
Http.start : Task(a, err), Request -> Cmd
```

This separates declaring a task source from starting a request, mirroring the
current `task_source` / `start_str` pattern but replacing the path string with a
structured request.

### Higher-level helpers

```roc
Http.get_text : Str -> Task(Str, HttpError)
Http.post_json_text : Str, Str -> Task(Response, HttpError)
Http.expect_2xx : Task(Response, HttpError) -> Task(Response, HttpError)
```

Need to avoid building a large framework prematurely. Start with enough helpers
to make examples readable, but keep the core request primitive explicit.

### Effect capability registry

HTTP should be one effect capability, not a special case that every future effect
copies poorly. Candidate concept:

- task source declares an effect kind and request encoder;
- browser runtime has handlers for effect kinds (`http`, `timer`, later storage,
  websocket, geolocation, etc.);
- host assigns request ids and owns lifecycle uniformly.

Open question: should effect kinds be string prefixes, integer handles, or a
closed host capability table?

## Browser Runtime Requirements

- Use `fetch` with `AbortController` for cancellation.
- Encode request data from WASM memory without reading Roc layouts.
- Materialize response data into WASM-owned buffers before calling
  `roc_ui_resolve`.
- Refresh memory views after any host call that may allocate.
- Ensure late settle after abort does not update disposed state.
- Provide telemetry for request start, resolve, reject, abort, ignored-late.

## Native Host Requirements

- Deterministic fake HTTP driver.
- Spec commands for resolving/rejecting named or indexed pending requests.
- No real network in ordinary native specs.
- Metrics for request lifecycle.
- Same engine path as browser result ingestion.

## Security and Policy Questions

- Should the framework restrict fetch URLs by default, or leave CSP/server policy
  to the app shell?
- How are credentials represented safely? Defaults should avoid accidentally
  sending credentials cross-origin.
- How are headers normalized? Are forbidden browser headers diagnosed?
- How does the dev server expose mock endpoints without shaping the production
  API around demos?
- Should `Http` include CSRF helpers, or is that app/domain code?

## Relationship to Subscriptions and Streaming

The design mentions `Sub(a)`, but the current concrete surface is mostly tasks
and intervals. Production apps may need:

- WebSocket / EventSource;
- online/offline status;
- visibility / focus status;
- media queries;
- storage events.

These should probably be subscriptions: long-lived sources declared by structure
and diffed by the host. Do not force streaming effects through one-shot HTTP
request machinery.

## Validation Plan

1. Add a native fake HTTP app/spec that covers:
   - successful text response;
   - non-2xx response;
   - network failure;
   - cancellation on scope disposal;
   - stale result ignored when a newer request wins.
2. Add browser contract tests for:
   - request encoding;
   - response buffer ownership;
   - abort on cancel;
   - memory view refresh around resolve.
3. Add minimal real browser demo using dev-server endpoint.
4. Add metrics to prove request lifecycle does not leak retained values or pending
   task records.

## Outstanding Questions

- What is the minimal request/response type to land first without painting the
  API into a corner?
- Should non-2xx be `Done(Response)` or `Failed(HttpError)` by default?
- Is JSON decode in scope for the first production HTTP milestone?
- Do we need concurrent requests per task source, or should a source have a
  latest-wins policy?
- How does explicit user cancellation appear in Roc? A `Cmd`, a state transition,
  or disposal only?
- How should timeouts be implemented in native specs vs browser runtime?
- What effect capability registry shape will also support storage, websockets,
  and browser APIs?
- How do we expose request telemetry without making it part of app semantics?

## Dependencies and Sequencing

This work is **mostly parallel** to the UI-boundary docs, because effects re-enter
through the existing signal-source path rather than the DOM attribute/event path:

- The main shared decision is with `WIRE_PROTOCOL_DESIGN_PREP.md`: whether effect
  start/cancel commands ride the render command buffer or a separate host→JS
  effect channel. Settle that before expanding effect commands.
- The browser runtime already has the cancellation primitive in place
  (`AbortController` and `roc_ui_resolve` in `runtime.mjs`), so the first
  milestone is mostly Roc-facing request/response shape plus a deterministic
  native fake driver, not new JS plumbing.
- Independent of the forms and attribute/event work, except that async form
  submit/validation will consume whatever request/cancellation model lands here.

Recommended order: can proceed immediately in parallel with the protocol slice;
it does not need to wait for the attribute/event boundary.

## Suggested First Milestone

Implement `Http.send_text` with structured request data and deterministic fake
native responses:

- methods, URL/path, headers, optional text body, timeout;
- response status, headers, text body;
- `HttpError` with network, timeout, canceled, unsupported;
- scope-disposal cancellation and ignored late result test;
- one browser contract test over real `fetch` mocked by the JS test harness.

Leave JSON, binary, multipart, websockets, and caching for follow-up milestones
unless a concrete app requires them immediately.
