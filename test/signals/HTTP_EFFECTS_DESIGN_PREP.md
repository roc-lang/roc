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
- request and result payloads are UTF-8 strings;
- policy is currently limited to documented Signals dev-server text endpoints.

This is not enough for production apps, which need real HTTP semantics,
structured request/response handling, cancellation, error classification, and a
clear story for adding non-HTTP effects without weakening the engine model.

The HTTP type surface should not be invented inside Signals. Signals should align
with a release of `roc-lang/http`, using that package's standard request,
response, method, header, body, and timeout types from the platform header. The
shared package provides platform-agnostic HTTP *data structures only*; Signals
supplies the execution.

Important alignment caveat: `roc-lang/http` is written around an effectful
`send!` function (the `Task`/`!` calling convention common to other Roc
platforms). Signals does **not** adopt that convention. Signals models effects as
sources: a request is a `Cmd` and the result re-enters the graph as a source
update. So Signals consumes the package's *types* (`Request`, `Response`,
`Method`) but keeps its own effects-as-sources execution model rather than
exposing a package-style `send!`.

## Research Notes

This document is now focused on package-aligned HTTP. General JavaScript
integration, `Sub`s, Elm ports, app-specific interop, and multiple mounting are
covered in `JS_INTEGRATION_DESIGN_PREP.md`.

### `roc-lang/http`

`roc-lang/http` is currently designed as **data structures only, no effects**. Its
README states that platforms expose primitives such as `Http.send!` in terms of
the package-defined `Request` and `Response` types, while platform-agnostic
packages can depend on those types and receive the effectful send function from a
platform or a pure mock in tests.

The package currently exposes `Method`, `Request`, and `Response` modules. The
fetched `main` branch shows the concrete shapes (verify against the pinned
release, since these may evolve):

- `Method := [OPTIONS, GET, POST, PUT, DELETE, HEAD, TRACE, CONNECT, PATCH,
  Unknown(Str)]` — already has an `Unknown(Str)` escape for custom methods;
- `Request : { method : Method, headers : List((Str, Str)), uri : Str,
  body : List(U8), timeout_ms : [TimeoutMilliseconds(U64), NoTimeout] }`;
- `Response : { status : U16, headers : List((Str, Str)), body : List(U8) }`;
- constructors `Request.from_method` / `Response.from_status` plus `with_*` and
  `add_header` builders and field accessors.

Note the concrete shapes so Signals does not reinvent them differently: headers
are `(Str, Str)` tuples (not `{ name, value }` records), timeout is a
`[TimeoutMilliseconds(U64), NoTimeout]` tag union (not `[None, Some(U64)]`),
bodies are byte lists (`List(U8)`), the request field is `uri` (not `url`), and
`Response` carries no status-text field.

Caveat on opacity: the types are **transparent records today** (the `.{ ... }`
method blocks are accessors/builders over an exposed record). The README states
the *intended* direction is to make `Request`/`Response` opaque and `Method`
non-exhaustive for forwards compatibility. Signals should therefore program
against the constructor/builder/accessor functions now, not the record fields, so
the later move to opaque types is a non-breaking change for the platform.

## Current Context

Relevant current files:

- `test/signals/platform/main.roc`
  - currently exposes `[Elem, Signal, Html, Ui, Http]` and has `packages {}`;
  - should import a released `roc-lang/http` package once the HTTP milestone
    starts.
- `test/signals/platform/Signal.roc`
  - `Task(a, err)`, `TaskStatus(a, err)`, `Signal.from_task`, `Signal.fold_task`,
    `Signal.start_str`, `Signal.task_source`, and `Signal.interval`.
  - current task sources convert success and failure payloads from `Str`, so HTTP
    response typing will require broadening the task payload bridge beyond strings.
- `test/signals/platform/Http.roc`
  - current text GET wrapper around a task-name prefix;
  - should become a Signals adapter over `roc-lang/http` types, not a replacement
    HTTP type package.
- `test/signals/src/engine.zig`
  - pending task lifecycle, task result ingestion, source updates, scope disposal.
  - current `appendPendingTask` stores `task_name` plus request bytes and assigns a
    host request id.
- `test/signals/browser/runtime.mjs`
  - JS task bridge already has `AbortController`, `cancelTask`, `resolveTask`, and
    task telemetry;
  - current `startTask` accepts `{ requestId, name, request, signal }` and resolves
    success/failure as strings.
- `test/signals/apps/async_effects.roc`
  - deterministic fake task, interval, cancellation, cleanup coverage.

Design constraints already established:

- effects are sources, not effects inside signal evaluation;
- result ordering enters the same propagation path as user events;
- request ids are host-assigned and tied to owning scope/node;
- disposing a scope cancels in-flight work;
- errors are values that apps render;
- JS must not decode Roc layouts or reconstruct typed values.

## Production Requirements

### HTTP request expressiveness

The API should expose the capability of the selected `roc-lang/http` release
rather than hand-defining new request/response records in Signals. At the time of
research, that means at least:

- methods from `http.Method`, including custom/unknown methods where supported;
- request URI;
- request headers, preserving duplicate header support;
- request body as bytes;
- request timeout;
- response status;
- response headers, preserving duplicate header support;
- response body as bytes.

Browser-specific policy still belongs to Signals or the browser host where it is
not represented by the package type:

- credentials / cookies policy;
- redirect policy if exposed;
- cache policy;
- CORS/browser policy errors where observable;
- abort behavior and late-result handling;
- mapping package request data onto `fetch` without JS decoding Roc layouts.

Future package releases may add more request/response metadata. Signals should
use package accessors/builders so adding such fields does not require breaking app
or package code.

### Error model

Separate three concerns:

1. **HTTP response data**: a completed HTTP exchange, including non-2xx status,
   should generally be `Done(Response)` at the low level. A `404` is not a
   transport failure; it is a response the app may want to inspect.
2. **Transport/runtime failure**: network failure, timeout, cancellation,
   unsupported request shape, response materialization failure, or observable
   browser policy failure should be `Failed(HttpError)`.
3. **Domain failure**: helpers may map non-2xx or body decode failures into
   app/domain errors, but that should be a helper layer over the low-level send.

Errors need to distinguish at least:

- network failure;
- timeout;
- cancellation due to scope disposal or explicit abort;
- unsupported request shape;
- CORS/browser policy failure where observable;
- response body materialization failure;
- decode failure in higher-level helpers.

Open question: cancellation due to scope disposal may be an internal lifecycle
completion rather than a user-visible `Failed(Canceled)`. Explicit user abort may
need to be visible.

### Typed decoding

Roc apps should not receive untyped JS objects. Candidate staged approach:

1. support `roc-lang/http.Response` with body bytes;
2. provide text convenience helpers in Roc by decoding response bytes;
3. add JSON decode helpers when the Roc JSON story is ready for this platform;
4. add binary-specific helpers only when a concrete app needs them.

The host should not know the layout of decoded `a` or `err`; typed conversion
belongs in Roc-provided callbacks/capabilities, consistent with confined erasure.
The browser runtime should only execute `fetch`, copy bytes, and report
transport metadata.

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

- pure/mock HTTP workflows based on `roc-lang/http.Request` and `Response`;
- ability to script success, non-2xx response, network failure, timeout, and
  cancellation;
- metrics for pending request count, started/canceled/resolved/ignored-late
  results;
- spec assertions for scope disposal cancellation and stale result behavior.

## Candidate API Directions

### Align with `roc-lang/http` package types

The platform header should import a released `roc-lang/http` package and expose or
re-export only the pieces the app needs. The exact package URL/version should be
pinned when implementation starts.

Conceptual platform shape:

```roc
platform ""
    exposes [Elem, Signal, Html, Ui, Http]
    packages {
        http: "https://github.com/roc-lang/http/releases/download/<version>/http.tar.zst",
    }
```

Exact syntax and package URL are implementation details to verify against Roc's
package manager and the selected release artifact.

The Signals `Http` module should consume package types instead of declaring local
request/response records. Apps build a request through the package builders, e.g.
`Request.from_method(GET) |> Request.with_uri("...")`, and read responses through
`Response.status` / `Response.headers` / `Response.body`. Final symbol names must
match the selected package release.

Because the package types are still transparent today but intended to become
opaque, Signals must program against the builder/accessor functions, not the
record fields. This is also what keeps the host honest: the platform-side Roc
adapter lowers a package `Request` into an explicit boundary representation using
package accessors, and lifts returned status/headers/body bytes into a package
`Response` using package builders. Zig and JS should receive only that explicit
boundary representation; they must not duplicate, pattern-match, or otherwise
know the package's record layout.

### Low-level send primitive

The low-level primitive should be close to (using the package types but Signals'
effects-as-sources execution, not the package's `send!`):

```roc
Http.send : Request -> Task(Response, HttpError)
```

or, if the existing two-step split between source declaration and start command is
preserved:

```roc
Http.request_task : {} -> Task(Response, HttpError)
Http.start : Task(Response, HttpError), Request -> Cmd
```

where `Request` and `Response` are the `roc-lang/http` types. This mirrors the
current `task_source` / `start_str` design while replacing the string request with
a package-defined `Request` value.

Note the success type is the package `Response` (a completed exchange, including
non-2xx) and only the error type is Signals-specific (`HttpError` for transport /
runtime failures). Response *bodies are `List(U8)`*, so text/JSON helpers decode
bytes in Roc rather than assuming the host returns a string.

Research question: whether Signals should keep a two-step `Task` declaration plus
`start` command, or expose a one-shot helper that hides that split while still
emitting the same descriptor data for the engine.

### Higher-level helpers

Helpers should be thin wrappers over package types, not alternative HTTP types
(`Response` below is the `roc-lang/http` type):

```roc
Http.get : Str -> Task(Response, HttpError)
Http.get_text : Str -> Task(Str, HttpError)   # decodes Response body bytes to Str
Http.expect_2xx : Task(Response, HttpError) -> Task(Response, HttpError)
```

`Http.get_text` is the migration path for the current string-only `get_text`
stub: same app ergonomics, but it now decodes the `Response` body (`List(U8)`) to
`Str` instead of receiving a string straight from the host.

Need to avoid building a large framework prematurely. Start with enough helpers
to make examples readable, but keep the core request primitive explicit and
package-compatible.

### Effect capability registry

HTTP should be one effect capability, not a special case that every future effect
copies poorly. Candidate concept:

- task source declares an effect kind and a typed request encoder;
- browser runtime has handlers for effect kinds (`http`, `timer`, later storage,
  websocket, geolocation, app interop, etc.);
- host assigns request ids and owns lifecycle uniformly;
- package-level HTTP remains portable because the effectful send primitive is
  supplied by the platform or by pure tests.

Prefer explicit capability descriptors over string prefixes. The current
`task_name` prefix is adequate for the stub, but a production effect registry
should make unsupported capabilities diagnosable and should not rely on ad hoc
string parsing for typed behavior.

Open question: should effect kinds be stable integer handles, interned names, or
a closed host capability table with optional app-defined interop channels?

## Browser Runtime Requirements

- Use `fetch` with `AbortController` for cancellation.
- Execute `fetch` from an explicit request boundary representation produced by
  the platform-side Roc adapter from `roc-lang/http.Request`; JS must not inspect
  Roc layouts or package record fields.
- Materialize response status, headers, and body bytes into WASM-owned buffers
  before calling `roc_ui_resolve` or its future structured-response replacement;
  the platform-side Roc adapter should construct the `roc-lang/http.Response`.
- Preserve duplicate headers where the browser exposes them; document any browser
  restrictions.
- Refresh memory views after any host call that may allocate.
- Ensure late settle after abort does not update disposed state.
- Provide telemetry for request start, resolve, reject, abort, ignored-late.

## Native Host Requirements

- Deterministic fake HTTP driver using the same package-accessor boundary path for
  `roc-lang/http.Request` and `Response`; native fakes should not rely on package
  record fields either.
- Spec commands for resolving/rejecting named or indexed pending requests.
- No real network in ordinary native specs.
- Metrics for request lifecycle.
- Same engine path as browser result ingestion.
- Pure mock examples should demonstrate package-aligned HTTP workflows without the
  browser runtime.

## Security and Policy Questions

- Should the framework restrict fetch URLs by default, or leave CSP/server policy
  to the app shell?
- How are credentials represented safely if not represented by the selected
  `roc-lang/http` release? Defaults should avoid accidentally sending credentials
  cross-origin.
- How are headers normalized? Are forbidden browser headers diagnosed?
- How does the dev server expose mock endpoints without shaping the production
  API around demos?
- Should `Http` include CSRF helpers, or is that app/domain code?
- Which differences between browser `fetch` and non-browser HTTP hosts should be
  explicit in Signals docs?

## Relationship to Subscriptions and Streaming

HTTP remains a one-shot task capability. Long-lived resources such as WebSocket,
EventSource, browser status signals, storage events, and app-specific JS bridges
should be designed through `Sub`s / JS integration, not forced through one-shot
HTTP request machinery. See `JS_INTEGRATION_DESIGN_PREP.md`.

## Validation Plan

1. Add `roc-lang/http` as a platform package and replace the current string-only
   `Http` stub with package-aligned request/response types.
2. Add a native fake HTTP app/spec that covers:
   - successful response with status, headers, and body bytes;
   - non-2xx response as a completed response;
   - network failure;
   - timeout;
   - cancellation on scope disposal;
   - stale result ignored when a newer request wins.
3. Add browser contract tests for:
   - request encoding from package accessors into `fetch`;
   - response status/header/body materialization;
   - duplicate header behavior or documented browser limitation;
   - abort on cancel;
   - memory view refresh around resolve.
4. Add minimal real browser demo using a dev-server endpoint.
5. Add metrics to prove request lifecycle does not leak retained values or pending
   task records.
6. Coordinate with `JS_INTEGRATION_DESIGN_PREP.md` on any shared payload or effect
   capability registry decisions; do not block HTTP on app-specific interop.

## Outstanding Questions

- Which `roc-lang/http` release should the Signals platform pin, and what exact
  package import syntax/URL should `platform/main.roc` use?
- Should Signals expose the package modules directly, re-export them through
  `Http`, or only accept package values at the `Http.send` boundary?
- How should opaque `Request`/`Response` values be encoded/constructed without
  depending on internal record layout?
- Should low-level non-2xx responses always be `Done(Response)`?
- What `HttpError` variants are Signals-specific? (Note: the package does not
  currently define an HTTP error type, so `HttpError` is Signals-owned.)
- The package's transparent records are slated to become opaque; confirm the
  pinned release's accessor/builder surface is complete enough to avoid touching
  record fields directly.
- Is JSON decode in scope for the first production HTTP milestone, or should the
  first milestone stop at response bytes/text helpers?
- Do we need concurrent requests per task source, or should a source have a
  latest-wins policy?
- How does explicit user cancellation appear in Roc? A `Cmd`, a state transition,
  disposal only, or a future subscription/task handle?
- How should timeouts be implemented in native specs vs browser runtime?
- What effect capability registry shape will also compose with future
  subscriptions and JS integration?
- How do we expose request telemetry without making it part of app semantics?

## Dependencies and Sequencing

This work is **mostly parallel** to the UI-boundary docs, because effects re-enter
through the existing signal-source path rather than the DOM attribute/event path:

- The main shared decision is with `WIRE_PROTOCOL_DESIGN_PREP.md`: whether effect
  start/cancel commands ride the render command buffer or a separate host→JS
  effect channel. Settle that before expanding effect commands.
- Add the selected `roc-lang/http` release to `test/signals/platform/main.roc` as
  part of the HTTP milestone; do not wait for the attribute/event boundary.
- The browser runtime already has the cancellation primitive in place
  (`AbortController` and `roc_ui_resolve` in `runtime.mjs`), but structured
  response values require replacing or extending the current string-only task
  payload bridge.
- Independent of the forms and attribute/event work, except that async form
  submit/validation will consume whatever request/cancellation model lands here.
- App-specific JS interop and `Sub` lifecycle research belongs in
  `JS_INTEGRATION_DESIGN_PREP.md`; it is not a prerequisite for package-aligned
  HTTP.

Recommended order: pin and import `roc-lang/http`, implement package-aligned HTTP
send with deterministic native fakes, and coordinate only the shared payload /
effect-registry questions with the JS integration work.

## Suggested First Milestone

Implement a package-aligned `Http.send` vertical slice:

- add a released `roc-lang/http` package to the Signals platform header;
- replace the current string-only request path with `roc-lang/http.Request`;
- return `roc-lang/http.Response` on completed HTTP exchange, including non-2xx;
- define Signals-specific `HttpError` for network failure, timeout, cancellation,
  unsupported request, and response materialization failure;
- support method, URI, headers, body bytes, and timeout from the package API;
- browser runtime maps the request to `fetch` and constructs response
  status/header/body bytes without JS reading Roc layouts;
- native fake HTTP driver can inspect package requests through the same
  accessor/lowered-boundary path and produce package responses deterministically;
- scope-disposal cancellation and ignored-late-result tests;
- one browser contract test with mocked `fetch`.

Leave JSON helpers, binary convenience APIs, multipart, websockets, storage, and
app-specific JS interop for follow-up milestones unless a concrete app requires
them immediately.
