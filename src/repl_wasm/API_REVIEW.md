# Adversarial API review

This review treats the browser demo as a conformance client: if it must parse a
human display string, infer whether state changed, guess an offset unit, or
reconstruct session state, the protocol is underspecified.

## Findings resolved before protocol commitment

| Risk | Prior behavior | Resolution |
| --- | --- | --- |
| Presentation leakage | Definition success was only `display: "assigned foo"`. | Removed presentation strings. Results carry definition name, kind, checked type, commit state, and revision. |
| Command coupling | The WASM adapter recognized CLI command spellings. | Language stepping and typed CLI command execution are separate APIs. The WASM path has no command router. |
| Ambiguous commit state | Expressions reported `committed: true`. | `committed` now means the snippet changed stored session definitions; expressions are false. |
| Silent missing types | A successful expression could return a null type if a second inspection failed. | A successful expression requires a checked type; inconsistency is an operation error. |
| Crash message indirection | A caller had to search the ordered event trace to find the terminal crash message. | Crashed snippets expose a dedicated `crash.message`; the event remains in the trace only to preserve effect ordering. |
| Implicit partial batches | Hosts had to inspect the last snippet to learn why evaluation stopped. | Batch results expose `completed`, `stop_reason`, `committed_count`, and final revision. |
| Failed-step mutation | A failed value could remove an earlier pending annotation. | Definition validation restores the exact prior state; diagnostic snippets never mutate definitions. |
| Nonfunctional cursor | `complete` echoed but ignored source and cursor. | Completion validates a UTF-8 byte cursor, filters by the exact identifier prefix, and returns an explicit replacement range. |
| Intermediate annotation failure | Completion could fail while a standalone annotation awaited its value. | The state is explicit; names remain available and `details_available` becomes false until checking is possible. |
| Opaque state | `get_state` exposed only concatenated source and module names. | It now returns ordered structured definitions, exact replay source, virtual module sources, pending-annotation state, and revision. |
| Weak diagnostics | Diagnostics were undifferentiated rendered strings. | Each diagnostic has a stable code and severity. Regions are nullable rather than invented. |
| Typo tolerance | Unknown JSON fields were ignored. | Version 1 requests are strict and reject unknown fields as `invalid_json`. |
| Internal error leakage | Unhandled Zig error names were returned to callers. | Unexpected failures use a stable `internal_error` envelope without implementation names. |
| Replay ambiguity | The demo replayed whole successful submissions, including expressions, and lost commits preceding a later failure. | It snapshots `definition_source` from `get_state` and replays definitions only. |
| Offset mismatch | JavaScript selection offsets are UTF-16 while the module consumes UTF-8. | The protocol declares UTF-8 byte offsets and the demo converts at the boundary. |
| JavaScript integer precision | An unbounded 64-bit numeric revision would eventually exceed JavaScript's exact integer range. | Revisions are unsigned 32-bit counters scoped to one disposable WASM instance. |

## Contract boundaries retained intentionally

| Area | Version 1 contract |
| --- | --- |
| Completion | Session-definition prefix completion only. No claim of contextual fields, tags, keywords, builtins, ranking, or documentation. |
| Diagnostic regions | Nullable. Compiler-rendered diagnostics have stable category codes, but precise source ranges are not fabricated when the reporting pipeline does not expose one. |
| Non-blocking warnings | Version 1 currently returns blocking diagnostics and runtime events. It does not claim warning enumeration; `capabilities.features.diagnostic_scope` reports `blocking_only`. |
| Virtual modules | `set_modules` atomically replaces stored source and rejects duplicate names. Modules are checked when imported, not eagerly during configuration. |
| Cancellation | Host-owned. Terminating the worker/instance is the hard cancellation boundary. |
| History and persistence | Host-owned. `get_state.definition_source` is the exact definition replay representation for one module set. |
| Resource limits | Governed by the WebAssembly instance memory limit and host request policy; the protocol does not promise a universal source-size ceiling. |
| Concurrency | One session per instance. Hosts that need concurrent independent sessions instantiate multiple modules. |

## Conformance expectations

- A frontend can render definitions without inspecting `source` or parsing a
  message.
- Every successful expression has both `value` and `type`.
- Every diagnostic has `code`, `severity`, `message`, and a nullable `region`.
- Every state-changing result identifies the resulting revision.
- Completion replacement ranges are valid UTF-8 boundaries in the supplied
  source.
- A crash or failed request does not corrupt the next request.
- Duplicate virtual-module replacement leaves the previous module set and
  definitions intact.
