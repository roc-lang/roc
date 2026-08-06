# Roc REPL WebAssembly module

`repl.wasm` is a self-contained, pure REPL. It does not expose the playground's
compiler-inspection state machine or the echo platform. One WebAssembly instance
owns one in-memory session.

## Host ABI

- `roc_repl_alloc(len) -> ptr`
- `roc_repl_free(ptr, len)`
- `roc_repl_process(ptr, len) -> response_ptr`
- `roc_repl_free_response(response_ptr)`

`roc_repl_process` accepts UTF-8 JSON. Its result begins with a four-byte
little-endian payload length followed by the UTF-8 JSON response. The module
exports its linear memory as `memory`.

Every request is an envelope with `protocol`, `id`, `op`, and optional `params`:

```json
{"protocol":1,"id":1,"op":"eval","params":{"source":"x = 41\nx + 1"}}
```

Every response echoes `protocol` and `id` and contains either `ok: true` with a
`result`, or `ok: false` with a stable structured `error`. Unknown request
fields are rejected so misspelled parameters cannot silently change behavior.
The installed `protocol.d.ts` describes the complete version 1 request and
response shapes for JavaScript and TypeScript embedders.

The protocol contains no frontend presentation strings. In particular,
definition results do not return text such as `assigned foo`; they return
`kind`, `definition_kind`, `name`, `type`, `committed`, and `revision` fields.
For example:

```json
{
  "source": "foo = \"bar\"",
  "kind": "definition",
  "definition_kind": "value",
  "name": "foo",
  "status": "ok",
  "committed": true,
  "revision": 1,
  "type": "Str",
  "diagnostics": [],
  "events": []
}
```

## Operations and scope

| Operation | Contract |
| --- | --- |
| `capabilities` | Reports protocol operations, ownership boundaries, and unavailable effects. |
| `eval` | Parser-backed multiline/batch input; evaluates left to right and stops at the first failed snippet. Earlier successful definitions remain committed. |
| `analyze` | Reports whether input is complete, incomplete, or invalid without changing the session. |
| `complete` | Filters session definitions by the identifier prefix ending at `params.cursor` and returns insertion text plus an explicit replacement range. |
| `inspect` | Returns an expression's checked type without evaluating it. |
| `get_state` | Returns ordered structured definitions, exact replay source, virtual module sources, pending-annotation state, and revision. |
| `clear` | Clears definitions while retaining virtual modules. |
| `set_modules` | Atomically replaces named in-memory modules and clears definitions. |

Evaluation is left-to-right. Each snippet reports whether it changed session
state through `committed`; expressions always report `committed: false`.
The batch result reports `completed`, `stop_reason`, `committed_count`, and the
final session `revision`. Earlier successful definitions remain available when
a later snippet fails.

Revisions are unsigned 32-bit counters scoped to one WASM instance, so they are
exact JavaScript numbers. Hosts must not compare revisions across instances.

Expression values, checked types, definition metadata, diagnostics, and ordered
runtime events are separate fields. Diagnostics have a stable `code`,
`severity`, human-readable `message`, and nullable `region`. Runtime crashes are
language-level results and do not poison the next request.

Version 1 enumerates blocking diagnostics only; non-blocking compiler warnings
are not part of the current contract. This is reported by the capabilities
response rather than being left implicit.

## Text offsets and completion

Source strings are UTF-8. All protocol offsets are zero-based UTF-8 byte
offsets; `capabilities.offset_unit` and every completion response state this
explicitly. A cursor beyond the source or inside a multi-byte code point is an
`invalid_cursor` request error.

Completion is deliberately scoped to names stored in the current REPL session.
It does not claim contextual field, tag, keyword, or builtin completion. The
scope is reported by `capabilities.completion_scope`. A completion response
contains `prefix`, `cursor`, `replacement`, `items`, and whether checked type
details are currently available. A standalone annotation is a valid
intermediate state; names remain completable while `details_available` is
false.

## Frontend boundary

`ReplSession.stepLanguageWithConfig` accepts Roc syntax and returns a typed
language result. Terminal commands are parsed by the CLI frontend and passed as
typed commands to `executeCommandWithConfig`; they are not recognized by the
WASM protocol. The browser demo renders only structured fields and never parses
compiler display text.

The module has no filesystem, network, package resolver, stdin, history store,
or platform effects. Hosts own persistence and cancellation. The demo uses a
Web Worker; Stop terminates the worker, creates a new module instance, and
replays the exact structured `definition_source` returned by `get_state`.

See `API_REVIEW.md` for the adversarial contract review and remaining explicit
limitations.
