# Wire Protocol Generalization Design Prep

Temporary working note for evolving the Signals JS↔WASM/browser wire protocol.
This document captures requirements and open questions before the final design is
folded into `DESIGN.md`, `GUIDE.md`, and `NEXT_STEPS.md`.

## Problem Statement

The current browser boundary uses a small fixed-width command buffer with integer
op-codes and operands. This is a good foundation: it avoids per-patch FFI calls,
keeps JS as a thin executor, and avoids decoding Roc layouts in JavaScript.

However, production UI needs a much wider open-ended surface for attributes,
events, payloads, effects, form reconciliation, and possibly multiple mounts. If
every new capability requires a new fixed enum value and custom handling across
Roc descriptors, Zig command generation, and `runtime.mjs`, the protocol becomes
the bottleneck for framework growth.

The goal is to generalize the protocol while preserving the properties that made
the current design attractive.

## Current Context

Relevant current files:

- `test/signals/src/render_commands.zig`
  - shared render op ids, command counts, fixed-width command record shape.
- `test/signals/src/wasm_host.zig`
  - serializes render commands into the browser command buffer.
- `test/signals/browser/runtime.mjs`
  - drains command records, applies DOM operations, sends events back.
- `test/signals/browser/runtime_contract.test.mjs`
  - JS-side command-buffer and event-marshalling contract tests.
- `test/signals/browser/wasm_memory_views.mjs`
  - memory-view refresh policy after allocating host calls.
- `test/signals/DESIGN.md`
  - states the intended boundary: patch-op stream host→JS plus event call JS→host,
    no serialized tree and no JS-side diff.

Current command records are optimized for a small closed set of DOM operations.
Strings cross as `(ptr, len)` UTF-8 slices. JS refreshes typed-array views after
host calls that may allocate.

## Research Update: Dynamic-Length Protocol Evaluation

The dynamic-length hypothesis is worth pursuing, but not as a wholesale
replacement of the current command buffer yet. The evidence points toward a
**hybrid protocol**:

- keep fixed-width records for the current hot render operations;
- add dynamic-length records for open-ended attrs, events, payload specs, form
  reconciliation, and structured effect requests;
- treat reference tables as a later measured optimization, not the first
  generalization mechanism.

The current wire record is six 32-bit words: `op` plus five operands. It is a
good fit for `AppendChild`, `RemoveNode`, `MoveBefore`, `SetChecked`, and other
small closed commands. It becomes awkward when a command naturally carries a
name plus a value plus options plus a payload descriptor.

Implementation inspection also showed that the closed surface is deeper than the
browser record. `engine.zig` currently indexes descriptor streams by fixed
`RenderTextField`, `RenderBoolField`, and `RenderEventKind` values. A wire-only
change will not unlock arbitrary attrs/events; the descriptor schema and the
wire protocol need to evolve together.

### Proposed dynamic record shape for a first spike

Use an explicit-length byte record:

```text
record:
  op          : u16
  flags       : u16
  payload_len : u32
  payload     : payload_len bytes
  padding     : zero to next 4-byte boundary
```

All integer fields are little-endian. Records start 4-byte aligned. JS advances
with:

```text
next = offset + 8 + align4(payload_len)
```

Use opcode-specific typed operands in the first spike, not a tag before every
operand. Use fixed `u32` integers and `u32` string lengths initially. Do not add
varints until command-buffer bytes are a measured problem.

Example dynamic operations:

```text
SetAttrText:
  elem_id   : u32
  name_len  : u32
  name_utf8 : bytes
  value_len : u32
  value_utf8: bytes

RemoveAttr:
  elem_id   : u32
  name_len  : u32
  name_utf8 : bytes

BindEvent:
  elem_id          : u32
  event_id         : u32
  event_name_len   : u32
  event_name_utf8  : bytes
  listener_options : u32
  payload_spec_len : u32
  payload_spec     : bytes
```

For the first attr/event slice, send strings inline. Interning can be added once
metrics show repeated names/specs are a real cost. Large payloads, especially
future HTTP bodies or binary data, should use explicit buffer slices or a
separate payload buffer instead of one huge inline dynamic record.

Malformed dynamic records must fail clearly. Required checks:

- fewer than 8 bytes remain for a header;
- `payload_len` extends beyond the dynamic buffer;
- unsupported nonzero `flags`;
- unknown opcode;
- operand decoder reads beyond `payload_len`;
- invalid UTF-8 once the dynamic decoder uses fatal string decoding.

The runtime must not skip unknown opcodes just because a record length is
available. Unknown commands mean the wasm artifact and JS runtime disagree.

### Byte estimate

The rough estimate below comes from
`test/signals/research/wire_protocol_dynamic_size_estimate.mjs`.

Assumptions:

- current fixed records are 24 bytes plus side string bytes;
- fixed + refs keeps 24-byte records and adds 8 bytes of table metadata per
  unique string/spec in the scenario;
- dynamic records use the proposed 8-byte header, 4-byte alignment, `u32` ids,
  and `u32` string lengths;
- hybrid keeps current fixed records for hot ops and uses dynamic records only
  for future open-ended ops.

| Scenario | Current fixed | Fixed + refs | Dynamic | Hybrid |
| --- | ---: | ---: | ---: | ---: |
| initial simple mount | 285 | 325 | 204 | 285 |
| text update | 33 | 41 | 28 | 33 |
| class update | 50 | 58 | 44 | 50 |
| keyed row insert | 106 | 122 | 76 | 106 |
| keyed row move | 24 | 24 | 20 | 24 |
| future SetAttr aria-label | 40 | 56 | 36 | 36 |
| future BindEvent key payload | 55 | 71 | 56 | 56 |
| current StartTask text | 61 | 77 | 60 | 61 |
| future HTTP request | 76 | 116 | 84 | 84 |

The table does not prove dynamic records are faster or globally better. It does
show:

- dynamic records are not obviously byte-expensive;
- fixed + refs can be worse for one-off open-ended data;
- rich event payloads are dominated by descriptor bytes, so the dynamic benefit
  is simpler ownership/diagnostics rather than size;
- structured HTTP likely needs explicit payload slices or a separate effect
  buffer once bodies grow.

### Browser/runtime cost assessment

Dynamic parsing in JS is more code than a `Uint32Array` stride, but it should be
acceptable for the open-ended command families:

- fixed hot commands remain allocation-light and fast;
- dynamic commands can be decoded with `DataView` plus `Uint8Array.subarray`;
- one host call can still drain in one pass;
- view refresh after `memory.grow` is unchanged;
- strings remain borrowed for the drain;
- event payload specs are sent at bind time and retained by the listener, not
  resent on every event.

For form-heavy apps, keep `SetValue`, `SetChecked`, and common events fixed
until reconciliation policy data forces dynamic operands. A form app that emits
many open-ended attr updates per keystroke should be measured with command-byte
and drain-time metrics before adding interning.

## Non-Negotiable Properties

Any generalized protocol must keep these properties:

- JS remains a thin executor, not a reactive runtime.
- The host owns logical identity; JS owns DOM identity; ids keep them in lockstep.
- DOM nodes never cross the boundary.
- Roc layouts never cross into JS; JS reads only protocol records and byte slices.
- The host emits explicit operations; JS does not infer missing meaning by
  inspecting the DOM.
- One host call drains one batch of command records where possible.
- Browser contract tests cover codec behavior; engine semantics stay in native
  specs.
- Any allocation during a host call can grow memory, so JS view-refresh discipline
  remains mandatory.

## Requirements

### General render operations

The protocol should support:

- create element by tag;
- create text node;
- insert/move/remove nodes;
- set/remove text content;
- set/remove general text attributes;
- set/remove general boolean attributes;
- set DOM properties where attributes are insufficient (`value`, `checked`,
  selected state, maybe style properties);
- set/remove event listeners with options and payload descriptors;
- form/input-specific reconciliation operations;
- cancel/start effect operations if effect commands share the same buffer.

### Open-ended names without repeated string cost

General attrs/events require names. Options:

1. send `(ptr, len)` name every time;
2. intern names into a per-app/per-mount string table and send `name_ref`;
3. use static enum refs for common names plus dynamic refs for custom names.

Current design already uses `tag_ref` / `accessor_ref` concepts. Generalization
should keep the door open for string/ref tables, but the dynamic-length research
suggests not making them mandatory in the first slice. Inline dynamic names are
simpler for one-off attrs/events; interning should be added only after command
byte metrics show repetition worth optimizing.

### Versioning

The protocol should include a way to detect runtime/wasm mismatch:

- protocol version constant exported by wasm and checked by JS;
- or command-buffer header with version;
- or generated runtime table.

Today the runtime and wasm artifacts are built together during development, but
production deployment needs a clear failure mode if they drift.

### Multiple mounts

If multiple instances per page move from open question to requirement, command
records or host exports may need a mount handle. The protocol should not assume
module-global singleton forever unless the design deliberately chooses one WASM
instance per mount.

### Observability

Telemetry should record enough to debug boundary failures without being app
semantics:

- command batch count and op counts;
- malformed command diagnostics;
- listener bind/unbind;
- event payload encode failures;
- memory growth/view refresh;
- effect start/resolve/cancel if included.

## Candidate Directions

### Option A: fixed-width base records plus reference tables

This remains a viable optimization path:

- command record stays fixed-width for fast draining;
- operands may refer to side tables for strings, payload specs, event options, and
  attribute names;
- hot operations keep compact specialized op-codes;
- general operations use references.

Example conceptual operations:

```text
CreateElement { dom_id, tag_ref }
CreateText { dom_id, text_ref }
InsertBefore { parent_id, child_id, before_id_or_zero }
RemoveNode { dom_id }
SetText { dom_id, text_ref }
SetAttr { dom_id, name_ref, value_ref }
RemoveAttr { dom_id, name_ref }
SetBoolAttr { dom_id, name_ref, bool }
SetPropertyText { dom_id, property_ref, value_ref }
SetPropertyBool { dom_id, property_ref, bool }
BindEvent { dom_id, event_id, event_name_ref, options_ref, payload_spec_ref }
ClearEvent { dom_id, event_id }
SetInputValue { dom_id, value_ref, reconcile_policy_ref }
```

This avoids widening every command for rare cases while allowing the app surface
to grow. It is strongest when names/specs repeat enough to amortize the table
definitions. It is weakest for one-off open-ended commands, where the table
becomes extra protocol state just to send data once.

Do not choose this as the first generalization layer unless a real batch shows
repeated dynamic names/specs dominate command bytes. Reference tables are
straightforward to add later behind the same dynamic/fixed command vocabulary.

### Option B: dynamic-length command stream

A variable-length stream carries each command's operands directly:

```text
header: op:u16 flags:u16 payload_len:u32
payload: opcode-specific typed operands
```

This is a good fit for:

- arbitrary `SetAttr` / `RemoveAttr`;
- `BindEvent` with event name, listener options, and payload descriptor bytes;
- form commands with reconciliation policies or selection data;
- structured effect start/cancel records.

Use explicit length fields rather than operand tags determining the full command
length. A length-bearing header gives JS a uniform truncation check and a clear
byte offset for diagnostics. Operand tags can be used inside payload descriptors,
but normal render commands should stay opcode-specific so JS does not become a
generic framework interpreter.

Dynamic records are not a reason for JS to recover meaning. The engine still
emits explicit commands; JS only executes the opcode's documented DOM/effect
operation.

### Option C: hybrid fixed hot ops plus dynamic open-ended ops

This is the recommended next direction.

Keep the current fixed-width records for hot closed operations. Add a dynamic
path only for the command families that are structurally open-ended:

- `SetAttrText`, `RemoveAttr`, `SetBoolAttr`, `RemoveBoolAttr`;
- `BindEvent` with event name, static listener options, and payload descriptor;
- future `SetInputValue`, `SetSelection`, `SetValidity`;
- structured effect records if effects share the same framing.

Two staging shapes are possible:

1. Add one fixed-width `extended` op whose operands point at a dynamic payload
   slice. This minimizes churn and preserves command ordering in the current
   buffer, but each dynamic command pays both a fixed record and dynamic bytes.
2. Add a separate dynamic byte stream and drain fixed and dynamic commands in
   order through a shared sequencing mechanism. This is cleaner long term but
   requires more protocol machinery in the first slice.

The first detailed design spike should choose between those staging shapes. It
should not simultaneously add string interning or generated schemas, because
that would make the spike harder to evaluate.

### Separate descriptor protocol from command protocol

There are two related but distinct protocols:

1. **Descriptor ingestion** inside WASM: Roc `Elem` descriptors become engine
   tables.
2. **Render command buffer** from WASM host to JS.

Generalizing attrs/events touches both. Keep them conceptually separate:

- Roc descriptors carry typed/capability-backed signal/event definitions.
- Render commands carry DOM executor instructions only.
- Event payload descriptors may need representation in both places: retained by
  host for native specs and serialized/ref'd to JS for browser extraction.

### Generated shared constants

Current fixed ids are manually mirrored across layers. As the protocol grows,
manual mirroring becomes riskier. Research whether shared constants can be
generated from one source for:

- op ids;
- fixed field ids;
- payload primitive ids;
- listener option bit flags;
- protocol version.

This does not require compiler changes; it can be a build-time artifact for the
Signals platform.

## Event Payload Encoding

The generalized event boundary likely needs a small payload byte format. Options:

1. Existing primitive payloads only, with generalized accessor refs.
2. Tagged primitive records encoded into a simple host-owned format.
3. JSON string payloads for generalized events.
4. App-specific generated encoders/decoders.

JSON is tempting for ease, but it may erase types, add allocations, and hide
errors. A small typed primitive-record format is more consistent with the current
integer protocol. Need a spike before choosing.

Minimum primitives to consider:

- unit;
- bool;
- string;
- signed/unsigned integer;
- float;
- maybe list of strings for file names or selected options.

Payload descriptors should be sent once at bind time and retained by the JS
listener. Per event, JS executes that explicit descriptor against the browser
event object and serializes only the requested primitive leaves back to WASM.
For example, a keyboard payload `{ key, shift_key }` can be represented as a
record descriptor with `event.key` as text and `event.shiftKey` as bool.

Submit should start with static listener options such as `prevent_default` and
`stop_propagation` on the binding. Dynamic prevent/stop decisions returned from
reducers are more expressive, but they couple DOM event policy to reducer
execution timing and should be a separate design decision.

## Native vs Browser Parity

`DESIGN.md` asks whether the native spec runner should consume the same command
buffer as the browser. This becomes more important as the protocol generalizes.

Options:

1. Keep native sink as direct enum calls and browser as buffer serialization.
   - simpler now;
   - risk: two render surfaces drift.
2. Make native consume the same command-buffer records through a native executor.
   - stronger parity;
   - may reduce observability or make tests less direct unless carefully wrapped.
3. Keep direct native sink but generate both from one command schema.
   - middle ground.

Do not force native to consume the dynamic byte stream in the first slice. The
native host is the observability host, and direct sink calls are still better for
work-budget assertions. The near-term parity goal should be either generated
schemas for both runtimes or a native dynamic decoder used as a contract test
harness, not replacing the native sink immediately.

## Compatibility and Migration

Because this is a research platform, backwards compatibility between temporary
artifacts may not matter yet. But production eventually needs:

- protocol version mismatch detection;
- clear error messages when JS runtime and WASM app disagree;
- stable enough command schema for cached app artifacts;
- tests that fail when op ids are changed without updating runtime.

Add version checks before the generalized protocol lands:

```text
roc_ui_protocol_version() -> u32
roc_ui_protocol_features() -> u32
```

JS should check these before mount and fail clearly if the wasm artifact and
runtime disagree. Dynamic record decoders should include byte offsets and op
names in errors.

## Performance Questions

- Does a reference-table protocol reduce or increase total bytes for common apps?
- How many string refs are repeated in real app command streams?
- Are general operations measurably slower than specialized hot ops?
- Does payload descriptor execution in JS matter compared with event handler cost?
- Are command buffers still drained in one pass without dynamic allocation in JS?
- How does memory growth during large command batches affect view refresh cost?
- What is the command-buffer byte split between fixed records, fixed string
  buffer bytes, dynamic record bytes, and event payload bytes?
- Does a fixed outer `extended` op add too much overhead compared with a true
  dynamic stream?

## Validation Plan

1. Add protocol schema documentation or generated table.
2. Add JS contract tests for:
   - generalized set/remove attr;
   - generalized event binding with payload descriptor;
   - protocol version mismatch;
   - command buffer read after memory growth;
   - malformed command diagnostics.
3. Add one native spec app that uses general attrs/events but asserts engine
   semantics natively.
4. Add benchmark counters for command count and command-buffer bytes emitted per
   event.

## Outstanding Questions

- Should command records remain fixed-width forever, or is a variable-length
  stream acceptable once op count grows?
- How should string/name interning be scoped: global runtime, per WASM instance,
  per mount, or per batch?
- Should general attribute names be sent as strings or interned at descriptor
  ingestion time?
- Can render command and event payload descriptors share a table format?
- Do effect start/cancel operations belong in the render command buffer, or in a
  separate host→JS effect buffer?
- How should runtime/wasm protocol version mismatch be reported in production?
- Should native tests consume the same buffer for parity, or is generated schema
  enough?
- What is the smallest schema-generation step that removes manual mirrored ids
  without adding build complexity?

## Dependencies and Sequencing

This is the **foundation layer** for the other three prep docs. It should be
researched first, because the others encode their new capabilities through it:

- `ATTRIBUTE_EVENT_PAYLOAD_BOUNDARY_DESIGN_PREP.md` needs the general
  `SetAttr`/`RemoveAttr`/`BindEvent` ops and the payload descriptor
  representation defined here.
- `CONTROLLED_INPUTS_FORMS_DESIGN_PREP.md` needs reconciliation ops
  (`SetInputValue`, future `SetSelection`) and submit/focus events carried here.
- `HTTP_EFFECTS_DESIGN_PREP.md` shares the open question of whether effect
  start/cancel commands ride this buffer or a separate effect channel.

Recommended order: land protocol versioning plus a hybrid dynamic attr/event
slice first, then build the attribute/event boundary on it, then forms. HTTP and
effects can proceed in parallel once the effect-command question is settled, and
should reuse the same dynamic framing if they need structured host-to-JS data.

## Suggested First Milestone

Status: the minimal custom text-attribute portion of this milestone is now
implemented end to end. The landed slice includes protocol version/features,
`Extended` dynamic records, `SetAttrText` / `RemoveAttr`, the Roc
`Html.attr` / `Html.attr_s` API, native `expect_attr` coverage, and browser
runtime decoding. Generalized events, bool attrs, form properties, and richer
byte metrics remain future slices.

Implement a protocol vertical slice for generalized attributes/events using the
hybrid direction:

- add protocol version check;
- add a dynamic payload path, either a fixed outer `extended` op or a true
  dynamic byte stream;
- add `SetAttrText` / `RemoveAttr` general ops with inline names/values;
- add `BindEvent` with event name, static listener options, and an inline
  primitive payload descriptor;
- add command-buffer byte/count metrics for fixed records, fixed string bytes,
  dynamic bytes, and event payload bytes;
- add JS contract tests for dynamic attr/event decoding, malformed dynamic
  records, protocol version mismatch, and memory growth before command drain;
- add one app/spec using `aria-*` or `data-*` attrs plus a real keyboard or
  submit event.

Keep existing specialized op-codes for hot fields during the experiment, then use
measurements to decide whether any should be collapsed into the generalized path.

## Readiness for Detailed Design Spike

There is enough evidence to start a detailed design spike for the hybrid
attr/event protocol slice.

The spike is ready because the core direction is now constrained:

- JS must remain a thin executor of explicit host commands.
- The engine keeps event routing and payload kind validation.
- Fixed hot ops stay unchanged.
- Dynamic records are reserved for open-ended data.
- Inline dynamic strings are the first version; interning is deferred until
  measured.
- Malformed record handling and version mismatch behavior are required, not
  follow-up polish.

The spike is **not** ready to decide these broader questions:

- replacing every fixed render command with dynamic records;
- adding varints;
- adding per-mount string/spec interning;
- making native consume the same byte stream as its primary sink;
- merging DOM render commands and effect commands into one buffer;
- dynamic prevent-default/stop-propagation returned from reducers.

Detailed design spike deliverables should be:

- exact dynamic record schema and op ids for `SetAttrText`, `RemoveAttr`, and
  `BindEvent`;
- exact payload descriptor v1 for unit, text, bool, and small records;
- staging choice: fixed outer `extended` op vs separate dynamic stream;
- version/feature check API;
- malformed-record error contract;
- files and tests to change;
- metrics to capture before deciding whether to add refs/interning.
