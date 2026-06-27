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
should probably formalize a string/ref table rather than scattering special
cases.

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

## Candidate Direction

### Keep fixed-width base records, add reference tables

A likely near-term shape:

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
to grow.

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

Research should decide this before large protocol expansion.

## Compatibility and Migration

Because this is a research platform, backwards compatibility between temporary
artifacts may not matter yet. But production eventually needs:

- protocol version mismatch detection;
- clear error messages when JS runtime and WASM app disagree;
- stable enough command schema for cached app artifacts;
- tests that fail when op ids are changed without updating runtime.

## Performance Questions

- Does a reference-table protocol reduce or increase total bytes for common apps?
- How many string refs are repeated in real app command streams?
- Are general operations measurably slower than specialized hot ops?
- Does payload descriptor execution in JS matter compared with event handler cost?
- Are command buffers still drained in one pass without dynamic allocation in JS?
- How does memory growth during large command batches affect view refresh cost?

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
  `SetAttr`/`RemoveAttr`/`BindEvent` ops and the payload-spec table defined here.
- `CONTROLLED_INPUTS_FORMS_DESIGN_PREP.md` needs reconciliation ops
  (`SetInputValue`, future `SetSelection`) and submit/focus events carried here.
- `HTTP_EFFECTS_DESIGN_PREP.md` shares the open question of whether effect
  start/cancel commands ride this buffer or a separate effect channel.

Recommended order: land the protocol's reference-table + versioning shape first as
a thin slice, then build the attribute/event boundary on it, then forms, with
HTTP/effects proceeding in parallel once the effect-command question is settled.

## Suggested First Milestone

Implement a protocol vertical slice for generalized attributes/events:

- add protocol version check;
- add string/name refs for attribute and event names;
- add `SetAttr` / `RemoveAttr` general ops;
- add `BindEvent` with event name, options, and primitive payload spec ref;
- add command-buffer byte/count metrics;
- add JS contract tests and one app/spec using a real keyboard or submit event.

Keep existing specialized op-codes for hot fields during the experiment, then use
measurements to decide whether any should be collapsed into the generalized path.
