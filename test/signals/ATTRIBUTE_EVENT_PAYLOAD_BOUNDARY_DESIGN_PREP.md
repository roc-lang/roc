# Attribute / Event / Payload Boundary Design Prep

Temporary working note for expanding the Signals browser-facing UI boundary. This
is not an enduring design document yet. Its purpose is to capture requirements,
current constraints, candidate directions, and unresolved questions before the
final design is folded into `DESIGN.md`, `GUIDE.md`, and `NEXT_STEPS.md`.

## Problem Statement

The current Signals UI boundary is intentionally small: apps can set a few
well-known fields and bind a few event kinds. That was useful for proving the
core engine, but it is too narrow for real production web apps.

Today the platform surface is effectively closed:

- text-like fields: `text`, `role`, `label`, `test_id`, `value`, `class`;
- bool fields: `checked`, `disabled`;
- event kinds: `click`, `input`, `check`, `pointer_down`, `pointer_up`,
  `pointer_enter`, `pointer_leave`;
- payload shapes: unit, string, bool;
- payload accessors: none, `target.value`, `target.checked`.

This means many normal browser capabilities are inexpressible without changing
`Node.roc`, the Zig engine, the render-command layer, and `runtime.mjs` together.
Examples include `href`, `src`, `id`, `name`, `type`, `placeholder`, `style`,
`aria-*`, `data-*`, keyboard events, focus/blur, form submit, file input,
clipboard, drag/drop, scroll, composition, and event metadata such as keys or
pointer coordinates.

The goal is to generalize the boundary without losing the properties that make
the current engine tractable:

- Roc apps describe explicit data; hosts do not infer meaning from the DOM.
- The JS runtime remains a thin executor, not a reactive runtime.
- Hot paths can still use specialized representations where measured.
- Typed Roc values remain protected by confined erasure; JS never decodes Roc
  layouts.

## Current Context

Relevant current files:

- `test/signals/platform/Node.roc`
  - `Attr` is a closed enum with `StaticText`, `SignalText`, `StaticBool`,
    `SignalBool`, and `OnEvent`.
  - Fields, event kinds, payload kinds, and payload accessors are hard-coded `U64`
    constants.
- `test/signals/platform/Html.roc`
  - exposes convenience constructors for a small set of tags/attrs/events.
- `test/signals/src/engine.zig`
  - collects descriptor streams for text attrs, bool attrs, and event descriptors.
  - routes source updates to signal graph records.
- `test/signals/src/render_commands.zig`
  - owns fixed render op ids and command-buffer record shape.
- `test/signals/browser/runtime.mjs`
  - applies integer patch ops to DOM nodes and marshals event payloads back to
    `roc_ui_event`.
- `test/signals/browser/runtime_contract.test.mjs`
  - protects the JS↔WASM contract surface.

The design already names this as an open question under event payload accessor
format: carry explicit accessor descriptors in the descriptor tree so JS
serializes only requested leaves.

## Requirements

### Expressiveness

The app-facing API must be able to describe common web UI without host changes
for every field:

- arbitrary static text attributes: `id`, `href`, `src`, `alt`, `title`,
  `placeholder`, `name`, `type`, `autocomplete`, etc.;
- arbitrary signal-backed text attributes;
- arbitrary static/signal-backed boolean attributes where boolean DOM semantics
  apply;
- `aria-*` and `data-*` attributes;
- event handlers for common browser events beyond the current fixed set;
- event payloads that can include selected event/target/currentTarget fields;
- prevent-default / stop-propagation policy, either per binding or per event
  result;
- namespace-sensitive attributes where required, especially SVG.

### Safety and Model Discipline

- Host and JS must not recover meaning from DOM state that was not explicitly
  declared by Roc.
- Event payload extraction must be declared by the descriptor, not ad hoc JS logic
  per app.
- JS must serialize only the requested payload leaves.
- The event payload format crossing into WASM must remain layout-independent from
  Roc values.
- The host must keep ownership and lifetime of retained Roc callbacks and values.
- Invalid or unsupported boundary declarations should become diagnostics or host
  errors with clear messages; they should not silently degrade into wrong DOM
  behavior.

### Performance

- Keep specialized fast paths for hot fields if metrics justify them:
  `textContent`, `class`, `value`, `checked`, `disabled`, and common events.
- General attributes/events should be efficient enough for ordinary app code, but
  do not need to be over-optimized before real measurements exist.
- The descriptor stream should not grow many sparse per-field tables for every
  new web attribute.
- Event marshalling should avoid serializing full event objects.

### Testability

- Native specs should be able to assert behavior through semantic locators where
  possible.
- JS contract tests should cover payload extraction, listener options, attribute
  application/removal, and memory-view refresh after event payload allocation.
- The app suite should add the smallest examples that prove new classes of
  boundary behavior, not catalog every HTML attribute.

## Candidate Direction

### 1. Split hot fields from general fields

Keep existing specialized field ids for hot fields and introduce general
attribute descriptors for the open-ended browser surface.

Possible Roc-level shape:

```roc
Attr := [
    StaticText({ field : U64, value : Str }),
    SignalText({ field : U64, signal : Box(SignalExpr), read : TextReadHandle }),
    StaticBool({ field : U64, value : Bool }),
    SignalBool({ field : U64, signal : Box(SignalExpr), read : BoolReadHandle }),
    StaticAttr({ name : Str, value : Str }),
    SignalAttr({ name : Str, signal : Box(SignalExpr), read : TextReadHandle }),
    StaticBoolAttr({ name : Str, value : Bool }),
    SignalBoolAttr({ name : Str, signal : Box(SignalExpr), read : BoolReadHandle }),
    OnEvent({ kind : EventKind, msg : Msg, options : EventOptions }),
]
```

Open question: whether `EventKind` should be a string, an interned string ref, or
a tagged union with specialized common variants plus `Custom(Str)`.

### 2. Make payload access explicit data

Instead of `payload_kind` plus one hard-coded accessor id, an event binding should
carry a payload descriptor. The descriptor says what JS should read from the
browser event and how to encode it.

Possible minimal stages:

1. keep existing unit/string/bool payloads, but generalize the accessor name;
2. add records composed of primitive leaves;
3. add lists only if a real event requires them;
4. add app-defined payload decoders only if static descriptors are insufficient.

Example descriptor concepts:

```text
PayloadSpec =
  unit
  text(EventPath)
  bool(EventPath)
  float(EventPath)
  int(EventPath)
  record(List({ field_name, spec }))

EventPath =
  event.key
  event.code
  event.clientX
  target.value
  target.checked
  target.files.length
  currentTarget.dataset.foo
```

JS executes this descriptor against the event object and writes a boundary format
into WASM memory. The host passes the payload bytes to the retained Roc reducer
through the correct typed capability.

### 3. Listener options and event policy

Real apps need at least:

- `preventDefault`;
- `stopPropagation`;
- capture vs bubble;
- passive listeners for scroll/touch where appropriate;
- maybe once.

Need to decide whether prevent/stop are static options on the binding or dynamic
results from the reducer. Static options are easier and deterministic; dynamic
results are more expressive but couple reducer execution to browser event policy.

### 4. Attribute removal semantics

Signal-backed optional attributes require explicit absence semantics. Do not use
empty string as a sentinel.

Options:

- add `SignalAttrMaybe : Signal([None, Some(Str)])`;
- model boolean attrs separately and remove when false;
- provide convenience APIs in `Html` while keeping a small core descriptor.

## Requirements for Research Spikes

1. Build a small app with:
   - `href`, `aria-label`, `data-*`, `id`, `placeholder`;
   - static and signal-backed variants;
   - keyboard event with `{ key, shift_key }` payload;
   - submit event with prevent-default.
2. Prove native spec runner can assert the important behavior without becoming a
   browser duplicate.
3. Prove JS contract tests can validate event payload extraction through the
   command/event boundary.
4. Measure command-buffer growth and event payload allocation for the generalized
   path vs current specialized path.

## Outstanding Questions

- Should general attributes be strings only initially, or should typed numeric / token / URL helpers exist at the API layer?
- How should invalid attribute names be handled? Reject at build/ingest time, sanitize, or pass through?
- Should `style` be a raw string, a list/map of declarations, or both?
- How should SVG namespaces be represented?
- How much accessibility policy belongs in helpers vs raw attrs? For example,
  should `button` set role automatically, or should native tag semantics be used?
- Should event names be unrestricted strings or a curated enum plus custom escape hatch?
- What is the minimal event payload shape needed for keyboard, submit, pointer,
  clipboard, drag/drop, and file input?
- Are payload descriptors app-authored directly, or hidden behind typed helpers
  like `Html.on_key_down`?
- Can payload extraction failures be represented as reducer non-delivery with a
  diagnostic, or must every payload extractor be total?
- How do we preserve `expect_metric_delta` work-budget assertions when general
  attr/event descriptor tables are added?

## Dependencies and Sequencing

This layer sits **on top of** the wire protocol and **underneath** forms:

- It depends on `WIRE_PROTOCOL_DESIGN_PREP.md` for the general
  `SetAttr`/`RemoveAttr`/`BindEvent` ops, the name/string reference table, and the
  event payload byte format. Do not design the descriptor shape here without
  co-designing its wire encoding there.
- `CONTROLLED_INPUTS_FORMS_DESIGN_PREP.md` is the primary consumer: form controls
  need `type`, `name`, `required`, `aria-*`, and `submit`/`focus`/`blur`/keyboard
  events, all of which this boundary must express first. Forms can act as the
  vertical slice that proves this boundary.
- The prevent-default/stop-propagation policy here also unblocks form `submit`.
  Note the current runtime hard-codes prevent-default to pointer events only
  (`preventDefaultForRocEvent` in `runtime.mjs`); generalizing it is part of this
  work.

Recommended order: settle the protocol encoding, then this boundary, then use the
forms milestone to exercise both end to end.

## Suggested First Milestone

Add a general text attribute and a generalized keyboard event payload as a thin
vertical slice:

- `Html.attr : Str, Str -> Attr`;
- `Html.attr_s : Str, Signal(Str) -> Attr`;
- `Html.on_key_down : KeyPayload -> Msg` or equivalent helper;
- native descriptor support;
- browser command/event support;
- one focused app/spec and one JS contract test.

This proves the open boundary shape without attempting to catalog the web.
