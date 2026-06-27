# Controlled Inputs / Forms Design Prep

Temporary working note for growing Signals form support from text input and
checkbox demos into production-quality controlled inputs and forms. This document
captures requirements and open questions before the final design is folded into
`DESIGN.md`, `GUIDE.md`, and `NEXT_STEPS.md`.

## Problem Statement

The current browser executor has a guarded `SetValue` policy: equal values are
no-ops, differing values are deferred while the input is focused or composing,
and the latest deferred value is applied after blur unless the user's input has
already matched it. This is the right starting point, but it is not enough for
production forms.

Real apps need robust text editing, IME, selection preservation, validation,
submit behavior, many control types, accessibility, and predictable interaction
between browser-local editing state and signal-backed canonical state.

The design question is not just "add more inputs." It is: what reconciliation
contract lets Roc own app state while the browser preserves user editing
semantics?

## Current Context

Relevant current files:

- `test/signals/platform/Html.roc`
  - `text_input` with signal-backed `value` and `on_input` string payload.
  - `checkbox` with signal-backed `checked` and `on_check` bool payload.
- `test/signals/platform/Node.roc`
  - fixed fields for `value`, `checked`, `disabled`.
  - fixed accessors for `target.value`, `target.checked`.
- `test/signals/browser/controlled_input_policy.mjs`
  - standalone deterministic policy for deferred focused/composing value updates.
- `test/signals/browser/controlled_input_policy.test.mjs`
  - JS guard for the current executor-level policy.
- `test/signals/DESIGN.md`
  - lists focused masking, validation, and selection-preserving normalization as
    open questions.

## Requirements

### Text editing correctness

Controlled text inputs must handle:

- normal typing;
- paste;
- undo/redo where the browser supports it;
- selection/caret preservation;
- IME composition start/update/end;
- programmatic canonical value updates while focused;
- validation or formatting that changes the value;
- blur-time normalization;
- stale host updates arriving after newer user input;
- password/search/email/number/tel/url variants.

The framework should avoid common controlled-input bugs:

- cursor jumps to end on every update;
- composition text is overwritten mid-IME;
- app normalization races user input;
- delayed host echoes overwrite newer user edits;
- equal-value updates unnecessarily reset selection.

### Form controls breadth

Production apps need at least:

- text-like inputs;
- textarea;
- checkbox;
- radio groups;
- select single and multiple;
- number inputs with string preservation during editing;
- date/time inputs if supported by target browser needs;
- file input as a special uncontrolled/browser-owned control;
- disabled/read-only/required/invalid state;
- labels and accessible descriptions;
- form submit and reset events.

### Validation model

Validation may be:

- synchronous field validation;
- cross-field validation;
- async validation;
- submit-time validation;
- browser constraint validation integration;
- app-rendered validation messages.

Need a clear boundary between:

- canonical domain state;
- draft editing state;
- validation state;
- browser-native validity state.

### Accessibility

Forms need first-class support for:

- label association;
- `aria-describedby`;
- invalid/error state;
- required state;
- fieldset/legend/group semantics;
- focus management after errors;
- keyboard submit behavior.

This depends on generalizing attributes/events. Do not design forms in isolation
from the attribute/event/payload boundary work.

## Candidate Model: Reconciliation Policy, Not Blind Assignment

Controlled fields should have an explicit reconciliation policy. Possible policy
families:

1. **Immediate controlled**
   - every signal value update attempts to set DOM value;
   - safe for non-focused or simple controls;
   - current guarded no-op/defer policy is a variant of this.

2. **Focused draft with commit**
   - browser-local draft is authoritative while focused;
   - Roc receives draft changes through events;
   - canonical signal may update derived UI but does not overwrite the focused
     field until commit/blur unless a policy allows it.

3. **Formatter / normalizer**
   - app supplies a normalization function and optional selection mapping;
   - host applies normalized value only at safe points or with selection repair.

4. **Uncontrolled host/browser-owned**
   - for file input and possibly some browser-native controls;
   - app receives events but does not set value.

The key design question: should this be expressed as separate `Html` helpers
(`text_input`, `draft_input`, `masked_input`) or as an explicit policy argument?

## Selection Preservation

Formatting while typing requires more than value equality. Need to track:

- current DOM value;
- current selection start/end/direction;
- composition status;
- last value sent to Roc;
- last canonical value received from Roc;
- pending deferred canonical value;
- optional selection transform when canonical value differs.

Open question: should selection data be exposed to Roc payloads, or kept entirely
in JS reconciliation policy?

A conservative first milestone can avoid selection transforms and only guarantee:

- no overwrite while focused/composing;
- apply latest deferred value on blur;
- no-op when equal.

But production masking (currency, phone numbers, credit card, slug fields) will
need selection-aware normalization.

## Submit and Form Events

Forms need an event model that supports:

- `submit` with prevent-default by default for app-managed forms;
- `reset`;
- Enter-key behavior;
- button type semantics (`submit`, `button`, `reset`);
- disabled submit button;
- validation before submit;
- async submit state and duplicate-submit prevention.

This requires generalized events and payload policy. Current click/input/check
is insufficient.

## Native Spec Runner Requirements

Native specs should support semantic form actions and expectations beyond current
coverage:

- `focus <locator>`;
- `blur <locator>`;
- composition sequence or a simplified IME action;
- selection/caret simulation if selection preservation is part of the contract;
- `submit <locator>`;
- `expect_invalid`, `expect_required`, `expect_described_by` if those become
  framework-supported semantics;
- metric assertions around form events and controlled updates.

Need to avoid over-modeling the browser in native tests. Browser-only behavior
such as actual IME event order should be contract-tested in JS where necessary.

## Browser Runtime Requirements

- Track focused/composing state per DOM node.
- Store deferred value updates per input.
- Clear state on node removal to avoid leaks.
- Refresh memory views after event payload allocation.
- Apply `SetValue`, `SetChecked`, and future `SetSelection` commands safely.
- Respect browser constraints for file inputs and security-sensitive controls.
- Use event listeners that match native browser behavior (`input`, `change`,
  `compositionstart`, `compositionend`, `beforeinput` if needed).

## Relationship to General Attribute/Event Boundary

Forms are blocked by the generalized boundary in several places:

- need `type`, `name`, `placeholder`, `autocomplete`, `required`, `readonly`,
  `aria-*`, `form`, `min`, `max`, `step`, etc.;
- need events: `submit`, `change`, `focus`, `blur`, `keydown`, composition,
  beforeinput, invalid;
- need payloads beyond string/bool;
- need prevent-default for submit.

Therefore form work should either follow the boundary generalization or be the
vertical slice that proves it.

## Outstanding Questions

- What is the smallest input reconciliation contract that is safe enough for real
  apps without overbuilding masking support?
- Should focused draft state live in Roc app state, JS executor state, or both?
- How should app-authored normalization preserve selection?
- Should number inputs expose strings while editing and parse only on commit?
- How are radio groups represented: separate checked controls or one group value?
- How should file input be modeled without pretending the app can set its value?
- Should browser constraint validation be used, ignored, or wrapped?
- How should submit prevent-default be expressed?
- Do we need first-class focus commands for error handling and accessibility?
- What parts of IME behavior can native specs model, and what must remain JS
  contract coverage?

## Dependencies and Sequencing

Forms are the **highest layer** of the four prep docs and depend on both lower
layers:

- Blocked by `ATTRIBUTE_EVENT_PAYLOAD_BOUNDARY_DESIGN_PREP.md` for control
  attributes (`type`, `name`, `required`, `readonly`, `min`/`max`/`step`,
  `aria-*`), form events (`submit`, `change`, `focus`, `blur`, keyboard,
  composition, `invalid`), richer payloads, and submit prevent-default.
- Blocked by `WIRE_PROTOCOL_DESIGN_PREP.md` for reconciliation ops
  (`SetInputValue` with a policy ref, future `SetSelection`) and for carrying the
  new events/payloads.
- Largely independent of `HTTP_EFFECTS_DESIGN_PREP.md`, except that async
  submit/validation state reuses the effects model.

Recommended order: do not start forms until the attribute/event boundary and its
wire encoding are at least a working slice; then forms become the integration
test that proves those layers on a real production surface. The conservative
reconciliation contract (no overwrite while focused/composing, apply on blur,
no-op when equal) can be built before selection-aware masking.

## Suggested First Milestone

Use a focused form app to prove a minimal production-safe contract:

- generalized attrs for `type`, `placeholder`, `name`, `required`, `aria-*`;
- `submit` event with prevent-default;
- text input guarded against focused/composing overwrites;
- blur-time deferred value application;
- checkbox remains supported;
- one validation message tied by `aria-describedby`;
- native semantic spec plus JS controlled-input contract test.

Defer selection-preserving masks, file input, radio/select, and async validation
until the minimal reconciliation contract is proven.
