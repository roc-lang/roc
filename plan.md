# Runtime Host-Effects Harness Plan

## Goal

Build a production-faithful runtime host-effects harness for eval tests.

This harness is for observing only what a real host can observe through the
existing `RocOps` / `host_abi` callback boundary:

- `dbg`
- `expect_failed`
- `crashed`

The harness must record:

- exact callback kind
- exact UTF-8 bytes passed to the callback
- exact event order
- whether execution returned normally or terminated via crash

It must **not** observe or depend on:

- source-site ids
- extra test-only metadata
- interpreter-only introspection
- stdout/stderr scraping
- any change to `src/builtins/host_abi.zig`

## Non-Negotiable Design Rules

1. `host_abi` is fixed production contract.
   - do not add fields
   - do not add side channels
   - do not add test-only wrappers to the ABI payloads

2. The harness must assert exactly what the production host receives.
   - exact `dbg` bytes
   - exact `expect_failed` bytes
   - exact `crashed` bytes
   - exact event order

3. No synthetic structure is allowed beyond the event kind itself.
   - if two different source sites emit identical `dbg` bytes, the harness may
     only assert that two identical `dbg` events occurred in order

4. No host-side formatting or rewriting.
   - do not prepend `Expect failed: `
   - do not print `dbg`
   - do not re-render runtime values for assertions

5. Keep value parity and host effects as separate contracts.
   - final returned-value semantics remain in the `Str.inspect((...)) -> RocStr`
     runner
   - host effects get their own runner
   - mixed legacy tests must be split, not merged into a muddled mega-harness

6. Port legacy tests by strengthening them toward exact byte assertions.
   - no substring matching for `dbg` payloads
   - no “one of these several strings is acceptable”
   - no fuzzy assertions carried forward from unstable old interpreter behavior

7. If a legacy test title encoded obsolete behavior, keep production semantics
   instead of preserving the obsolete assertion.
   - example: old expect-failure tests that assumed crash behavior must be
     rewritten around the actual `expect_failed` callback contract

## Runtime Contract

Each backend run in this harness yields:

- `events: []const HostEffect`
- `termination: returned | crashed | problem | timeout`

Where:

- `HostEffect.dbg(payload_bytes)`
- `HostEffect.expect_failed(payload_bytes)`
- `HostEffect.crashed(payload_bytes)`

There is no other observable state.

## Ideal File Ownership

### New Files

- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/test/RuntimeHostEnv.zig`
  - shared production-faithful `RocOps` recorder
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/test/host_effects_runner.zig`
  - process-isolated backend runner for host-effects tests
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/test/host_effects_tests.zig`
  - data-driven runtime host-effects cases

### Existing Files To Reuse, Not Distort

- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/test/helpers.zig`
  - shared source compilation helpers
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/interpreter.zig`
  - must continue forwarding the real callbacks
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig`
  - must continue emitting the real callbacks

### Existing Test-Only Host Plumbing To Replace Or Retire

- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/test/TestEnv.zig`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/crash_context.zig`

These currently embody old test-only behavior:

- printing `dbg`
- rewriting expect payloads
- storing crash separately from a unified event stream

The new harness must not depend on those semantics.

## Harness Architecture

### 1. `RuntimeHostEnv`

Create one shared runtime host environment whose only job is to record raw
production callback traffic.

Required behavior:

- `roc_dbg`
  - append one `.dbg` event with exact copied bytes
  - do not print
  - do not reformat

- `roc_expect_failed`
  - append one `.expect_failed` event with exact copied bytes
  - do not prepend text
  - do not halt execution

- `roc_crashed`
  - append one `.crashed` event with exact copied bytes
  - mark termination as `.crashed`
  - stop current execution via the existing crash boundary mechanism

This env should also own any test-side memory for recorded payloads so every
result is self-contained and serialization-safe.

### 2. Runner Model

Use the same robustness principles as the current parallel eval runner:

- fork per test/backend run
- hard timeout around child execution
- serialize result to the parent
- parent compares normalized recorded results

Do **not** reuse the current `Str.inspect` runner result type by stuffing host
effects into optional fields. Keep a separate runner and a separate result type.

### 3. Backend Coverage

The runtime host-effects harness should run the same currently-supported
backends as the inspect runner:

- LIR interpreter
- dev backend

LLVM/WASM remain out of scope for this project.

### 4. Test Definition Model

Tests should be data-driven.

Each case should specify:

- test name
- Roc source
- source kind / module imports if needed
- exact expected ordered events
- expected termination

Do not encode source-site identity or hidden metadata in test expectations.

### 5. Split Mixed Legacy Tests

Some old tests asserted both:

- final returned value
- host effect stream

For those, keep the final-value coverage in the `Str.inspect` runner and port
only the host-effects portion into the new harness.

Do not create hybrid assertions in one harness.

## Exact Legacy Tests To Port To This Harness

These are the old `origin/main` tests that belong in the runtime host-effects
harness.

Source file:

- `origin/main:src/eval/test/interpreter_style_test.zig`

### A. Crash / Expect Runtime Callback Tests

Port these as runtime host-effects tests:

1. `interpreter: crash statement triggers crash error and message`
2. `interpreter: crash at end of block in if branch`
3. `interpreter: expect expression succeeds`
4. `interpreter: expect expression failure crashes with message`

Normalization rules:

- `expect expression succeeds`
  - assert no `expect_failed` event
  - assert `.returned`

- `expect expression failure crashes with message`
  - do **not** preserve the obsolete “crashes” assumption from the old title
  - port it as:
    - one `expect_failed` event with exact raw callback bytes
    - normal `.returned` termination

### B. Direct `dbg` Runtime Callback Tests

Port these as runtime host-effects tests:

1. `interpreter: dbg statement in block`
2. `interpreter: dbg statement with string`
3. `dbg: integer literal`
4. `dbg: negative integer`
5. `dbg: float value`
6. `dbg: boolean True`
7. `dbg: boolean False`
8. `dbg: empty string`
9. `dbg: list of integers`
10. `dbg: tuple`
11. `dbg: record`
12. `dbg: empty record`
13. `dbg: tag without payload`
14. `dbg: tag with payload`
15. `dbg: function prints as unsupported or function marker`
16. `dbg: expression form returns unit`
17. `dbg: multiple dbg calls in sequence`
18. `dbg: nested dbg calls`
19. `dbg: in if-then-else branch`
20. `dbg: in match pattern`
21. `dbg: in for loop`
22. `dbg: as final expression returns unit`
23. `dbg: with arithmetic expression`
24. `dbg: inside function body`
25. `dbg: function called multiple times`
26. `dbg: with string containing special chars`
27. `dbg: large integer`
28. `dbg: variable after mutation in binding`
29. `dbg: list of strings`

Normalization rules:

- old substring-based assertions must become exact byte assertions
- `dbg: record` must assert one exact final payload string, not field-presence substrings
- `dbg: with string containing special chars` must assert the exact escaped/quoted bytes
- `dbg: function prints as unsupported or function marker` must stop allowing multiple
  acceptable outputs; the production output must be pinned to one exact byte string

### C. Mixed Final-Value + Host-Effects Regression

Port this as a host-effects test while keeping final-value coverage in the
`Str.inspect` runner:

1. `issue 8729: var reassignment in tuple pattern in while loop`

Host-effects harness responsibility:

- assert the exact ordered `dbg` payloads
- assert normal `.returned` termination

Inspect harness responsibility:

- keep the final returned value assertion

## Explicitly Out Of Scope For This Harness

Do **not** port any tests from these files into runtime host effects:

- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/test/anno_only_interp_test.zig`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/test/comptime_eval_test.zig`
- `origin/main:src/eval/test/mono_emit_test.zig`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/test/stack_test.zig`

Reason:

- they are comptime-summary, problem-store, emission-shape, or unit-test suites
- they require different harnesses
- even when they mention `dbg`, `expect`, or `crash`, their contract is not
  runtime `RocOps` observation

## Additional New Coverage Required

The old suite is necessary but not sufficient. Add these new runtime
host-effects tests because they are required by the fixed-ABI production model:

1. Two different `dbg` sites that intentionally emit identical bytes.
   - assert only ordered duplicate payloads
   - prove the harness does not smuggle in hidden site identity

2. `dbg` events before a crash are preserved in order.
   - assert prior `dbg` payloads and final `crashed` payload

3. `expect_failed` does not halt execution.
   - assert `expect_failed` event followed by later observable effects and `.returned`

4. Mixed `dbg` and `expect_failed` ordering.
   - assert exact interleaving order

5. Repeated host effects from loops / recursion.
   - assert exact sequence and count, not aggregate summaries

## Phases

## Phase 1: Fixed-ABI Audit

Audit the current test-side host plumbing and remove any behavior that is not
faithful to production `host_abi`.

Audit targets:

- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/test/TestEnv.zig`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/crash_context.zig`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/test/helpers.zig`

Required findings to eliminate:

- any `dbg` printing
- any expect-message rewriting
- any host-effect observation path that is not an ordered raw event log

## Phase 2: Implement `RuntimeHostEnv`

Implement the shared raw-event recorder.

Success criteria:

- exact bytes recorded for all three callback kinds
- ordered event log
- explicit termination state
- no output printing
- no string rewriting

## Phase 3: Implement Process-Isolated Runner

Implement the dedicated host-effects runner.

Success criteria:

- child isolation
- hard timeout
- parent receives serialized event log + termination
- interpreter and dev both run through the same harness contract

## Phase 4: Port Exact Legacy Inventory

Port every test listed in:

- `Crash / Expect Runtime Callback Tests`
- `Direct dbg Runtime Callback Tests`
- `Mixed Final-Value + Host-Effects Regression`

Success criteria:

- every listed legacy test is accounted for
- each one lands in the new host-effects harness or is explicitly split with the
  `Str.inspect` runner where appropriate
- no fuzzy assertions remain

## Phase 5: Add New Fixed-ABI Coverage

Add the new tests listed in `Additional New Coverage Required`.

Success criteria:

- the harness explicitly codifies the limitations and guarantees of the fixed ABI
- duplicate payloads from distinct sites are treated correctly
- ordering and non-halting semantics are covered

## Phase 6: Final Audit

Audit the old runtime host-effects surface against the new harness.

Checklist:

1. Every legacy runtime host-effects test from `origin/main:interpreter_style_test.zig`
   is either:
   - ported to the new harness
   - or explicitly split with value assertions left in the inspect runner
2. No remaining host-effects assertions depend on:
   - `renderValueRoc`
   - `renderValueRocWithType`
   - printed debug output
   - rewritten expect/crash strings
3. No new harness code depends on changing `host_abi`

## Success Criteria

This plan is complete only when all of the following are true:

1. The runtime host-effects harness observes exactly the existing production
   `host_abi` contract and nothing more.
2. All 34 listed legacy runtime host-effects tests are ported or correctly split.
3. The harness compares exact ordered callback payload bytes.
4. The harness distinguishes only by event kind, payload bytes, order, and
   termination state.
5. No old test-only host formatting or rewriting remains on the active path.
