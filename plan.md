# Cor-Style Monotype Re-Inference Removal Plan

## Goal

Eliminate the remaining monotype re-inference paths listed in sections 1, 2,
and 3 of [reinfer.md](/Users/rtfeldman/.codex/worktrees/1d55/roc/reinfer.md)
by changing the Zig compiler to follow the same architectural strategy `cor`
uses:

- earlier stages produce explicit typed expression facts
- monotype consumes those facts directly
- later stages never reconstruct result types or result vars from local syntax
- missing explicit facts are compiler bugs, not situations to recover from

This work must optimize for long-term architecture only:

- no workarounds
- no fallbacks
- no heuristics
- no transitional dual systems
- no “make tests pass first, clean up later”

The only acceptable end state is that the cor-style path is the only path.

## Scope

This plan covers only the remaining monotype items from `reinfer.md`:

### 1. Monotype expected-type fallback

- [`src/monotype/lower.zig:4149`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig:4149)
  `lowerExprWithExpectedType`

### 2. Remaining monotype expression type recovery

- [`src/monotype/lower.zig:4800`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig:4800)
  recorded-method result type
- [`src/monotype/lower.zig:4803`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig:4803)
  plain field-access result type
- [`src/monotype/lower.zig:4817`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig:4817)
  arithmetic result type

### 3. Remaining monotype result-var rediscovery

- [`src/monotype/lower.zig:6443`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig:6443)
  plain-call result var
- [`src/monotype/lower.zig:6467`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig:6467)
  recorded-method result var

## Required Cor-Style End State

The target architecture is:

1. Every executable expression entering monotype already has an explicit solved
   result fact.
   - result type
   - and, where needed for later lowering, explicit result var identity

2. Calls and method calls are resolved before monotype into one explicit
   executable call shape.
   - no monotype code may ask “what is this call’s return type?”
   - no monotype code may ask “what is this method call’s return type?”
   - no monotype code may recover result vars from the callee

3. Field access must use the access expression’s own explicit solved result
   fact.
   - receiver type may still be used for field ordinal/layout lookup later
   - but never to reconstruct the result type

4. Arithmetic must be represented the way `cor` represents kernel operations:
   by explicit earlier-stage callable/signature facts.
   - monotype may consume builtin/kernel-call result facts
   - monotype may not derive arithmetic result type from the lhs

5. `lowerExprWithExpectedType` must stop being a recovery API.
   - callers must provide an explicit usable target type fact
   - otherwise monotype should hit a debug invariant

6. Result locals must be created the `cor` way.
   - from the expression’s own explicit result type
   - not by rediscovering a callee return var

## Work Plan

### Phase 1. Replace section 1 with cor-style explicit expected facts

1. Audit every caller of
   [`lowerExprWithExpectedType`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig:4125)
   and classify why it can currently reach `.placeholder`.

2. For each path, move the missing fact production earlier.
   Expected high-value sites:
   - closure-body lowering
   - tag payload lowering
   - record field lowering
   - call argument lowering
   - block / branch / loop body lowering

3. Introduce one explicit monotype-internal representation for “lower this expr
   with explicit result fact”.
   - do not keep two overlapping calling conventions
   - the explicit-fact path must become the only path

4. Once every caller is fixed, delete the placeholder fallback at
   [`src/monotype/lower.zig:4149`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig:4149)
   and replace it with a debug invariant.

### Phase 2. Replace section 2 with cor-style typed expressions

#### 2a. Recorded-method result type

1. Remove the idea that monotype computes method-call result type locally.

2. Make the earlier dispatch-resolution step produce one explicit executable
   call fact that already includes the result type.

3. Lower recorded-method calls exactly like ordinary explicit calls from that
   fact.

4. Delete
   [`lowerRecordedMethodResultType`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig:6506)
   from monotype’s decision-making role.

#### 2b. Plain field-access result type

1. Stop treating receiver type as the source of truth for access result type.

2. Ensure the access expression itself carries its authoritative solved result
   type into monotype.

3. Keep receiver-type lookup only for field ordinal / layout mechanics where
   needed later.

4. Delete the result-type recovery at
   [`src/monotype/lower.zig:4803`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig:4803).

#### 2c. Arithmetic result type

1. Stop representing arithmetic as a monotype-local “special case whose result
   type comes from the lhs”.

2. Convert arithmetic/comparison lowering to consume explicit builtin/kernel
   signature facts established earlier, matching `cor`’s `KCall` strategy.

3. Make monotype consume the explicit result type from that earlier fact.

4. Delete the lhs-driven recovery at
   [`src/monotype/lower.zig:4817`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig:4817).

### Phase 3. Replace section 3 with cor-style result-local creation

#### 3a. Plain-call result var

1. Stop using callee return-var lookup as the source of the call result.

2. Ensure call lowering yields one typed executable expression whose result type
   is already explicit.

3. Where a named local is required, synthesize it from the expression’s own
   result type, the way `cor` lowers expressions into fresh IR vars.

4. Delete the recovery logic at
   [`src/monotype/lower.zig:6443`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig:6443).

#### 3b. Recorded-method result var

1. Apply the same rule to recorded-method calls.

2. Once dispatch lowering produces one explicit typed executable call shape,
   create result locals from that expression result fact rather than by calling
   `lowerRecordedMethodResultVar(...)`.

3. Delete the recovery logic at
   [`src/monotype/lower.zig:6467`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig:6467).

## Verification Plan

### Phase 4. Verify all tests still pass

After the cor-style conversion is complete and before any cleanup commit:

1. Run the focused suites first:
   - `timeout 600s zig build test-eval -- --threads 1`
   - `timeout 600s zig build test-eval-host-effects -- --threads 1`

2. Run any additional targeted suites that touch the changed areas if needed.

3. Do not accept green tests if they depend on retained fallback code.
   - if a test only passes because an old recovery path still exists, the work
     is not done

### Phase 5. Delete unnecessary fallbacks

1. Remove any transitional helpers that were introduced only to bridge old and
   new representations.

2. Replace remaining “should never happen” branches with debug invariants where
   appropriate.

3. Re-run the verification suite after every deletion wave until the cor-style
   path is the only path left.

## Commit Plan

### Phase 6. Commit

Once:

- the cor-style implementation is the only implementation
- all relevant tests are green
- no transitional code remains

then commit the code changes.

## Fresh Audit Plan

### Phase 7. Re-audit `reinfer.md`

After the commit:

1. Re-run the same “delete it and run tests” audit for the surviving items.

2. Update [reinfer.md](/Users/rtfeldman/.codex/worktrees/1d55/roc/reinfer.md)
   with the new state.
   - do not commit `reinfer.md`

3. Report exactly which items still cannot be deleted without breaking tests,
   and why.

## Success Criteria

This plan is complete only when all of the following are true:

- the remaining section 1 / 2 / 3 monotype items have been converted to the
  cor-style architecture
- no fallback or recovery path remains for those items
- tests still pass
- the cleanup commit is made
- `reinfer.md` is refreshed
- the remaining non-removable items are explicitly reported
