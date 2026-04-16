# Plan: Delete Post-Check Method Resolution Completely, Then Rebuild It Lazily

## Global Rule

Do **not** run `zig` until this plan explicitly says it is allowed.

That means:

- no `zig build`
- no `zig test`
- no `zig build run`
- no compile-checking for partial progress
- no “just check whether this parses”

At the beginning, and throughout the deletion phase, focus only on deleting the old method-resolution architecture.

## Phase 1: Delete Absolutely Every Trace Of The Existing Post-Check Method Resolution System

Goal:

- remove all post-check method-resolution code
- remove all method-resolution helpers used after type checking
- remove all post-check recovery/reconstruction logic for method targets
- leave all post-check method calls as hard TODO panics

Important scope:

- this phase is about **method resolution after type checking**
- parser / AST / CIR syntax distinction between `a.foo` and `a.foo()` is not the target of deletion
- check-time type-checking of method syntax is not the target of deletion
- everything after checking that tries to turn a method call into a target def **is** the target of deletion

### 1.1 Delete all post-check method-target lookup code

Delete every code path after checking that does any of the following:

- resolves a method target from a receiver var
- resolves a method target from a source var
- resolves a method target from a type-var alias statement
- resolves a method target from text, names, or idents during lowering/specialization
- reconstructs a method target from any checker leftovers
- performs any cross-module method lookup after checking

This includes deleting, not rewriting:

- post-check lookup helpers
- post-check dispatch-resolution helpers
- post-check “recorded dispatch” lowering
- post-check exact-target recovery logic
- any “temporary” compatibility path

### 1.2 Delete all post-check method-resolution entrypoints

The compiler is not “done deleting” until every post-check method-resolution entrypoint is gone.

All places that previously resolved method calls after checking must now do only one thing:

- panic with exactly:

```text
TODO method resolution
```

That means:

- no fallback
- no stub implementation
- no old code kept around behind conditionals
- no “best effort”
- no partial resolution path

### 1.3 Delete all traces, not just active calls

Do not stop at deleting the call sites.

Also delete:

- dead helper functions
- structs and enums that only existed for the old post-check method-resolution system
- comments describing the old system
- debug code for the old system
- tests specifically tied to old post-check resolution behavior
- obsolete terminology that implies the old system still exists

If a type, helper, or comment only makes sense because the old post-check method-resolution architecture existed, delete it.

## Phase 2: Verify The Old Post-Check Method Resolution System Is Truly Gone

Still do **not** run `zig`.

Verification in this phase is by reading, grepping, and auditing source only.

### 2.1 Verify all post-check method-resolution sites are now TODOs

Audit all post-check stages:

- typed CIR consumer code
- monotype
- monotype lifted
- lambdasolved
- lambdamono
- IR lowering
- any helper modules they use

For every place that previously resolved methods after checking, verify it is now either:

- deleted
- or a hard panic with:

```text
TODO method resolution
```

### 2.2 Verify the compiler has no remaining post-check resolution machinery

Do a grep-based audit and keep deleting until this is true:

- the zig compiler has absolutely no clue how to resolve methods after type checking
- every old post-check method-resolution path is gone
- no post-check method lookup logic survives in any form

This verification is not complete until:

- grepping cannot find active post-check method-resolution helpers
- grepping cannot find old recovery pathways
- grepping cannot find source-var / alias-stmt / later-stage method-target reconstruction logic
- there is no remaining shred of evidence in post-check code that the previous method-resolution system still exists, other than intentional `TODO method resolution` panics

### 2.3 Commit the full deletion checkpoint

Once the old post-check system is fully gone and the source audit confirms it:

- commit the deletion checkpoint

This commit is expected to leave the compiler non-working for methods after checking, by design.

## Phase 3: Study The Replacement Design

Still do **not** run `zig`.

Read and use:

- [`method_design.md`](/Users/rtfeldman/.codex/worktrees/1d55/roc/method_design.md)

Use it as the architectural source of truth for the replacement.

Key constraints from that design:

- `a.foo` and `a.foo()` are already syntactically distinct from parsing onward
- no later-stage disambiguation between field access and method calls is needed
- checking records explicit solved vars on method-call nodes
- checked modules emit explicit attached-method definition metadata
- global attached-method index is built only after checked modules are loaded globally
- specialization is the only place exact method resolution runs
- specialization rewrites method calls to direct calls
- no later stage after specialization performs method lookup

## Phase 4: Implement The Replacement Without Running Zig

Still do **not** run `zig`.

Implement the replacement architecture from `method_design.md`.

### 4.1 Checked-output facts

Ensure checked output preserves exactly the facts required for lazy specialization-time method resolution:

- explicit method-call nodes remain explicit
- record access remains separate
- method-call nodes preserve solved receiver/arg/result facts needed by specialization
- per-module attached-method metadata is explicit

### 4.2 Global index construction

Implement the global attached-method index construction that happens after checked modules are loaded into the global compilation context.

This must be:

- exact
- explicit
- cross-module aware
- free of fallback/recovery logic

### 4.3 Specialization-time resolution

Implement the only legal method-resolution path:

- specialization-time resolution from the instantiated receiver nominal plus method ident through the global attached-method index

No other stage may resolve methods.

### 4.4 Rewrite method calls to direct calls during specialization

By the end of specialization:

- unresolved method-call nodes are gone
- calls are rewritten to exact direct calls

Later stages must no longer need any method-resolution logic.

## Phase 5: Add Tests Before Re-Enabling Zig

Still do **not** run `zig`.

Implement tests for:

- polymorphic method resolution
- cross-module method resolution
- cross-module **and** polymorphic method resolution at the same time

Use eval tests if they can cover cross-module behavior.

The tests should verify the intended lazy specialization-time resolution architecture, including:

- same polymorphic function specializing to different concrete method targets
- method resolution across module boundaries
- polymorphic code defined in one module and specialized from another module
- field access continuing to stay separate from method calls

Do not postpone the test implementation until after running `zig`.

## Phase 6: Only Now Re-Enable Zig

Only after all previous phases are complete may `zig` be run again.

At this point:

- the old post-check method-resolution system is deleted
- the replacement is implemented
- the tests are written

Now it is finally allowed to:

- run `zig`
- fix breakage
- get the tests passing

## Phase 7: Make It Work Under The New Architecture

Now that `zig` is allowed:

- run the targeted method-resolution tests first
- fix failures under the new architecture
- then expand to broader relevant suites

Rules for this phase:

- do not resurrect the old post-check method-resolution system
- do not add fallback logic
- do not add heuristic logic
- do not reintroduce alias/source-var recovery
- do not move exact resolution out of specialization

If a failure suggests any of those, treat that as architectural regression and fix the earlier stage/output instead.

## Completion Criteria

The work is complete only when all of the following are true:

- the old post-check method-resolution system was fully deleted first
- there was a deletion checkpoint commit where every post-check method path was only `TODO method resolution`
- the replacement from `method_design.md` is fully implemented
- specialization is the only place exact method resolution happens
- cross-module and polymorphic tests exist
- cross-module polymorphic method resolution works
- later stages after specialization perform no method lookup
