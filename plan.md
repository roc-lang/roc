# Plan: Post-Check Pipeline Completion

This plan tracks the remaining verification and maintenance work for the
post-check compiler pipeline described in `design.md`.

The active architecture is:

```text
checked modules
  -> Monotype IR
  -> Monotype Lifted IR
  -> Lambda Solved IR
  -> Lambda Mono IR
  -> LIR
  -> ARC insertion
```

There is one public checked-module-to-LIR lowering path. Every stage consumes
explicit data from the previous stage. No stage may guess, recover, repair,
approximate, or fall back to another lowering path.

## Completed

- The Cor-style post-check modules exist under `src/postcheck`.
- Public lowering goes through Monotype, Monotype Lifted, Lambda Solved, Lambda
  Mono, direct LIR lowering, and ARC insertion.
- Source static dispatch, method equality, type dispatch, and source `for` are
  removed while producing Monotype IR.
- Iterator `for` uses resolved `.iter` and `.next` dispatch plans and explicit
  loop-carried iterator state.
- Lambda Solved owns callable flow in the type graph.
- Lambda Mono turns function values into explicit finite callable values or
  erased callable values.
- Direct LIR lowering owns layout commitment, runtime schemas, function result
  data for `ConstStore`, and statement-only LIR emission.
- Compile-time evaluation stores checked Roc values in `ConstStore`; cached
  consts restore into ordinary Monotype expressions.
- `src/mir`, `src/ir`, and `src/lir/lower_ir.zig` are deleted.
- Required deleted-path greps over `src` return no matches.
- `zig build minici` passed after switching to the new path.

## Remaining Work

The remaining work is verification and cleanup discipline, not another design
or another lowering path.

1. After every cleanup commit, run the audits below.
2. Run focused tests for any touched stage.
3. Run `zig build minici`.
4. If a check fails, fix the producer that failed to publish the required data.
   Do not add a fallback, recovery pass, later reshaping pass, or runtime check
   whose only purpose is maintaining compiler invariants.
5. Keep `design.md`, `AGENTS.md`, and post-check comments aligned with the
   architecture above.

## Required Audits

Run the maintained audit through `zig build minici`.

These paths must not exist:

```sh
test ! -e src/mir
test ! -e src/ir
test ! -e src/lir/lower_ir.zig
```

## Invariant Audit

Boundary checks that exist only to maintain compiler invariants must be
debug-only. Release builds should execute no validation loops for layouts,
callable encodings, schemas, static-dispatch plans, or stage IRs unless the
loop is required for normal compilation output.

Minimum debug-only stage checks:

- Monotype IR contains no source dispatch, method equality, type dispatch, or
  source `for` nodes.
- Monotype IR contains only closed monomorphic types.
- Monotype IR contains no runtime tag discriminants, layout ids, or callable
  representation ids.
- Monotype Lifted IR contains no closure expressions or local function
  definitions in expression position.
- Lambda Solved has every function type in `args/callable/ret` form.
- Lambda Solved has no unresolved callable slot before Lambda Mono lowering.
- Lambda Mono contains no function type, no value-call node, and no unresolved
  lambda set.
- Lambda Mono contains no runtime tag discriminants or layout ids.
- LIR lowering receives only Lambda Mono IR.
- Backends receive only ARC-complete LIR.

## Documentation Rule

Repository docs should describe one architecture: checked modules to Cor-style
post-check IRs to LIR. Historical implementation details belong only in commit
history, PR discussion, or explicitly marked migration notes outside the
authoritative design.
