# Guarded Eval And Glue Stabilization Plan

## Goals

The goal is to get eval tests passing first, then glue tests passing, while preserving the long-term architecture:

- Lambdamono has one executable representation truth after planning.
- Callable and capture truth are owned by the planner/queue fact handles, not by env side fields, expression metadata side tables, or source-type rediscovery helpers.
- Each compiler stage consumes explicit facts produced earlier.
- No fix may reintroduce reconstruction, fallback, heuristic inference, local rebuilding, best-effort recovery, competing source/executable carriers, or semantic side channels.
- If a test failure exposes missing information, the fix is to thread the explicit fact from the stage that actually knows it.
- If a test failure exposes old cruft, delete it before continuing with test debugging.

## Hard Command Rule

Every Zig invocation must go through the guard script:

```sh
ci/guarded_zig.sh zig ...
```

Do not run `zig ...` directly.

The wrapper runs all checked-in guardrails before the requested Zig command:

- all `ci/*.pl` Perl checks, including `ci/semantic_audit.pl` and `ci/check_postcheck_architecture.pl`
- `ci/check_ownership_boundaries.py`
- `bash ci/check_debug_vars.sh`
- `zig build check-fmt`
- `zig run ci/zig_lints.zig`
- `zig run ci/tidy.zig`
- `zig run ci/check_test_wiring.zig`

If the wrapper precheck fails, stop the test loop and fix the architectural/lint/audit failure first. Do not bypass or weaken the wrapper.

## Stabilization Order

1. Run the gate itself first:

```sh
ci/guarded_zig.sh zig build check-semantic-audit
```

2. Fix any compile/lint/audit failures under the explicit-facts architecture only.

3. Run targeted eval tests before broad eval:

```sh
ci/guarded_zig.sh zig build test-eval -- --test-filter '<targeted eval name>'
```

Start with tests covering:

- ordinary dispatch specialization
- polymorphic specialization
- closure early returns
- boxed erasure boundaries
- non-Box values containing zero erased nodes
- aggregate literals with explicit executable result types
- empty-list typing/defaulting
- compile-time constants and expect execution

4. Run broader eval checks:

```sh
ci/guarded_zig.sh zig build test-cor-pipeline
ci/guarded_zig.sh zig build test-eval
```

5. Only after eval is green, move to glue:

```sh
ci/guarded_zig.sh zig build test-glue -- --test-filter 'glue regression: ZigGlue interpreter succeeds on fx platform'
ci/guarded_zig.sh zig build test-glue
```

## Failure Handling Rules

For every failure:

- Inspect the failing stage and identify the explicit fact that should have existed.
- Add or thread that fact from the earliest stage that owns it.
- Keep the emitter/backend/interpreter mechanically dumb; they consume planned facts and descriptors only.
- Rerun `perl ci/semantic_audit.pl` immediately after any semantic lowering/eval/glue/bridge/specialization change, or rerun through `ci/guarded_zig.sh` if the next step is a Zig command.

Forbidden fixes:

- reintroducing source lookup recovery
- rebuilding callable shape from args/receiver/result
- storing callable/capture truth in env entries
- adding side tables keyed by expression or symbol for semantic truth
- deriving executable callable signatures outside `ExecPlan` or queue summaries
- merging/strengthening executable types after planning
- using body-derived bind refinement as a source of truth
- adding `fallback`, `recover`, `heuristic`, `reconstruct`, `best effort`, or `override` logic in semantic compiler/eval/lowering paths
- adding debug trace prints or investigation diagnostics
- weakening audits or allowlisting a real semantic violation

## Audit Loop

After every meaningful fix batch, run:

```sh
perl ci/semantic_audit.pl
perl ci/check_postcheck_architecture.pl
```

Also use targeted grep checks for any newly suspicious family. If the audit finds anything, the task becomes deleting that family and strengthening the audit so it cannot return.

The stabilization is not complete until:

- `ci/guarded_zig.sh zig build test-eval` passes
- `ci/guarded_zig.sh zig build test-glue` passes
- all wrapper prechecks pass
- the semantic audits remain clean
- no known reconstruction/fallback/heuristic/duplicate-truth pattern has been reintroduced

## Pitfalls To Avoid

- Do not confuse making tests green with making the compiler correct. Correctness means preserving one explicit source of truth.
- Do not add temporary compatibility paths. Temporary paths become permanent duplicate truths.
- Do not make the queue optional. Queued callable summaries and executable signatures are the post-queue truth.
- Do not infer erasure from containers. Only `Box.box` and explicit erased boundaries create erased executable nodes.
- Do not let compile-time eval inspect semantic values to reconstruct compiler constants. Use LIR execution plus schema/constant graph facts.
- Do not let glue-specific fixes bypass eval invariants. Glue uses the same semantic architecture.
