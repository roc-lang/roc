# Pin Deferred Spec Requests to Checked Use-Site Types

## Problem

A program that `roc check` accepts panics during `roc build`/`roc run` in the
Monotype instantiation solver:

- roc-lang/roc#9968 — a parser built from record-combinators (an optional
  parameter followed by a rest parameter) panics with `instantiation unified
  a non-empty record with an empty record`
  (`src/postcheck/monotype/solve.zig:793`, `unifyRowWithEmpty`, reached from
  `unifyConcrete` at `solve.zig:524`).

The mechanism, verified in lldb on the repro: the combinator chain threads a
phantom, type-level-only row through a shared type variable — the failing
binding is `to_action = { get_params : {} }`, which appears **only as an
unused type argument of a nominal `Builder` type**, never as a value. The
checker solves this fine. During Monotype lowering, however, the requester's
per-spec instantiation graph re-derives call types by replaying value-flow
constraints, and a phantom position has no value-level edge to travel on:
`unifyThroughBacking` (`solve.zig:657`) relates a named node to a structural
node through the *backing* type and never pairs the named type arguments, so
no evidence ever delivers `{ get_params : {} }` to the graph node behind the
deferred request. When the deferred request seals
(`sealDeferredSpecRequestsFrom`, `src/postcheck/monotype/lower.zig:2829`),
the unresolved row node takes its `row_default` and closes to `{}`. The
callee template's own checked annotation then contradicts the sealed request
— and the solver panics **by design**: its header doctrine says a
"specialization that needs more than its requested type is a unification
conflict rather than a silent rewrite" (`solve.zig:7-8`).

The defect is not the panic; the panic is correct enforcement. The defect is
that the request was sealed from *re-derived* value-flow evidence instead of
from the type the checker already proved for the use site. This is the
backlog's core disease shape — a fact proven during checking (the phantom-row
binding), re-derived downstream through structural replay, a default papering
over the gap, and a panic at the consumption site — landing inside the
recently-landed instantiation-graph machinery itself.

## Background

The compiler pipeline: parse → canonicalize → type-check (checked artifacts;
all user-facing failures end here) → postcheck: Monotype IR
(monomorphization; `src/postcheck/monotype/`) → Monotype Lifted → Lambda
Solved → Lambda Mono → LIR → ARC → backends. `design.md` is authoritative;
read "Monotype Instantiation" and "Row, Nominal, Alias, And Opaque
Authority" before starting.

Monotype lowering solves each specialization in a per-spec instantiation
graph (`src/postcheck/monotype/solve.zig`). Procedure-template body requests
discovered while lowering a specialization are **deferred to the end of the
requesting specialization** and sealed then, so that requests are made at
final types and specialization keys stay stable (`solve.zig:25-27`). A row
node that no evidence has pinned by seal time takes its `row_default` —
which is the correct treatment for *genuinely unconstrained* rows (that is
what defaults are for), and the wrong treatment for rows the checker already
solved but the graph never learned about.

Each deferred request already carries the checked identity of what it is
requesting: the `deferred_templates` entry records `source_fn_ty` (the
checked function type at the use site) alongside the mono `fn_ty`
(`lower.zig:2586-2596`). Today that checked type is consulted only on the
**callee** side, **after** sealing, and **only when the requester and the
template live in the same module**: `lowerTemplateWithMonoFor` constrains
`source_fn_ty` against the template's public function type solely under
`moduleBytesEqual(source_ty_view.key.bytes, view.key.bytes)`
(`lower.zig:1451-1453`), then constrains the template's checked root
(`constrainTypeToMono`, call at `lower.zig:1455`, implementation in
`BodyContext` at `lower.zig:~7376`). By that point the requester's graph has
already sealed the request with the defaulted row, and the contradiction is
unrecoverable.

The check-side sibling of this bug was already fixed once: the open-row
widening work (issue roc-lang/roc#9614, PR roc-lang/roc#9617) kept
`row_default`-closed rows widenable during constraint solving. This project
is the Monotype-side counterpart, at a stage where widening is by-design
impossible (sealed snapshots) — so the row must arrive *before* sealing,
from checked data.

## Evidence

All symbols verified in the current tree.

- `src/postcheck/monotype/solve.zig`: header doctrine (`:7-8`), deferred
  request rationale (`:25-27`), `unifyConcrete` (`:524`),
  `unifyThroughBacking` (`:657` — relates named↔structural through the
  backing; named type arguments are not paired), `unifyRowWithEmpty`
  (`:793`, the panic).
- `src/postcheck/monotype/lower.zig`: deferred request creation carrying
  `source_fn_ty` (`:2586-2596`), `sealDeferredSpecRequestsFrom` (`:2829`,
  drain at `:~2887`), the same-module-only `constrainKnownType` condition
  (`:1451-1453`) followed by `constrainTypeToMono` (`:1455`);
  `BodyContext.constrainTypeToMono` (`:~7376`).
- lldb specifics from the #9968 repro: failing template `Param.maybe_str`;
  the non-empty side is the phantom `to_action = { get_params : {} }`
  (field label resolved through the interner); the empty side is the sealed
  request mono, requested from the app spec's graph via the
  `main!`/`cli_parser` chain.
- Discriminating experiments (all preserved in the investigation scratchpad,
  reproducible from the issue): replacing `{ get_params : {} }` with `{}`
  fixes it; making the second combinator's phantom concrete fixes it;
  decoupling the combinators' shared type variable fixes it (the shared-var
  chain is load-bearing); changing the phantom's *other* argument from `{}`
  to `I64` does **not** change the panic (rules out positional/slot-pairing
  bugs); local-vs-top-level placement of the parser is irrelevant.
- The landed instantiation-graph work this extends: the per-spec solver
  graph (merged to main ~2026-06-12) whose eager-materialize/refresh and
  request-deferral design this project completes rather than revises.

## Solution design

Make every deferred request consume the checker's solved use-site type as
explicit input to its graph node, before sealing can default anything.

1. **Pin at request creation.** When a deferred template request is created
   (`lower.zig:2586-2596`), constrain the request's function-type node in
   the *requester's* graph against `instNode(request.source_fn_ty)` — the
   checked use-site type it already carries. "Requesting at final types"
   then means *final per the checker*, not "whatever the graph learned
   through value flow". Phantom bindings travel as checked data;
   `row_default` at seal time goes back to meaning "genuinely unconstrained".

2. **Delete the same-module restriction, don't widen it.** The
   `moduleBytesEqual` condition at `lower.zig:1451` exists because the
   callee-side constraint needs the source type expressed in the callee's
   type space. With pinning done requester-side (where `source_fn_ty` is
   native), the callee-side special case becomes redundant for what it was
   compensating for; remove it or reduce it to a debug agreement check —
   whichever the migration shows, but do not leave two overlapping
   constraint paths with different module conditions.

3. **Audit `unifyThroughBacking`'s named-argument pairing.** Pinning fixes
   the delivery of checked facts to requests; the propagation hole — a named
   node meeting a structural node loses the named arguments' pairing — may
   still starve *intra-graph* nodes that only connect through a backing.
   Decide with evidence: add a debug counter/assert for "row node defaulted
   at seal while its checked counterpart was concrete", run the full corpus,
   and pair named arguments through the backing if any site still fires.

4. **Enforcement stays.** The seal-time contradiction panic remains exactly
   as it is (debug assertion / release unreachable per design.md). After
   this project it is unreachable for checked programs; it is the regression
   tripwire, not a condition to soften.

## What success looks like

- The #9968 repro checks, builds, and runs correctly on all backends.
- The debug check from item 3 fires nowhere on the snapshot corpus, the
  examples, and the roc-parser package suite.
- There is exactly one place where a deferred request learns its type — the
  pin at creation — and no module-conditional constraint path shadows it.
- `row_default` at seal time is reachable only for rows the checker left
  genuinely unconstrained (assertable in debug by comparing against the
  checked type's row).

## How to evaluate the result

### Correctness ideal

- *Requests consume checked types*: a deferred request's sealed type is a
  function of `source_fn_ty` plus genuine defaults — never of which
  value-flow edges happened to exist. Enforced by the pin plus the item-3
  debug check.
- *No silent rewrite*: the solver's conflict-over-rewrite doctrine is
  untouched; the panic remains the enforcement of record.
- Behavioral: full snapshot corpus and cross-backend eval corpus
  (interpreter/dev/LLVM/wasm agreement) unchanged; the #9968 repro and the
  matrix below become permanent tests.

### Performance ideal

One `instNode` + one unification per deferred request, each bounded by the
size of the requested function type — work proportional to data the request
already stores. No new traversals, no name lookups, no per-seal re-walks.
Measure Monotype lowering time on the specialization-heavy corpus
(roc-parser examples) and `examples/`; require parity within noise. Zero
effect on generated code.

## Tests to add

Write the regression test first and confirm it panics on the unmodified
tree:

- `issue_9968`: the issue's optional-param + rest-param parser as a CLI run
  test (`test/cli/issue_*.roc` + `src/cli/test/parallel_cli_runner.zig`
  convention): `.exit = .not_panic`, `not_contains` the
  `instantiation unified` panic strings, expected program output.
- Phantom-row matrix: a nominal type with a phantom type argument bound to a
  non-empty record row / a tag row / a nested nominal, each threaded through
  a combinator chain with a shared type variable, each consumed only at the
  type level; assert build + run output.
- The discriminating controls from the investigation, pinned as tests: the
  concrete-phantom variant (must keep working) and the `I64`-argument
  variant (must now also work, and guards against slot-pairing regressions).
- Cross-module split: requester and template in different modules with a
  phantom-carrying request (exercises the deleted `moduleBytesEqual`
  condition).
- Debug-build corpus check: the item-3 counter asserts zero
  defaulted-while-checked-concrete rows across `zig build snapshot` and the
  CLI suite.

## Related projects

- The landed total-dispatch-plans work — the same discipline (downstream
  consumes checked data; no re-derivation) applied to dispatch; this project
  applies it to specialization request types.
- The landed immutable specialization identity work (`SpecIdentity`,
  `SpecBuilder` in `src/postcheck/monotype/`) — the other identity/typing
  surface of `lower.zig` specialization records; pinning requests at checked
  types also reduces the pressure that made request/solved dual entries
  diverge.
