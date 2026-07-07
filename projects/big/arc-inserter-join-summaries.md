# ARC Inserter Join Summaries

## Problem

ARC insertion in `src/lir/arc.zig` still answers join and liveness questions
by re-walking ownership-neutral LIR under the current `OwnedSet`. That was a
reasonable implementation shape when the all-owned inserter asked a small
number of local questions, but it is now a compile-time bottleneck on
generated, join-heavy code.

The concrete stress case is structural JSON encoding. `JsonEncodeRoundTrip.roc`
contains a 25-field `Shape` with lists, sets, dicts, nested records, tag
unions, nullable/missing fields, and 29 `Json.to_str` sites. Structural
`encoder_for` lowering expands every field and payload into `Try`-sequenced
state-passing code; every `Try` sequence introduces control flow. The LIR is
valid, but ARC insertion then repeatedly analyzes the same generated regions
with slightly different ownership sets. This single CLI test took 212 seconds
with `roc test --no-cache`, and a sample of the running
compiler showed the main thread almost entirely in:

```text
checked_pipeline.lowerCheckedModulesToLir
  -> arc.insert
  -> arc.Inserter.rewritePath
  -> arc.Inserter.scheduleRewriteJoin
  -> arc.Inserter.joinBodyOwnedSet
  -> arc.Inserter.analyzeJumpsToJoin
  -> arc.Inserter.processAnalysisPath
```

This is not a JSON runtime problem and not a single structural encoder
lowering problem. A minimal multi-payload-tag encoder compiles in a few
seconds. The large test is slow because generated encoder LIR creates many
joins and many refcounted locals, and the production ARC inserter treats join
ownership as an on-demand path-interpretation problem.

The design document already names the intended remedy in "RC Statement
Emission": if ARC insertion becomes hot, replace on-demand scans with one
precomputed per-statement liveness table per proc, consumed by the same
decision points. That narrow liveness table is necessary but no longer
sufficient: the current hotspot is also `joinBodyOwnedSet`, which recursively
walks join remainders and nested scoped joins to compute keep sets. The
production inserter needs the same structural treatment as the certifier
project: finite summaries, worklists, and explicit joins, not repeated
rediscovery.

## Background

The compiler pipeline: parse -> canonicalize -> type-check -> postcheck IRs
(Monotype -> Lifted -> Lambda Solved -> Lambda Mono) -> LIR lowering -> ARC
insertion -> backends. `design.md` at the repo root is authoritative.

LIR lowering emits ownership-neutral control flow. ARC insertion then:

- solves borrow modes and proc ownership signatures with `arc_solve.zig`;
- rewrites each proc body in `arc.zig`, emitting explicit `incref`,
  `decref`, `decref_if_initialized`, and `free` statements;
- optionally emits mode-specialized proc variants for optimized builds;
- in debug builds, runs `arc_certify.zig` over the emitted LIR.

Backends, the interpreter, and LirImage consume explicit RC statements only.
They must not infer ownership, and this project must not move ownership
policy out of ARC.

The inserter's rewrite pass threads an `OwnedSet` through one proc variant at
a time. A set bit means "this path currently carries the ownership unit for
this local." Joins are the hard part because a join body is emitted once but
may be reached from many jumps. A local can enter the shared body as owned
only when every reachable jump reaches that join with the unit owned and the
body actually needs it.

The current implementation answers that with local analysis calls:

- `joinEntryOwnedSet(entry_owned, remainder)` filters the entry state to the
  locals used in the run-once remainder.
- `joinBodyOwnedSet(entry_owned, join_id, ..., remainder, body)` calls
  `analyzeJumpsToJoin` from the remainder to all jumps targeting that join,
  intersects the resulting `OwnedSet`s, then filters the result to locals
  used in the join body.
- `analyzeJumpsToJoin` allocates a fresh `AnalysisSeen` map per call and
  recursively follows LIR paths under the current ownership state.
- `groupUsedInPath` and `localValueUsedInPath` answer "used before rebind?"
  questions from arbitrary statement starts. `computeReadsBeforeRebind` is
  already a backward dataflow cache for singleton-group questions, but
  multi-member borrow groups still fall back to forward scans and loop-specific
  contexts require separate cache entries.
- `join_body_memo` caches only by `(join_id, owned_digest)` inside the current
  proc emission. A different but overlapping ownership state is a different
  entry and therefore a fresh walk.

The debug certifier already landed this discipline: its distinct-entry-state
enumeration was replaced with a finite lattice fixpoint (`JoinGroup` joining
in `src/lir/arc_certify.zig`), and ownership-transfer keying is centralized
in `arc.zig`'s transfer layer. This project is the production-inserter
counterpart: it attacks compile time in `arc.zig`, not the debug-only
certifier.

One existing LIR pass is the closest implementation model:
`src/lir/tag_reachability.zig`. It tracks possible tag values in monotone
`TagSet` / `ValueInfo` summaries, iterates until no local or return summary
changes, then rewrites switches. ARC summaries need a richer domain, but the
shape is the same: compute facts once over LIR, converge structurally, then
consume the facts without ad hoc re-walks.

## Evidence

All paths relative to the repo root; symbols verified against the current
tree.

- `src/lir/arc.zig`
  - `insert` creates per-emission scratch state:
    `scan_needles`, `scan_visited`, `scan_stack`,
    `reads_before_rebind_cache`, `active_loop_keep_ids`, `join_body_memo`.
  - `scheduleRewriteJoin` computes `entry_keep` with `joinEntryOwnedSet` and
    `body_keep` with `joinBodyOwnedSet` before it can rewrite the remainder
    and body.
  - `joinBodyOwnedSet` memoizes by `(join_id, owned_digest)` but otherwise
    calls `analyzeJumpsToJoin`, intersects every jump state's `OwnedSet`, and
    then calls `groupUsedInPath(body, local, null)` for each surviving local.
  - `analyzeJumpsToJoin` creates a fresh `AnalysisSeen` and
    `AnalysisScopedJoinMap` for each query. Its `.jump` case can recursively
    call `joinBodyOwnedSet` for scoped joins, so nested generated joins
    amplify the cost.
  - `analysisSeenContainsOrAppend` clones full `OwnedSet`s into buckets keyed
    by statement plus digest. This is exact but allocation-heavy.
  - `computeReadsBeforeRebind` is a real backward dataflow summary, but it is
    scoped to read-before-rebind queries and is cleared for every proc
    emission. It does not summarize join-body keep sets or ownership transfer
    through remainders.
  - `localValueUsedInPath` and `anyNeedleUsedInPath` are still forward graph
    scans over the statement graph.
- `src/postcheck/monotype/lower.zig`
  - `lowerStructuralEncoderFor`, `lowerEncodeShapeToState`,
    `lowerEncodeRecordToState`, `lowerEncodeTagUnionToState`, and
    `lowerEncodePayloadArrayItemsFromState` generate large state-passing
    encoders. Every field/payload is sequenced through a `Try` state, so
    records and tag unions produce dense join-heavy LIR before ARC ever runs.
- `test/cli/JsonEncodeRoundTrip.roc`
  - 575 lines, 27 expects, 29 `Json.to_str` sites.
  - The main `Shape` record has 25 fields, including nested structural
    containers and custom/derived encode paths.
  - `zig-out/bin/roc test --no-cache test/cli/JsonEncodeRoundTrip.roc`
    measured `All (27) tests passed in 212494.0 ms`.
- `src/lir/tag_reachability.zig`
  - `Pass.analyze` loops while any `ValueInfo` changes.
  - `TagSet.mergeFrom` and `ValueInfo.mergeFrom` are monotone joins.
  - The pass rewrites only after the fixed point is complete. This is the
    model to copy for summary-first LIR analysis.
- `design.md`
  - "ARC Borrow Inference" says borrow inference is a least-fixed-point
    computation over finite lattices and that emission consumes solved modes
    and precise lifetimes.
  - "RC Statement Emission" explicitly says profiling-hot insertion should
    be fixed with precomputed per-statement liveness tables consumed by the
    same decision points, not weaker scanning.

## Solution design

Replace on-demand path interpretation in the production ARC inserter with
per-proc summary tables. The rewrite walk still emits RC statements, and it
still consumes the same solved `RcSig` and ownership-transfer decisions. The
change is how it answers liveness and join-keep questions.

### Part 1: shared transfer decisions

Build on the landed ownership-transfer keying layer in `arc.zig` (the
`transferFor*` functions shared by both walks). The summary solver must not
create a third copy of transfer semantics.

Create a shared inserter transfer layer used by:

- the rewrite walk, which materializes RC statements;
- the summary/dataflow walk, which updates abstract ownership state;
- the existing analysis-only paths during migration, until they are deleted.

The helper returns small decision structs for each ownership-moving statement:
which source units move, which args must be retained, which target unit is
born, which previous target unit dies, and which conditionally initialized
payload state is affected. It routes all ownership-unit keys through
`Solution.unitLocalOf`, exactly like the keying project requires.

This part is a prerequisite for confidence, not a performance feature by
itself. The performance win comes from making the analysis consume the
decisions once through summary tables.

### Part 2: per-proc liveness summaries

Generalize `computeReadsBeforeRebind` into the liveness table promised by
`design.md`.

For each proc emission, build a control-flow summary graph over the original
ownership-neutral body:

- nodes are `LIR.CFStmtId`s;
- edges are normal `next` edges, switch arms, continuations, string-match
  arms, initialized/uninitialized branches, join remainders, and jump-to-body
  edges using the collected `JoinBodyMap`;
- each node records local reads and local defs in terms of ownership-unit
  locals / borrow-group leaders, not raw aliases.

Run a backward fixed point to compute, for every statement and loop-keep
context, the set of unit groups read before rebind from that point. Then:

- `groupUsedInPath(start, local, loop_keep)` becomes a bit test;
- `joinEntryOwnedSet` no longer scans one local at a time;
- `joinBodyOwnedSet` no longer filters candidate body locals by forward
  graph scans;
- `localValueUsedInPath` and `anyNeedleUsedInPath` leave the hot path and
  are deleted after migration.

Loop keep-sets must remain explicit. A loop-specific table is allowed when
the keep-set changes the answer; it is keyed by the stable loop keep id
already tracked in `active_loop_keep_ids`. There must be no heuristic "close
enough" reuse across loop contexts.

### Part 3: join keep-set dataflow

Replace `joinBodyOwnedSet` / `analyzeJumpsToJoin` re-walks with a
simultaneous finite dataflow over join summaries for one proc emission.

Each join gets a summary:

```text
JoinSummary {
  entry_keep: OwnedSet,      // units that must survive into the remainder
  body_keep: OwnedSet,       // units that enter the shared body as owned
  body_reachable: bool,
}
```

The abstract state is a must-owned set: a unit is present at a merge only if
all paths represented by that merge carry it. The join operator is set
intersection for existing path states, plus explicit insertion of owned join
params and maybe-initialized params exactly where the current code inserts
them. Transfer functions are the shared statement decisions from Part 1.

The equations are the existing comments in executable form:

- `entry_keep(join)` is the subset of the incoming state that is read in the
  join's run-once remainder before being rebound or released.
- `body_keep(join)` is the intersection of the states at every reachable jump
  to that join, filtered to units read in the shared body, plus the owned
  join params and conditional maybe-initialized params.
- nested scoped joins contribute through their own `body_keep` summaries
  instead of recursively calling `joinBodyOwnedSet`.
- an unreachable body has `body_reachable = false` and an empty jump-state
  contribution, matching today's behavior.

Use a worklist. When a join's `entry_keep` or `body_keep` changes, enqueue the
regions that depend on it. The domain is finite: each summary is a bitset over
the proc's locals plus a boolean, and every update moves by intersection or
explicit finite insertion prescribed by a statement transfer. The module doc
comment must state the monotonicity argument and the termination bound.

The rewrite walk then becomes a consumer:

- `scheduleRewriteJoin` reads `entry_keep` and `body_keep` from the summary
  table;
- `.jump` releases `owned - body_keep(target)` without asking a recursive
  analysis question;
- switch continuation handling uses precomputed branch exit summaries rather
  than allocating fresh `OwnedSet` arrays per question;
- `AnalysisSeen`, `AnalysisScopedJoinMap`, `JoinBodyMemo`, and the
  `analyzeJumpsToJoin` family are deleted.

If the summary solver cannot express a current behavior, that is a design
bug in the solver. It must not fall back to the old re-walk path in normal
compilation.

### Part 4: instrumentation and migration

Land this behind counters before deleting old code.

Temporary debug counters should track:

- number of `joinBodyOwnedSet` calls;
- number of `analyzeJumpsToJoin` path tasks;
- number of `OwnedSet` clones in analysis;
- number of liveness graph scans;
- number of summary solver iterations;
- maximum statement count, local count, and join count per proc emission.

Migration order:

1. Add counters and record the current numbers for
   `test/cli/JsonEncodeRoundTrip.roc`, a small generated encoder, and a
   hand-written join-heavy LIR test.
2. Add the liveness table and switch `groupUsedInPath` callers to it. Keep
   the old scanners under a debug differential check for one step.
3. Add the join-summary solver, run it alongside the old
   `joinBodyOwnedSet` path in debug builds, and assert byte-identical keep
   sets for every queried join.
4. Switch rewrite emission to the summary table.
5. Delete the old analysis walkers and the migration counters, keeping only
   cheap permanent stats if useful for future profiling.

## What success looks like

- `arc.zig` no longer contains `analyzeJumpsToJoin`,
  `AnalysisSeen`, `AnalysisScopedJoinMap`, or `JoinBodyMemo`.
- `joinBodyOwnedSet` and `joinEntryOwnedSet` are gone or reduced to table
  lookups.
- `groupUsedInPath` is a bit test over a precomputed table; forward liveness
  graph scans are not on the insertion hot path.
- Post-ARC LIR is byte-identical before and after on the existing corpus,
  except for explicitly documented latent bugs discovered by the shared
  transfer-keying audit.
- `JsonEncodeRoundTrip.roc` no longer spends minutes in ARC insertion; the
  test runner's ordinary timeout is not close to being hit.
- Debug builds still certify the output. Release/dev behavior differs only in
  compile time, not in emitted ownership semantics.

## How to evaluate the result

### Correctness ideal

The summary solver is a semantics-preserving replacement for the current
analysis walkers. During migration, run old and new analyses together in
debug mode and assert:

- every queried `entry_keep`, `body_keep`, and `body_reachable` matches;
- every `groupUsedInPath` query matches the old scanner;
- emitted post-ARC LIR matches byte-for-byte on the full test corpus;
- the debug borrow certifier passes with zero new findings.

Add a module-level soundness comment to `arc.zig` or a new
`src/lir/arc_summaries.zig` explaining the summary domain, join operation,
monotonicity, and how each LIR statement transfer corresponds to the shared
transfer-decision helper.

Mutation testing: deliberately perturb the summary solver (drop one jump
edge, use union instead of intersection for body keep, omit maybe-initialized
params, ignore a switch continuation) and verify differential tests or the
borrow certifier catch the error.

### Performance ideal

ARC insertion should be near-linear in the size of one proc emission times
the number of summary iterations, not proportional to
`joins * distinct OwnedSet digests * reachable statement paths`.

Measure before and after:

- wall time for `roc test --no-cache test/cli/JsonEncodeRoundTrip.roc`;
- ARC insertion wall time and counters on generated structural encoders with
  N fields and M repeated encode sites;
- `OwnedSet` clone counts and debug-allocator stack trace samples in a debug
  compiler build;
- optimized builds with mode specialization enabled, because variants re-run
  insertion and can multiply analysis cost.

The performance target for the JSON round-trip test is seconds, not minutes.
The generated-stress suite should show roughly linear growth when adding
fields or encode sites. A regression in emitted RC quality is not an
acceptable tradeoff; if fewer scans change RC placement, the change must be
explained by a correctness bug in the old inserter or rejected.

## Tests to add

- A CLI performance regression for `test/cli/JsonEncodeRoundTrip.roc`, or a
  smaller extracted module with the same structural shape, with an enforced
  compile-time budget generous enough for CI variance but far below minutes.
- A generated structural encoder stress test: N-field records containing
  nested records, lists, dicts, nullable fields, and multi-payload tags;
  assert ARC summary counters grow linearly enough to catch reintroduced
  per-join re-walks.
- Direct `ArcTest` LIR unit tests for:
  - nested joins whose bodies are reached by multiple jumps with different
    owned sets;
  - switch continuations that merge branch exit ownership;
  - maybe-initialized join params requiring conditional ownership;
  - borrow groups with multiple aliases, proving group liveness is answered
    by the table.
- Differential debug tests that run old and new join summaries together
  during migration, deleted or converted to invariant tests once the old path
  is removed.
- A post-ARC LIR byte-identity corpus check, as used to land the
  ownership-transfer keying layer.

## Related projects

- [A Decision-Tree Match Compiler](./decision-tree-match-compiler.md)
  - references `src/lir/tag_reachability.zig`, the existing LIR monotone
  fixpoint pass this project should use as an implementation model.
