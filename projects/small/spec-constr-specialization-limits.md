# Specialization Limits for spec_constr

## Problem

The call-pattern specialization pass has no spec-count, shape-depth, or fuel
limit of any kind (verified by search: no cap identifier exists in
`src/postcheck/monotype_lifted/spec_constr.zig`). GHC's SpecConstr — the
pass's cited ancestor — ships `-fspec-constr-count` and depth limits
precisely because call-pattern specialization can manufacture ever-deeper
patterns and diverge. This implementation can too, and the divergent path is
concrete:

- `createSpecializations` loops `while (wrote_spec)`
  (`spec_constr.zig:484`); writing a specialization can append new specs,
  keeping the loop alive.
- New specs are spawned *during cloning* by `ensureCallPatternForValues`
  (`spec_constr.zig:853`), which records a `Spec` for any new
  constructor-shaped pattern. Dedup is exact-structural
  (`patternEql`/`shapeEql`), so a strictly deeper shape is never deduped.
- The spawn site for a recursive self-call: when the callee is already on
  `inline_stack` (the guard that prevents infinite *inlining*,
  `spec_constr.zig:~2699`), cloning falls back to the plain path
  (`cloneExprPlain` → `cloneCallProc` → `ensureCallPatternForValues`). The
  recursion guard against infinite inlining is exactly what feeds unbounded
  *specialization*.
- Shape growth: cloning substitutes the specialized parameter with its known
  `Value`; if the body's recursive call wraps that parameter in a
  constructor, the resulting value is one constructor deeper,
  `shapeFromValue` (`spec_constr.zig:1276`, no depth cap) yields a deeper
  `Shape`, and a new distinct spec is recorded. The next round deepens
  again.

Failure scenario, expressible in ordinary Roc — a function that matches a
recursive tag-union parameter and rebuilds it one constructor deeper on the
recursive call:

```roc
Tree : [Leaf, Node(Tree, I64)]
deepen = |t, n| when t is
    Leaf -> if n == 0 then Leaf else deepen(Node(Leaf, n), n - 1)
    Node(_, _) -> if n == 0 then t else deepen(Node(t, n), n - 1)
```

`t` is deconstructed (so its argument position is specializable), and each
recursive call passes `Node(<known t>, _)` — specs for `Node(Leaf)`,
`Node(Node(Leaf))`, … are generated without bound: a compile-time infinite
loop. All shapes and values live in `pass.arena` and are freed only at pass
end, so non-termination is also unbounded memory. Consumer-style recursion
(passing sub-parts; shapes shrink or stabilize) and the intended
Stream/Iter pipelines (static, finite nesting) are safe; the risk is
specifically match-and-rebuild-deeper. The pass runs whenever
`inline_mode != .none` (`src/lir/checked_pipeline.zig:237-239`), i.e. in
every optimized build.

A secondary quality defect in the same machinery: `rewriteCallProc` /
`cloneCallProc` select the **first** matching spec in insertion order, so a
more-general pattern recorded earlier shadows a more-specific one recorded
later — correctness-neutral, but the call gets a less-specialized worker.

This project comes out of the 2026-07 comparative review of postcheck
against the cor `lss` prototype it was productionized from; spec_constr has
no cor counterpart, so it lacks the reference-implementation scrutiny the
rest of the pipeline got.

## Background

The compiler pipeline: parse → canonicalize → type-check → postcheck:
Monotype IR → **Monotype Lifted** (closure lifting; plus `spec_constr.zig`
call-pattern specialization for optimized builds) → Lambda Solved → Lambda
Mono decisions → LIR → ARC → backends. `design.md` is authoritative.

spec_constr records constructor-shaped call patterns at direct calls,
reserves worker function ids per pattern, and clones each source function
into per-pattern workers over a symbolic `Value` environment that folds
field reads and known matches and inlines known calls. Specialization is an
optimization: the sound fallback for "don't specialize this call" is to
leave the original direct call in the output. A cap therefore never costs
correctness — only optimization on pathological shapes that today do not
compile at all.

## Evidence

All symbols verified in the current tree.

- `src/postcheck/monotype_lifted/spec_constr.zig`: `createSpecializations`
  drain loop (`:482-498`), `ensureCallPatternForValues` (`:853`),
  `shapeFromValue` with no depth cap (`:1276`), `inline_stack` membership
  check feeding the plain-clone fallback (`:~2699`, append at `:2767`),
  exact-structural `patternEql`/`shapeEql` (`:~4014`), arena-lifetime shape
  storage. No `limit`/`fuel`/`cap`/`max_*` identifier in the file.
- `src/lir/checked_pipeline.zig:237-239`: pass gated only on
  `inline_mode != .none`.
- GHC precedent: SpecConstr's `-fspec-constr-count` / `-fspec-constr-recursive`
  exist for exactly this divergence class.

## Solution design

1. **Per-source-function spec budget.** A comptime constant (GHC's default
   count is 3; start near that, tune on the corpus). `ensureCallPatternForValues`
   checks the count of specs already recorded for the source function; at
   the cap it records nothing and the call lowers as the ordinary direct
   call. The `while (wrote_spec)` drain then terminates structurally:
   total specs ≤ functions × budget.

2. **Shape-depth cap in `shapeFromValue`.** Constructor nesting beyond a
   small comptime depth (e.g. 8) degrades that subtree to the opaque
   "any value" shape rather than a deeper pattern. This kills the
   deepening ratchet even within the budget, and keeps the specs that do
   get minted shallow enough to match future calls (deep specs rarely
   match anything again).

3. **Most-specific spec selection.** When several recorded specs match a
   call, prefer the most specific (deepest total shape) rather than
   first-inserted. With the budget in place this is a handful of
   comparisons per call.

4. **No silent-cap invisibility.** Count cap hits in a Debug-build counter
   surfaced by the pass's existing debug reporting, so a corpus run shows
   how often the budget binds (per the no-silent-caps discipline).

## What success looks like

- The `deepen` repro above compiles under `--opt=speed` in bounded time and
  memory, and runs correctly.
- Total specs recorded for any source function never exceeds the budget;
  the drain loop's termination no longer depends on the shape lattice being
  finite.
- On the existing corpus (examples, roc-parser suite, snapshot corpus),
  `--opt=speed` output binaries are unchanged, or changed only where the
  most-specific-selection fix picks a better worker.

## How to evaluate the result

### Correctness ideal

- A cap can only leave a residual direct call — never fold, redirect, or
  specialize incorrectly. Cross-opt agreement (interpreter vs `--opt=dev`
  vs `--opt=speed`) on the repro and the corpus is the ground truth.
- Termination is structural (budget arithmetic), not empirical.

### Performance ideal

- Compile time on the corpus unchanged within noise (the caps should not
  bind on realistic programs — confirm via the item-4 counter).
- Runtime of specialization-benefiting benchmarks (Stream/Iter pipelines)
  unchanged: the caps must be generous enough that the intended
  specializations all still fire. Use CI benchmarks.

## Tests to add

Write the divergence regression first and confirm it hangs (or trips a
timeout) on the unmodified tree:

- `spec_constr_deepen`: the match-and-rebuild-deeper program under
  `roc build --opt=speed` with the CLI test harness timeout
  (`src/cli/test/parallel_cli_runner.zig` convention), asserting successful
  build and correct run output.
- A consumer-recursion control (list/tree fold that only destructures):
  must still specialize (assert via the debug counter or emitted-spec
  count) — pins that the caps don't lobotomize the pass.
- Most-specific selection: a function called with both `Cons(1, Nil)`-shaped
  and `Cons(x, xs)`-shaped patterns; assert the deeper-shaped call binds to
  the deeper spec (unit-level test over the pass, or output-shape
  assertion).
- Budget tripwire: unit test constructing more distinct call patterns than
  the budget for one function; asserts spec count == budget and all calls
  still lower (residual direct calls).

## Related projects

- [spec-constr-static-match-soundness.md](./spec-constr-static-match-soundness.md)
  — the same pass's match-verdict soundness; both projects touch
  `bindPatToValue`-adjacent code and can share test scaffolding. Land in
  either order.
- [store-generation-counters.md](./store-generation-counters.md) — the
  landed GuardedList work that already hardened this pass's
  iterate-while-mutate hazards; this project addresses its termination
  hazard.
