# A Decision-Tree Match Compiler

## Problem

Match compilation in LIR lowering is a per-branch backtracking chain, not a
decision tree. Each branch of a `match` is lowered as an independent test
chain that jumps to a per-branch "miss" join point on failure, falling through
to the next branch's chain. Concretely:

- `lowerBranchChain` (src/postcheck/solved_lir_lower.zig, duplicated in
  src/postcheck/lir_lower.zig) iterates branches in reverse, wrapping each in
  its own miss join (`patternCanMiss` decides whether one is needed).
- `lowerTagPatternThen` calls `discriminantSwitch`, which emits a ONE-ARMED
  `switch_stmt` per branch: it allocates a fresh `.u16` local, re-reads the
  scrutinee's discriminant into it via `assign_ref` with a `.discriminant`
  op, and switches on that single value with everything else going to the
  miss default. An N-tag match therefore becomes N chained one-armed
  switches, each re-reading the same discriminant.
- String patterns could not tolerate this shape at all: `lowerBranchChain`
  contains a hand-written special case (`strPatternBranchGroupStart` /
  `lowerStrPatternBranchGroup`) that scans for runs of adjacent string
  patterns and emits a single `str_match_set` statement for the group.

The costs are concrete. The dev backends (x86-64, aarch64) ship these naive
chains verbatim — no later pass cleans them up. The LLVM backend relies on
LLVM to rediscover the jump table that a decision-tree compiler would have
emitted directly. And an entire compensating optimization pass exists
largely because of branches a decision tree would never emit (see Evidence:
PR 9726's tag-reachability pass).

There is also a residual correctness hazard from the last round of fixes:
the invariant "Monotype expression ids referenced from multiple positions
get DUPLICATED downstream; sharing must go through join points" is
undocumented and unenforced. PR 9707 removed the one known violator (the
list-pattern desugarer), but nothing prevents a recurrence.

## Background

The compiler pipeline: parse → canonicalize → type-check → postcheck:
Monotype IR → Monotype Lifted → Lambda Solved → Lambda Mono → LIR lowering →
ARC insertion → backends. design.md at the repo root is authoritative.

There are TWO LIR lowerers:

- src/postcheck/solved_lir_lower.zig (~7.7k lines) — production. Lowers
  Lambda Solved IR directly to LIR, keeping Lambda Mono decisions without
  materializing Lambda Mono trees in release builds.
- src/postcheck/lir_lower.zig (~4.2k lines) — a parallel Lambda-Mono-based
  lowerer used by debug verification and test harnesses.

Several fixes have had to land twice, once in each, and the copies drift.

FIVE executors run Roc code: the interpreter (src/eval/interpreter.zig), the
dev backends for x86-64 and aarch64 (src/backend/dev/), the LLVM backend
(src/backend/llvm/MonoLlvmCodeGen.zig), and the wasm backend
(src/backend/wasm/). Engine divergence — `roc test` and `roc run` giving
different answers — has been the de facto bug detector.

Two pipeline facts matter for match compilation:

1. Monotype is a DAG: an expression id may be referenced from multiple
   positions, and LIR lowering re-lowers each reference (tree semantics).
   Sharing a continuation in LIR must go through LIR join points (`join` /
   `jump` control-flow statements), which is exactly what the per-branch
   miss joins do today.
2. Exhaustiveness is decided by the checker. `closeExhaustiveVars`
   (src/check/Check.zig) commits exhaustiveness verdicts into the type store
   (closing extension and payload vars). Per design.md, lowering must
   consume those verdicts, not re-derive them; the chain's terminal
   statement is `runtime_error` or `comptime_exhaustiveness_failed`.

## Evidence

- `lowerBranchChain`: src/postcheck/solved_lir_lower.zig and
  src/postcheck/lir_lower.zig (near-duplicate copies).
- `lowerTagPatternThen` → `discriminantSwitch`
  (src/postcheck/solved_lir_lower.zig): fresh `.u16` local + `assign_ref`
  discriminant read + one-armed `switch_stmt` per branch.
- String special case: `strPatternBranchGroupStart`,
  `lowerStrPatternBranchGroup`, `directStrPattern`, emitting
  `str_match_set` — inside `lowerBranchChain` in both lowerers.
- PR roc-lang/roc#9478: patched holes in the out-of-band list-pattern
  desugarer (nested list patterns panicked; the desugarer expanded to
  expression-level if-cascades).
- PR roc-lang/roc#9707: DELETED that desugarer after discovering exponential
  blowup. Monotype desugared list patterns to trees whose miss arm
  referenced the shared rest-of-match by expression id (a DAG in Monotype);
  LIR lowering re-lowers each reference, giving ~(elements+1)^branches
  statements — measured 1136 → 4272 → 16816 LIR statements for 3 → 4 → 5
  branches. 9707 made list patterns first-class (`PatData.list` /
  `ListPattern` in src/postcheck/monotype/ast.zig, re-exported by
  monotype_lifted/ast.zig, mirrored in lambda_mono/ast.zig) through four IR
  stages, lowered via the shared per-branch miss join (linear).
- PR roc-lang/roc#9726: added src/lir/tag_reachability.zig (~1.1k lines) —
  a whole-program fixpoint over all proc specs that deletes switch edges on
  tag discriminants no construction site can produce. It runs only at
  `--opt=size`/`--opt=speed` (`tagReachabilityForOpt` in src/cli/main.zig;
  invoked via the `tag_reachability` flag in src/lir/checked_pipeline.zig).
  Much of what it cleans up is branches a decision-tree compiler would
  never emit.
- PR roc-lang/roc#9849: derived equality/patterns on nominal records must
  decompose through the NOMINAL operand — nominal layout ≠ backing layout
  since declared field order. Pattern compilation must unwrap nominals the
  same way (`lowerNominalPatternThen` today).
- `closeExhaustiveVars`: src/check/Check.zig; exhaustiveness analysis in
  src/check/exhaustive.zig.

## Solution design

Implement a Maranget-style decision-tree match compiler (Maranget 2008,
"Compiling Pattern Matching to Good Decision Trees") at ONE stage: the
LIR-lowering boundary, as a single shared module consumed by both lowerers.

Data structures:

1. **Pattern matrix**: rows = branches (pattern vector + optional guard +
   body id), columns = occurrences (paths into the scrutinee: root, tag
   payload i, field i, tuple item i, list element i / rest). Built from the
   input IR's `PatData` via a thin accessor interface so the same module
   works over Lambda Solved (`Lifted.Branch`) and Lambda Mono
   (`LambdaMono.Branch`) inputs.
2. **Occurrence table**: each occurrence lowers to at most one LIR local,
   computed once (one discriminant read per tested position, one field read
   per destructured position).
3. **Tree nodes**: `Switch(occurrence, arms, default)`, `Leaf(branch)`,
   `Guard(branch, then-leaf, else-subtree)`, `Fail(verdict)`. Arms are
   multiway: tag discriminants, int/dec literals, string arms (lowered to
   `str_match_set`), list-length buckets (exact lengths, plus a `>= k` arm
   when any row has a rest pattern).

Algorithm:

- Column selection by necessity-based heuristics (Maranget's `f`, `d`,
  `b` combination is the reference; pick by measuring on the test corpus).
- Specialization/defaulting of the matrix per constructor, standard
  Maranget rules; records/tuples destructure without a test; nominal
  patterns unwrap through the nominal operand per the 9849 rules before
  testing the backing value.
- **Join points for shared continuations**: each branch body is lowered
  exactly once and bound to a LIR `join` whose params carry the branch's
  pattern bindings; leaves `jump` to it with the bound locals. Guards lower
  as a conditional between the body join and a jump to the residual
  subtree (guard fallthrough re-enters the tree, never re-tests columns
  already known).
- **Default arms** consume the checker's committed exhaustiveness verdicts:
  if the checker closed the match (via `closeExhaustiveVars`), the tree has
  no `Fail` node and the last arm becomes the switch default; otherwise
  `Fail` lowers to `runtime_error` / `comptime_exhaustiveness_failed`
  exactly as today. Lowering must never re-derive exhaustiveness.
- Binding order and guard side-effect order must match the current chain
  semantics (source order of branches; guard evaluated only after its
  branch's pattern fully matches).

Document the sharing invariant explicitly (in design.md and in the module
doc comment): Monotype expression ids referenced from multiple positions
are duplicated downstream; sharing must go through join points. Add a
debug-build statement-count lint: after lowering a match, assert the
statement count is O(total pattern size) — a hard multiplier bound, using
PR 9707's counting methodology — so exponential regressions fail loudly.

Migration order:

1. Extract the shared module (e.g. src/postcheck/match_tree.zig) with the
   accessor interface; coordinate with ../big/unify-build-pipelines.md — if
   that lands first, only one call site exists.
2. Implement tree construction + LIR emission with statement-count tests.
3. Switch solved_lir_lower.zig to it behind a flag; differential-test old
   vs new across all five executors on a generated pattern corpus.
4. Switch lir_lower.zig; delete the flag.
5. DELETE: the per-branch tag path of `lowerBranchChain` (chained one-armed
   `discriminantSwitch` calls and per-branch discriminant re-reads), and
   the string-grouping special case (`strPatternBranchGroupStart`,
   `lowerStrPatternBranchGroup`) — strings become ordinary tree arms.
6. Re-measure src/lir/tag_reachability.zig. Keep the pass (it may still
   catch cross-function narrowing), but quantify its remaining benefit; if
   intra-function wins were most of it, consider disabling it at some opt
   levels.

## What success looks like

- An N-branch match on tags of one scrutinee lowers to exactly ONE
  `switch_stmt` with N arms (plus a checker-sanctioned default), with ONE
  discriminant read.
- PR 9707's measured counts (1136/4272/16816 for 3/4/5 branches) become
  linear and are asserted by tests.
- `strPatternBranchGroupStart` / `lowerStrPatternBranchGroup` no longer
  exist; `str_match_set` is emitted by the tree like any other arm kind.
- Both LIR pipelines call one match-compilation module; no duplicated
  match-lowering logic remains.
- The sharing invariant is documented and the debug statement-count lint is
  active.

## How to evaluate the result

### Correctness ideal

- Semantics identical to the chain, including guard side-effect order and
  binding order.
- Differential testing across all five executors (interpreter, dev x86-64,
  dev aarch64, LLVM, wasm) on a generated pattern corpus: random nesting of
  tags, records, tuples, lists (with rests), strings, literals, guards.
- Exhaustive small-universe enumeration: for small types, run all patterns
  × all values and compare against the interpreter.
- The ARC certifier (src/lir/arc_certify.zig) passes: join-point
  restructuring changes lifetimes — verify RC schedules stay balanced.

### Performance ideal

- Generated-code benchmarks on match-heavy code, dev backend especially (it
  gets no LLVM cleanup): expect large wins from one switch replacing N
  chained tests.
- LIR statement counts asserted for representative matches.
- tag_reachability time re-measured; the whole-program fixpoint may shrink
  or become removable at some opt levels.
- Compile time must not regress: decision-tree construction is near-linear
  for typical matches; track LIR-lowering time on match-heavy modules.

## Tests to add

- Statement-count regression tests using PR 9707's methodology: assert
  linear growth for the 3/4/5-branch list-match family and for N-branch
  tag matches.
- Differential engine tests over a generated pattern corpus (all five
  executors agree on result and on crash/exhaustiveness diagnostics).
- Guard-order tests: guards with observable side effects (dbg/expect) fire
  in source branch order, and only after their pattern matches.
- Binding-order tests: `as` and nested bindings visible to guards and
  bodies with correct values.
- Exhaustiveness-default-arm tests: matches where the checker committed
  impossibility (closed ext var) lower with no `runtime_error` node; open
  matches still produce one.
- Nominal-record pattern tests per PR 9849 (declared field order differs
  from backing layout order).

## Related projects

- [Unify the two build pipelines](../big/unify-build-pipelines.md) — removes
  the second copy of match lowering; this project's shared module is the
  fallback if that lands later.
- [Checked arithmetic ops in LIR](../small/checked-arithmetic-lir-ops.md) —
  same duplicated-lowerer failure mode; its cross-engine conformance suite
  is the harness this project's differential tests should extend.
