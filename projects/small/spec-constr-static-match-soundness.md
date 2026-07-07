# Sound Static-Match Verdicts and One Nominal Representation in spec_constr

## Problem

Two open issues panic at the same invariant in the call-pattern
specialization pass, for two different underlying defects:

- roc-lang/roc#9975 — `roc test --opt=speed` on a small list-indexing
  program panics `known constructor match had no matching branch`
  (`src/postcheck/monotype_lifted/spec_constr.zig:2332`, in
  `simplifyKnownMatchValue`).
- roc-lang/roc#9969 — `roc build` on a list-parser app panics at the same
  invariant through a different path.

Both programs are valid; both were verified in lldb.

**Defect 1 (9975): a two-valued verdict where three values are needed.**
`simplifyKnownMatchValue` (`spec_constr.zig:2311`) statically evaluates a
match against a symbolic `Value`. Its branch test, `bindPatToValue`
(`spec_constr.zig:2812`), returns `bool` — but the pass's `Value` union
(`spec_constr.zig:268-275`: `expr`, `tag`, `record`, `tuple`, `nominal`,
`callable`) has no list or literal payload representations, so list
patterns, string patterns, and numeric-literal patterns are **statically
undecidable** and simply `return false`
(`spec_constr.zig:2860-2868`, "List patterns are not statically bound during
specialization"). `false` means "cannot decide", but the caller treats it as
"definitely does not match": in the 9975 repro the scrutinee clones to a
`Value.tuple` whose elements are opaque `Value.expr`s, every branch pattern
is a tuple whose first element is a list pattern, all three branches "fail",
and the invariant concludes checker exhaustiveness was violated. The
depth-0 guard (`if (scrutinee == .expr) return null`, `spec_constr.zig:2312`)
only protects fully-opaque scrutinees, not opaque *components*.

**Defect 2 (9969): one fact, two IR representations, a one-directional
consumer.** Per design.md "Constructing Nominal Values", only explicit
`Type.Tag(..)` syntax canonicalizes to a nominal-wrapper expression;
unqualified tags promoted to a nominal type by unification stay **bare** tag
expressions, and Monotype lowering preserves that (bare `.tag` created at a
nominal type at `src/postcheck/monotype/lower.zig:~8339`; `.nominal` exprs
only from checked `.nominal` at `:~8347`). Checked *patterns* on nominal
scrutinees always carry the wrapper (`lower.zig:~20611`). So
"bare-tag-at-nominal-type" is a sanctioned IR state — and spec_constr's
matcher is asymmetric about it: the value-side helpers `tagFromValue` /
`recordFromValue` / `tupleFromValue` (`spec_constr.zig:3973-3995`) look
*through* `.nominal` values, but the `.nominal`-**pattern** case
(`spec_constr.zig:2853-2859`) requires the value to be syntactically
`.nominal` and returns `false` otherwise. In the 9969 repro, case-of-case
rewriting (`cloneCaseOfCaseValue`, `spec_constr.zig:2596`) pushes outer
`Try`-nominal-wrapped patterns against a bare `Value.tag` (`Err(ArgErr)`
from `extract_param_loop`) **at the same nominal type** — every branch
"fails", same panic. Empirical confirmation: qualifying the constructors as
`Try.Err(..)`/`Try.Ok(..)` in the source makes the identical program build
cleanly.

Context that elevates this beyond two point fixes: these are the **third and
fourth** shipped bugs in this one pass — issue roc-lang/roc#9717 (span
invalidation) and roc-lang/roc#9801 (slice captured across `Cloner` appends,
realloc, dangling slice) were iterate-while-mutate memory bugs in the same
file. spec_constr re-implements pattern-match semantics, inlining, and
use-counting as a second interpreter over the IR; it is the highest
invariant-density consumer in postcheck, and its match semantics are
enforced only by this panic.

## Background

The compiler pipeline: parse → canonicalize → type-check → postcheck:
Monotype IR → **Monotype Lifted** (closure lifting; plus
`spec_constr.zig` call-pattern specialization for optimized builds, added in
PR roc-lang/roc#9593) → Lambda Solved → Lambda Mono → LIR → ARC → backends.
`design.md` is authoritative; read "Constructing Nominal Values" and "Row,
Nominal, Alias, And Opaque Authority" before starting.

spec_constr specializes functions by call-pattern shape: while recording
call patterns (`recordCallPattern`, `spec_constr.zig:791`) and writing
specializations (`writeSpecialization`, `spec_constr.zig:871`), it inlines
and partially evaluates bodies over symbolic `Value`s, and
`simplifyKnownMatchValue` folds a match whose scrutinee is statically known
to a single branch. Folding is an optimization: the sound fallback for "I
can't decide this match statically" is to **leave the residual match in the
output**, which costs nothing at runtime beyond the match that was going to
execute anyway. The current code has no way to express that fallback per
branch — only per whole-scrutinee (the depth-0 `.expr` guard).

The panic at `spec_constr.zig:2332` is the correct enforcement of a real
invariant — a *known* constructor that matches *no* branch of an exhaustive
match is a checker bug — but only when every branch verdict is a definite
no-match. Today it also fires when any verdict was merely unknown.

## Evidence

All symbols verified in the current tree.

- `src/postcheck/monotype_lifted/spec_constr.zig`: `Value` union with no
  list/literal payloads (`:268-275`); `simplifyKnownMatchValue` (`:2311`)
  with depth-0 guard (`:2312`) and the invariant
  `Common.invariant("known constructor match had no matching branch")`
  (`:2332`); `bindPatToValue` (`:2812`) — `.nominal` pattern case demanding
  a syntactic `.nominal` value (`:2853-2859`), undecidable pattern set
  returning `false` (`:2860-2868`); one-directional look-through helpers
  `tagFromValue` (`:3973`) and siblings; reach paths `recordCallPattern`
  (`:791`) → inline/clone for 9975 and `writeSpecialization` (`:871`) →
  `cloneCaseOfCaseValue` (`:2596`) for 9969.
- `src/postcheck/monotype/lower.zig`: bare `.tag` construction at nominal
  types (`:~8339`), `.nominal` wrappers only from checked `.nominal`
  (`:~8347`), patterns always wrapped (`:~20611`).
- lldb specifics: 9975 — scrutinee `Value.tuple` of two `Value.expr`s,
  branch patterns tuples with list-pattern heads; 9969 — bare `Value.tag`
  (`Err`, payload `ArgErr`, same `TypeId` as the `.nominal`-wrapped
  patterns).
- Discriminating experiment: qualifying the four constructors
  (`Try.Err`/`Try.Ok`) in the 9969 repro's `Param.roc` builds cleanly —
  pinning defect 2 on the representation split, not the match logic.
- Prior same-pass bugs (different mechanism — mutation safety, both fixed):
  roc-lang/roc#9717, roc-lang/roc#9801.
- Not the decision-tree project: 9969/9975 live in this mid-pipeline
  optimizer, several stages before LIR match lowering; a decision-tree
  compiler at the LIR boundary
  ([../big/decision-tree-match-compiler.md](../big/decision-tree-match-compiler.md))
  would neither remove this static matcher nor its need for sound verdicts.

## Solution design

1. **Three-valued verdicts.** Replace the matcher's `bool` with
   `enum { match, no_match, unknown }` through `bindPatToValue` and its
   callers (`bindPatToMatchValue`, `bindPatToReusableValue`,
   `simplifyKnownMatchValue`, `cloneCaseOfCaseValue`). Semantics:
   - statically undecidable pattern forms (list, string, numeric literals)
     and any pattern probing a `Value.expr` component yield `unknown`;
   - `unknown` on any branch **aborts the fold** for that match — the
     residual match stays in the output (the optimization simply does not
     fire); guards already abort today and keep doing so;
   - the `spec_constr.zig:2332` invariant fires **only** when every branch
     is a definite `no_match` — which really is a checker-exhaustiveness
     violation, and stays debug-assert/release-unreachable per design.md.

2. **One representation for nominal values.** Kill defect 2 at the root
   rather than symmetrizing the workaround: Monotype lowering wraps every
   tag/record/tuple expression whose type is nominal in the `.nominal`
   wrapper at construction (the type is in hand at the creation site,
   `lower.zig:~8339`), making "value is `.nominal`-wrapped iff its type is
   nominal" a **total IR invariant**. Then delete spec_constr's
   one-directional look-throughs (`tagFromValue`-family `.nominal` cases)
   instead of adding a second direction, and debug-assert the invariant at
   spec_constr's `Value` construction sites. The wrapper is type-level
   structure only — no layout or runtime-representation change.
   - Scope escape hatch: if the consumer audit finds a genuine dependent of
     bare-tag-at-nominal-type that cannot be migrated in this project's
     budget, the minimal *sound* fallback is a type-directed look-through in
     the `.nominal` pattern case (mirror `tagFromValue`, keyed on the
     value's `TypeId`, both directions). That is strictly a fallback: the
     normalization is the design-conformant fix (one fact, one
     representation), and choosing the fallback graduates the normalization
     into its own follow-up.

3. **Pin the pass's match semantics with tests, not folklore.** The verdict
   enum makes "unknown ≠ no-match" a type-level distinction; add the
   assertion matrix below so the next `Value` variant or pattern form added
   to the pass must decide its verdict explicitly.

## What success looks like

- The #9969 and #9975 repros build, test, and run correctly.
- `bindPatToValue` and its callers contain no `bool` verdicts;
  grepping the pass for the verdict enum shows every pattern form mapped to
  an explicit `match`/`no_match`/`unknown`.
- Either no consumer of bare-tag-at-nominal-type exists in Monotype Lifted
  (normalization landed, look-throughs deleted), or the explicitly-chosen
  fallback is in place with the normalization filed as follow-up — not both
  paths half-done.
- The `:2332` invariant still exists and still fires on a synthetic
  all-definite-no-match corpus (it must remain the enforcement of record).

## How to evaluate the result

### Correctness ideal

- *Verdict soundness*: a fold happens only when a branch definitely matches
  and all earlier branches definitely do not. `unknown` can only make the
  output less optimized, never wrong.
- *Representation totality*: one nominal fact has one representation;
  enforced by the construction-site debug assert, not by consumer
  tolerance.
- Behavioral: `--opt=speed` output equals `--opt=dev`/interpreter output on
  the full eval corpus (the pass is optimization-only, so cross-opt
  agreement is the ground truth); snapshot corpus unchanged.

### Performance ideal

Where the fold still fires, generated code is unchanged. Where it now
aborts, the residual match was previously being folded **unsoundly** (panic
or miscompile), so no legitimate optimization is lost; confirm on the
roc-parser corpus that `--opt=speed` binary size and spec_constr pass time
stay within noise. The normalization adds one wrapper node per
nominal-typed constructor expression at compile time — no runtime cost, no
layout change; measure Monotype lowering time to confirm noise-level.

## Tests to add

Write the two regression tests first and confirm each panics on the
unmodified tree:

- `issue_9975`: the issue's `nth` program under `roc test --opt=speed`
  (`test/cli/` + `parallel_cli_runner.zig` convention; `.exit = .not_panic`,
  `not_contains` the invariant string, expected test output).
- `issue_9969`: the list-parser app under `roc build` and `roc run`,
  expected output asserted.
- The qualified-constructor control (`Try.Err`/`Try.Ok` variant of 9969)
  stays green — pins that both representations behave identically after
  normalization.
- Verdict matrix, exercised end-to-end through `--opt=speed` (list patterns,
  string patterns, numeric-literal patterns, each nested inside
  tuple/record/tag patterns over partially-symbolic scrutinees): asserts
  correct runtime output (residual match executed) and no panic.
- All-definite-no-match tripwire: a debug-build unit test constructing a
  known tag value against branches that definitely exclude it, asserting the
  invariant still fires (guards the enforcement from being softened).
- Cross-opt agreement: run the affected repros under interpreter, dev, and
  `--opt=speed`, asserting identical output (the pass's ground truth).

## Related projects

- [../big/decision-tree-match-compiler.md](../big/decision-tree-match-compiler.md)
  — orthogonal (LIR-stage match compilation); noted here because the two are
  easy to conflate. This project fixes the *optimizer's* static matcher.
- [cross-phase-coverage-parity-tests.md](./cross-phase-coverage-parity-tests.md)
  — the same producer/consumer discipline; the refutability suite is the
  natural home for the verdict matrix.
- The landed immutable specialization identity work (`SpecIdentity`,
  `SpecBuilder` in `src/postcheck/monotype/`) — spec_constr's `CallPattern`
  space is the second, separate specialization-identity space; relevant
  context for anyone extending the pass.
