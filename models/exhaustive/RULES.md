# Exhaustiveness model rules

Judgment: for a scrutinee type `t` and an ordered list of branches (each a list of alternatives plus a
guard flag), compute whether the patterns are `exhaustive`, which rows are `redundant`, and which rows are
`unmatchable`. The reference is `model.py`. The compiler side is `src/check/exhaustive.zig`, entered from
`checkMatch` (match expressions) and `checkDestructure` (`=` statements, arguments, `for`).

## Model rules

| Rule | What it says | Design reference | Compiler site (`src/check/exhaustive.zig`) |
|---|---|---|---|
| INHAB | Inhabitedness is a greatest fixed point. A closed union is inhabited if some tag has all payloads inhabited; a record or tuple if every field is. Rigid open unions, `List`, `Str`, numbers and rigid type variables are inhabited. Recursion is coinductive: `Loop := [Loop(Loop)]` is inhabited, `Bad := [B(Bad, [])]` is not. | design.md "Potential Inhabitedness Queries" | `isTypeInhabited`, `queryInhabitedGraph`, `solveInhabitedGraph` |
| CTOR-U | The constructor family of a closed union is its inhabited tags. Rigid open unions, `Str` and number literals have infinite families. | docs/langref/pattern-matching.md "Exhaustiveness"; docs/langref/tag-unions.md | `getUnionFromType`, `areAllCtorArgTypesInhabited`, `collectCtorsSketched` |
| CTOR-List | List patterns split lengths into exact lengths `0..L-1` and one variable-length class, `L = max(longest exact + 1, longest prefix + longest suffix)`. A list whose element type is uninhabited only has length 0. | docs/langref/pattern-matching.md "Lists" | `listAritiesWith`, `buildListCtorsForChecking`, `specializeByListAritySketched` |
| U | Usefulness (Maranget 2007) over typed columns. A wildcard against a complete family is tried under every constructor; otherwise the default matrix is used. A wildcard on an uninhabited column is not useful. | | `isUsefulSketched`, `usefulEnter`, `usefulNext` |
| EXH | A match or destructure is exhaustive iff `[_]` is not useful against the unguarded rows. | docs/langref/pattern-matching.md "Exhaustiveness", "`if` Guards on Branches" | `checkExhaustiveSketched` |
| UNM | A row is unmatchable iff it is not useful against the empty matrix, that is, it matches no value. | design.md "Potential Inhabitedness Queries" (an `Err` branch on `Try(Str, Box([]))` is unmatchable) | `checkRedundancySketched` via `isSketchedPatternInhabited` |
| RED | An unguarded, matchable row is redundant iff it is not useful against the earlier unguarded rows. | docs/langref/pattern-matching.md "Exhaustiveness" | `checkRedundancySketched` |
| RED-G | A guarded row covered by earlier unguarded rows can never run. The model records it as `redundant_guarded` and does not compare it, because the compiler treats every guarded row as useful. | | `checkRedundancySketched` (`row.guard == .has_guard`), `convertMatchBranches` (guard wrapper) |
| POL | When the scrutinee is a call result, its structural unions (reached through records, tuples, `List` and `Try`) are implicitly open with a flex tail. The reference treats them as closed, since the exhaustive match closes them. A user nominal's backing and `[]` are not opened. | design.md "Polarity: Output-Position Tag Unions Are Implicitly Open", "Uncommitted Tag Tails" | `collectFlexExtVars`, the flex-extension case in `usefulEnter` |

## Reporting policy

The compiler reports some verdicts differently from the raw model output. `compare.py:normalize_expected`
applies these before comparing.

| Rule | What it says |
|---|---|
| REP-ALT | When every alternative of a branch is useless, the compiler reports the branch once, at its first alternative. |
| REP-DESTR | Single-pattern positions (`=` statements, function and lambda arguments, `for`) report no unmatchable warning. |
| REP-GUARD | An unmatchable guarded row is not reported. |

A case whose verdict matches and whose set of useless rows matches, but with different redundant and
unmatchable labels, is given status `label`, not `disagree`.

## Known-bug emulations

`model.py` can reproduce known compiler bugs through `compat` flags. They are used only to sort fuzzer
disagreements into `known-<flag>` buckets, so a run surfaces new bugs instead of the same ones again.
Remove a flag when its issue is fixed.

| Flag | Compiler behavior it emulates | Issue |
|---|---|---|
| `uninh_sibling` | A missing uninhabited sibling makes a family look incomplete, so the found constructors' payloads are never checked. | [#13289](https://github.com/roc-lang/roc/issues/13289) |
| `empty_base` | The empty matrix is non-exhaustive even when the column is uninhabited. | [#13290](https://github.com/roc-lang/roc/issues/13290) |
| `flex` | On a flex union a wildcard is always useful, and when only the flex tail is missing the found constructors' payloads are never checked. | [#13291](https://github.com/roc-lang/roc/issues/13291), [#13294](https://github.com/roc-lang/roc/issues/13294) |
| `guard_first` | With any guard present, a match is accepted as exhaustive when the first tag (in name order) has an uninhabited payload. | [#13292](https://github.com/roc-lang/roc/issues/13292) |

`known-lambda_reject` is not a flag: `compare.py:classify` gives it to an exhaustive lambda argument that
the compiler rejects with a type mismatch ([#13293](https://github.com/roc-lang/roc/issues/13293)).
