# Match-compile model rules

Judgment: for every value `v` of a `match`'s scrutinee type and every guard input `k`, the compiled match
runs the first arm, in source order, whose pattern matches `v` and whose guard is true, and binds each of
the pattern's variables to the right part of `v`. The reference is `model.py` (`choose`, `match`,
`str_match`, `eval_guard`). The compiler side is the decision-tree compiler `src/postcheck/match_tree.zig`
(`Compiler.build`, `Builder.compile`, `Emitter`), entered from `src/postcheck/boxy/match.zig` (`.boxy`
lowering, `--specialize=no`) and from `MatchTreeCtx` in `src/postcheck/solved_lir_lower.zig` (`.lss`
lowering, `--specialize=yes`), then every LIR pass and backend after it.

## Model rules

| Rule | What it says | Language and design reference | Compiler site |
|---|---|---|---|
| ARM | The first branch, in source order, whose pattern matches and whose guard is true runs. | docs/langref/pattern-matching.md "`match`"; design.md "Pattern Lowering" | `Builder.compile`, `Builder.buildLeaves` (rows are never reordered; a row that does not test the selected occurrence ends the test group) |
| GUARD | A guard runs only after its pattern matched, with the pattern's variables bound. When it is false, matching continues with the next branch. | pattern-matching.md "`if` Guards on Branches"; design.md "Pattern Lowering" ("a guard failure re-enters the residual tree for the rows below it") | `GuardNode.otherwise`, `Emitter.emitBinds` (binds before the guard) |
| ALT | A branch with alternatives behaves as one branch per alternative, in order, each with the branch's guard and body. | pattern-matching.md "Branch Alternatives"; design.md "Pattern Lowering" (or-pattern alternatives keep their own guards) | or-pattern rows in `Compiler.build`; Monotype's typed body join |
| BIND | A variable is bound to the value at its position; `p as x` binds `x` to the whole value `p` matched; `.. as r` in a list binds the elements `..` matched, in order; `..r` in a record binds a record of the fields not listed. | pattern-matching.md "Names and `_`", "Lists", "Records", "Nested Patterns and `as`"; design.md "Pattern Lowering" (record-rest binders) | `Emitter.emitExtractions`, `Emitter.emitBinds`, `Ctx.recordRest`, `Ctx.asInfo` |
| PAT-Tag | A tag pattern matches that tag with every payload pattern matching. A nominal union's tags match bare or qualified. | pattern-matching.md "Tags", "Nominal Types" | `TestKind.tag` switches, `Ctx.tagVariant`, `Ctx.ctorKey` |
| PAT-Rec | A record pattern matches when each listed field matches; `..` allows unlisted fields. | pattern-matching.md "Records" | `Ctx.recordDestruct` |
| PAT-Tup | A tuple pattern matches element-wise. | pattern-matching.md "Tuples" | `Ctx.tupleItem` |
| PAT-List | An exact list pattern matches lists of its length; a pattern with `..` matches lists at least as long as its prefix plus suffix, the prefix against the first items and the suffix against the last. | pattern-matching.md "Lists" | `TestKind.list_len`, `LenCheck`, `Ctx.listView` |
| PAT-Lit | An integer or string literal matches an equal value, at any width (`U8`, `I64` with negative and extreme values, `U64`). | pattern-matching.md "Literals" | `TestKind.int_switch`, `eq_chain`, `str_set`, `Ctx.intSwitchValue` |
| PAT-StrCapture | A string pattern must start with its first literal; each capture stops at the first occurrence of the literal after it, and the string must end where the last literal ends. A capture with nothing after it takes the rest. | pattern-matching.md "String Patterns with Captures" | `str_match` / `str_match_set` in LIR (`src/lir/LIR.zig` `strMatchStep`, `strMatchEndMatches`), each backend's lowering of them |
| PAT-Nom | A nominal's backing pattern `N.(p)`, `N.(a, b)` or `N.{ .. }` matches when the backing value matches; this holds for opaque (`::`) types in their own module too. | pattern-matching.md "Nominal Types"; types.md "Opaque Nominal Types" | `Ctx.nominalInner`, `Ctx.retype` |

## What the model needs from the generator

- Every generated match is exhaustive by the exhaustiveness model's usefulness check
  ([usefulness.py](usefulness.py), copied from the `exhaustive` model), or ends in a catch-all.
  So no value reaches a failure terminal, and every expected result is an arm.
- Alternatives bind the same names at the same types (langref "Branch Alternatives").
- Two captures in a string pattern always have literal text between them.

## Out of scope

The checker's exhaustiveness and redundancy verdicts (the `exhaustive` model checks those). A redundant
or unmatchable branch only produces warnings, and its arm is simply never expected.
