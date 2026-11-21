# Rules

The model has two parts, and this file has one section for each.

1. **Rows and evidence** (`Types.lean`, `Check.lean`, `Replay.lean`): a small checker with open, closed and bounded tag rows,
   `?` error-row composition and static dispatch, plus a replay of Monotype instantiation over the checker's output.
2. **Lifted IR typing** (`Lifted.lean`): an independent typing check over the compiler's Lifted IR export
   ([FORMAT.md](FORMAT.md)).

Each rule names the Lean definition that implements it, the design.md section that states it, and the compiler function
that enforces it. The compiler sites were read at upstream commit `63434e9847` and are given as function names. Where the
model abstracts a rule, the abstraction is stated.

File abbreviations: `Check` = `src/check/Check.zig`, `unify` = `src/check/unify.zig`, `types` = `src/types/types.zig`,
`CA` = `src/check/checked_artifact.zig`, `SD` = `src/check/static_dispatch_registry.zig`, `DE` =
`src/check/dispatch_evidence.zig`, `solve` = `src/postcheck/monotype/solve.zig`, `lower` = `src/postcheck/monotype/lower.zig`.

## Part 1: rows and evidence

### Checker rules (`Types.lean`, `Check.lean`)

| Rule | What it says | Lean | design.md | Compiler site | Notes |
|---|---|---|---|---|---|
| R-TagOpen | a tag expression mints a fresh flex extension | `infer` `.tag` | Polarity: Output-Position Tag Unions Are Implicitly Open | Check `resumeTagCheck` | The nominal lift (`relateOwnedTagToExpectedNominal`) happens later, in R-Lift. |
| R-MatchClose | an exhaustive tag match closes the scrutinee row with an uncommitted `[]` | `infer` `.match_` | Uncommitted Tag Tails | Check `freshUncommittedTagTail`, `stepPatternCheck`, `closeExhaustiveVars` | Pattern-only tags (`rowLabelPatternOnly`, `omitRowLabels`) are not modeled; that is the area of [#13236](https://github.com/roc-lang/roc/issues/13236). |
| R-TagRows | the tag-row unification cases: same labels, a extends b, b extends a, both extend with one fresh tail | `unifyTagRows` | Row Joins | unify `unifyTwoTagUnions` | |
| R-ClosedFailFast | a closed side cannot gain the other side's labels | `unifyTagRows` | | unify `unifyTwoTagUnions`; solve `rowAdditionConflicts`, `unifyTagRows` | In `Mode.checker` this is a user type mismatch. In `Mode.mono` it is `UErr.widen`, the compiler-bug panic "instantiation widened a closed tag union". |
| R-RowEmpty | a non-empty row against `[]` | `unifyRowWithEmpty` | | unify `unifyFlatType`; solve `unifyRowWithEmpty` | `UErr.nonEmptyVsEmpty` in mono mode. |
| R-Records | record rows have no closed fail-fast; extra fields flow into the other tail | `unifyRecords` | | unify `unifyTwoRecords` | Uncommitted record tails and field kinds are not modeled. |
| R-Bounded | an annotation-minted tail may close but never gains a tag | `bindVar` (checker mode only) | Polarity: Output-Position Tag Unions Are Implicitly Open | unify `refuseTagsIntoBoundedExt` | |
| R-InstDropsBound | instantiation does not copy the bound | `copyGeneric` | Polarity: Output-Position Tag Unions Are Implicitly Open | types `Descriptor.bounded_row_ext` | |
| R-Polarity | annotation rows are open (bounded) in output position and closed in input position | `annTy` | Polarity: Output-Position Tag Unions Are Implicitly Open | types (annotation polarity) | `Try(ok, _)` (`annMode` 1) gives a fresh flex; `[.., ..]` (`annMode` 2) gives a rigid tail. |
| R-Lift | an anonymous row lifts into a nominal when liftable, and the nominal wins | `liftInto`, `tagRowLiftable` | Declared Backing Structure | unify `unifyTagUnionWithNominal`, `tagRowLiftable` | Monotype applies no liftability gate (solve `unifyThroughBacking`). |
| R-TryProject | a closed `?` error row with tags is projected: its heads are copied onto a fresh tail | `composeErr` | Try Return-Row Composition | Check `projectTryReturnRow`, `processReturnConstraints` | Recorded as a `Rel` (`tryProject`). |
| R-TryWhole | an open `?` error row with tags is unified whole | `composeErr` | Try Return-Row Composition | Check `processReturnConstraints` | |
| R-TryTail | a tagless error contribution is related to the composed row's tail, after all heads | `composeErr` | Try Return-Row Composition | Check `processReturnConstraints` | Recorded as a `Rel` (`constructionPayload`): the relation #12625 got wrong. |
| R-TryCompose | the composed row C is the annotated `Try`, else a fresh `Try(body.ok, fresh_err)`; the body is a contribution too | `inferLam` | Try Return-Row Composition | Check `processReturnConstraints` | |
| R-Coerce | an annotated function body relates to its result through a copy that re-opens the closed rows in the coercion's reach | `widenCopy`, `inferLam` | Row Subsumption | Check `widenedClosedRowsCopy` | Deferred result relations are not modeled. Recorded as a `Rel` (`resultCoerce`). |
| R-Gen | generalization is rank-based, with the value restriction for non-lambda definitions | `checkGroup`, `generalize`, `pinLevel` | | `src/types/generalize.zig` | Binding groups are found by forward references only (`groups`). |
| R-ValueRowClose | open tag tails of top-level values are closed at module end | `closeValueRows` | Value Rows: Uses Share One Row | Check `closeTopLevelValueRows` | Only tails with no requirements are closed; the compiler's "dischargeable" test is approximated. |
| R-WhereReq | an unresolved dispatcher carries the method requirement | `infer` `.meth` | Static Dispatch At The Checked Boundary | types `StaticDispatchConstraint` | `.open_row_value` receivers are not modeled separately. |

### Publication rules (`Replay.lean` `classifyAt`, `evidence`)

| Rule | What it says | Lean | design.md | Compiler site | Notes |
|---|---|---|---|---|---|
| E-Direct | a nominal or primitive dispatcher that has the method resolves directly | `classifyAt` | Static Dispatch In Monotype | CA `resolutionForMethodTarget` | |
| E-Forward | a quantified dispatcher carrying the requirement forwards (`evidence_dependent`) | `classifyAt` | Static Dispatch At The Checked Boundary | CA `resolveVarObligation`, `chainParamIndex` | |
| E-Unpinned | a still-flex dispatcher that nothing pins is `unreachable` (eq and hash would be structural) | `classifyAt` | Static Dispatch In Monotype; Literal Arguments | CA `resolveVarObligation`; SD `unpinnedDispatchResolution` | Eq and hash are not generated, so the structural branch is never taken. |
| E-Order | one entry per (quantified variable, method), in instantiation order | `evidence` | Static Dispatch At The Checked Boundary | DE `enumerateEvidenceParamsWithRequirements`, `emitConstraints` | The compiler's DFS order is approximated by `copyGeneric` pair order. |

### Monotype replay rules (`Replay.lean`)

| Rule | What it says | Lean | design.md | Compiler site | Notes |
|---|---|---|---|---|---|
| M-Copy | each request gets a fresh graph holding a copy of the template's checked type; an uncommitted `[]` publishes as plain `[]` | `copyAll` | Monotype Instantiation | lower `stepInstNode`; `src/check/sealed_type_view.zig` | |
| M-ClosedResultWiden | only a template's direct result row or `Try` error row, when closed, widens to the request | `closedResultRowTail`, `Cfg.boundaryWiden` | Closed Result Rows At Template Boundaries | lower `checkedResultRowIsClosed`, `widenClosedResultRowToRequest` | |
| M-RowWiden | a checker subsumption relation (`Rel`) is lowered as `row_widen`, never as an exact unification | `replayOne` | Row Coercion Primitive | lower (`row_widen`) | Checked as monotonicity: after sealing, every label of the source is in the destination. |
| M-SealDefault | every unresolved variable seals to `[]` (`{}` in a record tail) | `sealTy` | Polarity: Output-Position Tag Unions Are Implicitly Open | solve `materializeUnresolved` | |
| M-Request | every call edge in a request yields a new request at its sealed instance type | `replayProgram` | Monotype Specialization | lower (request scheduling) | Requests are keyed by template and printed type. |

### Codec contracts (`Replay.lean`)

| Rule | What it says | Lean | Compiler site | Notes |
|---|---|---|---|---|
| D-Req | the format methods a derived encoder for shape T calls | `codecReq` | Check `encodeFormatMethodName`, `validateDerivedEncodeRecordMethods` | Encoder side only. The parser methods are symmetric and are not modeled. |
| D-Recorded | the calls the checker records for T | `codecRecorded` | Check `recordGeneratedCodecCall`, `stepDerivedParse`, `stepDerivedParseNominal` | Matches `codecReq` under the current rules. Its `preFix12229` flag drops the list children of a tag payload inside a record. |

### The checks

`rows-evidence model SEED COUNT` runs all four on each generated program; `rows-evidence calibrate` runs them on known bugs.

- **(a) Instantiation monotonicity.** `Viol.widen`: `unify .mono` of a template against a request fails. `Viol.notSubsumed`:
  a widening relation loses a label after sealing.
- **(b) Evidence completeness and functionality.** `Viol.missingEvidence`: a dispatch site or evidence entry resolves to no
  method at the sealed type. `Viol.duplicateEvidence`: two entries for one requirement.
- **(c) Unreachable-dispatch soundness.** `Viol.unreachableInhabited`: an entry published `unreachable` is requested at an
  inhabited sealed type. "Requested" over-approximates "reached".
- **(d) Codec contract completeness.** `codecCheck`: `calls(K) = Req(T, F)` as multisets, over every shape up to depth 3.

These are executable decision procedures. No theorems are proved yet.

### Calibration (`Calibrate.lean`)

Each `Cfg` toggle restores a rule as the fixing PR describes it before the fix. The calibration requires the current rules
to pass and the toggle to make the matching check fail:

| Bug | Current rules | Pre-fix rule |
|---|---|---|
| [#12625](https://github.com/roc-lang/roc/issues/12625) (`Err(tag)` after `?`) | replay clean | (a): "widened a closed tag union with Early", the issue's message |
| [#12552](https://github.com/roc-lang/roc/issues/12552) (point-free alias of a where-constrained function) | replay clean | (c): `to_str` published `unreachable`, reached at `Nm` |
| [#12229](https://github.com/roc-lang/roc/issues/12229) (list payload of a tag inside a record) | (d) holds up to depth 3 | (d) fails on 8 shapes, for example `[A({ f : List(U64) }), B]` |

It also requires the model to accept, with a clean replay, the programs of
[#13236](https://github.com/roc-lang/roc/issues/13236) and [#13237](https://github.com/roc-lang/roc/issues/13237), which
design.md accepts and the compiler rejected. Calibration shows the checks detect those rule failures. It does not show that
the model would have predicted them unaided.

### Divergences

**Between design.md and the compiler** (as read at `63434e9847`):

1. Static Dispatch At The Checked Boundary lists five evidence kinds. The compiler also has `from_callable` and
   `from_scheme` (SD `Resolution`). The model has neither.
2. design.md says an unpinned copy resolves against its default owner. In the compiler only numeric-literal default phases
   at root edges pick an owner (CA `rootDefaultObligation`). Every other unpinned copy is `unreachable`, or structural for eq
   and hash (CA `resolveVarObligation`). The model follows the compiler (E-Unpinned).
3. Static Dispatch In Monotype defines `unreachable` as a constrained variable no specialization edge can ever supply. The
   compiler also applies it to records and tag unions whose row tail is open (CA `openRowOfStructure`,
   `resolveObligationStep`), and turns eq and hash into `structural` instead. The model follows the compiler for flex
   dispatchers only.
4. Closed Result Rows At Template Boundaries says the hole in `Try(Str, _)` generalizes. The compiler closed it to `[]` when
   the body has two or more result branches ([#13237](https://github.com/roc-lang/roc/issues/13237), since fixed). The model
   follows design.md (`annMode` 1 gives a generalizable flex).
5. Row Subsumption says the coercion's reach covers `Try`'s ok row and nested tag payloads. Closed Result Rows At Template
   Boundaries says only the direct result row and the `Try` error row widen at a template boundary, and that a `Try`'s ok row
   and every nested row relate exactly. `row_widen` (M-RowWiden) reconciles the two. Hand-written programs with nested-reach
   coercions compile and run correctly.

**Between the checker and Monotype**, from reading the compiler. The model encodes items 1 and 4.

1. Local inferred rows stay open in the checker. Monotype seals them to `[]` (solve `materializeUnresolved`) and treats such
   a tail as closed and widenable at template boundaries (lower `checkedResultRowIsClosed`).
2. In the both-extend case the checker's fresh tail is a plain flex (unify `unifyTwoTagUnions`). Monotype's defaults to `[]`
   (solve `joinRowRests`).
3. Bounded tails are not read anywhere in Monotype (`bounded_row_ext` does not appear in lower.zig). Only the checker
   enforces "never gains a tag".
4. Duplicate labels: the checker relates their payloads (unify `relateChainDuplicateTags`). Monotype's `flattenTagRow` keeps
   the first occurrence and does not relate them.
5. Same nominal: Monotype also unifies the backings (solve `unifyConcrete`); the checker compares arguments only (unify
   `unifyNominalType`).

### What part 1 leaves out

- Numeric literal settling. Every generated literal carries `.U64`, because an unpinned literal defaults to `Dec`.
- Field kinds, optional record fields, tuples, opaque nominals, imports and modules, effects and `var`.
- Hosted rows, platform `main!` rows and builtins. This is why
  [#12879](https://github.com/roc-lang/roc/issues/12879) (`Str.split_last` plus `map_ok` plus an open `main!` error row) is
  not reproduced.
- Request-determined result rows in recursive groups ([#12585](https://github.com/roc-lang/roc/issues/12585),
  [#12617](https://github.com/roc-lang/roc/issues/12617)).
- Pattern-only tag bookkeeping and the unmatchable-pattern pass (the area of
  [#13236](https://github.com/roc-lang/roc/issues/13236)).
- Runtime representation: layouts, storage of zero-sized payloads. This is why
  [#13235](https://github.com/roc-lang/roc/issues/13235) is outside the model; the fuzzer found it through `roc test`.
- Lacks constraints. `[Zero, ..e]` does not require `e` to lack `Zero`, so the model accepts some programs the compiler
  correctly rejects.
- An anonymous `..` tail in an annotation becomes a rigid, which is stricter than the compiler.

## Part 2: Lifted IR typing (`Lifted.lean`)

The rules are written from design.md and the Lifted AST (`src/postcheck/monotype/ast.zig`). The compiler has no typing
checker for Lifted IR, so this one is the reference; it reads the IR through `ROC_LIFTED_IR_EXPORT` (FORMAT.md).

### Relations

- **Equality** (`eqT`): structural and coinductive, by label text. Aliases compare through their backings; nominals do not
  (Monotype `type.zig` `typeViewEql`). The export drops the fields that an iterator-tier relation or a per-use lift
  through an inspectable backing would need, so the model does not relate those; see FORMAT.md.
- **Row join** (`rowSub`): the value's tag rows are subsets of the target's at every depth (design.md "Row Joins"; the
  Lifted `row_widen` expression).
- **Divergence** (`diverges`): expressions that transfer control (`ast.zig` `transfersControl`), and blocks ending in
  `unreachable`. The value rules do not apply to them.

### Rules

| Rule | What it checks | design.md or compiler site |
|---|---|---|
| WF-Type, WF-Local, WF-Node, WF-Fn | every referenced type, local, node and function exists | the exporter (model only) |
| T-Local, T-List, T-Tuple, T-Record, T-RecordUpdate, T-Tag, T-Nominal, T-Let, S-Let | each child equals the slot it fills | Lambda Solved IR |
| T-Literal, P-Literal | a numeric literal is at a numeric primitive; `int_lit` may also be at `Bool` | lower `BodyContext.boolLiteral` |
| T-Cond, T-StructuralEq | conditions, guards and `structural_eq` results are `Bool` | |
| T-CallProc | a direct call that is not demand-shaped: arguments equal the parameters, the result equals the callee's result | Lambda Solved IR |
| T-CallShaped | a demand-shaped call: each callee parameter and the result is the call's type, a hole, or its layout representative (record to tuple in row order, nominal to backing, componentwise) | Shaped requests |
| T-CallValue, T-FnRef, T-Capture | callee function type, function reference type, capture slot types | |
| T-FieldAccess, T-TupleAccess | the accessed field or item exists and has the expression's type | |
| T-Match, T-If, T-Block, T-Join, T-Loop, T-JumpLoop, T-Try, T-InitializedPayload, T-StaticData, T-TypedBoundary | branch, final and loop values have the expression's type | |
| T-RowWiden | a `row_widen` operand row-joins into the result | Row Coercion Primitive |
| T-Return, F-Body | a returned or final value row-joins into the function's result; a `return_` target is the enclosing result | Lambda Solved IR |
| L-TypedLocal | typed locals match their locals | |
| P-Bind, P-As, P-Record, P-Tuple, P-List, P-Tag, P-Nominal | each sub-pattern has the type its position requires | |
| C-Closed | a demand hole appears only as a `List` or `Box` element | Holes through the pipeline |
| C-Scope | every local a body reads is bound in that function (ignoring order) | |

### Not checked

Rules the model does not implement: T-Unit, T-Break, T-Continue, T-Jump, T-StructuralHash, T-Dbg, T-Message, P-Str,
and the pre-lift forms (T-Lambda, T-DefRef, T-FnDef, C-Lifted). Also not checked: low-level operation typing, string pattern
steps, and the order of bindings within a body.

### Sensitivity

`./run.sh mutate` changes one id per mutant in real exports (an expression's or pattern's type, a `local` reference, or a
direct callee) and counts how many the checker rejects. Over 400 mutants of the exports of 1,100 generated programs it
caught 395. Over 400 mutants of the exports of `test/cli/BoxyChainedTagRow.roc`, `test/cli/BoxyStoredValues.roc` and one
generated program it caught 386; 8 of the 14 misses were callee swaps in `BoxyChainedTagRow`, whose functions share one
signature. The kinds of miss:

- an `int_lit` retyped from `u64` to `Bool` in an unchecked position (a low-level argument);
- a block retyped inside an unchecked position;
- a callee swapped for one with the same signature;
- a local swapped for another in-scope local of the same type.

`tests/all-kinds.lifted` is a hand-written export that uses every record kind; it must pass.
`tests/call-result-is-backing.lifted` is the shape of [#13264](https://github.com/roc-lang/roc/issues/13264): a direct call
typed at a nominal's backing while the callee returns the nominal. It must fail T-CallProc.
