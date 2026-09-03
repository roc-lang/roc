# Polarity, phase two: making lowering total for implicitly open rows

This plan covers the lowering-side work that the polarity checker change
(design.md "Polarity: Output-Position Tag Unions Are Implicitly Open") left
open. The checker semantics are settled and are not changed here: an
extensionless tag union in an output position of an annotation is
implicitly open (a fresh flex extension, instantiated fresh at every use),
the annotation bounds its own definition (`Tag Not In Annotation`), and a
where-method signature is instantiated per body use and closed per
obligation. Phase two makes postcheck accept every program the checker
accepts, with no panic reachable from a type-correct program.

The bar is production quality, not a prototype. Every behavior change below
is stated as a rule, is declared in design.md (Rewrite Inventory or the
architecture text it amends), and is pinned by tests at each level it
touches: checker integration tests, Monotype/LIR tests, and CLI or platform
fixtures that run the built compiler. Each work item is one jj change,
described before the work starts, with no debug prints and no undeclared
solver mutation.

## 1. Where things stand

Stack on `main` (`96c3b2fa`), bottom to top:

| Change | Title | Scope |
|---|---|---|
| `kusqzzsn` | Refill `any_negative` in the ARC certifier's recycled state | One line in `src/lir/arc_certify.zig`; an upstream merge artifact that stops every `lir`-dependent build on this base. Not polarity. Dropped at the next rebase once `main` carries the fix. |
| `kupupkyt` | Open output-position tag unions implicitly (polarity) | Checker, types, display, tests, design.md |
| `wyzrkrmn` | Drop redundant `..` from output positions in Builtin and fixtures | Mechanical, snapshots |
| `wlylqvxq` | Instantiate where-method signatures per body use | Checker, instantiator |
| `rzysvtry` | Suggest the listed tag a Tag Not In Annotation typo resembles | Report hint (`findBestTypoSuggestion` reused; listed tags captured at mint time, including through alias markers) |
| `rprvoylp` | Close implicitly open tag rows before structural derivation | `closeTagRowsForDerivation` at all seven derivation sites, `RedirectRule.derivation_marker_ext_closure`, design.md Rewrite Inventory entries, six pinning tests |

Verified state at the top of the stack: checker integration suite green
(601/601); Parser CLI suite green (59/59); full `run-test-zig` was
5004/5014 before the last two commits, and those two commits change no
solver behavior outside derivation. The remaining failures are all in
postcheck and are the subject of this plan:

| Symptom | Where | Item |
|---|---|---|
| `resolved Monotype view requested for an unresolved instantiation node` building `test/http-headers/app.roc`, `test/json-decoder/camel_app.roc`, `test/json-decoder/camel_direct_app.roc`, and `roc test` on `test/cli/ParserTopLevelStored*.roc` and `issue_10888_json_parse_repeated_nested_field.roc` | `lower.zig` `resolvedPreparedCodecCallsForBoundary` | W2 |
| `lir_inline_test` "nested iterator results retain the callee-authored representation", case `closed direct Try method` | `checked_artifact.zig` plan classification | W3 |
| `lir_inline_test` "issue 10121 shared JSON helpers preserve optional nested round trips": checker rejects with four `missing method` (encoder) errors | `Check.zig` encoder derivation | W4 |
| Same test after W4: `instantiation widened a closed tag union` during a Builtin lambda specialization | `solve.zig` `unifyTagRows` via `selectExprRepresentationAtNode` | W5 |
| A where-method use that widens its copy panics when the implementation's own return row is closed (`instantiation widened a closed tag union` in `instantiateTargetFromPlanNode`) | `lower.zig` dispatch lowering | W6 |

## 2. Work items

Each item states the decision, the rule as it will be written in
design.md, the code touch points, the tests, the verification, and the
risks. Items are ordered so that every commit compiles and its own tests
pass on top of the previous one.

### W1. Build fix on this base (landed: `kusqzzsn`)

The one-line `refilled_fields` addition sits as the first commit of the
stack so every later commit builds. It is not part of polarity and must not
be squashed into any polarity commit. When upstream fixes the artifact,
rebase and drop it.

### W2. Stored-codec preparation commits row defaults

**Mechanism.** A stored parser or encoder constant (`parse_headers =
Encoding.HttpHeader.parser_for()`) is restored at its use site by
`restoreConstParserRuntimeFnAtNode` (`lower.zig` ~33872). Restoration
prepares one generated call per format method (`prepareStructuralCodecCallsAtNode`
→ `prepareCustomCodecCallsAtNode` → `prepare*CodecCall`, ~43690–45822). Each
prepare function instantiates the format method's checked scheme fresh
(`target_ctx.instNode(lookup.target.callable_ty)`) and relates only the
encoding, the state, the error row, and the ok row when the result is the
state. The ok-payload protocol union of methods such as
`parse_record_start : … -> Try([Counted(..), Uncounted(..)], [BadHeader])`
is never related to anything. Under polarity that union's extension is a
quantified flex in the Builtin scheme, so the instantiated node is
`InstVariable{ origin = checked_variable, row_default = .empty_tag_union }`,
unresolved. The eager restore then demands resolved views of every prepared
call (`resolvedPreparedCodecCallsForBoundary`, non-frozen branch, ~43821)
before the graph freezes, and Phase-B sealing, which is the one place that
applies row defaults (`GraphTypeFinals` / `materializeUnresolved`), has not
run. Before polarity these positions were closed structures.

**Decision.** Ground row defaults at exactly that chokepoint: in
`resolvedPreparedCodecCallsForBoundary`'s non-frozen branch, before
`currentPhaseTypeForNode`, walk `prepared.callable_node` and
`prepared.shape_node` and set every reachable unresolved cell that carries a
`row_default` to the content sealing would give it. Rows only: numeric
default phases stay with `materializeLiteralDefault`'s runtime-demand rule.
Cells without a default are left untouched so the resolved-view invariant
still fails loudly for genuinely unresolved types.

Why this point and not earlier: the prepare functions relate the callee's
error row to the outer result's error row after instantiation. Grounding
per prepared call right after `instNode` would close the callee's error row
before that relation and turn one panic into `instantiation widened a closed
tag union`. The chokepoint runs after every preparation relation and is the
exact mirror of `sealedPreparedCodecCallsForBoundary`, which applies the
same defaults through the sealer in Phase B.

Why not the two-phase restore: the architecture design.md describes for
derived codecs (Phase-A prepare, Phase-B emit from sealed types,
design.md ~5719–5728) would avoid early defaulting entirely, but the eager
stored-codec restore emits its body during relation production
(`lowerParseResultFromState` is written against `Type.TypeId`s) and the
`frozen_sealed_emission` branches in the restore functions cannot currently
execute after freeze. That refactor is the long-term direction and is listed
under follow-ups; it is not a prerequisite for correctness.

**Rule text (design.md, Polarity section's lowering paragraph and the
Monotype "defaults apply only at final sealing" statement ~7005; also the
doc comment at `lower.zig` ~15535 "Checked defaults are applied here, never
by an eager consumer").** Add the single declared exception: "A stored codec
restore prepares its generated format-method calls before the graph
freezes and must emit their bodies from resolved views. Immediately before
those views are taken, `InstGraph.groundRowDefaults` commits the row
defaults of every cell reachable from a prepared call's callable and shape
nodes to the content final sealing would materialize. Numeric defaults are
never committed there. A derived codec determines each protocol row exactly,
so no later relation can widen a grounded row; the `unifyTagRows` invariant
enforces that."

**Code.**
- `src/postcheck/monotype/solve.zig`: `pub fn groundRowDefaults(self: *InstGraph, root: NodeId)`, modeled on the old branch's `groundUnresolvedDefaults` but without the numeric arm; `requireRelationProduction()`; visits `list/box/tuple/func/tag_union/record/named` children; `.redirect => unreachable`.
- `src/postcheck/monotype/lower.zig` `resolvedPreparedCodecCallsForBoundary`: in the `!frozen_sealed_emission` branch, `try self.graph.groundRowDefaults(prepared.shape_node); try self.graph.groundRowDefaults(prepared.callable_node);` before the two `currentPhaseTypeForNode` calls, with a comment citing the rule.
- Doc comment updates at `lower.zig` ~15535 and design.md ~7005; extend the Polarity section's lowering note (design.md ~4433).

**Tests.**
- `solve.zig` unit test next to the existing row-default tests (~6290–6420): a func node whose ret is a tag union with an `InstVariable.checkedVariable(null, .empty_tag_union)` tail becomes resolved after grounding; a bare `checked_variable` without default stays unresolved; a numeric-phase leaf stays unresolved.
- CLI: the six registered fixtures (`parallel_cli_runner.zig` ~1509–1515, suite `subcommands`, "stored top-level parser") must pass; add `issue_10888_json_parse_repeated_nested_field` to the same expectation if it is not already there.
- Platform: `run-test-zig-http-header-decoder-platform`, `run-test-zig-json-decoder-platform` (all three apps).

**Verification.** `timeout 120 ./zig-out/bin/roc test --no-cache test/cli/ParserTopLevelStoredParser.roc` and siblings; `zig build run-test-cli -- --suite subcommands --filter "stored top-level parser"`; the two platform steps; `zig build run-test-zig-lir-inline` (must stay green: the deferred structural path shares the prepare functions).

**Risks.** (1) The callee spec was keyed as an open request (`draftOpenRequestKey`) before grounding; a later identical closed request may specialize the same format method twice. Not a correctness issue; measure spec counts on the JSON fixtures and note the follow-up to re-key. (2) `shape_node` grounding is a superset of what is observed unresolved; it is harmless for derived shapes because derivation determines rows exactly, but the unit test must cover a shape with a record whose field carries a defaultable tail. (3) The doc invariant is weakened by exactly one declared case; the Phase-B assertions ("deferred structural serialization changed its sealed result type") stay satisfied because grounding yields the same content sealing would.

### W3. Plan classification: a defaultable row tail is closed

**Mechanism.** `specializeResolvedStaticDispatchPlanCallables`
(`checked_artifact.zig` ~21644) decides `direct_closed` vs
`direct_parametric` with `rootContainsIdentityVariables(plan.callable_ty)`
(~21684, and the iterator twin ~21713). Publication marks every flex as an
identity variable (~7791), so a method whose return row is
`Try(Iter(U64), [Unavailable])` with a defaultable flex tail is now
`direct_parametric`. Monotype's parametric path bails at
`completeDeferredIteratorResult` (`lower.zig` ~32164) because the request
node is not resolved, the callee's private iterator representation is never
adopted, and `Builtin.Iter.next` becomes reachable. The closed path already
seals such tails (`lowerCheckedTypeVariable`, ~6348, `row_default →
empty tag union`).

**Decision.** A flex identity variable that carries a row default and no
numeric default phase is closed for the purpose of this classification: it
has exactly one instantiation. Introduce a helper on the checked type store
(`rootHasParametricIdentity` or similar) that walks the payload and returns
true only for `.rigid`, or `.flex` with `row_default == null or
numeric_default_phase != null`, and use it at both sites. Do not change
`rootContainsIdentityVariables` itself; other consumers rely on its meaning.

**Rule text (design.md, TypeDigest / plan classification).** "A checked
row-tail variable with a row default has a single canonical instantiation
(its closed form), so a direct dispatch plan whose callable's only identity
variables are such tails is `direct_closed`."

**Tests.** `lir_inline_test` "nested iterator results…" (all seven cases);
add one case where the method's return row is written with an explicit
extra tag and a named extension to pin the parametric side.

**Verification.** `zig build run-test-zig-lir-inline`; the full lir-inline
suite; snapshots.

**Risks.** Any other place that expects `direct_parametric` for defaultable
tails; grep every consumer of the classification before changing it. This
mirrors the old branch's "row default is concrete" change but is scoped to
plan classification, not to compile-time root eligibility.

### W4. Encoder derivation tolerates implicitly open optional rows

**Mechanism.** `Shape : { item : Try({ bar : Str, count : U64 }, [Missing]) }`
puts `[Missing]` behind an alias marker that resolves to a fresh flex at each
use. The parser derivation already tolerates `[Missing | flex]`
(`unboundTryInfoFromNominal`, pinned by `pinWildcardOptionalParseField`),
but the encoder derivation requires a closed row
(`varSupportsDerivedEncodeRecordField` → `missingTryInfoFromNominal` →
`varIsExactUnitTagUnion` → `tagExtIsClosedEmpty`) and reports `missing
method` for `encoder_for`. `[Null]` has the same requirement on both sides.

**Decision.** `rprvoylp` closes every reachable flex extension before the
encoder's eligibility check and pins an encoder derivation over
`Try(Str, [Missing])` fields, so this diagnosis is expected to be subsumed.
W4 is therefore a verification item: run `lir_inline_test` issue 10121 on
the stack; it must get past type checking. If it does not, mirror the
parser (accept `unboundTryInfoForVar` in `varSupportsDerivedEncodeRecordField`,
pin the row before `missingTryInfoForVar` in `validateDerivedEncodeRecord`)
under the existing "Derived Parser Tag-Row Closure" rule extended to
encoders. Either way, add the `[Null]` encoder and parser cases as checker
tests: they share the closed-row requirement and are not pinned today.

**Tests.** Checker integration tests for encoder derivation of a record with
`Try(_, [Missing])` and `Try(_, [Null])` fields reached through an alias;
`lir_inline_test` issue 10121 must get past type checking.

### W5. Closed stored row against a widened request in a Builtin body

**Mechanism (located, provenance to confirm).** With W4 in place, issue
10121 panics with `instantiation widened a closed tag union` from
`selectExprRepresentationAtNode` → `selectRequestRepresentation` inside a
pending spec job for a Builtin lambda. The rows are expected
`[InvalidJson(Str), MissingRequiredField(Str)]` (closed) against produced
`[InvalidJson(Str)]` (closed). The diagnosis's hypothesis is that
`Json.invalid_json : [InvalidJson(Str), ..]`, a top-level value whose row is
quantified, reaches Monotype sealed closed while the derived parser's
request row was widened by `constrainDerivedParserRequiredFieldError`. The
json-decoder camel failure ("instantiation unified a tag union with a
non-tag-union type" under `restoreCapturingConstFnAtNode`) is probably the
same family.

**Decision.** Diagnose before fixing: one traced run printing the checked
expression and template name at `selectExprRepresentationAtNode`, to confirm
the producer (the `invalid_json` value, or the `Err(Json.invalid_json)`
sites in `Json.parse` / `JsonEncoding.parse_str`). Then choose between:
- (a) the value is a stored constant sealed at its own closed row: relate
  requests to stored constants through the produced-value witness with
  checked-only tags allowed (the old branch's `producedTagValueWitness`
  change), declared as "a stored constant's ground row may be narrower than
  a request that widened its implicitly open row";
- (b) the value is on the eval path and the specialization's request row is
  related exactly against the template's sealed row: relate at the
  template's own row and re-tag at the use, reusing the W6 re-tag helper.
The fix must come with a `lir_inline_test` reproduction that names the
producer, so the provenance is pinned and not inferred.

### W6. Where-method uses: deliberate per-use plans and closed-implementation re-tag

**Mechanism (verified with the built compiler).** A body use that widens
its where-method copy already lowers correctly when the implementation's
own return row is open: the artifact's plan carries the use's copy as
`callable_ty`, `paramIndexFor` cannot match the copy's fresh root to the
where-clause var and falls back to a same-name match with
`independent_callable = true`, and Monotype then instantiates the
implementation's scheme against the use's callable. Widening, `?` into a
wider row, exhaustive closing, two independent uses, and nested evidence all
pass on both backends. It panics only when the implementation's return row
is closed in its scheme (the body returns a top-level constant, an
input-position parameter, or a nominal field):
`instantiateTargetFromPlanNode` → `relateFunctionRequestInterface` →
`unifyTagRows` "instantiation widened a closed tag union".

**W6a. Make per-use plans deliberate.** The working behavior rests on a
fallback intended for a different case. Record each body use's copy: in
`instantiateWhereMethodForUse`, pair the use's fresh fn var with the
where-clause fn var (in the style of `scheme_use_pairs` /
`recordSharedSchemeUse`); publish the pairs; have `paramIndexFor` /
`chainParamIndex` recognize a recorded per-use copy explicitly and produce
`evidence_dependent{ independent_callable = true }`, and turn the same-name
fallback into an invariant for the remaining cases. Lessons from the old
branch's codec-key commits apply: thread the use's var explicitly, never key
across an instantiation boundary by var identity or by name, and never let
one evidence record resolve at two callables (`evidenceNodeForTarget`
memoization). Tests: the scratch programs from the investigation as
`lir_inline`/CLI fixtures (widen; widen with a tag that sorts between
`Err`/`Ok` to prove the implementation is specialized at the wider row;
`?` into a wider row; one closing use and one widening use; an
implementation with its own where-clause, exercising `.synthesize` nested
evidence; and an implementation with a `requires_record` evidence schema,
which is untested today).

**W6b. Closed implementation, widened use.** Decision: a caller-side re-tag
at the dispatch call for top-level result rows, the narrow form of the
row-subsumption coercion. In `instantiateTargetFromPlanNode` /
`methodTargetNodeFromPlan`, detect that the plan's return row lists tags the
implementation's closed return row lacks; relate the arguments exactly and
the return components at included width the way `relateHostedTryWidening`
does, without unifying the rows; request the implementation at its own row;
in `lowerResolvedDispatchAtNode` wrap the call in a re-tag `match` built by
`errorRowInjectionExpr` for a bare union or `hostedTryReturnInjectionExpr`
for `Try`, with the Try capability computed from the mono types rather than
the hosted-only publication gate. Deep positions (the widened row nested in
a `List`, a record field, a tuple) are out of scope and must fail with a
user-facing build error that names the use and the implementation, never
with an invariant panic. Rule text (design.md, new "Row re-tag at
where-method uses" entry): "A where-method use may widen the method's
result row. When the resolved implementation's row is closed, lowering
re-tags the implementation's value into the use's row at the call; only the
top-level result row is re-tagged, and a widened row in a nested position is
a build error." Rejecting at check time instead is not expressible: the
widening can happen at the constrained function's callers, after its body
is checked. Tests: `WidenClosedImpl`, `WidenParamImpl`, `QuestionClosedImpl`
as fixtures on both backends, plus a nested-position fixture asserting the
build error text.

**Docs.** Rewrite the "Lowering note" in design.md's where-method paragraph:
open implementations specialize per use as a plain scheme instantiation;
closed implementations are re-tagged at the use.

### W7. Documentation and description

Update design.md's Polarity lowering note to describe W2, W3, W6 as declared
rules; add Rewrite Inventory entries for `groundRowDefaults` (Monotype
mutation) and the re-tag; refresh the PR description's verification section.

## 3. Sequencing and commit stack

Order: W1 (already present, move to the bottom) → W2 → W3 → W4 → W5 → W6a →
W6b → W7. W2 through W4 are independent of each other and can be developed in
parallel worktrees but land in this order so each commit's verification is
monotone. W5 depends on W4. W6b depends on W6a's fixtures. Every commit is
created with `jj new -m` before its first edit, carries the trailer lines,
and is verified in isolation with its item's commands before the next
starts; the full `run-test-zig` and `run-check-snapshots` run after W3, after
W5, and after W6b.

## 4. Verification matrix

| Level | Command | Gate |
|---|---|---|
| Checker | `zig build run-test-zig -- --test-filter "check type"` | green after W4, W6a |
| Monotype/LIR | `zig build run-test-zig-lir-inline` | green after W3 (iterator), W5 (10121), W6 |
| Stored codecs | `zig build run-test-cli -- --suite subcommands --filter "stored top-level parser"` | green after W2 |
| Platforms | `zig build run-test-zig-http-header-decoder-platform`, `zig build run-test-zig-json-decoder-platform` | green after W2 (and W5 for camel variants if they share the family) |
| Everything | `zig build run-test-zig`, `zig build run-check-snapshots` | 100% after W6b |

## 5. Risks and rollback

- W2 weakens one documented invariant by a declared exception; if a later
  relation ever widens a grounded protocol row, the `unifyTagRows` invariant
  surfaces it at the exact site. Rollback is removing the two calls.
- W3 changes plan classification; every consumer of the classification is
  enumerated in the commit message. Rollback is restoring the helper's
  callers.
- W6b introduces the first non-hosted re-tag; it is gated to top-level result
  rows and fails loudly elsewhere, so it cannot silently produce a wrong
  representation.
- The stack sits on a `main` that needs W1 to build; if upstream fixes the
  artifact first, rebase and drop W1.

## 6. Follow-ups, deliberately out of scope

- General row-subsumption coercions (closed values widening into open rows
  in any position), which would also let a closed body publish an open row.
- Cross-module widening of annotated weak values (currently grounded closed,
  as on `main`).
- Two-phase stored-codec restore, replacing W2's grounding with Phase-B
  emission.
- Re-keying open-keyed format-method specializations after grounding.
