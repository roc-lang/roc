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
| Same panic for a stored parser whose shape has an optional `?:` field, raised at the restore function's FIRST eager view of the shape node (`lower.zig` ~33935, before any codec call is prepared), so no chokepoint grounding can reach it. Found by the W2a review (2026-09-03); reproduced on `main` + W1 (`kusqzzsn`) with a `..`-style probe, so pre-existing, not a polarity regression. Probes: scratchpad `w2a-review/ParserTopLevelStoredOptionalField*.roc`. | `lower.zig` stored-codec restore functions | W2b |
| `lir_inline_test` "nested iterator results retain the callee-authored representation", case `closed direct Try method` | `checked_artifact.zig` plan classification | W3 |
| `lir_inline_test` "issue 10121 …" (five tests): four `missing method` (encoder) errors, then `instantiation widened a closed tag union` in a Builtin lambda specialization — as measured BEFORE `rzysvtry`/`rprvoylp`. Re-measured 2026-09-03 in a clean workspace at the plan commit: all five pass. `rprvoylp`'s derivation closure subsumed both. | resolved by `rprvoylp`; W4 pins it | W4 (W5 folded in) |
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

### W2. Stored-codec restore: ground row defaults now (W2a), then emit in Phase B (W2b)

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

**Decision (W2a, transitional; landed as `tsrzvryw`).** Ground row defaults at exactly that chokepoint: in
`resolvedPreparedCodecCallsForBoundary`'s non-frozen branch, before
`currentPhaseTypeForNode`, walk `prepared.callable_node` and set every
reachable unresolved cell that carries a `row_default` to the content
sealing would give it. The prepared call's `shape_node` needs no grounding:
it is always a sub-node of the root shape the restore already viewed
resolved (verified across all sixteen `prepare*` appenders and the four
restores; a fresh `instNode` in the prepare range only ever feeds a
callable node), and the resolved view taken right after would panic if
that ever stopped being true. Rows only: numeric
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

Why W2a is not the end state: design.md already claims the two-phase
discipline for codec generation ("Parser generation runs after the
instantiation graph freezes, so derived-codec parsers obey the Phase-A/
Phase-B boundary", ~5737–5748) and states that a defaultable checked
variable becomes durable `[]` only at final sealing (~7002–7006). The eager
stored-codec restore violates both today; polarity merely made a row reach
it unresolved. The deferred structural path already has the machinery the
restore lacks — a `.pending_deferred` reservation, a boundary record
(`deferred_structural_serializations`), Phase-A preparation of codec calls
and `??` field defaults, and Phase-B emission from sealed types
(`emitDraftDeferredStructuralSerializations`, `sealedPreparedCodecCallsForBoundary`,
`sealedPreparedFieldDefaultsForBoundary`) with the assertion that emission
creates no new runtime demand. W2a is an eager consumer committing a
default, exactly the class the doc comment at `lower.zig` ~15535 forbids
("never by an eager consumer"), and it is the same helper the old branch
grew to thirteen call sites. It lands first because it is thirty lines and
unblocks nine programs; W2b removes it in this plan, not in a follow-up, so
the invariant is restored rather than annotated.

**Rule text for W2a (design.md, the Monotype "defaults apply only at final
sealing" statement ~7005, and the doc comment at `lower.zig` ~15535).** Add
the single declared exception, marked transitional and citing W2b: "A stored codec
restore prepares its generated format-method calls before the graph
freezes and must emit their bodies from resolved views. Immediately before
those views are taken, `InstGraph.groundRowDefaults` commits the row
defaults of every cell reachable from a prepared call's callable node to
the content final sealing would materialize. Numeric defaults are
never committed there. A derived codec determines each protocol row exactly,
so no later relation can widen a grounded row; the `unifyTagRows` invariant
enforces that."

**Code.**
- `src/postcheck/monotype/solve.zig`: `pub fn groundRowDefaults(self: *InstGraph, root: NodeId)`, modeled on the old branch's `groundUnresolvedDefaults` but without the numeric arm; `requireRelationProduction()`; visits `list/box/tuple/func/tag_union/record/named` children; `.redirect => unreachable`.
- `src/postcheck/monotype/lower.zig` `resolvedPreparedCodecCallsForBoundary`: in the `!frozen_sealed_emission` branch, `try self.graph.groundRowDefaults(prepared.callable_node);` before the two `currentPhaseTypeForNode` calls, with a comment citing the rule and saying why the shape node needs none.
- Doc comment updates at `lower.zig` ~15535 and design.md ~7005; extend the Polarity section's lowering note (design.md ~4433). Not the Rewrite Inventory: that inventory classifies solver-mutating rewrites in checking, and `groundRowDefaults` is a Monotype graph mutation.

**Tests.**
- `solve.zig` unit test next to the existing row-default tests (~6290–6420): a func node whose ret is a tag union with an `InstVariable.checkedVariable(null, .empty_tag_union)` tail becomes resolved after grounding; a bare `checked_variable` without default stays unresolved; a numeric-phase leaf stays unresolved.
- CLI: the six registered fixtures (`parallel_cli_runner.zig` ~1509–1515, suite `subcommands`; their names contain "stored", only two contain "stored top-level parser") must pass. `issue_10888_json_parse_repeated_nested_field` is already registered (~1461, "issue 10888: JSON parser retains metadata …", asserting no "postcheck invariant violated").
- Platform: `run-test-zig-http-header-decoder-platform`, `run-test-zig-json-decoder-platform` (all three apps).

**Verification.** `timeout 120 ./zig-out/bin/roc test --no-cache test/cli/ParserTopLevelStoredParser.roc` and siblings; `zig build run-test-cli -- --suite subcommands --filter stored --filter "issue 10888"`; the two platform steps; `zig build run-test-zig-lir-inline` (must stay green: the deferred structural path shares the prepare functions).

**Risks.** (1) The callee spec was keyed as an open request (`draftOpenRequestKey`) before grounding; a later identical closed request may specialize the same format method twice. Not a correctness issue; measure spec counts on the JSON fixtures; W2b removes the cause. (2) The four restore functions take an eager resolved view of the shape node right after `instNode` (~33786, ~33935 and the encoder twins), before any codec call is prepared, so a shape whose own cells are unresolved (an optional `?:` field slot) panics there, ahead of the chokepoint; W2a does not cover it (decided 2026-09-03, option 1: W2a stays minimal, W2b owns it — pre-existing on `main`). The chokepoint's `shape_node` grounding was therefore a provable no-op and was dropped; the declared exception names only callable nodes. (3) The doc invariant is weakened by exactly one declared case; the Phase-B assertions ("deferred structural serialization changed its sealed result type") stay satisfied because grounding yields the same content sealing would. (4) A format-method implementation whose own protocol row was closed by its body (a closed-source return) meets a grounded request that lists more tags: that is the W6b family inside codec land and W6b's adapter covers it; W2a must not paper over it with a wider grounding.

**W2b. Two-phase stored-codec restore.** Decision: the four BodyContext-level
restores with dead `frozen_sealed_emission` branches (`lower.zig` ~33786,
~33937, ~34104, ~34274: parser and encoder runtime functions, `AtNode` and
plain) split into a Phase-A half that runs while the graph accepts relations
— instantiate the constructor plan against the request, bind const source
captures, restore the encoding capture, `prepareStructuralCodecCallsAtNode`,
prepare `??` field defaults the way `prepareDraftDeferredExprs` does,
`buildParserRestoredPrecomputedPlan`, reserve the runtime boundary
`.pending_deferred`, and append a boundary record alongside
`deferred_structural_serializations` — and a Phase-B half run by the same
pass as `emitDraftDeferredStructuralSerializations`: sealed prepared calls
and field defaults, `lowerParseResultFromState` against sealed `TypeId`s,
`addFn` with a `.sealed` `mono_fn_ty`, the capture lets, and
`fillExprReservation`. The eager `sameClass(parsed_node, runtime_fn.ret)`
check becomes the Phase-B `typeEql` assertion. The non-frozen branch of
`resolvedPreparedCodecCallsForBoundary` and `groundRowDefaults` are deleted.
The Builder-level `restoreConstParserRuntimeFnExpr` (own graph, sealed by
`sealActiveBodyDraft`, no prepared codec calls) is unaffected. W2b also
owns the optional-field case from the status table: the eager shape view
disappears with the split (the shape is sealed in Phase B like every
other type), and the W2a review's probe lands as registered `test/cli`
fixtures — a stored parser and a stored encoder over
`{ foo : Str, bar ?: Str }` — asserting no panic on both backends. Until
W2b lands that program panics exactly as it does on `main`. Rule text:
delete W2a's exception; the ~7005 statement and the ~15535 doc comment read
as on `main`, and the Phase-A/Phase-B paragraph at ~5737 gains one sentence:
"Stored codec restores (`parser_runtime` / `encoder_for_runtime` constants)
prepare in Phase A and emit in Phase B like every other codec body." Tests:
the same fixtures; `run-check-snapshots` on the JSON and http-headers
fixtures must show no lowered-output change against W2a (the sealed body
equals the eagerly emitted one). Cost: medium — the mechanism exists, the
work is moving the emission halves across the freeze and making the stored
path prepare field defaults; I could not size it more precisely without a
build. Risk: `enterCallableBodyDemandScope` and `constFnEvidence` must be
valid in Phase B; the "produced a new checked runtime-value demand"
assertion is the guard.

### W3. Plan classification: a body-local defaultable row tail is closed

**Landed as `wwnsvqrn` (2026-09-03), with two deviations Jared kept.**
(1) Instantiated plan callables are fresh clones
(`instantiateResolvedDispatchTargetCallable` → `cloneCheckedTypeRootSubstituting`,
distinct roots for unsubstituted variables), so membership by type id
cannot see quantification; the artifact keeps a build-only, never
serialized `identity_origins` record (clone → source), marks synthetic
variable roots, and `identityOrigin` fails loudly for a marked root with
no declared origin (projectors declare fresh instances). Open question 2
is thereby answered artifact-side: the quantification notion is the
artifact's own, and a checker flag would still need the clone-to-origin
mapping. (2) `lower.zig` `requireClosedCheckedType` demanded
digest-closedness of a `direct_closed` sealed-cell result and rejected a
body-local `Try` binding; it now asks the shared
`CheckedTypePayload.variableSealsToRowDefault` question, with the
quantification half guaranteed by its two producers (documented at the
guard). Also from review: classification is order-independent over the
union of every referencing span (hoisted compile-time roots are collected
under both the definition template and its entry wrapper); two more
fixtures (nested generalized local, recursive method). Residual, by the
rule's own choice: rigid-bearing callables stay parametric, so iterator
representation adoption is not restored inside generic callables (same
as `main`).

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

**Decision.** The classification keeps the meaning design.md gives it
("independent of the enclosing specialization", ~7756), which is not the
same as "carries a row default". A defaultable, unconstrained flex row tail
is independent only when no specialization edge can bind it: it is not an
identity variable of the enclosing template's checked function root, of
that root's where-clause signatures, or of a nested generalized scope's
root. The failing iterator case qualifies (`wrapped = rows.wrapped()` is
body-local). A tail shared with the enclosing function's own return row
does not, and the earlier draft's rule ("defaultable ⇒ closed") breaks it:

    Rows := {}.{ wrapped : Rows -> Try(Str, [Unavailable]), wrapped = |_| Ok("x") }
    wrap : Rows -> Try(Str, [Unavailable])
    wrap = |rows| rows.wrapped()
    use : Rows -> Try(Str, [Unavailable, Other])
    use = |rows| { s = wrap(rows)?  Ok(s) }

(`OpenMethodWidenedCaller.roc`, passes today on the built compiler on both
backends because the plan is `direct_parametric`.) The dispatch's callable
ret tail is `wrap`'s implicitly open extension, which `use`'s `?` widens
to `[Unavailable, Other]` in `wrap`'s specialization. Classified closed,
`lowerClosedDirectProcedureDispatch` (~37879) lowers `plan.callable_ty`
through `lowerType`, sealing the tail to `[]`, then
`constrainTypeToMono(checked_ret_ty, function.ret)` exact-unifies that
closed `Try(Str, [Unavailable])` with the request's `[Unavailable, Other]`:
`instantiation widened a closed tag union`.

Mechanism: a predicate on the checked type store,
`callableIdentityIsSpecializationIndependent(callable, enclosing_identity)`,
walking the payload: `.rigid` → parametric; `.flex` with `row_default ==
null` or `numeric_default_phase != null` → parametric; a defaultable `.flex`
that is a member of `enclosing_identity` → parametric; otherwise closed.
`enclosing_identity` is the identity-variable set of the enclosing template
root plus its where-clause signatures and nested-scope roots; the root
builder already collects identity-variable slots per published root
(`identity_variables`, ~6956/7566), so `specializeResolvedStaticDispatchPlanCallables`
is driven per template over its plan-ref span (or receives the set per
plan) instead of over the flat plan table. `rootContainsIdentityVariables`
is unchanged: its other consumers — the substitution fast path (~4932) and
the payload identity walk that decides digest identity (~6602) — need the
var-based meaning.

Rejected alternative: seal defaultable tails on the Monotype side before
the `typeIsResolved` gate in `completeDeferredIteratorResult` (~32164).
That makes iterator completion a second eager consumer of row defaults
(W2a's class) and leaves every polarity-opened method call
`direct_parametric` — a precision and compile-time regression against
`main`, where the same calls were closed.

**Rule text (design.md "Static Dispatch In Monotype", the `direct_closed`
bullet ~7756).** "A checked flex row tail that carries a row default and no
constraints, and that the enclosing template does not quantify (it is not
an identity variable of the template's root, its where-clause signatures,
or a nested generalized scope), has exactly one instantiation — its row
default — and does not make a direct plan parametric; the closed path seals
it to that default (`lowerCheckedTypeVariable`). A tail the enclosing
template quantifies is parametric, as any other identity variable."

**Tests.** `lir_inline_test` "nested iterator results…" (all seven cases);
`OpenMethodWidenedCaller` as a `lir_inline`/CLI fixture on both backends
(pins the exclusion: must remain `direct_parametric` and pass); the same
program called only at its own row (still parametric by rule; pins that
the rule is about quantification, not about observed widening); one case
with an explicit extra tag and a named extension to pin the rigid side.

**Verification.** `zig build run-test-zig-lir-inline`; the full lir-inline
suite; snapshots.

**Risks.** Any other place that expects `direct_parametric` for defaultable
tails; grep every consumer of the classification before changing it. The
compiler already carries two notions of variable identity — digest identity
(any variable, `rootContainsIdentityVariables`) and compile-time-root
concreteness (`checkedTypeIsConcreteCompileTimeRoot`, `.flex => false`,
~1703) — and boxy and glue already treat a defaultable tail as closed
(`boxy/plan.zig` ~5360, `glue.zig` ~4434). This predicate is a third; the
commit documents all three side by side so they cannot drift silently. The
old branch's sprawl is not re-imported: the compile-time-root gate and the
digest dedup (`vkoyrzmonxko`, `tzskosxk`) stay untouched, so no constant
moves between the stored and eval paths.

### W4. Pin the derivation-closure outcome for issue 10121 (W5 folded in)

**Landed (2026-09-03), one correction to step 1.** The parser side never
had the closed-row requirement for `[Missing]`: `nominalSupportsDerivedParseField`
accepts `[Missing, ..flex]` through `unboundTryInfoFromNominal` /
`varIsOpenOptionalParseError`, and `pinWildcardOptionalParseField`
would unify the row with closed `[Missing]` during validation if the
walker had not already closed it (the walker runs at constraint
resolution, before validation reaches the pin). So three of
the four new alias-marker tests pin `closeTagRowsForDerivation`
(encoder `[Missing]`, encoder `[Null]`, parser `[Null]`) — verified by
temporarily no-op'ing the walker, which fails exactly those plus three
existing `rprvoylp` tests — while the parser `[Missing]` test pins the
issue-10121 shape through the pre-existing wildcard pin and says so in
its name. The two CLI fixtures pass with warnings from compile-time
evaluation (unused match branch; condition known at compile time) and are
registered as `.not_panic` + "All (1) tests passed", not on an exit code,
so they pin the round trip rather than the warning heuristics, with a
`not_contains` stderr needle for `missing method` because `roc test`
prints its success line before consulting checker errors.

Review discoveries recorded for later, outside W4's file set: the
existing `rprvoylp` test "derived parser closes an implicitly open Dict
key union" still passes with the walker no-op'd, so its name overstates
what it pins (cause not investigated); `pinWildcardOptionalParseField` is
now dead for annotated `[Missing]` rows and live only for bare
`Try(ok, _)` fields, worth a comment at the pin; and every `.not_panic` +
"All (N) tests passed" CLI entry is blind to checker errors that leave
the expect passing unless it carries `not_contains` needles for the
diagnostics it guards against. Fold the first two into W7's doc pass or
W8's test sweep.

**Finding (2026-09-03).** The plan's status table was measured on the
stack before `rzysvtry` and `rprvoylp` landed. Re-run in a clean jj
workspace at the plan commit `wlzsxolu` (no phase-two code), all five
`lir_inline_test` "issue 10121" tests pass (`zig build
run-test-zig-lir-inline --summary all -- --test-filter "issue 10121"`:
5/5). The earlier W4 failure — encoder derivation reporting `missing
method` because `[Missing]` behind an alias marker resolved to an open row
at the eligibility check — is exactly what `closeTagRowsForDerivation`
(`rprvoylp`) closes before every derivation site, as that item's design.md
Rewrite Inventory entries state. The earlier W5 panic was observed only
after a parser-style *tolerance* had been prototyped for W4, which left
the protocol rows open into Monotype; with the rows closed by derivation
closure, the Builtin lambda specialization never sees a widened request
and the panic is not reachable. The two harness-only diagnoses therefore
need no diagnosis and no fix. What remains true and worth keeping is the
evidence the review produced: the CLI path and the `lir_inline_test`
harness compile Builtin through different import-view topologies
(`lookupMethodTargetAcrossViews`, `lower.zig` ~17458), and the exact 10121
program plus its reductions run clean on the CLI on both backends.

**Decision.** One pinning commit, no behaviour change:
1. Checker integration tests for encoder AND parser derivation of a
   record with `Try(_, [Missing])` and `Try(_, [Null])` fields reached
   through an alias marker (the `[Null]` cases share the closed-row
   requirement and were never pinned; `rprvoylp` pinned only `[Missing]`
   for the encoder).
2. The review's CLI control programs promoted to `test/cli` fixtures and
   registered in `parallel_cli_runner.zig` (suite `subcommands`):
   `Issue10121Exact.roc` (the exact program as a compile-time value root)
   and `Issue10121Fn.roc` (the same body as a runtime function), run with
   `roc test --no-cache` on both backends, so the CLI path and the harness
   path cannot drift apart silently again. (`OpenMethodWidenedCaller.roc`
   belongs to W3 and lands there.)
3. The five 10121 `lir_inline_test` cases are the harness-side gate and
   are listed in the commit message as the tests this commit pins.
4. The status table above is the record; nothing in design.md changes,
   because `rprvoylp` already declared the rule.

**Verification.** `zig build run-test-zig -- --test-filter "check type"`;
`zig build run-test-zig-lir-inline -- --test-filter "issue 10121"`;
`zig build run-test-cli -- --suite subcommands --filter 10121`.

**Risks.** None to the compiler. The residual asymmetry between the CLI's
and the harness's import-view topology is not a failure today; if a later
item makes it one, the fixtures from step 2 name the CLI side of the
diff. Open questions 5 and 6 are closed by this finding.

### W6. Where-method uses: deliberate per-use plans and closed-implementation re-tag

**Original diagnosis (verified with the built compiler).** A body use that
widened its where-method copy happened to lower correctly for an open
implementation because `paramIndexFor` matched the method name after exact
callable identity failed. That behavior was useful evidence, but it was an
undeclared fallback and is not the W6a design. Widening, `?` into a wider row,
exhaustive closing, two independent uses, and nested evidence passed on both
backends through that route. It panics only when the implementation's return
row is closed in its scheme (the body returns a top-level constant, an
input-position parameter, or a nominal field):
`instantiateTargetFromPlanNode` → `relateFunctionRequestInterface` →
`unifyTagRows` "instantiation widened a closed tag union".

**W6a. Make per-use plans deliberate (implemented; focused verification
complete 2026-09-03).** A body use remains an exact raw scheme instantiation:
`instantiateWhereMethodForUse` writes `SchemeUseRecord.where_method_use`, keyed
by the use's raw constraint callable, with the pristine signature root and the
complete copy map. The record has no child dispatch requirements of its own. It is not
looked up by solved class and it is never matched by method name.

The additional case discovered during implementation is generalized
constraint dominance: unification can correctly omit one declarative target
while retaining another for the same receiver and method, leaving a dispatch
plan named by the omitted raw callable. Upstream's
`deduplicateGeneralizedDispatchRequirements` handles it: before a same-shape
duplicate is dropped, its callable is unified with the retained one through an
ordinary committed probe (`unifyEquivalentGeneralizedCallables`; a probe that
cannot establish the pair rolls back and keeps both). The omitted callable
class IS the retained class afterwards, so every scheme-use pair, nested
evidence key, and body expression typed by the dropped callable resolves to the
retained relation and no separate witness is needed. The branch's earlier
`GeneralizedDispatchTargetShare` raw-witness table (a serialized, scoped
relation between omitted and retained callables carrying shape-only or
where-use proofs, with its unifier candidate plumbing, boundary flushes, and
checked-artifact relation graph) was superseded by that committed-probe
unification when the stack was rebased onto it on 2026-09-15, and is deleted.
`paramIndexFor` keeps upstream's same-name fallback for an independent
callable; it is the where-use record, not a name match, that decides whether
the slot's nested evidence may be reused.

The nested-evidence decision is explicit. A plan whose callable is not the
evidence slot's own callable is an independent callable
(`independent_callable`). When that callable is a per-use where-method copy
whose pristine signature is the slot's callable (`SchemeUseRecord.where_method_use`),
the plan reuses the slot's checked nested-evidence vector
(`reuse_slot_nested_evidence`); otherwise nested evidence is synthesized from
the plan's own callable. A `requires_record` evidence schema cannot be
synthesized, so it requires the where-use reuse. A direct Monotype unit pins
both sides: where-use reuse preserves the slot's resolved nested vector, while
synthesis of a `requires_record` schema is rejected. The combined W6a LIR
fixture exercises the reuse path.

Codec durability: when deduplication merges two requirements, the retained
entry keeps `deferred_generated_codec` and `pristine_codec_is_scheme_only`
if either side had them (upstream's flag-OR merge), so encounter order does
not matter.

Current verification: the equivalent correctly ordered focused command,
`zig build run-test-zig-module-check --summary all -- --test-filter "scheme use"`,
passed all 7 build steps and both selected tests. The literal requested spelling,
`zig build run-test-zig-module-check -- --test-filter "scheme use" --summary all`,
reached the test runner but was rejected because `--summary all` was forwarded
to the test binary (`unrecognized command line argument: --summary`). The final
focused suite was green before the rebase: static/mutable ModuleEnv
roundtrips, checked-artifact and ModuleEnv cache fingerprints, serialization
sizes, mutable warm-cache preservation of exact raw where-use records,
accepted/rejected cross-module generated-codec revalidation (2/2), the
`requires_record` Monotype gate (2/2), and the combined W6a LIR regression
(1/1). The share-table gates (unifier omission producer, relation
composition/cycle, codec-order deduplication, target-share row preservation)
were deleted with the table. The LIR regression pins ordering-sensitive widening, `?` into a wider
row, closing plus widening at independent uses, nested synthesis, and
proof-backed `requires_record` reuse. Closed-implementation widening remains
the deliberately separate W6b expected-failure class.

**W6b. Closed implementation, widened use.** Decision: a result-row
widening ADAPTER at the template boundary, generalizing the hosted `Try`
adapter, not a second re-tag site at the dispatch call. The compiler has
this mechanism end to end already: a request wider than a template's
declared closed result row is related component-wise without unifying rows
(`relateHostedTryWidening` ~1432), the template is specialized at its
declared row, and a generated `.checked_generated` adapter at the requested
row calls it and re-tags (`completeTemplateReservation` `.hosted` arm
~4640–4700, `hostedTryAdapterSourceType` ~10843, `hostedTryAdapterBody` /
`hostedTryReturnInjectionExpr` / `errorRowInjectionExpr` ~10941–11060).
Hosted is the instance where the declared row is the host ABI; a Roc
implementation whose published result row is closed (its body returns a
closed-source value) is the other instance, and only where-method uses can
reach it — a direct caller of such a function at a wider row is already a
checker mismatch. Work: (1) lift the declared-vs-requested comparison out
of the `.hosted` arm into a pre-step that also runs for `.roc` templates
whose checked root has a closed result row (bare union or `Try`), taking
the narrowed source type from the REQUEST's tags by the declared labels as
`hostedTryAdapterSourceType` does (a polymorphic implementation's rigid
payloads come from the request, never from `lowerType` of the checked
root); (2) make the relation in `instantiateTargetFromPlanNode` /
`methodTargetNodeFromPlan` (~30540, ~39625) width-aware — arguments exact,
result at included width when the implementation's row is closed and the
plan's row includes it — so the request reaches template completion at the
wider row instead of panicking in `unifyTagRows`; (3) compute the `Try`
capability from the type (`hostedTryAdapterCapabilityForRoot` ~19539 is
already generic over any function returning `Builtin.Try` with a closed
error row) and publish it for every template with a closed result row, not
only behind `isHostedProcedureExpr` (~19758). Chosen because one keyed
mechanism — an adapter per (template, requested type) — serves dispatch
plans, `.synthesize` targets, and iterator plans without touching each
call-lowering path, and the hosted path stops being a special case. Cost:
the hosted arm is restructured (pinned by the existing hosted `?`
fixtures), and the request relation gains a width mode.

Nested positions. Monotype lowering has no user-facing diagnostic channel:
`Common.invariant` is a debug panic that compiles to `unreachable` in
release, `Common.compilerBug` panics in every mode, and nothing under
`src/postcheck/monotype` appends problems. "A build error from lowering"
would be a new mechanism; this plan does not add one. A widened row in a
position the adapter cannot re-tag (inside a `List`, a record field, a
tuple, a tag payload, a non-`Try` nominal) is decided by the checker. The
earlier draft's claim that check-time rejection is not expressible is wrong
in the direction that matters: a body use's widening is observable when
the constrained function's body is checked (its fresh extension resolved
to a row carrying tags — the audit's own test), and each marker's position
in the signature is known when it is minted. Two checker shapes were
possible; Jared decided (e) on 2026-09-03 (question 3, now closed): (d) per-use opening is
restricted to the positions the adapter can re-tag — the direct result row
and a `Try`'s rows — and every other output position of a where-method
signature stays closed as written, so a nested widening is an ordinary
mismatch at the body use and the set of opened positions grows with the
coercion generator; or (e) per-use opening stays everywhere and the
obligation reports a new problem when the implementation's row at a
widened nested marker is closed, which needs the widened markers recorded
per signature and the implementation's scheme inspected before the
obligation unifies it. Decision: **(d)** — reversed from (e) on 2026-09-14, see below. Per-use
opening is restricted at GENERATION time to the positions the adapter can
re-tag: the direct result row and a `Try`'s rows. Every other output
position of a where-method signature stays closed as written, so a nested
widening is an ordinary type mismatch at the body use, reported at the
body use's own region. The set of opened positions grows with the coercion
generator (§6). The nested-position fixture asserts that mismatch.

**Why (e) was reversed (2026-09-14).** (e) rested on two premises that the
code does not support:

1. *"The obligation inspects the implementation's row at a widened nested
   marker."* It cannot see markers at all. `instantiateTypeScheme`
   (`Check.zig:6434-6454`) instantiates the enclosing scheme with
   `.polarity_var_behavior = .close`, and `instantiate.zig:519,530` mints a
   FRESH `empty_tag_union` var with no back-link to the pristine marker.
   By the time the obligation runs it sees `[]`, not a marker.
2. *"Each marker's position in the signature is known when it is minted."*
   Nothing records position. The instantiator carries `current_polarity`
   only (`instantiate.zig:290-296`); `OpenedMarkerExt` is `{ ext,
   listed_tags }` (`instantiate.zig:364-367`).

So (e) could not be marker-keyed; it would have to be path-keyed, which
requires a structural path encoding, a NEW SERIALIZED `ModuleEnv` table
(obligations fire cross-module — `where_clause_test.zig:304` is an existing
green test of that shape), a cache bump to 75, probe-rollback and
rehydration arms, and a new non-local invariant that structural paths stay
stable across generalization, cross-module copy, the closing instantiation,
and cache round-trip. Nothing else in the tree depends on path stability;
every existing cross-phase identity is a Var, a node index, or a
`TypeDigest`. Estimated 540-840 lines across 9-13 functions.

That is the same shape as the reverted W6b stack — a serialized side table
plus bookkeeping to keep an identity alive across phases — for a rule that
would still be narrower than the adapter's own lowerability test (the
checker has only `tagExtIsClosedEmpty`, "literally `[]` now";
`row_default` does not exist until publication), i.e. two rules that can
drift with no assertion tying them. (d) is ~20 lines in one function, needs
no new problem kind, no serialization, no cache bump, and no cross-module
transport, and its diagnostic is strictly better: an ordinary mismatch at
the body use instead of "the implementation you resolved to has a closed
row at a position you cannot see".

Rule text (design.md, a new "Result-Row Widening Adapter" section beside
Hosted Try Question Widening, which becomes its first instance; the
where-method paragraph's lowering note cites it): "A procedure template
whose published result row is closed may be requested at a row that
includes it — the same tags with usable payloads, plus others — when a
where-method body use widened its copy of the signature and the obligation
resolved to that implementation. The request is related component-wise
without unifying the rows, the template is specialized at its declared
row, and a generated adapter at the requested row calls it and re-tags the
result. Only the direct result row and a `Try`'s rows are adapted; a
hosted template is the instance where the declared row is the host ABI."
Tests: `WidenClosedImpl`, `WidenParamImpl`, `QuestionClosedImpl` on both
backends; a closed implementation reached through `.synthesize` nested
evidence; a closed implementation with a rigid payload, pinning the
request-derived narrowing; the hosted `?` fixtures unchanged; the
nested-position fixture per the checker decision.

**Docs.** Rewrite the "Lowering note" in design.md's where-method paragraph
(~4467): open implementations specialize per use as a plain scheme
instantiation; closed implementations get a widening adapter.

### W7. Documentation and description

Update design.md's Polarity lowering note to describe W3 and W6 as declared
rules and to state that stored-codec restores are Phase-A/Phase-B consumers
(W2b). Upstream's `deduplicateGeneralizedDispatchRequirements` (committed-probe
unification of same-shape callables) is the Rewrite Inventory entry; W6a adds
no entry of its own since the share table was deleted at the 2026-09-15
rebase. `groundRowDefaults` (deleted by W2b) and the W6b adapter are
Monotype mechanisms declared in the Monotype sections. Option (e)'s W6b
checker rejection is a declarative Polarity rule unless its implementation
adds a solved-graph mutation, in which case that mutation needs its own Rewrite
Inventory entry. `kupupkyt` already bumped the checked-module cache version
(`CACHE_VERSION`, `src/compile/cache_config.zig`; 92 → 93 after the rebase)
because checked `row_default` data and weak-value grounding changed; W6a bumps
it again (94) for the new `SchemeUseRecord` slot and checked-plan flags.
Refresh the PR description's verification section.

### W8. Report `Tag Not In Annotation` as a Type Mismatch (last; Jared, 2026-09-03)

**Mechanism today.** `auditImplicitOpenExts` (`Check.zig` ~14743) appends a
bespoke problem, `tag_union_extended_beyond_annotation`
(`problem/types.zig` ~708: annotated-union region, the first extra tag, a
typo suggestion), rendered by `buildTagUnionExtendedBeyondAnnotationReport`
(`report.zig` ~2261) under its own title with a prose explanation of
polarity. It shows neither type: the reader does not see what the body
produced nor what the annotation lists, and the hint duplicates the
tag-typo logic the Type Mismatch report already has
(`findBestTypoSuggestions`, `report.zig` ~3092).

**Decision.** Report it through the ordinary Type Mismatch machinery, the
way an annotation mismatch is already reported: `makeMismatchReport`
(`report.zig` ~535) with the `.type_annotation` shape ("It has the
type: … / But the annotation says it should be: …", `report.zig` ~955),
so the rendered report is a regular `TYPE MISMATCH` whose two type
snapshots make the extra tags visible and whose typo hint comes from the
shared diff logic. Concretely: the audit appends a `type_mismatch` problem
with a new context variant (`problem/context.zig` ~55, beside
`type_annotation`) carrying the annotated union's region, an "actual"
snapshot of the resolved row (the listed tags plus every extra tag the
body produced — all of them, not only the first) and an "expected"
snapshot of the union as the annotation wrote it: the recorded
`listed_tags` with a closed extension, built as a fresh var at audit time
so the annotation's own var (which shares the widened row) is not what is
displayed. The headline names the definition and says the annotation
does not list the tag(s); the two type blocks follow; the one-sentence
polarity explanation ("callers may use the result at a wider union; the
annotation still bounds the definition") stays as the closing line since
it is the reason the program is rejected. The bespoke problem, its report
builder, its `problem/store.zig` and `snapshot_tool` plumbing, and its
`Ident` fields are deleted; `markErroneous` on the extension stays.
Nothing about *when* the audit fires changes.

**Rule text (design.md Polarity section, ~4410).** Replace "reports `Tag
Not In Annotation`" with: "reports a Type Mismatch in the annotation
context, showing the row the body produced against the union the
annotation wrote; the annotation bounds the definition and only
instantiation widens it."

**Tests.** The checker integration tests that assert the old title/text
(`type_checking_integration.zig`, ~11 sites) assert the new report: the
`TYPE MISMATCH` title, both type lines, and the typo hint where one
applied before (`rzysvtry`'s cases must keep their hint). The CLI test for
issue 10689 (`parallel_cli_runner.zig`, expects "tag not in annotation")
asserts the new wording. `zig build run-check-snapshots`; any snapshot that
carried the old problem is regenerated with `--update-expected`.

**Verification.** `zig build run-test-zig -- --test-filter "check type"`;
`zig build run-test-cli -- --suite subcommands --filter "10689"`;
`zig build run-check-snapshots`.

**Risks.** The expected snapshot is synthesized, not taken from a var the
solver owns; it must not leak a var into the type store that later passes
mistake for a real type (build it in the snapshot store directly if the
snapshot API allows, otherwise mint and discard under the audit's own
scope). The report region is still the annotated union, because the
producing expression is unknown once the tag has been absorbed by
unification; recording the producer would be a solver change and is out
of scope.

## 3. Sequencing and commit stack

Order: W1 (already present, move to the bottom) → W2a → W3 → W4 (pins
only) → W6a → W6b → W2b → W7 → W8 (last: it only changes how one
diagnostic renders, and it touches the same integration tests and
snapshots every other item may regenerate). W2a through W4 are
independent of each other and can be developed in parallel worktrees but
land in this order so each commit's verification is monotone. W6b
depends on W6a's fixtures. W2b is independent and lands last among the
code items so the nine unblocked programs are green early; it is not
optional (open question 1). Every commit is created with `jj new -m` before
its first edit, carries the trailer lines, and is verified in isolation
with its item's commands before the next starts; the full `run-test-zig`
and `run-check-snapshots` run after W3, after W6b, and after W2b.

## 4. Verification matrix

| Level | Command | Gate |
|---|---|---|
| Checker | `zig build run-test-zig -- --test-filter "check type"` | green after W4, W6a, and W6b's nested-position rule |
| Monotype/LIR | `zig build run-test-zig-lir-inline` | green after W3 (iterator, `OpenMethodWidenedCaller` stays green); the five 10121 cases stay green throughout (W4 pins them) |
| Stored codecs | `zig build run-test-cli -- --suite subcommands --filter stored --filter "issue 10888"` | green after W2a; unchanged lowered output after W2b |
| Where-method fixtures | `roc test --no-cache --opt=interpreter` and `--opt=dev` on the W6 fixtures | green after W6a (open impls), W6b (closed impls) |
| Platforms | `zig build run-test-zig-http-header-decoder-platform`, `zig build run-test-zig-json-decoder-platform` | green after W2a (verified 2026-09-03: all three json-decoder apps and the http-headers app build and run) |
| Diagnostic | `zig build run-test-zig -- --test-filter "check type"`, `run-test-cli` filter `10689`, `run-check-snapshots` | green after W8 |
| Everything | `zig build run-test-zig`, `zig build run-check-snapshots` | 100% after W6b and again after W2b and after W8 |

## 5. Risks and rollback

- W2a weakens one documented invariant by a declared, transitional
  exception; if a later relation ever widens a grounded protocol row, the
  `unifyTagRows` invariant surfaces it at the exact site. Rollback is
  removing the two calls. W2b restores the invariant; its risk is the
  size of the emission move, guarded by the Phase-B assertions.
- W3 changes plan classification; every consumer of the classification is
  enumerated in the commit message, and the quantified-tail exclusion is
  pinned by `OpenMethodWidenedCaller`. Rollback is restoring the two call
  sites.
- W6b restructures the hosted adapter arm into a general one; the hosted
  `?` fixtures are the regression gate. The adapter re-tags only the
  direct result row and a `Try`'s rows; every other position is a checker
  decision, so lowering cannot silently produce a wrong representation and
  never reports.
- The stack sits on a `main` that needs W1 to build; if upstream fixes the
  artifact first, rebase and drop W1.

## 6. Follow-ups, deliberately out of scope

- General row-subsumption coercions (closed values widening into open rows in
  any position), which also let a closed body publish an open row. DECIDED
  2026-09-15: this is the intended end state, not one option among several.
  Closing-by-body is a defect: it makes two functions with identical
  signatures behave differently for their callers, and
  `test/fx-open/issue_9963_hosted_try_question_mark.roc` was its standing
  witness until the hosted instance was fixed on 2026-09-16 (§9.2); a
  NON-hosted forwarder still exhibits it, and subsumption is still the fix.
  Out of scope for THIS PR only. W6b's adapter is the coercion's
  first instance and its LOWERING half is permanent; the checker-side
  `?`-condition redirect is the half subsumption deletes. Nested positions
  extend the adapter (and, under option (d), open the corresponding signature
  positions) rather than rewrite it.
- Cross-module widening of annotated weak values (currently grounded closed
  by `closeWeakValueImplicitOpenExts`, as on `main`). Grounding in the
  checker is the right boundary: a weak value has one representation, its
  row is a module-local shared variable, and importers, glue
  (`glue.zig` ~4434 already reads a defaultable tail as closed), the LSP,
  and the artifact cache all see the type `main` published. Widening it
  across modules would need the same coercion as above, not a lowering
  default.
- Re-keying open-keyed format-method specializations (moot after W2b).

## 7. Open questions for Jared

1. (Decided 2026-09-03: this PR.) W2b lands in this PR; it also carries
   the optional-field stored-codec fix, which is why W2a was kept minimal
   (option 1 of the W2a review finding).
2. (Decided 2026-09-03: artifact side, with the clone-origin record; see
   the W3 landing note.)
3. (Decided 2026-09-03: (e). **REVERSED 2026-09-14: (d)**, after recon
   showed (e)'s two enabling premises are false — markers are closed to
   `[]` before the obligation runs, and mint-time positions are not
   recorded. Per-use opening is restricted at generation time to the
   adapter-reachable positions; a nested widening is an ordinary mismatch
   at the body use. See W6b "Nested positions" for the full rationale.)
4. W6b mechanism: the adapter at the template boundary is my choice over
   the earlier draft's call-site wrap. If restructuring the hosted arm is
   judged too risky for this PR, the fallback is the call-site wrap with
   the same rule text minus "hosted is an instance" — it is a second
   re-tag site and should then be listed as debt.
5. (Closed 2026-09-03.) W5's panic is not reachable on the stack; see W4.
6. (Closed 2026-09-03.) The 10121 harness tests pass on the stack; the CLI
   controls become fixtures under W4 so the two paths cannot drift.

## 8. Handoff for the next agent (written 2026-09-03, end of the first execution session)

This section is self-contained: the session's scratchpad (research reports,
probe programs, logs) does not survive, so everything a successor needs is
either here, in `design.md`, or in the repository.

### 8.1 Where the stack is

On `main` `96c3b2fa`, bottom to top (jj change ids; every commit is
described in full, with trailers):

| Change | Item | State |
|---|---|---|
| `kusqzzsn` | W1 build fix | landed; drop at the next rebase once `main` fixes `any_negative` |
| `kupupkyt`, `wyzrkrmn`, `wlylqvxq`, `rzysvtry`, `rprvoylp` | phase-one checker work | landed, reviewed, PR #10434 (draft) |
| `wlzsxolu` | this plan | keep amending in place: edit the file in the working copy, then `jj squash --from @ --into wlzsxolu --use-destination-message polarity_phase_two.md` |
| `tsrzvryw` | W2a | landed, implemented + adversarially reviewed; callable-node grounding only |
| `wwnsvqrn` | W3 | landed, implemented + reviewed twice; full `run-test-zig` 5039/5046 (7 skipped) and `run-check-snapshots` clean on its tree |
| `vrpryvko` | W4 | landed, implemented + reviewed; tests and fixtures only |
| `wrtzpoum` | design.md widening decision | landed |
| `ktlykkxv` | W6a | landed; producer, lifecycle, codec dominance, serialization/recheck, `requires_record`, and combined LIR focused gates green |
| `vruxrtsl` | W6b, Monotype half | landed + pushed; three probes green on both backends, postcheck 457/457, lir-inline at baseline parity, W6a gate 2/2. Adversarial review in flight |

Bookmark `jared/polarity` points at `vruxrtsl` (W6b Monotype half) and is pushed.

W6b's Monotype half needed no change to `solve.zig`: the widening relation
never unifies the differing rows, so `RowWidthRelation` is untouched and
neither `unifyTagRows`'s panic nor the `relateOpaqueInterface` twin is
reachable from it. It applied at NINE relation sites, not the six the plan
named — the evidence target root, the draft-template path, and interface
replay also unify a closed root with a widened request — and adapter spec
jobs had to route to the coordinator from two dispatch points, not one.

**As pushed, `vruxrtsl` is NOT landable.** Adversarial review found three
blockers, all of which turn a previously loud panic into a WRONG RUNTIME
VALUE. Two claims in that commit's message are also false and must not be
trusted: there is no `hasPrivateTypeDestination()` guard in the pre-step,
and the borrow-across-transaction fix it claims is not in the diff
(`requested_args` is still spanned before `beginTransaction` and duped after
`commitTransaction`). Both verified directly in the source.

| # | Defect | Consequence |
|---|---|---|
| B1 | `resultRowWideningOrNull` accepts a widening at ANY type argument of a same-definition nominal — a `Try`'s **Ok** arg, or any alias type argument. The gate computes a `ClosedResultRow` but never passes it to the relation, and `resultRowWideningAdapterSourceType` narrows only the ERR row, so it returns null and no adapter is minted while the rows stay unrelated. | miscompile |
| B2 | On the `local_context_dependent` path the relation is honoured but the body is lowered at the declared row and no adapter is generated — the adapter exists at exactly one site, the relation at nine. | miscompile |
| B3 | The pre-step mixes worker-workspace and coordinator type ids (`lowerType` writes to `activeTypeStore()`, `sameMonoType` reads `program.types`). In release this is an arbitrary in-bounds read. | miscompile |

Root cause, and the fix to make: **three sources of truth for "is this row
closed"** — the graph (`tagRowIsClosed`, unresolved ext ⇒ open), the
dispatch callable's checked type (a fresh clone), and the template root's
checked type. The relation uses the first two, the adapter the third, and
every gap between them is a wrong tag discriminant. The relation must
RECORD the widening it performed and completion must CONSUME that record,
instead of re-deriving the answer from a different type. Every site that
declines to unify must fail CLOSED: assert an adapter is reachable, so an
uncovered site restores the panic rather than silently miscompiling.

Also to fix: the closedness rule classifies an ordinary implicitly-open
annotated result row as closed, so the adapter fires for a broad class of
ordinary programs (not just where-method uses, contradicting this plan's
own premise that "only where-method uses can reach it") and pulls them out
of parallel body shards onto the coordinator — a compile-time regression.
And the pre-step's capability invariant is evaluated before the predicate
that says the path is relevant, adding release-mode UB sites on a hot path.

### 8.1.1 W6b restart (2026-09-14)

A first W6b attempt ran from `ssvqsxro` through 23 WIP commits and was
**reverted**: it grew `Check.zig` from 37,525 to 112,958 lines (+59,292 in
one commit), added 42 side-table `SafeList` fields to `ModuleEnv`, and left
the full checker gate red (1,257/1,383 pass, 6 fail, 120 crash) with W6b's
actual deliverable — the result-row widening adapter — still unwritten.

Cause, for the record: W6a's raw-witness validators were strict enough to
reject PRE-EXISTING, unrelated checker state, and each rejection was
answered by building more checker machinery instead of loosening the
validator or deferring the case. Several detours (`Node` payload width,
`SmallStringInterner` quadratic validation, `checkExpr` frame size) were
defects that also exist on `main` and are not polarity.

That stack is preserved at bookmark `jared/polarity-w6b-archive` and is not
part of this PR. **Standing rule for the rest of this plan: fix polarity;
do not fix pre-existing `main` defects. A pre-existing defect is reported,
not repaired, and never worked around with new machinery.**

### 8.1.2 W6b decisions taken at restart

Recorded here rather than rediscovered later:

- **Closed-result-row predicate.** The pre-step uses the
  `CheckedTypePayload.variableSealsToRowDefault` rule (`.rigid => false`,
  no constraints, no numeric default phase), NOT `checkedTypeIsClosedTagRow`
  (which counts a rigid carrying an empty-tag-union default as closed). A
  rigid result row is parametric — the caller supplies it — so the other
  rule would mint adapters for polymorphic templates. This keeps W6b on the
  same closure rule W3 and W6a share.
- **The widening width is structurally non-recursive.** It applies to
  exactly the result cell and a `Try`'s two type arguments, then drops back
  to `.exact`. `relateMatchingRequestContainers` otherwise propagates one
  `row_width` uniformly through `.func` argument and result positions
  alike, and `RelationStamp` memoizes on `row_width`, so a mode that leaks
  into an argument or a nested `List`/record/tuple/tag-payload position
  would be a memoized silent wrong-representation bug. Arguments stay exact.
- **Six relation sites, two panic sites.** The request relation is doubled
  at every dispatch call site: `instantiateTargetFromPlanNode`,
  `methodTargetNodeFromPlan`, and the four follow-up relations after each
  `methodTargetNodeFromPlan` call. The twin panic `"opaque interface
  relation widened a closed tag union"` (`relateOpaqueInterface`) is
  reachable alongside `unifyTagRows`'s.
- **Verification must be Debug.** `Common.invariant` compiles to
  `unreachable` in release, so a release-mode run proves nothing about the
  panic being gone.
- **`lowerType` collapses rigids silently.** `lowerCheckedTypeVariable`
  returns the empty tag union for a variable with no row default, so
  deriving a `.roc` template's declared row from `lowerType` of the checked
  root yields a silently wrong type. The declared row comes from the
  request, guarded explicitly.

### 8.1.3 Grounding pass against the code (2026-09-14)

Every remaining item was re-grounded in the source before implementation.
Corrections to sections 1-7, which are NOT amended in place so the drift
stays visible:

**Verification tooling — the most important finding.** Only two mechanisms
can prove the widening adapter actually ran: counting Monotype fns whose
`source.fn_def == .checked_generated`, or the specialization counters
(`monotypeCountersForModule` / `expectMonotypeSpecializationCountersWithin`).
**Every CLI fixture is mechanism-blind** — `OpenMethodWidenedCaller` asserts
only exit success, "All (1) tests passed", and the absence of panic needles,
so it passes identically whether the program compiles to one wide
specialization or to an adapter plus a narrow one. `lir_inline_test` runs the
LIR interpreter ONLY, so dev-backend coverage exists solely through
`test/cli`. A value-only probe must therefore use a tag that SORTS BETWEEN
the declared tags and observe every constructor, and even then it cannot say
which mechanism ran.

**There is no snapshot of lowered output anywhere.** The `# MONO` snapshot
section is a CIR re-emitter (`snapshot_tool` `generateMonoSection`), not
Monotype/LIR. W2b's stated gate — "`run-check-snapshots` must show no
lowered-output change against W2a" — is therefore **impossible as written**.
Substitute a Monotype view equivalence (`expectEquivalentMonotypeProgramViews`)
or spec-count equality in a lir test.

**W6b / B1.** Confirmed structurally. `resultRowWideningOrNull` accepts a
widening at EITHER `Try` type argument, while `closedResultRowOrNull`,
`checkedRootHasClosedResultRow`, `requireLoweredDeclaredRowLabels`,
`resultRowWideningAdapterSourceType` and `resultRowWideningAdapterBody` all
handle only `args[1]` (the error row). That asymmetry is the bug, and the fix
is a five-function lockstep change.

**W6b / B2 — the plan's reproduction recipe is wrong.**
`local_context_dependent` is driven by a `.local_proc` evidence TARGET
(`specEvidenceLocalOwner`), not by "a dispatch from inside a generalized
local scope". A dispatch inside `helper = |r| ...` does NOT make the target
`.local_proc`; the closed-row template's own evidence must point at a
lambda-bound method declared by a statement inside a function body. A probe
written from the plan's description passes and proves nothing. The miscompile
at the inline-body path stays UNPINNED until a fixture reproduces it. Note
the separate `.local_proc` route through `lowerDraftNestedFromContext`
PANICS rather than miscompiling.

**W6b rule text does not exist.** Five code comments cite a design.md
"Result-Row Widening Adapter" section that was never written.

**A `Try`'s OK row is not adapter-reachable, and never was.** This plan says
throughout that the adapter re-tags "the direct result row and a `Try`'s
rows", plural. That is wrong: `closedResultRowOrNull` returns
`nominal.args[1]` (the error argument) and nothing for `args[0]`, and
`hostedTryReturnInjectionExpr` asserts `sameMonoType(ok, ok)` with
`Common.invariant("Try adapter changed Ok type")`. A probe widening a `Try`'s
ok row against a closed implementation panics on both the pre-W6b and
post-W6b binaries. **The adapter-reachable set is exactly: the direct result
row, and a `Try`'s ERROR row.** Decided 2026-09-14: both halves are
restricted to that set rather than extending the adapter to the ok row —
the relation relates every other type argument (a `Try`'s `args[0]`, every
alias type argument) at `.exact` so a widening there restores the loud panic,
and per-use opening is withheld from those positions in the checker. An
ok-row widening is therefore an ordinary mismatch at the body use, like any
other nested position. Extending the adapter to the ok row is a follow-up
under §6, not part of W6b.

**W2b.** The four BodyContext restores are confirmed (line numbers drifted
by roughly +510-530), but the Builder-level `restoreConstParserRuntimeFnExpr`
takes its OWN eager resolved view, so the optional-field fix needs SIX sites,
not four, and the plan's claim that the Builder-level restore "is unaffected"
holds only for prepared codec calls. `groundRowDefaults` has ONE production
caller plus five in unit tests — not the thirteen the plan feared. The
existing deferred-structural boundary record cannot be reused as-is: it
carries a dispatch plan and structural evidence where the stored restore
needs a const fn value, a store view, capture lets and a precomputed parser
plan. Sizing: ~500-800 lines.

**W2b follow-up: sites 1-2 are harder than "they also have an eager view".**
The two Builder-level restores prepare NO codec calls at all — they lower
format-method calls through the ordinary unfrozen path. Moving their emission
behind the freeze therefore REQUIRES giving them
`prepareStructuralCodecCallsAtNode`, or they hit `Common.invariant("sealed
structural codec requested an unprepared callable")`. That is new work the
plan never contemplated, and it is why the honest sizing is ~+810/-500 rather
than the plan's 500-800.

**The plan's W2b gate is not merely impossible, its GOAL is unachievable.**
"The sealed body equals the eagerly emitted one" cannot hold literally:
deferring changes the order draft exprs are allocated, and the Phase-B tail
REPLACES the reservation with a copy of the lowered expr, orphaning the
original. Substitute gate: `structuralJsonMonotypeStatsForSource` (an existing
helper returning functions/definitions/locals/expressions/template_misses/
nested_misses from raw module source), with the W2a numbers MEASURED BEFORE
ANY EDIT, asserting equality on functions/definitions/locals, a bounded
inequality on expressions (Phase-B emission orphans one expr per boundary),
and `misses <= baseline` (W2a's open-request keying was expected to
double-specialize; W2b removes the cause, so misses may fall but must never
rise). Plus `roc build --timings` counters compared across binaries.

**Both risks the plan names for W2b are already retired by existing code.**
`enterCallableBodyDemandScope` has an explicit frozen branch that swaps in
`enterSealedCallableBodyScope`, and `constFnEvidence` only walks the evidence
chain and touches no graph. The real highest-probability blocker is instead
`ParserPrecomputedPlan`: `buildParserRestoredPrecomputedPlan` takes a sealed
`shape_ty`, so it must move to Phase B, where it calls `restoreConstNodeAtType`
for `Str` field-name literals — which can in principle reach
`constrainTypeToMono`, i.e. relation production, after the freeze.

**Appendix A's path is site 4** (`restoreConstParserRuntimeFnAtNode`), proven
because the registered `ParserTopLevelStoredParser.roc` fixture is Appendix A
minus the `bar ?: Str` field and passes today. But WHICH eager view panics is
still open, and there is a genuine contradiction to resolve: an annotated `?:`
field pins `.optional` concretely (so its slot is closed, not unresolved), and
an `.undetermined` field kind is rejected for a compile-time root by
`checkedFieldTypesAreConcreteCompileTimeRoots` — so BOTH documented mechanisms
for an unresolved cell are excluded on paper. The unresolved cell is something
else, most likely inside the generated parser's protocol rows. Settle it by
running the fixture on the pre-change binary in Debug and reading the frame;
it does not change the design, since all views at those sites move to Phase B.

Fan-out is 14 points, derived by diffing the two existing deferred lists
(record, store field, `.empty`, two deinit loops, two discard-after-seal
points, the discarded-state assertion, the Phase-A fixpoint arm and its
`.pending_deferred` assertion, and THREE Phase-B emit call sites — the shard
path, the ordinary spec-job seal, and the coordinator commit — not one).
Nothing here is serialized, and it must stay that way.

**W8.** `findBestTypoSuggestions` (`report.zig`) is the wrong citation — it
handles RECORD-FIELD typos; the tag typo hint comes from the snapshot diff
and arrives automatically once real snapshots are supplied, so the bespoke
hint is simply deleted. A type snapshot CANNOT be built without minting a
var, and "mint and discard under a probe" is not viable because probe
rollback truncates the type store and invalidates the region index the report
needs — so mint for real at the entry's region. The audit does not currently
hold the annotated UNION var (only the ext var and the listed tags), so
`ImplicitOpenExt` must carry it, added at three mint sites; the alias-marker
site is the hard one, having neither the union var nor a reliable region.
Eleven integration-test sites and exactly one CLI test, both as the plan
says; ZERO snapshots carry the old title, so that regeneration step is a
no-op. Sizing: ~150-250 lines.

**W8 follow-up: the alias-marker problem does not exist.** A deeper pass
refuted both halves of the difficulty recorded above.

- *The region is reliable.* Every call path that can open a marker passes
  `.{ .explicit = anno_region }` to `recordOpenedMarkerExts`; the
  `Region.zero()` branch is dead code. Proven behaviourally: the existing
  test at `type_checking_integration.zig:2797` already renders a caret under
  the annotated alias through exactly this path.
- *The union var is obtainable* — one line in `instantiate.zig`'s
  `stepTagUnion.await_ext` stage, where the marker and its union meet and
  `listed_tags` is already attached.
- **But W8 does not need it.** Synthesize the "actual" row as
  `{ tags = listed_tags, ext = entry.var_ }`. `TypeWriter.gatherTags` and
  `diff.gatherTagsFromUnion` both flatten ext chains, so this renders exactly
  the listed tags plus EVERY extra tag — the plan's "all of them, not only
  the first" — and is identical to what the real union var would render,
  because that var's content is literally that pair at mint time. This keeps
  W8 out of `src/types/instantiate.zig` entirely, adds no `ImplicitOpenExt`
  field, and is strictly safer: `makeMismatchReport` degrades to "the
  difference is not visible in this display" when both sides format
  identically, and a synthesized actual carrying `ext = entry.var_` cannot
  collide, since the audit has already proved that ext carries ≥1 tag.

Two further corrections: the report arm should pass
`ProblemRegion{ .direct = ctx.region }` rather than
`regionIdxFrom(actual_var)` — `getRegionSafe` SILENTLY drops the caret when a
var's index is past the region list, and `.direct` reproduces today's caret
byte-for-byte while removing all dependence on where synthesized vars land.
And `snapshot/diff.zig` carries a stale comment claiming the polarity audit
shares its typo helper; after W8 the audit no longer calls it, so the comment
must be rewritten while the function stays.

Exactly ONE exhaustive switch over `Context` exists repo-wide (in
`report.zig`), so the new context variant has a single arm to add.

### 8.1.4 Second review round (2026-09-14): the structural fix is still owed

The blocker fixes closed B1 and B3 but the same defect SHAPE reappeared at
new sites, because the prescribed root fix was skipped.

| # | Defect | Status |
|---|---|---|
| B1' | `.defer_open` is position-blind. A DECLARATION REFERENCE at `.result`/`.try_row` defers every marker beneath it, at any depth, because the instantiator flips polarity only through `stepFunc` — `List`/record/tuple/tag-payload args never demote. So `Statuses : List([Ok(Str), Err(Str)])` as a where-method result reopens the nested marker. | CONFIRMED BY PROBE: checks clean, then panics in `unifyTagRows` |
| B2' | `.local_proc` dispatch targets decline at three sites where the adapter is unreachable BY CONSTRUCTION (a local proc has no `checked_fn_root` and never reaches `completeTemplateReservation`). Prevented only by an incidental exact re-relate, which is `Common.invariant` — `unreachable` in release. | code-confirmed |
| B3' | The relation is handed `lookup.target.callable_ty`, completion uses `template.checked_fn_root` — and it is `checked_fn_root` that is the SUBSTITUTING clone (`specializeRoot`), the opposite of what was assumed. Closedness is id-independent for a fresh tail, but a substituted tail flips the predicate, fail-open and unguarded. | code-confirmed |

Also: the fail-closed assert added for B2 is STATICALLY DEAD (its condition
already contains `!local_context_dependent`), and both new `lir_inline_test`s
are COMPILE-ONLY — `lowerMonotypeModuleWithOptions` never runs the
interpreter, so they pin "an adapter was minted" and no value correctness. An
adapter with a wrong tag mapping passes both.

**Why it recurred: an over-broad scope rule.** This plan's restart rule was
written into subagent briefs as "no new side tables", and the implementer
therefore skipped the prescribed fix — "the relation RECORDS the widening it
performed and completion CONSUMES that record" — because it appeared to need
new state. That is the wrong reading. The explosion this branch is recovering
from was 42 SERIALIZED `ModuleEnv` tables plus validator families; a value
threaded along a call path is not that. **Corrected rule: prefer the smallest
state that solves the problem, and add a table when one is genuinely needed,
justifying it. What is forbidden is unjustified proliferation, fixing
pre-existing `main` defects, and exceeding a stated budget without stopping.**

Required for the next round, in priority order:
1. One source of truth: the relation records the widening; completion consumes
   it. No re-derivation from a different type id.
2. Fail-closed at ALL FIVE declining sites, with live assertions.
3. `.defer_open` becomes position-aware: defer only a marker on the
   instantiation root's own row or the root `Try`'s `args[1]`; close every
   marker reached under any other constructor.
4. Tests that EXECUTE, not merely lower.
5. `annoApplyIsBuiltinTry` gates on `apply.base == .builtin`, not ident text
   (shadowing `Try` is only a warning and the local binding wins).

### 8.1.5 Whole-space sweep (2026-09-14): the design IS salvageable

A systematic enumeration of every relation/unification site against a
request, every route into template completion, and every route that lowers a
body around it, answers the question this plan kept re-opening.

**Why four rounds found the same shape.** Three independent recognizers each
answered "is this a widening?" from a different representation — the checked
type (`closedResultRowOrNull`), the graph (`resultRowWideningOrNull`), and the
mono type (`resultRowWideningAdapterSourceType`) — and every relation site had
to re-derive "can an adapter serve me?" by hand. Any new site, or any
representation the three disagree on, reproduces the defect. That is
structural, not bad luck.

**The fix now in the tree is the right one.** Recording the decline once and
consuming it once makes the decline and the reservation the same fact, and
`AdapterReachability` means a site that cannot reach completion cannot
decline. Three leaks remain, in priority order:

1. **One recognizer, not three.** `closedResultRowOrNull` resolves aliases,
   and the checker treats an alias as transparent for reach — but the GRAPH
   recognizers refuse aliases (`kind == .nominal` required, `isBareTagRowNode`
   rejects `.named`, and an alias's def is not `Try`'s). So for
   `IoResult(a) : Try(a, [IoErr(Str)])` the checker opens per use, lowering
   declines to recognize, the exact relation fires, and `unifyTagRows` panics.
   HIGH likelihood on real code — hosted rows are always closed, so `?`
   through an aliased hosted result is the natural driver. Best fix: have the
   CHECKER publish the widening per call edge in the checked artifact (it
   already publishes `hosted_try_adapter`, where-method scheme-use records and
   the hosted-widening redirect rule), so lowering never recognizes at all.
2. **No hard-coded `false`.** Three durable-type routes into completion pass
   `widened_result_row = false` unconditionally. They should assert that no
   widening source type exists for that request and `compilerBug` otherwise,
   rather than silently taking the body path.
3. **The backstop must be loud in EVERY mode.** Every "loud" rejection in the
   table bottoms out in `Common.invariant`, which is `unreachable` outside
   Debug. The widening-specific guards were upgraded to `compilerBug`, but the
   one invariant whose violation actually changes emitted code — the closed-row
   rejection in `unifyTagRows` / `relateOpaqueTagRows` — was not. Promoting it
   converts every remaining hole in this section from a wrong tag discriminant
   into a build stop, in release too. Cheapest and highest-value of the three.

Two further findings, both unpinned:

- **For-clause aliases bypass instantiation.** An `is_for_clause_alias`
  annotation unifies the app's alias DECLARATION var — whose body carries
  markers at every depth — directly into the annotation, with no polarity or
  reach resolution. If that annotation is a where-method signature, the nested
  markers are then opened per use. Needs a platform test
  (`requires { Model }`, `Model : { items : [Pending, Done] }`, and a
  where-method returning `Model`).
- **A second declining relation exists.** `relateCustomParserErrorInjection`
  relates only shared labels' payloads and never checks closedness — the same
  defect shape in the parser-error-injection mechanism, whose adapter is the
  parser runtime rather than the template adapter. Not audited; out of W6b's
  scope but it should be recorded as debt rather than forgotten.

### 8.1.6 The `unify_test` failure is OURS, from W6a (settled 2026-09-15)

`unify_test` "declarative static dispatch representative survives repeated
merges" fails with `expected @enumFromInt(5), found @enumFromInt(6)`. Four
separate agents called it pre-existing. **It is not.** Each only checked its own
parent commit, and every one of those parents sits above the culprit.

The test NAME exists on `trunk()`, which is why the mistake was easy. But the
failing assertions were ADDED by `ktlykkxv` (W6a), which is the only commit on
this branch that touches `src/check/test/unify_test.zig` or `src/check/unify.zig`.
It was authored and never observed green: W6a's own verification paragraph claims
focused gates, none of which included this test.

Cause, derived in full rather than guessed. W6a plumbs the receiver operands as
RESOLVED vars, and the retained side is recorded from the a-side. `publicResolved`
returns the storage's checked var, and `Store.union_` always keeps B as the
surviving checked representative. So the first unify records the caller's own var,
that merge makes the other var the class representative, and the second unify
records THAT one. Nothing extra is minted; the test simply spells the retained
receiver with a var that stopped being the representative after the first merge.

**Severity: low.** Every production consumer reads `retained_receiver_var` only
through `resolveVar(...).var_`, and only to compare it against the omitted
receiver's root; `recordGeneralizedDispatchTargetShare` is called with the
OMITTED var, never the retained one. Both vars are in the same equivalence class,
so recording either is behaviourally identical. The field is a validation witness,
and its doc comment calling it the "Raw receiver" is the misleading part — it is
the class's checked representative at merge time, which the store explicitly does
not promise to be the caller's var.

**Fix:** one line, test-side — compare roots rather than spellings at the second
assertion, and tighten that doc comment. Do NOT "fix" the production code; there
is no defect there.

(Moot after the 2026-09-15 rebase: the unifier candidate, its test, and
`recordGeneralizedDispatchTargetShare` were deleted with the share table.)

### 8.1.7 Two branch test failures are OURS, from phase one (settled 2026-09-15)

**Status 2026-09-16: BOTH ARE NOW FIXED.** issue_9826 by the two-case split
below; issue_9963 by the reversal recorded at the end of this section and in
§9.2. The analysis is kept because it is the diagnosis the fixes rest on.

Both fail on the branch and both PASS at `kusqzzsn`, measured with a
marker-checked binary in a comparison workspace. Neither is pre-existing.
They are not both fx-open: issue_9826 is a `test/cli` fixture registered in
the `subcommands` suite, and only issue_9963 is under `test/fx-open`.

**issue_9826 — a rejection that stopped happening.** `wyzrkrmn` deleted the
exact `..` the test exists to reject, in a HOSTED lambda annotation:
`line! : Str => Try({}, [LineErr(IOErr), ..])`. Hosted annotations are
`.as_written`, so that `..` was load-bearing; with it gone there is nothing
left to reject and `roc check` correctly reports no errors. Exactly ONE
host-boundary position was touched by that commit, and this is it. Note the
compiler never advised this strip: a hosted annotation records no
implicit-open ext, so no redundant-open warning fires there. The edit was
manual over-reach. The entry does not silently pass with nothing to assert:
it asserts `.exit = .failure` plus two needles, so a clean check FAILS it, and
it is one of the branch's baseline failures.

**RESOLVED 2026-09-15 as a two-case split, which is strictly more coverage
than the original.** `test/cli/issue_9826_open_host_boundary/hosted/` gets its
`..` back and stays the NEGATIVE case, so its registration line is unchanged.
A new sibling `hosted_no_ext/` (a copy of the directory as `wyzrkrmn` left
it) is the POSITIVE case, registered as `"issue 9826: roc check accepts an
extensionless hosted signature"` with `.exit = .success`. Its value is
polarity-specific and nothing else covers it: host boundaries opt out of
implicit opening, so an extensionless hosted row must stay closed; if that
opt-out ever regressed the row would be implicitly opened, the closed-row
check would reject it, and this case would fail loudly.

**issue_9963 — a valid program that stopped compiling.** The obvious lead was
wrong and is recorded here so it is not re-tried: `via_question!` and an app's
`main!` are NOT host-boundary positions. An app `main!` is not a `provides`
def, and `via_question!` has a body so it is not a hosted lambda. Measured:
restoring only `Fallible.roc`'s `..` while leaving `main!` closed gives a
clean check, so the seven app `main!` strips were genuinely redundant.

The real mechanism is `kupupkyt`, not `wyzrkrmn`. That commit collapsed a
written `..` and an absent extension into the SAME recorded flex in ordinary
output positions. `via_question!`'s body is `Ok(host_call!({})?)`, so at the
`?` the probe `expected=[HostErr(Str), ..flex]` against `actual=[HostErr(Str)]`
now SUCCEEDS by binding the flex to `[]`. Hosted Try Question Widening
therefore declines, the binding is performed for real, and `via_question!`
generalizes with a CLOSED row — after which the caller's `?` is rejected. At
base the same `..` was the rigid `#others`: the probe failed, the rule fired,
the row stayed open.

**Restoring `..` does NOT fix this one** — it is now the same flex either way.
The consequence is broader than one fixture: after `kupupkyt` there is no
longer any way to spell "this output row stays open" on a function whose body
produces a closed row. design.md claims this pairing "now arises only where
closed rows still exist"; issue_9963 is a live counterexample. That design.md
sentence was corrected on 2026-09-15 in the same change that recorded the
decision below. (Still true for a NON-hosted body as of 2026-09-16: the fix in
§9.2 restores the open publication for a `?` on a direct hosted call by making
the rule decide instead of the probe, not by restoring a spelling.)

**Recorded 2026-09-15 as KNOWN RED, DELIBERATE — REVERSED AND FIXED
2026-09-16.** The position taken here was:

> `test/fx-open/issue_9963_hosted_try_question_mark.roc` stays failing. It is
> the standing witness that closing-by-body must be replaced by row
> subsumption (design.md "Polarity"): its platform module holds two functions
> with identical annotations whose callers are treated differently because one
> forwards a closed hosted row with `?` and the other reconstructs it with
> `match`. It is not stale and not pre-existing: it passes at `kusqzzsn` and
> regressed with `kupupkyt`. Do not patch or disable it: every available patch
> is a host-specific special case this design intends to delete. The harness
> offers no expected-failure status; `CliCase.skip = .{ .always = ... }` would
> only silence the two `subcommands` cases, because `SimpleTestSpec` has no
> skip field and the `test/fx-open` platform cases it generates cannot be
> skipped at all. So the test stays simply red, and the PR description carries
> the reason.

Everything in that block about the MECHANISM is still accurate — including the
regression attribution to `kupupkyt` above. What was wrong is the last
inference: that every available patch is host-specific. §9.2 below records the
measurement that falsified it and the fix that landed. The fixture is green,
its three registrations pass, and a second witness of the same defect
(`test/cli/SpecConstrInlineScopeRebaseGrowth.roc`, which forwards
`Fallible.via_question!({})?` one level deeper through its own `read_line!`)
went green with it. No fixture was edited.

### 8.1.8 W2b closeout (2026-09-15)

**What W2b actually achieved: a narrowing from four sites to two, not an
elimination.** The four `BodyContext`-level stored-codec restores
(`restoreConstParserRuntimeFn`, `restoreConstParserRuntimeFnAtNode`,
`restoreConstEncoderForRuntimeFn`, `restoreConstEncoderForRuntimeFnAtNode`)
now prepare in Phase A and emit in Phase B, from sealed types only. The two
Builder-level restores (`restoreConstParserRuntimeFnExpr`,
`restoreConstEncoderForRuntimeFnExpr`) do NOT, and W2b never claimed
otherwise — they build their body in a private graph they create and destroy
themselves, seal it with `sealActiveBodyDraft`, and still take eager resolved
views inside it (`resolvedCheckedTypeView` of the dispatcher). They are the
last eager codec consumers, and 8.1.3's follow-up note explains why moving
them is separate work: they prepare no codec calls at all, so deferring them
requires giving them `prepareStructuralCodecCallsAtNode` first. design.md's
three over-claiming sites (~4633, ~5976, ~7281) were corrected to say this
rather than "there is no exception".

**The W2a chokepoint and `groundRowDefaults` are fully deleted.** Both the
non-frozen branch of `resolvedPreparedCodecCallsForBoundary` (which called
`InstGraph.groundRowDefaults(prepared.callable_node)`) and the
`InstGraph.groundRowDefaults` method itself are gone; the whole
`resolvedPreparedCodecCallsForBoundary` helper is gone with them. `grep -rn
groundRowDefaults src/` returns nothing. W2a's declared design.md exception
went with it.

**Three deviations from the plan's W2b text, all deliberate.**

1. *The precomputed-plan builder moved to Phase B, not Phase A.* The plan
   listed `buildParserRestoredPrecomputedPlan` among the Phase-A steps. It
   takes a sealed `shape_ty`, and in Phase A the shape is exactly what is not
   yet decided — asking for it there is the eager view W2b exists to remove.
   It runs in `emitStoredParserRuntimeBody` on `sealer.sealNode(boundary.shape_node)`.
   8.1.3 flagged the risk that its `restoreConstNodeAtType` calls for `Str`
   field-name literals might produce relations after the freeze; measured, they
   do not.
2. *`addFn` keeps a graph-node `mono_fn_ty`, not a `.sealed` one.* The plan
   asked for `.sealed`. The pre-W2b eager restore already passed
   `DraftTypeCell.fromGraphNode(request_fn_node)`, so the graph-node cell is
   the faithful preservation and `.sealed` would have been a behaviour change
   smuggled in under a refactor.
3. *The gate is a Monotype-footprint comparison, not a snapshot comparison.*
   The plan's gate ("`run-check-snapshots` must show no lowered-output
   change") is impossible — no snapshot carries lowered output. 8.1.3
   proposed `structuralJsonMonotypeStatsForSource` with a window on
   expressions and locals; the window turned out to be unnecessary and was
   removed. `stored_parser_gate_source` measures fns=10 defs=11 exprs=535
   locals=108 misses=14/0 both before and after W2b — an exact match, because
   the eager path already reserved and filled, so deferring orphans nothing
   new. Two further gates (`stored_parser_optional_gate_source`,
   `stored_encoder_optional_gate_source`) pin W2b's OWN numbers; they cannot
   be equivalence gates because those programs panicked before W2b.

**Residual notes for whoever reads this next.**

- *A second cross-phase `Type.TypeId` survives, and it is safe.*
  `Builder.RestoredConstSourceCapture.ty` is a Phase-A `Type.TypeId` carried
  in `boundary.source_captures` into Phase B. Unlike the deleted
  `expected_ret_ty`, it is never compared against a sealed type: Phase B feeds
  it to `fn_ctx.draftTypeCell`, i.e. `DraftTypeCell.fromActiveType`, which
  reconnects it to its graph node when the graph still holds a snapshot for it
  and otherwise falls back to a sealed cell. It re-enters the graph rather
  than asserting about it, so a stale pre-freeze view cannot silently pass.
- *`BodyContext.lowerCallableEvalBindingValue` is dead code.* No caller: the
  one `self.lowerCallableEvalBindingValue(...)` call site is inside `Builder`
  and resolves to `Builder`'s own same-named function, and both live call
  sites use the `AtNode` variant. This predates W2b (it is dead at
  `b6648d95` too), so it was left alone; `structural_test.zig` pins only the
  `AtNode` name, so deleting it is safe whenever someone wants to.
- *Nothing in the repository exercises the two `Type.TypeId`-shaped restores.*
  Measured 2026-09-15 with a temporary `std.debug.print` in each of the six
  restores and both Phase-B emitters, over every `.roc` file under `test/`
  (330 in `test/cli` via `roc test --no-cache`, 561 elsewhere via both
  `roc test` and `roc build`). Fifteen programs reach a stored-codec restore:
  twelve `test/cli` fixtures plus `test/http-headers/app.roc`,
  `test/json-decoder/camel_app.roc` and `camel_direct_app.roc`. EVERY one of
  them goes through `restoreConstParserRuntimeFnAtNode` or
  `restoreConstEncoderForRuntimeFnAtNode` and then the matching Phase-B
  emitter. ZERO reach `restoreConstParserRuntimeFn` /
  `restoreConstEncoderForRuntimeFn` (the `ty: Type.TypeId` shapes, reached
  only through `BodyContext.restoreConstFn`), and ZERO reach the Builder-level
  `restoreConstParserRuntimeFnExpr` / `restoreConstEncoderForRuntimeFnExpr`.
  So the two restores W2b changed most — the ones that lost `expected_ret_ty`
  and gained the `sameClass` deferral assertion — are covered by no test, and
  the two "last eager codec consumers" design.md now documents are not
  exercised either. The probe was removed before the final build. Two open
  options, for Jared: build a fixture that forces the `Type.TypeId` path (the
  W2b review sized a nested stored-codec fixture at 2-4 hours, needing two
  full format protocols and a nominal whose tag payload is a function), or
  collapse the `Type.TypeId` shapes into their `AtNode` twins.
- *Two `Common.invariant`-guarded result-type checks remain in the
  Builder-level restores* (`lower.zig` ~11355 and ~11482, "stored parser /
  encoder_for constructor result type differed from restored function type").
  They are pre-existing on `main` and were left alone. They are the Builder
  analogue of the `expected_ret_ty` checks W2b deleted, and they are sound
  there for a different reason: `fn_ctx.sameType` compares two types in the
  restore's own private graph within one phase, so there is no cross-phase
  staleness to hide. The two Phase-B retype guards, which DO retype an
  expression on the strength of the comparison, were changed from
  `Common.invariant` to `Common.compilerBug` so a release build reports
  instead of reinterpreting a layout.

### 8.2 The working agreement Jared set (binding)

- This session's driver owned jj; subagents never ran state-changing jj
  commands. One scoped commit per item, created with `jj new -m` BEFORE
  the first edit, described up front, finalized with `jj describe` when
  done. Never merge; rebase only.
- Per item: one implementer, then one adversarial reviewer on the diff,
  then the implementer applies the review's fixes, then finalize. The
  reviewer does not edit source.
- **If an implementer or reviewer finds anything the plan does not
  anticipate, stop, take it to Jared, and do not continue that item
  until Jared answers.** Do this even for good news (W4/W5 collapsing
  was handled this way).
- Production quality, not a prototype; long-term compiler health over a
  local optimum. Every behaviour change is a declared rule in
  `design.md` and is pinned by tests at each level it touches.
- Jared's answers so far are recorded in §7 and in each item's "Landed"
  note. Question 3 is closed: Jared chose option (e) and explicitly gave the
  driver the green light to continue the planned work autonomously. Ask only
  if implementation exposes a typing or lowering policy not covered by this plan.

### 8.3 What is next, in order

W6a is implemented and its focused verification is complete. Next: W6b → W2b (also owns the optional-field
stored-codec fixtures, Appendix A) → W7 → W8. Each
section above is the specification; the "Landed" notes on W2a/W3/W4
show the level of detail expected in a commit and what the reviewers
looked for. Verification matrix in §4.

Facts that were only in the lost scratchpad and matter for W6:
- Where-method widening lowered for OPEN implementations in the original
  diagnosis through a same-name match. The deliberate route is the exact raw
  `SchemeUseRecord.where_method_use`; when generalized dominance omits the
  callable named by the plan, upstream's committed-probe unification has
  already made the omitted class the retained one, so no separate share row
  exists (the branch's `GeneralizedDispatchTargetShare` table was deleted at
  the 2026-09-15 rebase).
- An independent callable synthesizes nested evidence unless its where-use
  record names the slot's own signature, in which case it reuses the slot's
  nested evidence. `requires_record` needs the latter and is pinned by the
  direct Monotype accepted/rejected gate plus the combined W6a LIR fixture.
- In generalized deduplication the retained requirement keeps either side's
  codec flags (upstream's flag-OR merge), so encounter order does not matter.
- Focused verification: `zig build run-test-zig-module-check --summary
  all -- --test-filter "scheme use"` passed 7/7 steps and 2/2 tests. Putting
  `--summary all` after `--` is rejected by the test runner, so the literal
  trailing-option form is not a valid verification command. The direct
  roundtrip/cache gates—including cold/warm preservation of exact where-use
  records—accepted/rejected cross-module codec gate, Monotype
  `requires_record` gate, and combined W6a LIR regression were green before
  the rebase; see W6a's verification paragraph.
- It panics only when the implementation's own return row is closed in
  its scheme (body returns a top-level constant, an input-position
  parameter, or a nominal field): `instantiateTargetFromPlanNode` →
  `relateFunctionRequestInterface` → `unifyTagRows` "instantiation
  widened a closed tag union". Appendix B programs marked "panics".
- The hosted `?` adapter (`relateHostedTryWidening`,
  `errorRowInjectionExpr`, `hostedTryReturnInjectionExpr`,
  `completeTemplateReservation` `.hosted` arm) is the mechanism W6b
  generalizes.
- Stored-codec restore facts for W2b are in the W2 section; the
  optional-field panic reproduces on `main` + W1, at the restore's first
  eager shape view (`lower.zig` ~33786/~33935 and the encoder twins).

### 8.4 Standing brief given to every subagent (copy verbatim into each brief)

- Checkout `roc-2`; never touch the sibling `roc/` checkout. Use a
  session scratchpad subdirectory per item for logs and temporary files.
- Never run a state-changing jj command (`new`, `commit`, `describe`,
  `squash`, `edit`, `abandon`, `rebase`, `restore`, `undo`, `bookmark`,
  `git push`, `workspace`). Reads: `jj --ignore-working-copy ...`. Note
  `--ignore-working-copy` shows the LAST SNAPSHOT: run plain `jj status`
  once first so uncommitted edits are visible to `diff -r @`.
- Builds are slow (`zig build roc` and full suites can exceed 10
  minutes). Run long commands in the background with a log file. NEVER
  end a turn while a background build or test is running: wait with a
  polling loop in a foreground call with a long timeout
  (`until ! pgrep -f 'zig build roc' >/dev/null; do sleep 20; done; tail <log>`),
  repeating the call if it times out. Report only when every
  verification result is in hand.
- Smallest module-scoped step first, then widen. Step names:
  `zig build run-test-zig-module-<module> -- --test-filter "<name>"`
  (`check`, `postcheck`, …), `run-test-zig-lir-inline`,
  `run-test-cli -- --suite <suite> --filter <substr> [--filter …]`
  (filters are OR'd substrings), `run-test-zig-http-header-decoder-platform`,
  `run-test-zig-json-decoder-platform`, `run-check-snapshots`
  (`run-snapshot-tool -- --update-expected <files>` regenerates EXPECTED),
  `run-test-zig` (everything), `run-test-zig -- --test-filter "check type"`
  (checker integration suite, ~10 min). Add `--summary all` to see pass
  counts on success (zig prints nothing on a fully green filtered run).
- Before the first `zig build roc`, copy `zig-out/bin/roc` to
  `<scratch>/roc-base` for before/after comparison. `roc test --no-cache
  --opt=interpreter|dev <file>` runs a fixture; `roc build <app>
  --no-cache --timings` prints Monotype specialization counters.
  `roc test` prints "All (N) tests passed" BEFORE consulting checker
  errors, so a `.not_panic` CLI entry must carry `not_contains` stderr
  needles for the diagnostics it guards against.
- `zig fmt --check` on every Zig file touched. No debug prints, no TODOs,
  no commented-out code, no undeclared solver mutation.
- Stay inside the plan; report anything unanticipated under **"Not in
  the plan"** with exact error text and `file:line`; never improvise a
  workaround.
- Report in four sections: Change log (or Findings ranked
  blocker/should-fix/nit with file:line, failure scenario, recommended
  fix), Verification (actual results, never "expected" without showing
  it), Not in the plan (or "Nothing outside the plan was found."),
  Concerns (long-term compiler health).

### 8.5 Traps learned this session

- During an API outage, fresh subagents can die on their first request
  while a `fork`-type agent (shares the driver's cached context) still
  runs; use a fork as the fallback rather than retrying blindly.
- Do not edit test files while a full `run-test-zig` is running; zig
  compiles test binaries per step and a mid-run edit can race it.
- Bare `main` does not build anything `lir`-dependent without W1
  (`kusqzzsn`); a comparison workspace must sit on `kusqzzsn`, not on
  `main@origin` (`jj workspace add <path> -r kusqzzsn`; `jj workspace
  forget <name>` afterwards).
- A probe written in the polarity style (no `..`) type-checks
  differently on `main`: to compare against `main`, add `, ..` to the
  format-method error rows (`[FormatError, ..]`), as the pre-polarity
  fixtures had.
- Squashing the plan edit into `wlzsxolu` rebases every descendant and
  makes any other jj workspace stale; that is harmless.
- `identityOrigin` (W3) is strict: any new producer that reserves and
  fills a synthetic VARIABLE root must record a clone origin or declare
  an instance, including tests (`testFillSyntheticVariableRoot`).

### Appendix A. W2b fixture: stored parser over a shape with an optional field

Panics today ("resolved Monotype view requested for an unresolved
instantiation node", first lowering frame the restore's eager shape
view) on this stack AND on `main` + W1. W2b registers it (and an encoder
twin over the same shape) as `test/cli` fixtures asserting no panic on
both backends.

```roc
ParserTopLevelStoredOptionalField :: [].{}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name| name

	parse_str : Format, State -> Try({ value : Str, rest : State }, [FormatError])
	parse_str = |_, state|
		match state {
			Present(value) => Ok({ value, rest: Done })
			Done => Err(FormatError)
		}

	parse_record_start : Format, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [FormatError])
	parse_record_start = |_, state| Ok(Uncounted(state))

	parse_record_field : Format,
	Encoding.FieldName.FieldNames(_shape),
	State -> Try(
		[
			Field({ field : Encoding.FieldName(_shape), rest : State }),
			TryField({ name : Str, rest : State }),
			TryFieldCaseless({ name : Str, rest : State }),
			Continue(State),
			Done(State),
		],
		[FormatError],
	)
	parse_record_field = |_, _, state|
		match state {
			Present(_) => Ok(TryField({ name: "foo", rest: state }))
			Done => Ok(Done(state))
		}

	parse_record_after_field : Format, State -> Try([Continue(State), Done(State)], [FormatError])
	parse_record_after_field = |_, state| Ok(Continue(state))

	skip_record_field : Format, State -> Try(State, [FormatError])
	skip_record_field = |_, _| Ok(Done)
}

State := [Present(Str), Done]

parse_stored : State -> Try({ value : { foo : Str, bar ?: Str }, rest : State }, [FormatError, MissingRequiredField(Str)])
parse_stored = {
	Shape : { foo : Str, bar ?: Str }
	Shape.parser_for(Format.Default)
}

expect {
	result = parse_stored(State.Present("stored"))?

	result.value == { foo: "stored" }
}
```

### Appendix B. W6 probe programs (status on the current stack, both backends)

W6a promotes the passing ones to `lir_inline`/CLI fixtures (plus the two
the W6a section adds: an implementation with its own where-clause for
`.synthesize` evidence, and a `requires_record` schema); W6b turns the
panicking ones green. Each is a module named after its file.

**Widen** (passes) — a body use widened to `[Ok, Err, Extra]`, open impl:
```roc
describe : a -> [Ok(Str), Err(Str), Extra] where [a.status : a -> [Ok(Str), Err(Str)]]
describe = |x| x.status()

Job := [Pending].{
    status : Job -> [Ok(Str), Err(Str)]
    status = |_| Ok("p")
}

main : [Ok(Str), Err(Str), Extra]
main = describe(Job.Pending)

expect match main { Ok(s) => s == "p", Err(_) => False, Extra => False }
```

**Widen2** (passes) — same with a tag that sorts between `Err` and `Ok`
observed through both constructors, proving the impl is specialized at
the wider row:
```roc
describe : a -> [Ok(Str), Err(Str), Extra] where [a.status : a -> [Ok(Str), Err(Str)]]
describe = |x| x.status()

Job := [Pending, Failed].{
    status : Job -> [Ok(Str), Err(Str)]
    status = |j| match j { Pending => Ok("p"), Failed => Err("f") }
}

show : [Ok(Str), Err(Str), Extra] -> Str
show = |v| match v { Ok(s) => "Ok(${s})", Err(e) => "Err(${e})", Extra => "Extra" }

expect show(describe(Job.Pending)) == "Ok(p)"
expect show(describe(Job.Failed)) == "Err(f)"
```

**Question** (passes) — `?` into a wider error row:
```roc
load : a -> Try(Str, [NotFound, Other]) where [a.fetch : a -> Try(Str, [NotFound])]
load = |x| {
    s = x.fetch()?
    Ok(s)
}

Src := [S].{
    fetch : Src -> Try(Str, [NotFound])
    fetch = |_| Ok("hit")
}

expect match load(Src.S) { Ok(s) => s == "hit", Err(_) => False }
```

**Closed** (passes) — exhaustive match closes the copy:
```roc
describe : a -> Str where [a.status : a -> [Ok(Str), Err(Str)]]
describe = |x| match x.status() {
    Ok(s) => s
    Err(e) => e
}

Job := [Pending].{
    status : Job -> [Ok(Str), Err(Str)]
    status = |_| Ok("pending")
}

expect describe(Job.Pending) == "pending"
```

**Both** (passes) — one closing use and one widening use of the same
method in one body:
```roc
both : a -> [Ok(Str), Err(Str), Extra] where [a.status : a -> [Ok(Str), Err(Str)]]
both = |x| {
    first = match x.status() {
        Ok(s) => s
        Err(e) => e
    }
    if Str.is_empty(first) Extra else x.status()
}

Job := [Pending].{
    status : Job -> [Ok(Str), Err(Str)]
    status = |_| Ok("p")
}

expect match both(Job.Pending) { Ok(s) => s == "p", Err(_) => False, Extra => False }
```

**NestedEvidence** (passes) — the implementation has its own where-clause
(nested evidence), used both widened and exhaustively:
```roc
Wrap(a) := [W(a)].{
    status : Wrap(a) -> [Ok(Str), Err(Str)] where [a.name : a -> Str]
    status = |w| match w { W(inner) => Ok(inner.name()) }
}

Thing := [T].{
    name : Thing -> Str
    name = |_| "thing"
}

describe : x -> [Ok(Str), Err(Str), Extra] where [x.status : x -> [Ok(Str), Err(Str)]]
describe = |x| x.status()

exhaustive : x -> Str where [x.status : x -> [Ok(Str), Err(Str)]]
exhaustive = |x| match x.status() { Ok(s) => s, Err(e) => e }

show : [Ok(Str), Err(Str), Extra] -> Str
show = |v| match v { Ok(s) => "Ok(${s})", Err(e) => "Err(${e})", Extra => "Extra" }

expect show(describe(Wrap.W(Thing.T))) == "Ok(thing)"
expect exhaustive(Wrap.W(Thing.T)) == "thing"
```

**ImplOpen** (believed passing; re-verify) — direct call of a method at a
wider row, no where-clause:
```roc
Job := [Pending].{
    status : Job -> [Ok(Str), Err(Str)]
    status = |_| Ok("p")
}

direct : [Ok(Str), Err(Str), Extra]
direct = Job.status(Job.Pending)

expect match direct { Ok(s) => s == "p", Err(_) => False, Extra => False }
```

**SubsetImpl** (believed passing; re-verify) — implementation row is a
subset of the signature's:
```roc
describe : a -> Str where [a.status : a -> [Ok(Str), Err(Str)]]
describe = |x| match x.status() {
    Ok(s) => s
    Err(e) => e
}

Job := [Pending].{
    status : Job -> [Ok(Str)]
    status = |_| Ok("pending")
}

expect describe(Job.Pending) == "pending"
```

**ClosedImplExhaustive** (passes) — closed implementation, closing use:
```roc
describe : a -> Str where [a.status : a -> [Ok(Str), Err(Str)]]
describe = |x| match x.status() { Ok(s) => s, Err(e) => e }

closed_value : [Ok(Str), Err(Str)]
closed_value = Ok("cv")

Job := [Pending].{
    status : Job -> [Ok(Str), Err(Str)]
    status = |_| closed_value
}

expect describe(Job.Pending) == "cv"
```

**WidenClosedImpl** (PANICS: `instantiation widened a closed tag union`)
— closed implementation (body returns a top-level constant), widened
use; W6b's adapter case:
```roc
describe : a -> [Ok(Str), Err(Str), Extra] where [a.status : a -> [Ok(Str), Err(Str)]]
describe = |x| x.status()

closed_value : [Ok(Str), Err(Str)]
closed_value = Ok("cv")

Job := [Pending].{
    status : Job -> [Ok(Str), Err(Str)]
    status = |_| closed_value
}

show : [Ok(Str), Err(Str), Extra] -> Str
show = |v| match v { Ok(s) => "Ok(${s})", Err(e) => "Err(${e})", Extra => "Extra" }

expect show(describe(Job.Pending)) == "Ok(cv)"
```

**WidenParamImpl** (PANICS) — implementation returns an input-position
parameter, so its row is closed:
```roc
describe : a -> [Ok(Str), Err(Str), Extra] where [a.status : a, [Ok(Str), Err(Str)] -> [Ok(Str), Err(Str)]]
describe = |x| x.status(Ok("arg"))

Job := [Pending].{
    status : Job, [Ok(Str), Err(Str)] -> [Ok(Str), Err(Str)]
    status = |_, v| v
}

show : [Ok(Str), Err(Str), Extra] -> Str
show = |v| match v { Ok(s) => "Ok(${s})", Err(e) => "Err(${e})", Extra => "Extra" }

expect show(describe(Job.Pending)) == "Ok(arg)"
```

**QuestionClosedImpl** (PANICS) — closed implementation reached through
`?` into a wider row:
```roc
load : a -> Try(Str, [NotFound, Other]) where [a.fetch : a -> Try(Str, [NotFound])]
load = |x| {
    s = x.fetch()?
    Ok(s)
}

closed_try : Try(Str, [NotFound])
closed_try = Ok("hit")

Src := [S].{
    fetch : Src -> Try(Str, [NotFound])
    fetch = |_| closed_try
}

expect match load(Src.S) { Ok(s) => s == "hit", Err(_) => False }
```

## 9. Deferred: row subsumption, and what it does to host widening

Decided with Jared on 2026-09-15, at the end of phase two. Nothing here is
implemented. It is written down because the discussion that produced it cost
several passes to reconstruct from three paragraphs of `design.md` that were
far apart, and because `test/fx-open/issue_9963_hosted_try_question_mark.roc`
was left deliberately RED as its standing witness. §9.2 has since been
reversed and the hosted instance fixed; §9.1 and §9.3–§9.6 are unaffected —
subsumption is not implemented.

### 9.1 The decision

**Closing-by-body is not the intended end state.** A closed value flowing into
an implicitly open output row should WIDEN into it — row subsumption — rather
than bind its extension shut. The row published at a position is the one the
annotation declares, whatever the body happened to construct.

The argument is interchangeability. A signature is the whole of what a caller
reads, so two definitions with identical annotations must be usable
identically. Today they are not, and the witness holds both halves in one
platform module:

```roc
via_question! : {} => Try(Str, [HostErr(Str)])
via_question! = |{}| Ok(FallibleHost.str_ok!({})?)          # publishes CLOSED

via_match!    : {} => Try(Str, [HostErr(Str)])
via_match!    = |{}|
    match FallibleHost.str_ok!({}) {
        Ok(value)         => Ok(value)
        Err(HostErr(msg)) => Err(HostErr(msg))               # publishes OPEN
    }
```

`via_match!` CONSTRUCTS its error, and a tag constructor mints its own open
row, so the annotation's flex is never bound. `via_question!` FORWARDS the
host's closed row through `?`, which binds the flex to `[]`. A caller that
unwraps the first into a wider row is accepted; the same call on the second is
rejected. The bodies differ; the signatures do not.

To get a genuinely closed output row under the intended rule you would have to
write the closure explicitly — something in the shape of `[MyErr, ..[]]` — so
that closedness is a thing the author states rather than a thing the body
leaks. That spelling does not exist today and is not designed.

### 9.2 The hosted half WAS patched — a rejection reversed on evidence

**Rejected 2026-09-15, reversed 2026-09-16.** Both positions are kept here
because the reason the first one was wrong is the useful part.

**What was decided on 2026-09-15 (the rejection):**

> Every available patch is host-specific, and the host-specific machinery is
> what subsumption is expected to DELETE. Patching it means writing, reviewing
> and then removing the same code.

Two shapes were considered and rejected under that premise:

- Make the existing use-site redirect fire here. It is a checker special case
  fighting polarity rather than expressing it.
- Desugar `?` on a direct hosted call into the reconstruct-by-`match` form that
  `via_match!` uses. This is the nicer of the two — the reconstruction yields an
  open row naturally and IS the re-tag at lowering, so no adapter is needed on
  that path — but it is still host-specific.

**Why it was reversed (2026-09-16, Jared's call).** The rejection rests on ONE
premise: that every available patch is a host-specific special case subsumption
will delete. Scoping work falsified it on three counts.

1. **Measured, on an UNPATCHED binary: the adapter already fires for a CLOSED
   published row.** `FallibleChannels.via_question_closed_wider!`
   (`test/fx-open/platform/FallibleChannels.roc:38-39`) has the identical body
   shape to the broken wrapper with only a wider annotation, and
   `test/fx-open/hosted_channels_declared.roc:20` printed `closed wider: ok`.
   So the widening machinery is NOT host-specific scaffolding awaiting
   deletion — it is the general mechanism, already working.
2. **The change removes an EXCLUSION rather than adding a special case.** The
   rule, its `RedirectRule.hosted_try_question_widening` member
   (`src/types/store.zig:795-799`), the design.md declaration and the accept
   and reject fixtures all already existed. `tryErrorRowNeedsUseSiteWidening`
   opened with a shortcut — decline if `probeCanUseAs(expected, actual)`
   succeeds, since ordinary unification then already relates the pair — and
   under polarity that probe succeeds on the exact pair the rule exists for,
   BY BINDING the annotation's still-open extension to `[]`. A shortcut past a
   rule is sound only when taking it is observationally the same as applying
   it; grounding the annotation's own extension is not. The shortcut was an
   implementation-level early return strictly NARROWER than the rule's declared
   condition, and an artifact of commit `kupupkyt` collapsing a written `..`
   and an absent extension into one flex (§8.1.7 above) — an accident, not a
   designed boundary.
3. **Marginal deletion cost is ~zero.** design.md's "Hosted Try Question
   Widening" already says the checker half of the rule comes out wholesale
   when subsumption lands. The roughly fifty lines this fix adds sit inside
   that same half and come out in the same sweep.

"Do nothing" was not free either: 9963 has THREE registrations, all asserting
success (`src/cli/test/platform_config.zig:87`,
`src/cli/test/parallel_cli_runner.zig:2165` dev, `:2166` interpreter), so
leaving it red meant either a permanently red pipeline or touching those same
three registrations to pin behaviour the design calls a defect.

**What landed** (commit "fix(check): stop hosted `?` from grounding its own
annotated error row"): the decline shortcut is skipped when the expected row
still ends open, and the rule's declared inclusion test decides on its own.
`tryErrorRowEndsOpen` walks the expected row's explicit extension chain —
a rigid tail (`..others`) and an already-closed row both read as not-open, so
only an annotation's implicitly opened extension takes the new path. No
fixture was edited, which is the acceptance bar in §9.6 below.

**This is NOT row subsumption, and §9.1 and §9.3–§9.6 stand unchanged.** The
fix is gated on `tryConditionIsDirectHostedCall` (`Check.zig:23923`), so it
covers the hosted instance only. Closing-by-body remains a general defect over
every closed source — an input-position parameter, a nominal field, a hosted
result — and a NON-hosted forwarder still publishes closed behind an open
annotation while its `match`-reconstructing twin publishes open. Row
subsumption is still the intended end state and still deletes the checker half
of this rule.

### 9.3 Hosted Try widening splits in two, with different lifetimes

This is the part that resolved the discussion, and it is easy to get wrong.

**The LOWERING half is permanent.** A widened request at a host boundary must
always be bridged by a generated adapter that calls the declared-type boundary
and re-tags, never by specializing the boundary at the widened layout. That is
not a typing decision — the host ABI is fixed by something outside the type
system. W6b already generalized it: hosted is the instance of the Result-Row
Widening Adapter in which the declared row is the host ABI.

**The CHECKER half is what subsumption subsumes.** The use-site redirect that
widens a `?` condition is exactly the special case general row subsumption
makes unnecessary, and is the part to delete once subsumption lands.

**They cannot be deferred together.** An explicit open extension is REJECTED at
host boundaries by rule, so a host error row is closed BY DECLARATION rather
than by inference. "A closed row meets a caller who wants it wider" therefore
arises at every host boundary, not in rare corners — which means the general
mechanism cannot be half-built, and also means it will be exercised constantly
once it exists.

### 9.4 The confusion that made this hard to see

One annotation spelling means three different things depending on what is
annotated, and `design.md` described them in three widely separated
paragraphs. They are now a single table there, added by this PR. Restated
here because it is the key to reading everything above:

| Annotated thing | The opened extension | What a use may do |
|---|---|---|
| a FUNCTION signature | quantified flex, instantiated fresh per call | each caller may widen independently |
| a VALUE binding | ONE weak flex shared module-wide, grounded to `[]` after the module solves | uses share and accumulate; later uses see what accumulated |
| a HOST BOUNDARY (hosted lambda, `provides` def, platform `requires` type) | none — the row is generated exactly as written | nothing; an explicit `..` reaching the boundary is an error |

Note the two host rules have different scopes: the opt-out from opening covers
all three host positions, but the rejection of a written `..` is enforced only
over types reachable from a hosted lambda or a `provides` def. A `requires`
clause carrying `..` is accepted when the `provides` definition narrows the row
away before the boundary.

### 9.5 What implementing it involves

Smaller than it first appears, because W6b built the lowering.

1. **Checker.** At the unification where a closed row meets an implicitly open
   annotated output row, coerce rather than bind. Two cases: an incoming row
   whose tags are a subset of the listed tags coerces and leaves the extension
   open; an incoming row carrying unlisted tags binds as today, and
   `auditImplicitOpenExts` reports it.
2. **Lowering.** W6b's result-row widening adapter is the coercion's first
   instance. It is wired to template completion for dispatch plans, so the open
   question is whether a value coerced inside an ordinary body needs a re-tag
   that the adapter does not currently reach.
3. **Deletion.** The checker-side hosted redirect comes out.

**The open question, which is answerable by measurement rather than argument:**
does stopping the bind SUFFICE? Make the checker not bind the marker at that
site, rebuild, and run the witness. Three outcomes — it passes (the pieces
already fit), it fails in LOWERING (an inner coercion is genuinely needed), or
it fails elsewhere in CHECK (something else depends on closing). That one
experiment scopes the whole item.

### 9.6 Consequences to re-check when it is done

- `auditImplicitOpenExts` fires on an extension that resolved to a row carrying
  tags. If coercion changes when that happens, re-check it. W8's synthesized
  report rows are only sound because the audit has ALREADY proved the extension
  carries tags before reporting.
- `closeWeakValueImplicitOpenExts` grounds a top-level weak value's still-open
  extensions to `[]`. Cross-module widening of annotated weak values is a
  separate deferred decision (section 6) and should be settled in the same pass,
  since both concern what a closed row means at a module boundary.
- `test/fx-open/issue_9963_hosted_try_question_mark.roc` should go green with no
  fixture edit. If it needs one, the implementation diverged from this design.
  (The HOSTED half met this bar on 2026-09-16 — green, no fixture edited, both
  rejection fixtures still rejected — but that is §9.2's narrow fix, not
  subsumption. The bar still stands for the non-hosted closed sources, which
  need a fixture of their own since no corpus case spells one today.)
