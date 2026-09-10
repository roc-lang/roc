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
constraint dominance. Unification can correctly omit one declarative target
while retaining another for the same receiver and method, leaving a dispatch
plan named by the omitted raw callable. Omission alone proves target identity,
not callable-leaf identity, so the approved W6a representation is a durable,
serialized raw-witness table, `GeneralizedDispatchTargetShare`. Each directed
row scopes the omitted and retained raw callable vars by raw receiver and method
and carries one of two proofs: equal generalized callable shape under the exact
owning boundary's public-identity anchors, or an exact raw
`where_method_use` record whose complete copy map connects the pristine
signature to the omitted callable. The evidence pass resolves and indexes this
table post-solve. Exact identity outranks where-use nested-evidence reuse, which
outranks callable synthesis; relations compose along chains and the strongest
parallel path wins. `Check.recordGeneralizedDispatchTargetShare` rejects a new
cycle, and checked-artifact validation rejects duplicate/contradictory edges and
dead reachable cycles. `paramIndexFor` has no same-name path.

The candidate lifecycle is transactional and boundary-owned. Unifier scratch
records both raw receiver operands. `Check.runUnify` validates their committed
roots after every result, including partial mismatches; probes roll the pending
list back. A candidate belongs explicitly to a scheme or recursive group,
recursive SCCs flush once under the union of member anchors, and each boundary
must consume or retire all candidates it owns. Only the boundary flush mutates
`ModuleEnv`, and checked output requires final quiescence.

The nested-evidence decision is now explicit. Shape-only sharing sets an
independent callable whose nested evidence is synthesized from that callable.
An exact raw where-use proof also keeps the callable independent but reuses the
retained slot's checked nested-evidence vector. A `requires_record` evidence
schema cannot be synthesized, so it requires the exact where-use proof. A
direct Monotype unit pins both sides: exact where-use reuse preserves the
retained slot's resolved nested vector, while shape-only synthesis is rejected.
The combined W6a LIR fixture exercises the proof-backed path successfully.

Approved codec amendment: generalized deduplication treats
`deferred_generated_codec = true` as stronger than `false`. It chooses the
final representative for each equal requirement identity before writing aliases, in
both `[false, true]` and `[true, false]` encounter orders, and points every
omitted callable directly at it. Equal-strength candidates keep the first.

Current verification: the equivalent correctly ordered focused command,
`zig build run-test-zig-module-check --summary all -- --test-filter "scheme use"`,
passed all 7 build steps and both selected tests. The literal requested spelling,
`zig build run-test-zig-module-check -- --test-filter "scheme use" --summary all`,
reached the test runner but was rejected because `--summary all` was forwarded
to the test binary (`unrecognized command line argument: --summary`). The final
focused suite is green: unifier omission ownership/partial-commit tests (4/4),
target-share relation composition/cycle tests (2/2), side-table and codec-order
deduplication tests including interleaved groups (2/2), static/mutable
ModuleEnv roundtrips (2/2), checked-artifact v77 and ModuleEnv cache fingerprints
(2/2 each), serialization sizes, mutable warm-cache preservation of exact raw
where-use records and exact-proof target-share rows (2/2),
accepted/rejected cross-module generated-codec revalidation (2/2), the
`requires_record` Monotype gate (2/2), and the combined W6a LIR regression
(1/1). The LIR regression pins ordering-sensitive widening, `?` into a wider
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
obligation unifies it. Decision: (e). Per-use opening stays at every
output position of a where-method signature; the checker records, per
signature, which markers a body use widened, and the obligation reports a
new problem (declared in the Polarity section) when the resolved
implementation's row at a widened nested marker is closed, before it
unifies the implementation with the signature. Open implementations at
nested positions keep working; closed ones are rejected at check time
with the implementation named. The nested-position fixture asserts that
rejection.

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
(W2b). W6a's `deduplicateGeneralizedDispatchRequirements` is a Rewrite
Inventory entry under Generalized Dispatch Requirement Deduplication: its
accepted tests cover equal anchored callable `TypeDigest`s, exact where-use
nested-evidence reuse, shape-only callable-derived evidence, and complete-codec
dominance in both input orders; its rejected tests cover unequal anchored
digests, literal requirements, distinct method/origin data, and shape-only
`requires_record`. `groundRowDefaults` (deleted by W2b) and the W6b adapter are
Monotype mechanisms declared in the Monotype sections. Option (e)'s W6b
checker rejection is a declarative Polarity rule unless its implementation
adds a solved-graph mutation, in which case that mutation needs its own Rewrite
Inventory entry. `kupupkyt` already bumped the checked-module cache version
(`CACHE_VERSION` 72 → 73, `src/compile/cache_config.zig`) because checked
`row_default` data and weak-value grounding changed; W6a uses version 74 for
the new `SchemeUseRecord` slot, target-share rows, and checked-plan flags.
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

- General row-subsumption coercions (closed values widening into open rows
  in any position), which would also let a closed body publish an open row.
  W6b's adapter is their first instance; nested positions extend it (and,
  under option (d), open the corresponding signature positions) rather
  than rewrite it.
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
3. (Decided 2026-09-03: (e).) Per-use opening everywhere; the obligation
   rejects a closed implementation at a widened nested marker with a new
   problem kind. See W6b "Nested positions".
4. W6b mechanism: the adapter at the template boundary is my choice over
   the earlier draft's call-site wrap. If restructuring the hosted arm is
   judged too risky for this PR, the fallback is the call-site wrap with
   the same rule text minus "hosted is an instance" — it is a second
   re-tag site and should then be listed as debt.
5. (Closed 2026-09-03.) W5's panic is not reachable on the stack; see W4.
6. (Closed 2026-09-03.) The 10121 harness tests pass on the stack; the CLI
   controls become fixtures under W4 so the two paths cannot drift.

## 8. Handoff for the next agent (updated 2026-09-10; W6b in progress)

This section is self-contained: the session's scratchpad (research reports,
probe programs, logs) does not survive, so everything a successor needs is
either here, in `design.md`, or in the repository.

### 8.1 Where the stack is

On `main` `96c3b2fa`, bottom to top (jj change ids; completed commits are
described in full, with trailers; W6b currently has its provisional title):

| Change | Item | State |
|---|---|---|
| `kusqzzsn` | W1 build fix | landed; drop at the next rebase once `main` fixes `any_negative` |
| `kupupkyt`, `wyzrkrmn`, `wlylqvxq`, `rzysvtry`, `rprvoylp` | phase-one checker work | landed, reviewed, PR #10434 (draft) |
| `wlzsxolu` | original plan | historical plan commit; current WIP checkpoints carry their own progress updates |
| `tsrzvryw` | W2a | landed, implemented + adversarially reviewed; callable-node grounding only |
| `wwnsvqrn` | W3 | landed, implemented + reviewed twice; full `run-test-zig` 5039/5046 (7 skipped) and `run-check-snapshots` clean on its tree |
| `vrpryvko` | W4 | landed, implemented + reviewed; tests and fixtures only |
| `wrtzpoum` | option (e) decision | declared in design.md |
| `ktlykkxv` | W6a | implemented; producer, lifecycle, codec dominance, serialization/recheck, `requires_record`, and combined LIR focused gates green |
| `ssvqsxro` / `cc8ace35` | W6b recovery/metadata checkpoint | pushed WIP; bounded 59-test gate green, but full checker gate is red: 1,257/1,383 pass, 6 fail, 120 crash |
| `qpmsttws` / `e67aaeb8` | W6b allocation-witness/cache prerequisite checkpoint | pushed and PR read-back verified; independent review accepted, combined gate 29/29 tests, 41/41 steps; cache version 83; WIP, not whole-W6b acceptance |
| `zkmyrwqu` / `1f69432c` | W6b record-update root-request mechanics | pushed WIP and PR read-back verified; final gate green (26/26 tests, 38/38 steps); review accepts mechanics only, complete admission ownership/retirement remains REQUEST CHANGES |
| `wpvvswsl` / `95dfea5a` | W6b checked-base record-update retirements | pushed and PR read-back verified; reviewed bounded WIP, final gate 38/38 tests, 41/41 steps; complete ownership and W6b remain unaccepted |
| `txopxlzx` / `10f613d6` | W6b checked-base boundary coverage | pushed and exact PR readback verified; reviewed bounded WIP; final gate 41/41 tests, 41/41 steps |
| `onmovlvr` / `92741c58` | W6b actual base-plan publication atomicity | pushed and exact PR readback verified; reviewed bounded WIP; final gate 43/43 tests, 41/41 steps |
| `mpqnvuvq` / `9c295876` | W6b checked-base owner completion atomicity | pushed and exact PR readback verified; reviewed bounded WIP; final normal 45/45 tests, 41/41 steps pass |
| `mwttkmrx` | W6b checked-error base-plan publication atomicity | reviewed bounded WIP; final normal 51/51 tests, 41/41 steps pass; publication pending |

The local and remote `jared/polarity` bookmarks now point at `mpqnvuvq` /
`9c295876`, directly above `onmovlvr` / `92741c58`, `txopxlzx` / `10f613d6`,
`wpvvswsl` / `95dfea5a`,
`zkmyrwqu` / `1f69432c`
and `qpmsttws` / `e67aaeb8`.
The driver verified the exact remote head and updated PR #10434 body with draft
status intact. This published checkpoint is explicitly incomplete. New child
`mwttkmrx` owns the reviewed and verified checked-error base-plan publication
allocation-test slice, whose publication is now pending.
Jared's updated authorization
on 2026-09-09 is to push WIP changes after each completed task and continue
until the feature is complete; this supersedes the earlier pause-before-W2b
instruction. Each task still requires independent adversarial review and
targeted verification. The driver alone controls jj and publication;
implementers and reviewers do not change VCS state. Pruning `.zig-cache` is
authorized only if disk space runs out. The user also suggested an incremental
watch build and compact diagnostics for a faster feedback loop. W6b is not
ready to publish as complete.

W6b checkpoint (2026-09-09):

**Latest broadly rechecked bounded checkpoint: `c8b4a737` (general subtree
invalidation retry, E2a/E2b, full-width CIR payloads, and the prior recovery
matrix). Focused invalidation gate `34110` passes 7/7 steps and 6/6 tests;
independent adversarial review accepts the bounded slice. Combined broader
gate `50225` passes 10/10 steps and 59/59 tests (can 25, check 34).
Full checker gate `30286` completed red on compiled `c124f1eb` (only this
progress document differs from code checkpoint `c8b4a737`): 1,257/1,383 tests
passed, 6 failed, and 120 crashed; 5/7 build steps succeeded. The full report
is saved and grouped for targeted follow-up. W6b is unfinished and
unaccepted as a whole. The reviewed recovery checkpoint was pushed as
`cc8ace35`. The newer witness/cache prerequisite `qpmsttws` is independently
accepted as a bounded task with combined gate `42891` green: 29/29 tests,
41/41 steps. This does not certify the unresolved full-checker failures.**

The fresh-flex witness and canonical-empty `SafeMultiList` serialization
prerequisite is now accepted and pushed as `e67aaeb8`, with the combined
29/29-test gate above. The subsequent record-update
root-request bridge (`zkmyrwqu`, cache version 84) is pushed as `1f69432c`: it preserves the actual raw
`R -> R` shared occurrence when the syntax's request `V` already redirects
to `R`, and validate the exact record-to-base source edge. Its types-only
gate passes 10/10 tests and independent types review accepts that slice.
The first checker fixture and original crash regression pass (3/3), including
real representative-change, fresh-context, rebuild, and serialization checks.
The complete corruption/OOM matrix now passes, including same-instance retry
after every injected publication allocation failure. Native/wasm sizes and
four compile/serde tests pass; measured version-84 golden retry passes 2/2.
Review accepts the lower-level mechanics only as an incomplete WIP checkpoint,
not complete admission: the owner bijection and retired-owner lifecycle remain
unimplemented requirements. Final shared-helper/bridge/schema gate `99548`
passes 26/26 tests and 38/38 steps. The driver published this accurately
scoped WIP checkpoint and opened `wpvvswsl` for ownership/retirement.
Incremental watcher `44170` was stopped after repeated generated object
linker errors; regular builds with minimal diagnostics are the current
verification mode. No disk-space error occurred and `.zig-cache` was not pruned.
Contributor/settlement normalization, remaining failure producers, option-(e)
rejection, downstream authority transport, adapters, and whole-phase gates are
still open. The detailed current-task evidence appears near the end of this
handoff section.

The newer E1 core checkpoint `b1298b89` is independently reviewed and passes
the unchanged historical invalid-import call regression: gate `6580` succeeds
7/7 steps and 2/2 tests (7 seconds/28 MB). Both recursive call owners, exact
slot-zero lookup causes, retained call plans, and fresh-context replay are
pinned. This is a targeted diagnostic-core milestone, not acceptance of the
whole call/callee slice. E2a is now independently reviewed and accepted at
`50d72f3e`: mixed-case retry `4609` passes 7/7 steps and 2/2 tests, then the
complete focused gate `98603` passes 7/7 steps and 7/7 tests (35 seconds/28 MB).
E2b real allocation-failure/retry coverage and temporary-diagnostic cleanup are
now accepted at `645c29e7`, together with the independently reviewed full-width
CIR payload dependency fix. Golden/size retry `92156` passes 32/32 steps and
2/2 tests; regression gate `54470` passes 10/10 steps and 23/23 tests
(canonicalizer 15, checker 8). The separate subtree-invalidation retry defect
is now fixed and independently accepted at `c8b4a737`. This follows authentic
fixture/topology acceptance at `6fec8731` (`49465`: 7/7 steps, 2/2 tests).
The complete tests prove every measured direct and central allocation failure,
exact metadata/ledger rollback or complete prefix commit, retry/idempotence,
root-owned omission removal, authentic return cycles, and clean/retry equality
through the actual checked-file completion/admission path. The broader recovery
matrix is green on this source; the full checker baseline is running.
The new owner tests pass the focused gate `73577` (10/10 steps, 4/4 tests).
The broader gate `33921` on the same frozen source passes 10/10 steps and
33/33 tests (can 11, check 22), including all six canonical where-ownership
tests and the prior direct-binder, serialization, transaction, and malformed-
source matrix. Independent review accepts the declaration, shared reader,
canonical receiver/argument distinction, and complete agreed raw-guard cases.
The real populated recovery fixture passes both canonical rebuilds and readonly/
mutable serialization replay (`83266`, checker 2/2 including aggregator).
The measured golden update independently passes its targeted rerun `22649`
(25/25 steps, 2/2 tests). The initial combined run failed only at the old golden;
it was not rerun as a whole after the targeted correction.
Independent review accepts the three
authentic source, activation/lookup, and completion transaction tests, including
Probe rollback, measured OOM sweeps, exact retry/revisit, and published/pending
diagnostic-state checks. Run `6400` passes 7/7 build steps and 4/4 tests
(three tests plus aggregator, 21 seconds/25 MB). The two test-setup corrections
are documented below: use the actual diagnostic stores, and initialize raw CIR
type slots with the real production helper before entering the tested seam.

Earlier accepted bounded evidence remains the lifecycle/shared-reader
correction and combined 16-test can/check gate `36886`, plus all five
functional direct-binder tests on `a42543f4` (`7691`: 6/6 including aggregator).
Those results do not certify the still-open portions of W6b.

The driver has now authorized bounded call/callee implementation (E), following
the implementer's concrete plan and a separate read-only producer audit. Its
first change declares the dual-range completion transaction in `design.md`;
implementation must retain the exact call root and argument checking outcomes,
publish the finite direct-binder operand cause, and wire the declared late
callee retirement edges. `ExprCheckFrame.finish` queues an erroneous lookup
but does not rewrite it: the call producer therefore consumes its exact pending
lookup retirement and draft, while terminal admission requires the completed
lookup singleton. Both retired-consumer and failure-reference capacities must
be available before either semantic range is appended. This is not a claim
that the earlier subtree-metadata invalidation is globally rolled back.
The historical multi-family invalid-import source stays unchanged. Independent
adversarial review, authentic allocation-failure/retry coverage, and focused
runtime gates are still required; no E implementation acceptance is claimed.
The tool's agent-thread limit currently prevents resuming the previous reviewer,
so the driver has directly assigned the read-only producer auditor (which made
no implementation edits) an independent adversarial review. Its preflight
requires repeated lookup-tracking entries for one live expression to agree on
the exact pattern and captured call token before either poisoning edge erases
that expression. A first-match choice must not discard a later authoritative
entry. It also preserves the nonrecursive split between basic callee-snapshot
replay and full annotated-lookup failure validation.
The E implementer now reports the core producer, both poisoning edges,
dual-range completion, and local/checked-boundary/fresh validation written,
with AST validation passing; the rebuild audit is still being finished.
The driver has explicitly split verification: freeze and independently review
the E1 core, then run the unchanged historical regression before finishing the
E2 structural/corruption and authentic OOM/retry matrix. This isolates compiler
integration failures before the larger test block; it does not waive any E
acceptance requirement. No core build or runtime result is claimed yet.
E1 snapshot `45838338` independently passes formatting and AST checks, and the
driver confirms both historical provider/consumer source strings are unchanged.
Its complete independent core review requests changes before the first build:
the expression-poison sweep must publish the annotated-callee failure rather
than assert that the use sweep already handled it; call completion must accept
an exact pending lookup retirement as well as a completed one; terminal
argument-plan references must not assume producer-time adjacency after rebuild;
an unsupported erroneous sibling must not suppress qualifying direct-binder
operand failures; and terminal replay needs the complete per-argument
direct-lookup-retirement-to-call-failure inverse. The existing direct-lookup
publication is the durable typed-outcome witness for that inverse, not a solved
error descriptor. The reviewer also requires the historical assertions to name
the exact recursive call owners, slot zero, and their own argument lookup causes.
The nonrecursive callee validation split, existing DAG/remaps, dual-capacity
reservation order, and disjoint relation-stamp states were otherwise reviewed.
The driver has authorized this bounded correction, with its lifecycle/converse
declaration first; another frozen independent review precedes the diagnostic
runtime gate. E1 and E2 remain unaccepted, and nothing has been pushed.

The corrected core snapshot `57546ba8` passed driver formatting/AST checks,
kept both historical source strings byte-identical, and received independent
approval for the historical diagnostic gate. All five review blockers were
resolved; this approval did not accept E2 or whole W6b. Gate `8302` then passed
compiler and builtin generation but failed the historical test (1 pass, 1
crash): `call operand retirement lost its canonical call payload`. The
canonical call reader requires a currently live expression tag for the callee,
which conflicts with the declared late-callee retirement lifecycle once that
lookup has been rewritten to `malformed`. The terminal call-argument reader
also needs an audit for completed lookup retirements. A bounded producer-
authority-preserving correction is now in progress, followed by independent
review and the same targeted rerun; do not broaden the generic expression-tag
predicate or treat arbitrary malformed nodes as authenticated expressions.
No build is active, no E acceptance is claimed, and nothing has been pushed.

The scoped lifecycle correction `45495b98` subsequently passed independent
review, driver formatting/AST checks, and the unchanged-source comparison.
Targeted rerun `53122` passed compiler/builtin generation and advanced beyond
the payload check, but still crashed (1 pass, 1 crash) at
`call operand retirement changed an exact formal`. The driver identified a
concrete inconsistency in the new nonrecursive lookup-origin inverse: it
requires the call-copy source to equal the binding-pattern variable before
distinguishing ordinary local binding instantiation from predeclared annotation
instantiation. The latter actually consumes the separately copied annotation
scheme and authenticates it through its exact support-step destination. The
implementer and independent reviewer are checking this origin-specific
correction before another frozen review and exact rerun. At that point E1
remained red and E2 had not begun.

The origin-specific correction `b1298b89` then passed independent review and
driver formatting/AST checks. It moves the pattern-source equality into only
the ordinary local-binding arm, preserving all predeclared support-chain and
attachment checks. Gate `6580` passes all 7 steps and both tests, including the
unchanged historical regression, its exact call/lookup evidence assertions,
fresh-context replay, and transient cleanup. The historical TEMP stderr is
still present deliberately until E2b cleanup; it does not indicate a failed
test. No build is active and no publication has occurred.

E2 is split into two independently reviewed tasks. E2a covers authentic
two-qualifying-argument and mixed-error
calls; exact lookup-tracking duplicates/conflicts; coordinated sibling
owner/slot/formal/cause corruption; missing, duplicate, and cross-owned failure
and retired-consumer ranges; produced and fresh-context replay; the combined
failure/retirement cycle negative; and byte-identical repeated canonical
rebuilds. E2b covers real central-rewrite allocation failures and exact retries,
including completing the call while its source lookup and annotation
retirements are still pending, then completing those sources for terminal
replay. It also removes the historical temporary diagnostics before E
acceptance. The proposed authentic staging seam extracts the existing
contiguous pre-poison `checkFileInternal` phase without changing its order;
tests reuse that production phase rather than fabricate Expected rows or
duplicate a long initialization sequence. The extraction itself requires
mechanical-equivalence and independent review. E2a is now authorized and in
progress; E2b has not started. E2a also pins the origin-specific rejection sides:
wrong local binding source, wrong predeclared support destination, sibling
support step, and mismatched annotation attachment.

E2a was frozen at `8bbae310` after driver formatting/AST checks and verification
that both historical regression source strings remained byte-identical. Gate
`29349` passed six of seven tests, including the historical regression, the
two-qualifying-operand/rebuild fixture, the coordinated corruption matrix,
tracking authority, and call-origin negatives. The mixed preexisting-error
fixture crashed at `a call operand publication changed an argument plan`.
Independent adversarial review found this same single blocker and no second
static defect in the bounded delta. The successful argument plan is intact:
the publisher uses the generic live-child phase, which rejects the authentic
canonical `.malformed` sibling despite the exact preexisting-runtime-error
retirement carried by that argument's typed checking outcome. A narrow
correction is now authorized: declare this producer phase in `design.md`,
authenticate that exact typed cause and baseline node/diagnostic/payload, and
retain all successful plan/formal/token checks and strict generic live-call
validation. Rejection-side tests must pin missing and substituted authority.
At that point E2a remained unaccepted and E2b had not started.

The bounded mixed-producer correction `50d72f3e` passed driver formatting/AST
checks, unchanged historical-source verification, and independent adversarial
review. Its private producer phase retains a live callee and requires every
malformed argument's exact canonical typed cause, unique legal preexisting
retirement, and node/publication/diagnostic/payload inverse. Generic live-call
validation and all successful plan/formal/token checks remain unchanged. The
mixed-only retry `4609` is green (7/7 steps, 2/2 tests, 7 seconds/26 MB), followed
by complete E2a gate `98603` on the same frozen source (7/7 steps, 7/7 tests,
35 seconds/28 MB). E2a is accepted as this bounded test/recovery slice, not as
whole W6b or even completion of E. E2b is now authorized and in progress:
mechanical shared production pre-poison phase, authentic central-rewrite
allocation-failure/retry tests with pending source retirements, and removal of
temporary historical diagnostics. No build is active and nothing has been
pushed; broad recovery and full checker gates still remain after E2b.

E2b preparation exposed a separate required follow-up before broader acceptance:
`markHoistInvalidatedExpr` inserts into the durable invalidated-expression map
before fallibly appending to a local traversal queue. If that append or a later
queue allocation fails, the local queue is discarded but inserted entries
remain; retry treats them as already visited and can skip unfinished
descendants. The driver, implementer, and independent read-only audit agree
this is a real general retry defect affecting descendant literal plans, known
or selected hoist roots, and omitted-default pruning. Permitting unrelated
invalidation prefix effects in the R_C transaction rule does not permit a
visited marker to suppress unfinished work. E2b continues with its scoped
retirement-ledger contract and a genuine fixture with no such metadata; it
must not prewarm, clear, reset, or reconstruct state to hide the issue, or
claim whole-rewrite rollback/general subtree retry. A separate bounded
design/implementation/review/test task must fix invalidation retry after E2b.
Reserving one queue append before publishing one visited entry is not by
itself sufficient: earlier queued entries are also lost on a later failure.
Direct reads of main base `96c3b2fa` and W6a parent `d74784ef` contain the same
faulty insertion/retirement/enqueue ordering: this defect predates the Polarity
stack rather than belonging to a new E1/E2a producer change.
A bounded independent design audit finds an off-side insertion-ordered
deduplicating worklist suitable: collect the exact typed-CIR descendant edges,
reserve the durable map, then infallibly publish the complete visited set,
retire literal plans in discovery order, and compact omitted defaults. Literal
retirement changes stamps and the side pool, not the topology being traversed;
all traversal helpers are private to this invalidation boundary. The required
new invariant is that durable map membership means a fully committed descendant
closure, not merely an enqueued node. Discovery/reservation failure must leave
logical invalidation state unchanged; a fully committed invalidation may remain
if later R_C publication fails. This is preparation for the separate normative
amendment and implementation, not acceptance of a fix.

After E2b acceptance at `645c29e7`, the driver authorized that separate
invalidation-retry task. The authentic fixture must retain both affected and
unaffected selected roots, literal plans, and omitted-default rows across a
branching call subtree; measured allocation failures must preserve logical
metadata before commit and retry to the clean result. The implementation and
normative amendment are in progress; no new invalidation gate has passed yet.
The named runtime-error subtree invalidation rule and Rewrite Inventory entry
are now written, and the ordered discovery/reserve/commit implementation is
drafted. The driver requested a first frozen fixture-shape gate to verify the
authentic arity-three call's selected roots, literal plans, and omitted-default
owners before the exhaustive allocation-failure sweep. That staging does not
reduce the final coverage or accept the production fix early.
The first fixture slice froze at `de1f08be`; driver review caught two test-only
fields accidentally inserted into `PendingInspectMethodUse`. The implementer
moved them to the intended test topology struct, and the exact corrected
snapshot is `c40bba22`. Driver format/AST checks pass. Focused gate `82241`
compiles successfully but fails the fixture test with `expected 4, found 20`
(1/2 tests pass). The exact assertion and the additional producer-owned rows
must be accounted for before correcting the fixture expectation; no retry/OOM
acceptance is claimed. The driver authorized only that diagnosis/correction
and history-neutral size-comment wording before rerunning the same narrow gate.
Independent producer accounting identifies twenty plans in seven contiguous
groups: before-list (0–2), explicit-3 backing record (3–4), call (5–8), middle
tuple (9–12), explicit-5 backing record (13–14), after-list (15–17), and
explicit-9 backing record (18–19). The corrected fixture at `77481786` pins all
six non-call owner/role/site ranges and the call's exact four-plan range, root,
argument, formal, and token relations; it does not blindly relax the count.
Targeted retry `84421` compiles but fails with `expected 3, found 1` (1/2 tests
pass). Exact test-stage diagnostics and producer tracing are required before
correcting that next fixture assumption. Both zero checker/type-problem
assertions and an actual omitted-default owner for the root-predicate test are
also required on the next thaw. The fixture and general retry fix remain
unaccepted; broader recovery and full checker have not been rerun.
Independent tracing explains the selected-root mismatch: the original bare
survivor lookups defer binding selection until the enclosing block finishes,
but its lexical-scope cleanup removes the known values before that flush. Only
the middle tuple is selected. A proposed unreferenced top-level-constant
replacement was rejected by the driver before building: `checkDef` explicitly
suppresses nested hoists in ordinary compile-time roots. The approved authentic
replacement puts each outside closed list beside an independent runtime `U64`
formal in a tuple, forcing its eligible list child to be selected while live.
It must prove three expression roots (no binding-root coverage claim), both
runtime lookups' exact second-formal identity, and nine producer-owned plan
groups (anticipated 26 rows, call start 8), without fabricating metadata. This
replacement is still being implemented and has not passed its fixture gate.
The runtime-sibling replacement subsequently froze at `6fec8731` and passed
independent fixture/topology review. Targeted gate `49465` is green: 7/7 steps,
2/2 tests (7 seconds/26 MB). Its actual producer state pins two outer formals,
both runtime lookups, three statement coordinates, all twenty-six plans in
nine exact groups (call range starts at 8), four omissions/four literal owners,
three distinct map-authenticated null-pattern expression roots, empty binding
and validation maps, zero checker/type problems, and the exact twelve-node
call-subtree BFS. Selected-root publication indices are not guessed. The
root-predicate assertion uses an actual published omitted-default owner.
This accepts only the authentic fixture/topology slice. The driver has now
thawed the source for measured exhaustive invalidation and central-rewrite
allocation failures, exact metadata snapshots and retry/clean comparisons,
idempotence, actual root-owned omission removal, authentic return-edge cycles,
and terminal replay; the general invalidation fix remains unaccepted.
For terminal coverage, the driver approved a mechanical extraction of the
entire production suffix after `checkFileThroughPrePoison` into
`checkFileFromPrePoison`: both poison sweeps through checked-file admission
and `finishTypecheck`, preserving the pooled solver environment's lifetime
and every operation's order. The rich invalidation fixture will use that
shared suffix rather than a second hand-maintained cleanup sequence. This
does not strengthen the older E2b test's explicitly staged replay claim.
The full invalidation slice froze at `f1051eb9`; driver formatting/AST checks
pass, and the shared suffix is byte-identical after exactly four `&env` to
`env` substitutions. Independent preflight corrected the cycle fixtures to
remain pre-admission and replaced a production-derived terminal oracle with
an independently enumerated, unique 31-expression set; the surviving default
literal is explicitly required to use `builtin_direct` resolution.
Focused gate `55787` is red: 3/6 tests pass, including both authentic cycles.
The rich fixture, omission, and exhaustive OOM tests stop at a newly added
`erroneous_value_exprs` assertion (expected 2, actual 4), before the allocation
sweeps run. All earlier fixture cardinalities remain unchanged. The author is
tracing all four producer-owned keys for an exact oracle correction, not a
blind count update. That correction froze at `c8b4a737`: the exact unique
erroneous-expression set is the two direct lookup operands, their call, and
the annotated outer/source lambda. Focused retry `34110` passes 7/7 steps and
6/6 tests (35 seconds/33 MB), and the independent reviewer accepts the entire
bounded invalidation slice without findings. The complete production tail,
natural durable-map reservation failure, both central failure classes, exact
inter-reservation failure, independent terminal 31-expression union, and final
admission are now dynamically exercised. Combined broader gate `50225` passes
10/10 steps and 59/59 tests (can 25 in 780 ms/6 MB, check 34 in 3 minutes/35 MB).
This accepts the invalidation fix and refreshes the broader baseline, not W6b
as a whole. The full checker gate is next; contributor migration has not begun.

E2b froze at `650ed2c5` for independent review. Driver formatting/AST checks
pass; the shared pre-poison phase is byte-identical after exactly seventeen
`&env` to `env` substitutions, its outer lifecycle is unchanged, and both
historical regression source strings remain byte-identical. Temporary
historical logging is removed. Focused gate `46607` is red: the new authentic
dual-range OOM test reaches terminal retry replay for allocation index zero,
then its added clean-instance versus retry-instance serialized comparison
differs at byte 142429 of equally sized 170436-byte buffers. The preceding
rollback, intermediate retirement, local/fresh replay, and same-instance
two-rebuild checks have been reached for that first case; the exhaustive sweep
has not completed. Exact serialized-field/representation diagnosis and
independent review are in progress. No assertion weakening, byte masking, or
state reset is accepted without establishing the cause and correct contract.
E2b remains unaccepted; no build or push is active at this diagnostic pause.

The diagnostic-only follow-up initially failed to compile (`8115`); its bounded
comptime-walker/inline-loop correction and alignment-safe header reads were
independently reviewed at `0d3d0f75`. Rerun `40655` reaches the test and identifies
the same first difference both before completion and after retry:
`ModuleEnv.store.nodes`, live node row 8, payload byte 13 (serialized offset
142429). Pending node count is 23 and completed count is 26, both with capacity
1544. Thus this first difference predates the injected allocation failure;
it is not evidence of retry damage. The exact active payload variant and the
remaining differences still need attribution. Static inspection finds that
the 16-byte external Node payload union has many 12-byte variants and is copied
as a whole by `Node.setPayload`; that representation also exists at main base
`96c3b2fa`. This is a concrete representation concern, not yet acceptance of a
fix or permission to mask unused bytes in this test. All original assertions
remain, the sweep still stops at allocation zero, and no push has occurred.

The independent representation audit and implementer agree on the structural
cause: `Node.init` zeroes the 16-byte payload, then constructing a smaller active
external-union variant and assigning the whole union through `Node.setPayload`
overwrites its inactive tail with undefined bytes. Both serialization and
retirement `original_payload` snapshots consume all sixteen bytes; changing only
the serializer or test comparator would leave an authority-record defect.
A separate design-first producer-representation correction is now authorized
before E2b can resume: every payload variant must explicitly cover all sixteen
bytes with zero-default reserved fields, including Annotation's implicit tail.
Exhaustive compile-time size and field-coverage checks must prevent recurrence;
semantic field offsets and the overall Node payload footprint stay unchanged.
Producer/independent-instance serialization tests and layout-fingerprint checks
must accompany the fix. Keep E2b's complete cross-instance equality and require
its pre-completion baseline too. No additional diagnostic-only run is needed
before this structurally justified fix. The separate subtree-invalidation retry
fix remains required afterward; neither dependency is accepted yet.

The full-width payload correction froze at `0ecd0668` and passed independent
adversarial review. All 124 variants explicitly cover sixteen bytes; exactly
109 reserved-tail additions preserve every semantic field offset. The direct
payload gate `53825` passes 1/1 test. Combined gate `96135` passes the three
canonicalizer payload/NodeStore tests (422 ms/1 MB) and the E2b recovery gate
(2/2 tests, 7 seconds/28 MB). The complete measured allocation-failure sweep,
exactly one inter-reservation failure, source-pending intermediate proof,
local/fresh replay, repeated rebuilds, and both pre-completion and terminal
cross-instance byte equalities now pass without masks or temporary diagnostics.
The combined command remains red only on recorded schema expectations:
`CACHE_VERSION = 82` produces ModuleEnv fingerprint
`69def018eb5816bbe41046fbbc3a71ad710403e88f5ac3336ced9c62b7c2a391`, and both native
and wasm32 measure `ModuleEnv.Serialized` at 3520 bytes rather than 3448. The
72-byte discrepancy is three existing W6b publication descriptors:
`malformed_type_annotation_publications`, `body_annotation_attachments`, and
`body_annotation_malformed_type_publications`, each 24 bytes. Node payload and
Node sizes remain 16 and 20 bytes. Only the measured hash golden and this
accounted-for size golden are authorized to change next; targeted retries and
the E2a/NodeStore regression gates remain before acceptance of the complete
dependency/E2b checkpoint. The separate invalidation retry fix is still next.

The measured golden update froze at `645c29e7` and passed independent review.
Targeted retry `92156` is green (32/32 steps, 2/2 tests), including native and
wasm32 serialization sizes. The subsequent E2a/E2b and existing NodeStore/literal
retirement regression gate `54470` is also green (10/10 steps, 23/23 tests:
canonicalizer 15 in 433 ms/3 MB; checker 8 in 42 seconds/29 MB). This accepts
E2b and its producer-representation dependency as bounded completed work,
not whole W6b. Temporary diagnostics are removed and neither cross-instance
byte assertion was weakened. The next implementation task is the separately
audited off-side subtree-invalidation transaction, with its own normative
declaration, authentic metadata-bearing OOM/retry tests, and independent review.
No push, bookmark move, final commit description, or PR-body update has occurred.

The E2a nonadjacent-plan positive is not claimed: the current producer reserves
each call's root and argument plans contiguously, and canonical rebuilding
groups those same-owner rows. Nested argument expressions produce plans owned
by different nodes, not an authentic nonadjacent retired-call group. Tests
must not fabricate a valid ledger permutation to claim otherwise. Terminal
proof still follows the explicit retained consumer/formal indices, and the
authentic repeated-rebuild test checks those exact references.

The independently reviewed general expression-frame decomposition and unchanged
depth-128 regression are accepted. Diagnostic run `86549` on
`f52cae3a` records 190 lambda/closure entries before the crash: its exact binary
reserves 85,608 bytes per `checkExpr` frame with a 16 MiB Mach-O stack budget.
The staged fix extracts all 59 expression cases into typed, non-inlined
helpers, retaining frame creation, original Expected timing, cleanup, and
finalization in their existing order. It preserves the accepted transaction
block byte-for-byte and removes deep-test temporary logging. Final independent
adversarial review accepts all 59 arms, lifetime/Expected ordering, and the six
required pointer-argument conversions. The exact reviewed files are applied;
formatting and AST checks pass. Run `20681` passes 7/7 steps and 5/5 tests
(unchanged depth 128, three transactions, aggregator; 28 seconds/31 MB).
The exact compiled binary reserves 7,264 bytes for `checkExpr`, 11,728 for its
lambda helper, and 800 for its closure helper, with the unchanged 16 MiB Mach-O
stack. The broader previous functional/shared-reader gate `5300` also passes
10/10 steps and 24/24 tests (can 4, check 20; check 2 minutes/32 MB).
This accepts the bounded stack fix, not unlimited recursion or whole W6b.

The bounded direct-binder source, lookup, completion, deep replay, raw-reader,
receiver/owner inverse, and canonical/serialization slices are accepted.
Call/callee
retirement, contributor/settlement tracking, option-(e) rejection, lowering
adapters, and whole-phase acceptance also remain open. Jared asked about a
checkpoint push; the bounded transaction/deep-regression/review threshold now
passes, including the broader focused regression set. Root has asked whether
to publish this explicitly unfinished W6b checkpoint while implementation
continues. The status question has not itself authorized a partial push.
The existing draft PR remains #10434 on `jared/polarity`; its old description
has been read but not changed. Details and exact run history follow.

Earlier bounded prerequisite history: the
provider/owner and synthetic-binding journal corrections have useful focused
coverage. The sole-target default handoff and corrected fixture helper have
passed independent review and all six original/new lifecycle regressions now
pass, including signature rollback and a genuine non-null default-child
parent. The share-cut correction now passes independent review, 6/6 focused
instantiator tests, and 4/4 focused checker tests, resolving the weak-receiver
and cap-free generational-discharge crashes. The selected-owner correction
now also passes nested-Try and the complete recursive matrix: combined run
`73206` has 21/23 passing tests, with only two new invalid-export fixtures
still being corrected. Independent production review has no remaining
blocker under the explicitly bounded atomic selection-input contract. Owner
fixture acceptance and version-80 serde/hash verification now pass in `4265`:
28/28 steps, 6/6 tests. The fixture truthfully covers a nominal receiver-
extension and its namespace's identical binding; no external-alias positive
is claimed. Staged default-target calibration passes independent review and
the combined owner/recursive/default checker run `68496`: 7/7 steps, 24/24
tests. It measures 58 allocations in the scoped compatibility transaction;
the runtime-calibrated finite failure sweep now passes independent review and
root run `93726` (7/7 steps, 2/2 tests). The reviewed shared immutable Builtin
setup then passes combined run `88439`: 7/7 steps, 24/24 tests, 2 minutes /
33 MB, including the sweep and full owner/recursive/default matrix. Every
consumer checker remains fresh. This bounded allocation-failure slice is
complete. The historical invalid-import regression is now restored and its
call-retirement failure is dynamically localized: the first argument is
erroneous after parameter lookup, and ordinary-call retirement loses the
already-published call-formal evidence before any successful relation stamp.
The active design-first repair must publish the malformed where-alias's exact
receiver cause, transport it through the formal/binder/lookup relation, and
preserve the call's successful shape/formal evidence under a distinct typed
retirement authority. The malformed-alias publisher alone is now reviewed and
passing; lookup/call transport is still being implemented. An annotation-wide
cause must not poison unrelated parameters. The broader default-use lifecycle extension still has the
adversarial findings inventoried below. No full checker-module gate has
completed on this tree.

- The driver resumed coordination after a service interruption. No build was
  active, and all temporary diagnostics had been removed. Only formatting and
  AST checks certify the latest source as a whole. Do not repeat the intervening
  fixture-only experiments or infer producer coverage from syntax.
- Default template roots now name the first target attempt, with canonical
  `reserved_0`, rather than a speculative SchemeUse. Committed children retain
  their actual SchemeUse. Exact Builtin owner/method lookup, identifier guards,
  durable cache-independent provider lookup, and journaled imported synthetic
  binding classification remain in place.
- A later, design-first change unconditionally roots committed
  `default_method_use` children. It is **not dynamically complete**. Exact
  diagnostic run `81416` supersedes the earlier movement hypothesis from
  `93163`: both selected movement subgraphs survive canonical rebuilding
  correctly. The bad call's live CIR payload has legitimately become
  `e_runtime_error`; `validateSelectedReceiverAnchorContext` incorrectly
  rereads that payload after immutable source validation has already passed.
  Snapshot `c009e860` changes every anchored arm to validate its unique
  immutable `DispatchSettlementSource`, preserving anchor, handle, receiver,
  movement, and uniqueness checks. Independent sol-ultra adversarial review
  found no blocking issue in this bounded production fix. This follows the
  existing source-lifetime rule and requires no new solver rewrite.
  Initial runs exposed test-only issues: `94167` expected one diagnostic but
  received three, and `82689` used `std.meta.eql` on an untagged union payload.
  Snapshot `38001be4` corrects the comparison and retains the unchanged Roc
  fixture as an honest nominal-dispatch retirement regression. It pins one
  `plus` arity mismatch, two literal-default warnings, both live and retired
  immutable sources, and byte-identical repeated boundary rebuilding. A second
  independent adversarial review accepted this test-only delta. Combined
  traced run `98717` passed all four named regressions (5/5 with aggregator,
  7/7 build steps): live source corruption, copied-component corruption,
  retired source/rebuild, and the original discarded numeric specialization.
  This bounded validator slice is accepted. The unfiltered traced run `63128`
  was intentionally interrupted after slow allocation-failure cases, without
  a completed result. Untraced Debug run `4990` was also intentionally
  interrupted at 71/1,349 after sampling exposed an admission-performance
  defect; neither interrupted run is a pass. Progress counters do not certify
  individual passes because failures may be buffered until the final summary.
  The future normalized ledger is not needed to explain the fixed anchor bug.
- Read-only sampling of the exact checker process found every sampled stack
  in repeated Builtin admission's `SmallStringInterner.validateSemanticState`
  prior-cell text-comparison loop. This is a quadratic validation cost, not
  an allocation-failure iteration in the displayed test. A bounded sol-ultra
  implementation replaced the global pairwise scan with preflight plus
  exact per-entry hash-probe validation, preserving allocation-free rejection
  of duplicate text/cells, missing entries, invalid offsets, and broken probe
  chains. The analogous redundant serial-interner occurrence scan is in scope.
  No Builtin validation is skipped or replaced with a trust-cache shortcut.
  Snapshot `d5853c8e` preserves exact admission with a first-match bijection:
  every entry must resolve to its own exact offset/id, with equal entry and
  occupied-cell counts. Collision-chain cost remains; the unconditional
  all-pairs scan is gone. The combined focused run `92407` completed green:
  13/13 build steps, 32/32 tests, comprising 30 interner tests (332 ms) and the
  previously sampled checker test plus aggregator (27 s). Coverage includes
  duplicate and missing entries, equal text at distinct offsets, empty texts,
  wrapped probes, malformed offsets/ranges/counts, lifecycle rollback and
  serialization, and 24,000/12,000-entry valid tables. Independent sol-ultra
  adversarial review accepted this base-only slice with no blocking findings:
  the preflights make all later dereferences safe, and the equal-count exact
  lookup relation preserves the deleted uniqueness checks. It remains
  allocation-free, immutable, and format-neutral. Full untraced checker run
  `9752` was started on the
  unchanged source, then deliberately interrupted after a second profile:
  2,148/2,148 samples were in fresh Builtin admission's
  `validateExpectedCallPlans`, predominantly its nested whole-plan scans.
  The suite was progressing between tests, but this is another measured
  W6b admission-performance defect. Run `9752` exited 130 without a completed
  test summary and is **not a pass**. Its exact owned process group was checked
  before interruption; all three build/test processes subsequently exited.
  The bounded call-plan slice at `4b09afa3` uses existing canonical
  call/formal/token authority to remove redundant scans. Focused run `76466`
  completed with 10/11 tests passing; the sole failure is the new large-group
  fixture's `BuilderBox.wrap` body returning a structural record where its
  annotation requires the nominal wrapper. This is not a green gate.
  Adversarial review also found an unjustified stronger check in that slice:
  not every raw live call node has reached call-shape publication. For example,
  canonicalization can reject a record builder after canonicalizing its field
  expressions, leaving unreachable child calls in the node store. The driver
  has requested removal of that newly added raw-call/root converse, an authentic
  rejected-parent regression, and correction of the nominal fixture. Existing
  token validation and exact root/formal/argument relations must remain intact.
  The optimization must preserve allocation-free admission, exact ownership,
  and zero-argument behavior; no new cache, schema, or validation bypass is
  authorized. That intermediate slice was unaccepted; the subsequent
  corrections and verification below supersede its status. The known false
  default-owner fixture still must be corrected before a full checker rerun.
  Correction `59c13262` removes only the unjustified raw-call/root converse,
  fixes the record literal to `{ value: value }`, and adds a real missing-`map2`
  record-builder regression asserting exactly two orphaned child calls and
  successful call-plan admission. Independent sol-ultra correction review
  accepted this delta; the existing token converse and ordered root/formal
  bijection remain intact. Corrected focused run `29097` completed with 11/12
  tests passing: the large-group fixture passed, but the orphan regression
  panicked during initialization (`checked module produced invalid call
  Expected plan topology`) before its assertions. This is not a green gate.
  The next narrow diagnosis is the existing shared snapshot predicate's
  requirement for a nonzero checker-authored call relation even when called
  by the checker-independent canonical-token validator. Canonicalization
  explicitly creates calls with that optional relation absent; only checking
  publishes it. Canonical topology and checked-root validation must preserve
  that phase distinction, with the established-root nonzero requirement still
  pinned by a negative test. No guessed reachability or missing-plan recovery
  is permitted.
  Correction `fd2e9f76` separates canonical topology decoding from the existing
  checked-call snapshot's nonzero-relation requirement. Only token validation
  uses the canonical helper; all prior checked/root/retirement consumers retain
  the existing stamp check. Independent sol-ultra review accepted the bounded
  correction. Targeted run `67721` completed green: 3/3 tests (two named tests
  plus aggregator), checker runner 14 seconds. The authentic orphan case now
  reaches its assertions; the corruption test removes a real checked call's
  stamp and requires tokens to remain valid while call-plan admission rejects
  it, then restores the positive state. Unchanged-source broader six-filter
  run `28353` also completed green: 12/12 tests, checker runner 1 minute,
  checker compile 59 seconds, with Builtin prerequisites cached. The bounded
  call-plan performance and phase-boundary correction slice is accepted.
  The unconditional nested scans are removed; the multi-test runtime is not
  a like-for-like speed measurement against the earlier single-test run.
  No completed full checker gate is claimed. The next dispatched sol-ultra
  task is fixture-only: authentic accepted copied default, rejected-first
  cache seed followed by accepted reuse, and same-decision/same-method
  distinct-offset committed children. Snapshot `550de3c0` contains that
  test-only patch. It replaces the false owner fixture with the bare `f`
  arithmetic copy, and adds rejected `bad`/accepted `good` reuse and
  same-decision two-offset fixtures. Helpers tie exact contributor copy steps
  and SchemeUses to raw named assignment-pattern roots, not the potentially
  retired use node. Assertions cover actual method offsets, exact child slots
  and raw callables, rejection arities, root/use counts, diagnostics, and
  cache-cleared byte-identical rebuilding. Formatting and AST checks pass;
  independent sol-ultra review accepted the fixture-only patch as honest,
  without a helper or test-quality blocker. Targeted run `92762` is RED:
  1 pass / 3 crashes (including aggregator); all three initializers panic at
  `checked module produced invalid marker-copy source authority` before any
  new assertion runs. No dynamic lifecycle acceptance is claimed. Preserve
  all three source inputs and localize the accepted-owner fixture's exact
  failing row/predicate before a production change. The aggregate validator
  also calls `validateSelectedMethodDecisionsContext`; its selected-only
  movement/evidence ownership converse is a candidate to audit alongside the
  cross-root provider checks, not an established cause. No full checker run is
  active, and these fixtures must not be weakened to avoid the failure.
  Diagnostic snapshot `314fca99` adds temporary stage/row labels only; accepted
  owner-only run `32230` completed but its output was lost during context
  compaction, so it establishes no result. Root's unchanged-source rerun
  `50247` failed compilation: a temporary print referenced the nonexistent
  top-level `DispatchSettlementSource.original_node_kind` field. No fixture
  executed in that run. Correct that diagnostic only, then repeat the same
  owner filter with output chunks preserved before display. Corrected
  diagnostic snapshot `9b4dcce0`, root run `64954`, completed RED (1 pass /
  1 crash): only `TEMP default-source stage=selected-decisions` fired.
  Cross-provider checks passed; neither the source-row nor final orphan
  discriminators fired. The exact earlier false return inside
  `validateSelectedMethodDecisionsContext` still needs line-level isolation.
  Diagnostic snapshot `f6d8bc6d` labels every false exit in that validator
  with its source line and relevant row; compound decision predicates include
  guarded raw-coordinate details. Formatting and AST checks pass. Root's
  identical owner-only run `56299` completed RED (1 pass / 1 crash). The exact
  failing predicate at diagnostic line 7602 is decision 0's
  `matching_dispatch_uses != 1`: two `dispatch_target` SchemeUses have the same
  raw `slot_data` callable, while `owner_binding_matches` is true. The loop
  counts raw callable alone. Trace both exact publication lifecycles before
  deciding whether the producer duplicated a committed use or the inverse
  chose an invalid uniqueness domain; no relaxation or deduplication is
  authorized by this diagnostic. The selected-only movement ownership gap
  is not this observed predicate. All chunks were preserved before display;
  no production behavior or fixture changed. Independent review accepted
  the TEMP patch as diagnostic-only and found complete false-exit coverage
  with preserved predicate order and guarded diagnostic reads.
  Narrow diagnostic snapshot `87f50586` additionally prints both matching
  SchemeUses and their exact owning copy-step/default/selected coordinates;
  root run `23403` completed RED (1 pass / 1 crash), confirming both exact
  producers: SchemeUse 1 at node 10/raw callable 240/scheme root 252 belongs
  to step 4 `default_method_use`, default decision 0/offset 0/root step 3;
  SchemeUse 2 has the same node/callable/root and belongs to step 5
  `scheme_use`, selected decision 0/constraint 2/root step 3/use step 5.
  Static producer tracing identifies the missing
  handoff: successful default compatibility commits its method copy and
  dispatch-target SchemeUse but never populates
  `dispatch_target_instantiations/by_fn_var`; its immediate ordinary-queue
  replay can therefore create a second selected target for the same raw edge.
  The proposed correction shares exact preflight and atomic target publication
  between selected and default paths, preserving the default child as the sole
  method copy. It must preserve parent lineage, canonical state/growth checks,
  rejected-probe/OOM rollback, and exact target-binding identity. Independent
  review initially treated the two rows as legitimate proof/executable uses,
  then withdrew that interpretation against the normalized single-settlement
  inverse and the documented sole-target contract. No consumer-uniqueness
  relaxation or new compatibility-only SchemeUse class is planned. The
  production correction is now assigned to the sol-ultra implementer. Amend
  the design before code to state the actual default target-selection timing:
  the default target is latched while the receiver is flex, unlike ordinary
  concrete selection. Share preflight/commit without guessing a post-unify
  shape, and preserve lineage termination and deferred-child ordering. Tests
  must assert one target SchemeUse/cache entry owned by the default child,
  no second selected decision, exact ordinary replay reuse, rejected
  arity/signature/OOM cleanup, siblings, and derived parent/state behavior.
  Source and fixtures are not yet fixed; no production acceptance is claimed.
  The chosen timing rule is now declared in `design.md`: capture the actual
  current receiver/callable state immediately before the first per-edge target
  copy and unification, whether still flex or already grounded by a latched
  sibling. Every new edge, including a parentless edge, records its digest.
  A cache hit preserves the first producer's target/provenance. The core
  shared preparation/publication refactor and one-target assertions for the
  original three fixtures are being implemented; temporary diagnostics must
  be removed before the first core freeze. First run those three regressions,
  then add the complete signature/OOM/lineage coverage and rerun the existing
  recursive-dispatch/finite-chain matrix. The proposed authentic lineage
  fixture is `Parent.make : Parent -> a` with written `a.from_numeral` and
  `a.plus` requirements and a discarded call result. Its copied plus edge
  should default with the make edge as its recorded parent. A plus-only where
  requirement is not arithmetic-defaultable; the from_numeral hook is
  essential. The fixture is not dynamically verified yet.
  Core snapshot `4a47583a` freezes the shared target preparation/publication,
  default cache handoff, exact cache-hit parent/binding checks, restored
  enclosing evidence-target context, and sole-target assertions in all three
  original fixtures. The bad-arity case additionally requires no target
  list/map row or dispatch-target SchemeUse. All TEMP diagnostics are removed;
  formatting and AST checks pass. `git diff --check` is unavailable in this
  jj workspace and is not a verified whitespace check. Root three-filter
  run `14544` completed RED (1 pass / 3 failures, not crashes): all three
  initializers now pass the former source-authority panic and fail an
  assertion `expected 5, found 7`. Root and reviewer identified the shared
  helper's invalid raw/canonical comparison:
  `testDefaultDecisionForBinding` correctly selects `use.scheme_root ==
  raw_pattern`, but incorrectly compares that raw pattern with canonical
  `step.source_root_var`. The producer/rebuilder explicitly distinguishes
  those coordinates. The test-only correction must validate ranges, follow
  the step's named root occurrence, assert its raw source is the exact pattern,
  then follow its canonical pair and assert the step-root and resolved-source
  relations. No shape search or numeric-expectation substitution is allowed.
  All output chunks were preserved. Independent sol-ultra core review found
  no production blocker on that exact snapshot. Test-only snapshot `0d1fdf49`
  corrects the helper with bounded raw-occurrence/canonical-pair checks;
  production and design are unchanged from `4a47583a`. Root's identical
  three-filter rerun `68687` completed GREEN (7/7 build steps, 4/4 tests
  including the runner; test execution 21 seconds / 25 MB peak RSS), with
  every output chunk preserved before display. Independent sol-ultra review
  accepted the helper correction with no issue.
  Signature rollback, forced OOM at the real transaction owner, non-null lineage,
  reverse cache reuse, and the existing recursive-dispatch matrix remain
  required before this bounded slice is accepted. The implementer is now
  adding that coverage and clarifying the design's distinction between the
  immutable pre-copy digest and growth comparisons over stored raw vars.
  Coverage review corrected an impossible test demand: `beginCommitProbe`
  rejects any enclosing solver Probe, not only another CommitProbe. Do not
  wrap default compatibility in an extra Probe or relax that invariant.
  Its production CommitProbe owns rollback of inner marker-copy effects.
  Allocation-failure tests must exercise that real lifecycle, including legal
  root-only precommit state and exact child/SchemeUse/target atomicity.
  Coverage snapshot `39f52c74` adds a same-arity bad-then-good signature
  regression with an explicitly annotated `Str` operand, authentic non-null
  default-child parent lineage, and reuse of a real selected `Dec.plus` target
  through the shared cache lookup helper. The reverse test does not invoke the
  entire default-compatibility pipeline. The sole-default helper additionally
  checks ordinary-worklist settled consumption. Formatting and AST checks
  pass; root's new-three-filter run `33032` and the independent exact-delta
  review were started together. Run `33032` completed RED (2/4 tests passed,
  2 failed, no crashes): the reverse-cache helper test passed; the parent
  source parsed `Parent.Value.make` as associated lookup and reported Does
  Not Exist; the signature test returned `TestUnexpectedResult` without an
  identified assertion because stack tracing was disabled. Do not attribute
  the latter to production before locating its exact failure. Rerun the
  original three fixtures with the recursive
  matrix because the shared helper gained that assertion. Forced-OOM coverage
  is not implemented yet. Design prose now distinguishes immutable digest
  repetition from growth over stored raw vars and states the embedding
  direction precisely; no production code changed in this coverage slice.
  The coverage review confirms the authentic parent/re-drive and signature
  fixtures, but requires a test-quality correction to reverse lookup: select
  the complete decision and exact SchemeUse first, then consume its authored
  `constraint_index`; do not assume the entire constraint-history pool is
  unique by raw callable. Rename that test to describe shared-cache helper
  coverage, and assert the retained negate sibling's offset precedes the
  rejected plus offset. Corrected snapshot `85f7ccf8` applies those changes
  and uses `(Parent.Value).make()` for value dispatch. Formatting and AST
  checks pass; root's focused new-three run `91187` with
  `-Ddebug-gpa-traces` is active to identify the signature assertion. This
  flag is for focused diagnosis, not the default for broad suites. Review
  includes the exact correction delta; no production change was made.
  Independent review accepts the corrected static coverage and design prose;
  the signature assertion and corrected parent fixture still need dynamic
  results. The reviewed finite OOM plan uses unchecked source `main = 5 + 6`,
  normal expression-checking producers before finalization, and the exact
  lhs literal-creation driver plus introduced constraint. Measure the
  allocation delta of `checkFlexVarConstraintCompatibility` once with failures
  disabled and resize failure forced, then recreate the staged source and
  fail each allocation in that measured operation. Route checker, type-store,
  and CIR allocators only around compatibility; construct the staged Env with
  the disabled failing allocator and keep that allocator alive through all
  deinitialization. Do not overwrite the managed variable-map allocator:
  `beginProbe` moves the old map aside and clones its live replacement with
  the current checker allocator; rollback restores the original map.
  Check child/SchemeUse/target atomicity and real probe, evidence-site, and Env
  restoration; allow legal pre-probe decision/import-root state and capacity
  changes. This tests the shared transaction from an authentic ordinary
  literal, not specialization materialization; full-source specialization
  regressions supply that separate lifecycle coverage. No broad `checkFile`
  sweep, synthetic postcheck cache mutation, or new test hook is planned.
  The final read-only staging audit confirms the exact setup: save the
  canonical `main` binop and lhs before `checkExpr`, match the aligned
  literal-creation ledger by that lhs source node, and obtain the active plus
  constraint/offset from its producer provenance rather than hardcoding a
  raw variable or range offset. Calibration first runs as a named success
  regression. Snapshot all Env rank-list/deferred lengths, probe and interner
  depths, evidence site, problems/snapshot marks, derivations, and SchemeUses;
  allow only complete phase-appropriate decision/import-root state before the
  target transaction. Require an induced failure after the exact root exists
  but before any child/target is retained. Add an unchecked borrowed-admitted-
  Builtin fixture API only if calibration demonstrates repeated admission cost.
  Traced run `91187` completed RED (2/4 tests passed, 2 failed, no crashes).
  Signature now stops at the newly added `bad_negate.offset < bad_plus.offset`
  assertion, so it has not yet exposed the original `39f52c74` signature
  failure. The corrected parent source checks cleanly but has no plus
  `default_method` root; the fixture's assumed default topology is unproved.
  Shared-cache reuse passes. The next diagnostic-only patch will print exact
  constraint/root/child/target topology and observe, rather than stop at, the
  disproven order assertion so tracing can reach the original failure. No
  acceptance follows from that temporary bypass; retain sibling assertions
  and remove all TEMP observations before the eventual clean rerun. No
  production bug has been established by these fixture failures.
  Diagnostic snapshot `91627c01` is running as traced two-filter session
  `28160`. The offset observation and parent topology dump are in the intended
  fixtures, but the full signature topology call accidentally landed in the
  older arity-rejection test and will not execute in this run. Keep the useful
  original-signature stack/offset and parent outputs distinct from that missing
  dump. Review confirms production is unchanged; the test-only helper also
  prints unchecked start-plus-length sums, safe for these already validated
  fixtures but not a generally corruption-safe diagnostic. Every TEMP site
  must be removed before acceptance.
  Run `28160` completed RED (1/3 tests passed, 2 failed, no crashes). It
  identifies the original signature failure exactly: the snapshot helper
  rejects `rejected_args[1]` as `Str`; both outer arities, expected `Dec`
  argument, bad-plus absence, good-plus root reuse, and negate-child target
  assertions pass before it. Inspect the rejected argument's actual snapshot
  representation before changing that check. The bad decision has plus
  offset 0 and negate offset 1: negate proves successful later continuation,
  not preservation of an earlier sibling. The clean parent fixture has zero
  default decisions and three target rows: local make raw callable 293, then
  Builtin from_numeral 298 and plus 304, both with parent 293 and complete
  selected decisions. It establishes parent lineage but takes ordinary
  selection, not the required default-child path. Keep that distinction; an
  inferred arithmetic-only target signature is a candidate replacement to
  investigate, not yet a verified fixture. No production defect is proven.
  Corrective/diagnostic snapshot `d21034e5` removes the entire broad TEMP
  helper and its misplaced call. Signature asserts plus precedes negate,
  describes later continuation, and keeps one bounded snapshot-content /
  formatted-value print immediately before the still-failing `Str` check.
  Parent now uses inferred `make = |_| { my_dec = 7.Dec; |x| x + my_dec }`
  with direct tuple discard of `(Parent.Value).make()` alongside the inspect
  closure. Static tracing establishes a copied nonliteral desugared-plus
  instantiation candidate with the make parent; the unchanged assertions
  still require an actual default child, not an ordinary-parent substitute.
  Root's traced two-filter session `96488` is active. Independent review
  accepts this exact correction; dynamic pinning behavior and the rejected
  snapshot representation remain pending. Formatting and AST checks pass.
  Run `96488` completed with 2/3 tests passing and one signature-fixture
  failure. The authentic non-null default-parent fixture passes all its
  default-root/child/sole-target/derivation/re-drive assertions. The signature
  snapshot is a nominal with formatted bytes `83,116,114` (`Str`), but its
  helper compares the unqualified `idents.str`. The ModuleEnv constants
  distinguish `Str` from `Builtin.Str`; `idents.dec_type`, already used by
  the expected argument check, is likewise qualified (`Builtin.Num.Dec`).
  Verify the imported nominal producer and correct the expected Str identifier
  to the exact qualified constant, not an either-name fallback. Remove the
  remaining TEMP print and rerun original/new fixtures plus the recursive and
  deep finite-chain matrix without allocation stack tracing. No production
  change is required by the evidence so far.
  Clean coverage snapshot `29ace3cb` applies only the exact qualified
  `builtin_str` correction and removal of the last TEMP print after `d21034e5`.
  Independent review verifies that declaration/snapshot coordinate; formatting
  and AST checks pass and Check.zig contains no TEMP diagnostics. Root's
  untraced combined session `64442` is active: original three default fixtures,
  three new signature/parent/shared-cache tests, `check type - dispatch -`,
  and the deep finite nested requirement-chain resource test. Final bounded
  coverage review is active. Forced-OOM implementation is next, not included
  or certified by this run; whole W6b remains unfinished and unpublished.
  Session `64442` completed RED: 17/20 tests passed and 3 crashed. All six
  original/new lifecycle tests and the deep finite-chain resource test pass.
  The failing recursive-matrix cases are `nested Try interpolation reports
  recursive dispatch` (invalid marker-copy source authority), `weak receiver
  grounded by a later requirement discharge stays legal`, and `cap-free
  generational discharge across pending scheme requirements` (both: one
  instantiated source constraint mapped to two destination constraints).
  Root's exact-three traced rerun `55042` is active on unchanged `29ace3cb`
  source. The implementer is tracing the mapping failures; the independent
  reviewer is tracing nested-Try authority. No production attribution or
  invariant relaxation is justified before exact stacks/producer evidence.
  Resolve these targeted failures before returning to the combined matrix;
  the reviewed staged OOM calibration/sweep is deferred, not waived.
  Traced session `55042` completed RED (1/4 tests passed, 3 crashed). Both
  mapping crashes originate in `canonicalizeWhereMarkerInstantiationConstraintPairs`
  via `publishLocalWhereMarkerCopyStep` →
  `instantiateBindingVarWithMarkerCopyInternal` →
  `instantiateExpectedCallBindingVar` → `instantiateExpectedCallCalleeForFrame`,
  not the similarly worded predeclared-annotation event check. Nested-Try
  fails inside `validateSelectedMethodDecisionsContext` as reported by the
  source-namespace validator; its exact inner predicate still needs isolation.
  No build is active at this checkpoint. The implementer owns any diagnostic
  edits in Check.zig; the reviewer supplies the independent nested-Try trace.
  Retain all existing tests and validation rules while identifying exact
  colliding constraint pairs/components and the selected-decision false exit.
  Static mapping trace identifies the concrete collision path: a weak
  descriptor-owned constraint S is also stored directly by a `.creation`
  TypeScheme requirement capture. Requirement-receiver ingress ordinal 0
  takes the rank-share cut and records S→S; its independent callable ingress
  appends copied D and records S→D. A diagnostic must pin those rows and
  ingress witnesses before production correction. The implementer proposes
  giving creation captures a distinct detached occurrence C (S→S and C→D),
  but this remains unapproved pending design, marker/evidence movement,
  terminal ownership, liveness, and Probe/OOM review. Do not merely suppress
  either row or relax the functional assertion. The same diagnostic freeze
  should identify every false exit in selected-method validation without
  changing predicate order, so nested-Try is localized in one run. The outer
  source-namespace stack does not prove the inner validator source loop passed.
  Diagnostic snapshot `59c093ef` is frozen and running as root traced
  exact-three session `80909`. It labels every selected-context false exit,
  preserves the dispatch-use/owner-binding short circuit, and dumps conflicting
  source/destination constraint rows plus exact requirement-ingress witnesses
  before the unchanged canonicalizer. Independent adversarial review accepts
  it for diagnosis only: additional reads are bounds-guarded, witness
  coordinates are printed without dereference, and no validation or mutation
  semantics changed. All TEMP code must be removed before acceptance.
  Capture detachment remains a proposal: distinguish raw-variable
  functionality from constraint-pair identity/primary-witness coverage and
  verify full terminal ownership rather than preserving one assertion alone.
  Session `80909` completed RED (1/4 tests passed, three crashes). Both
  constraint collisions are now dynamically pinned: requirement receiver
  ingress ordinal 0 is exact raw identity and contributes S→S, while its
  paired function ingress contributes S→D. The receiver has no primary
  static-dispatch function witness for S→S. The implementer is auditing the
  narrower producer correction: a virtual requirement-receiver identity cut
  publishes its endpoint occurrence, not descriptor-owned constraint-copy
  identities; ordinary structural share cuts retain their identity proofs.
  Capture-time cloning is not approved or implemented. Nested-Try fails
  specifically in `selectedReceiverOwnerBindingMatches`: decision 4,
  constraint 6, raw callable 413, receiver 412, whose current content is
  `err`, with provider type/definition 17457. The independent reviewer is
  tracing the immutable selection authority needed across error poisoning.
  No build is active; no validator weakening or publication is authorized by
  these diagnostic results. Remove all diagnostic labels before acceptance.
  The completed static audit supersedes both capture cloning and source
  reservation proposals: `recordIdentityConstraintProofs` overpublishes
  descriptor constraint pairs at every terminal share cut. Those cuts do not
  traverse the constraints and therefore cannot author their required primary
  function witnesses. The approved bounded correction removes that helper and
  both share-cut calls, retaining the exact raw occurrence and typed cut
  witness. Fresh structural copies and detached function ingresses retain
  their actual constraint-pair publication. Tests must distinguish both
  share-leaves/rank cuts from fresh and memoized copies, including a detached
  callable which mentions the shared receiver again. This is an untested
  implementation task at this checkpoint, not an accepted result.
  The independent review also specifies the selected-owner correction:
  persist the exact nominal/alias selection input's module identity and source
  declaration inline in `SelectedMethodDecision`, passed from the producer
  before target work. Both selected root-origin replay and terminal selected
  context replay must use that immutable owner tuple, not the receiver's
  post-error descriptor. The existing exact raw receiver-anchor and movement
  inverse remains mandatory. Reserved rows carry `none`; completed rows require
  bounded, valid owner coordinates and the exact owner/method/provider binding
  relation. Rebuild retains the tuple unchanged, and serialization/schema tests
  must cover it. The authentic nested-Try error is the positive; wrong owner,
  declaration, provider binding, and raw receiver are rejection cases. This
  follow-on implementation has not begun at this checkpoint.
  Clean share-cut snapshot `6e869f7f` removes the overpublication and all TEMP
  diagnostics. It also rejects local equal-index constraint pairs at producer
  canonicalization and artifact admission; cross-module equal integer indexes
  remain distinct namespace occurrences. Each admitted pair now requires one
  copied source. Three new types tests pin both terminal sharing policies and
  fresh/memoized copy publication; the detached scheme fixture now attaches S
  to its shared receiver and checks the sole S→D pair plus an injected S→S
  rejection. Root types session `86381` completed GREEN: 4/4 steps, 6/6 tests
  under `instantiator proof:`. Root traced checker session `40510` is active
  on this frozen source: detached-source converse, weak receiver, and cap-free
  generational discharge. Independent final review of the bounded diff is
  active. The forged S→S test pins identity rejection (the early equality guard
  now rejects it before the primary-function converse); do not claim that test
  isolates the latter predicate. Nested-Try owner authority remains next.
  Session `40510` completed GREEN: 7/7 steps, 4/4 tests, 21 seconds / 30 MB
  test-runner peak. The share-cut slice is accepted after independent final
  review: the two local pair producers both append fresh constraints, the
  local canonicalizer cannot see cross-module pairs, and equal numeric
  cross-module indexes remain valid. Source `6e869f7f` is the latest tested
  production checkpoint. No build remains active. The agents now exchange
  implementation/review roles for the selected-owner snapshot task described
  above; the full recursive matrix and handoff OOM sweep still follow it.
  Selected-owner snapshot `ee5bfcdd` is frozen for the first serialization/hash
  gate (`99546`, compile-module filters for the W6b table roundtrip and
  `MODULE_ENV_VERSION_HASH golden value`). It adds inline owner coordinates,
  threads the original matched nominal/alias tuple through selection, replaces
  both live-descriptor consumers, extends the exact-owner corruption fixture
  and nested-Try positive, adds nonempty static/mutable/OOM serde coverage, and
  advances cache version to 79. The old hash golden deliberately awaits measured
  output. Final adversarial review found a blocker: owner-key lookup does not
  itself prove that the source-declaration coordinate names an alias/nominal
  statement. Add `ownerModuleEnvSourceDeclMatches` before the binding lookup,
  with forged owner-key/non-type and out-of-range corruption coverage, after
  the active run ends. The test helper was already corrected to execute terminal
  validation independently even when root-authority lookup rejects. No owner
  implementation acceptance or recursive-matrix pass is claimed yet.
  Session `99546` completed at a compile error, before either test ran:
  `src/compile/test/module_env_test.zig:629` initializes `WhereMethodSource`
  without its required `source_ordinal`. No hash bytes or serde result were
  measured. The implementer is correcting that schema-stale fixture explicitly
  alongside the declaration guard and a coordinated provider-key/owner-tuple
  negative. Root will rerun the same two compile-module filters after review;
  no build is currently active.
  Correction snapshot `bf45ff14` adds the bounded alias/nominal owner-declaration
  guard before lookup, a one-past-end declaration negative, and a coordinated
  finalized-provider-key/selected-owner mutation. The latter first proves that
  the unguarded lookup still returns the original exact binding, then requires
  both contextual validators to reject; full provider entries/order flags and
  the decision coordinate are restored afterward. The stale serde fixture now
  explicitly supplies `source_ordinal = 0`. Root's identical compile-module
  rerun `73784` is active on frozen source; corrective review is active and the
  cache hash golden remains unmeasured.
  After that run, add the non-NONE one-past-end module-identity negative and
  an authentic external alias-owner positive. The reviewer identified the
  supported route: an empty-tag extension namespace declares a method whose
  first receiver annotation is a transparent alias; alias-associated blocks
  are not valid syntax. The existing alias-growth matrix uses backing/local
  methods and is not evidence for external alias owner-field publication.
  Session `73784` completed with 2/3 tests passing; the W6b static/mutable
  roundtrip and its exhaustive mutable-deserialization allocation-failure
  checks pass. The only failure is the deliberately stale hash golden. Measured
  module-env hash at cache version 79 is
  `b285b4176f97ddc225f1c48dd0eb383a9e9625b7afb7eb9b3b29229c14eca1ee`.
  Independent corrective review accepts the declaration guard and its
  guard-isolating negative. The implementer is updating the golden and adding
  the two remaining owner-coverage cases before the same compile-module rerun;
  the selected-owner checker regressions have not yet run. No build is active.
  Final owner-coverage snapshot `c5852805` updates the measured hash golden,
  adds the non-NONE one-past-end identity negative, and adds
  `selected method decision preserves its imported alias owner`. The alias
  fixture uses receiver-extension registration and asserts a nonempty sole
  selected decision with the provider identity, alias declaration (not its
  nominal backing), exact binding, and both contextual validators. Root's
  same-filter compile rerun `53835` completed GREEN: 25/25 build steps and
  3/3 tests, including static/mutable serialization and allocation-failure
  coverage. Final review found another owner-authentication blocker: Can
  registers the alias fixture's exact same implementation under both the
  `Alias` receiver-extension key and the `Extensions` declaration-owner key.
  Binding equality therefore cannot distinguish a corrupted owner coordinate
  retargeted to the other valid declaration. This snapshot is not accepted.
  The bounded correction captures the exact canonical provider `method_defs`
  entry index at the original owner-key lookup and retains it in the selected
  decision. Admission requires that indexed producer-authored registration's
  owner, method, and binding to match; it must not reconstruct the owner from
  solved types or disallow legitimate declaration-owner rejected selections.
  Tests must independently corrupt the owner and registration index, cover
  missing/out-of-range indices, and retain an authentic rejected namespace
  selection. Design/schema/serde updates and independent adversarial review
  accompany the correction. No build is active while implementation proceeds.
  The selected-marker owner matrix, nested-Try recursive regression, and alias
  positive still precede the full recursive matrix and staged handoff OOM sweep.
  Correction snapshot `d2c40cb1` adds the exact canonical registration lookup
  result, threads its index through prepared/cached targets and complete
  selected decisions, and authenticates that index in both contextual
  validators. Missing/out-of-range and independent alias/namespace-coordinate
  negatives are present; the forged non-type-key negative carries its exact
  post-sort index to continue isolating the declaration guard. An authentic
  rejected namespace selection retains its declaration-owner row. Nonempty
  serde includes the field, cache version is 80, and the hash golden awaits
  measurement. Root's combined untraced checker run `36386` is active on frozen
  source: the three owner fixtures, all twelve recursive-dispatch cases, deep
  finite-chain resource regression, and six default-target lifecycle cases.
  Independent adversarial review is active; no dynamic owner acceptance yet.
  Run `36386` stopped at compilation before tests: the new namespace fixture's
  `problem` capture shadows Check's imported module. The immediate correction
  renames that test capture. Review also identified a documentation-only
  conflation: the post-check `(MethodOwner, MethodNameId)` registry does not
  return canonicalization's `MethodBinding`/provider row. Restore that registry
  paragraph and keep the indexed lookup rule explicitly attached to checker
  `ModuleEnv.method_defs`. No production behavior change is required by either
  finding. The identical checker matrix follows the corrected freeze.
  Snapshot `37d1d88f` contains only that test-capture rename and stage-specific
  design correction on top of `d2c40cb1`. Root's identical combined checker
  rerun `73206` is active on frozen source. Final review is confirming this
  snapshot; the version-80 hash golden remains intentionally stale until the
  compile-module measurement.
  Run `73206` completed RED: 21/23 tests passed, two failed, no crashes. All
  twelve recursive-dispatch cases (including nested-Try), the 80-layer finite
  chain, all six default-target lifecycle regressions, and the original exact
  owner corruption fixture pass. Thus all three original recursive crashes
  are resolved on this production snapshot. The new imported-alias fixture
  fails because `Alias` is not exposed by its headerless provider; the rejected
  namespace fixture likewise has one canonicalization error. Correct both
  providers with explicit module exports, preserving their authentic
  receiver-extension registration and all assertions, then rerun those two
  failing filters before the combined matrix. The production fix does not
  change for these source-fixture errors.
  Review clarified the bounded ownership contract: owner tuple plus canonical
  provider-row index is the atomic checker-authored selection input. Admission
  rejects independent substitution of either coordinate and validates the
  selected canonical registration; it does not independently reconstruct the
  receiver's pre-poison owner. The raw anchor and movement proof authenticate
  the exact occurrence separately. A jointly consistent replacement tuple/row
  is not rejected by this bounded contract; moving the same assertion into a
  second ledger would supply no new earlier authority. State that limit in
  the design rather than claiming a stronger inverse. Normalized settlement
  work remains separately required. Source is thawed for those fixture and
  documentation corrections; no build is active.
  Correction `a4b4ab42` explicitly exports `Base`, `Alias`, and `Extensions`
  from both new providers without changing registrations or assertions, and
  clarifies the bounded selection contract in design.md. Independent final
  static review accepts the complete owner-registration slice with no
  production blocker: exact lookup/cache identity, bounded dual admission,
  reserved/complete lifecycle, recursion rule, rebuild/probe/serde ownership,
  and corruption-test scope are verified. Root's exact two-failure rerun
  `73769` is active on frozen source; dynamic acceptance and version-80
  serialization/hash verification remain pending.
  Run `73769` completed RED: 1/3 passed, both fixtures stopped at provider
  `assertNoErrors` because explicit module headers emit the expected sole
  `Module Header Deprecated` diagnostic. Neither consumer executed. The next
  test-only correction retains this still-supported explicit-export form,
  asserts exactly that provider diagnostic plus zero checker problems, and
  preserves every strict consumer assertion. This does not suppress other
  diagnostics, alter registration topology, or change production code. The
  existing imported-partial-scheme regression uses the same supported header
  form. Rerun the same two filters after the corrected freeze.
  Snapshot `f34b49ef` pins exactly the sole provider deprecation diagnostic
  and zero raw checker/type problems in both fixtures. Independent review
  accepts this delta: no diagnostics are suppressed, and both consumers'
  strict assertions remain unchanged. Root's identical two-filter run `8365`
  is active on frozen source. Disk preflight has ample free space; no disk-use
  error or cache deletion has occurred.
  Run `8365` completed RED: 2/3 tests passed. The rejected namespace fixture
  now passes its exact method-type diagnostic, declaration-owner registration,
  and both contextual validators. The alias fixture reaches its consumer but
  reports `Missing Method` on `Base`: initializing an annotated alias value
  from the concrete backing constructor resolves away the transparent alias
  before dispatch. Correct the fixture to dispatch through an explicitly
  annotated alias function parameter, without that prior backing-value
  unification. Preserve the sole selected decision and all held-coordinate
  corruption assertions; do not change alias typing or manufacture receiver
  state. Rerun this remaining failure first. The next implementer is drafting
  the staged OOM calibration outside the repository while source gates run;
  it is not applied or tested yet.
  Snapshot `d03819dc` changes only the alias fixture's consumer to
  `out : SelectedAliasProvider.Alias -> [One]` and
  `out = |value| value.run()`, retaining every registration/decision/corruption
  assertion. Root's alias-only rerun `95961` is active on frozen source;
  independent correction review is active. The namespace positive already
  passed in `8365`; do not count the pending alias run as a pass.
  Run `95961` completed RED: 1/2 tests passed. The parameter variant also
  reports `Missing Method` on `Base`; the claimed alias-selection ingress is
  disproven. Stop fixture guesses. Read-only tracing identifies why ordinary
  value dispatch cannot pin this case: both `mkReceiverDispatchConstraint` and
  `mkTypeMethodCallConstraint` unify a newly constrained flex with the
  receiver; unification explicitly directs a constrained-flex/alias relation
  to the alias backing. Type-directed syntax alone therefore does not solve
  this. The agents are auditing other real deferred-constraint producers before
  any further fixture edit. If no authentic external alias selection exists,
  the same-binding corruption regression should instead use a real nominal
  receiver-extension key and its namespace key, not change alias typing or
  manufacture solver state. Root's independent compile-module serde/hash
  measurement `28145` is active on unchanged `d03819dc`; repository source
  remains frozen during this read-only diagnosis.
  Run `28145` completed with 2/3 tests passing: version-80 static/mutable
  serialization and exhaustive mutable-deserialization allocation failures
  pass; only the deliberately stale golden fails. Measured hash:
  `5811839cadd809780e4dd04848db8f0c882bfc460662c21c077353dac4ae6a24`.
  Driver decision: the owner-authentication bug requires identical bindings
  under two distinct canonical owner keys, not an external alias selection
  specifically. Replace the invalid alias-positive fixture with an authentic
  nominal `Base` receiver-extension registration under `Extensions`; require
  the exact Base row, identical binding under the namespace row, and both
  held-coordinate corruption rejections. This retains the actual bug coverage
  without changing alias semantics or claiming an unproved alias ingress.
  The already-green rejected namespace fixture keeps its Alias signature.
  Broader alias-producer exploration is not an acceptance requirement for this
  bounded correction. The implementer is making that test-only change and
  updating the measured hash golden; no build is active.
  Snapshot `48d3aa97` implements the truthful nominal receiver-extension
  fixture (`selected method decision preserves its exact receiver-extension
  row`) and the measured version-80 hash golden. It keeps the same-binding
  two-row setup, independently corrupts the Base owner and namespace row
  index, checks both contextual validators, and preserves the already-green
  rejected namespace fixture. No production code changed. Root's single
  focused check+compile build `4265` is active on frozen source with those
  two owner filters and the W6b serde/hash filters; independent delta review
  is active. The failed alias fixtures establish backing dispatch for their
  attempted routes, not global unreachability of the checker alias branch.
  Run `4265` completed GREEN: 28/28 build steps, 6/6 tests. Checker owner
  fixtures pass (3 tests with aggregator, 15 seconds / 26 MB); compile
  serialization/hash tests pass (3 tests, 1 second / 3 MB), including exhaustive
  mutable-deserialization OOM coverage. Independent final adversarial review
  accepts `48d3aa97`; the selected-owner/registration correction is complete
  as a bounded prerequisite. All three original recursive crashes already
  passed on its unchanged production code in `73206`. The agents now exchange
  roles for authentic staged default-target OOM calibration: the former
  reviewer implements, the former implementer reviews, and root alone runs
  builds and jj. A scratch directory was reserved at
  `/private/tmp/w6b-oom-kPfBID`, but the final test was applied directly in the
  repository after the source thaw; no scratch implementation was used.
  Calibration/success is first, finite measured allocation-failure coverage
  follows; no new compiler behavior or test hooks are authorized. Root will
  repeat the owner/recursive/default matrix alongside the calibration test.
  Snapshot `34d5b641` adds the authentic staged calibration test and a
  test-only allocator-routing helper. It stops normal expression checking
  before default finalization, drives the exact lhs numeral's `plus`
  compatibility edge, and checks the transient decision, cached imported
  root, committed child/SchemeUse, and sole raw-edge target. Independent
  adversarial review accepts the corrected raw-occurrence versus canonical-
  pair assertions and allocator lifetime. Root run `68496` completed GREEN:
  7/7 steps, 24/24 tests, 2 minutes test runtime / 33 MB, including the full
  owner/recursive/default matrix. Calibration measured 58 allocations. The
  finite sweep is now authorized on thawed source, using runtime calibration
  and a fresh authentic staged checker for every failure index. It must remove
  the temporary allocation-count print and prove exact rollback while allowing
  complete pre-transaction draft/imported-root support to remain; calibration
  alone is not OOM acceptance.
  The finite sweep is implemented under `staged default target compatibility
  is atomic across every routed allocation`. It uses identical module/source
  setup and the same resize-failure boundary for calibration and every fresh
  injection. Review requires exact per-rank/deferred/diagnostic/snapshot/
  SchemeUse/derivation rollback, no partial target or child, and only a complete
  authenticated imported root as a permissible copy-step suffix. Final static
  review also requires the two exact transient contributor entries (explicit
  driver plus seeded lhs registration) and the lhs-only registration assignment;
  that tightening is complete and independently accepted. Frozen snapshot
  `4ba47cf3` failed root's focused run `39352` at compile time, before tests:
  the new region-length snapshot/assertion used `.items.len` on a SafeList
  instead of `.items.items.len`. The implementer is correcting those exact
  field accesses before rerunning the same filter; dynamic OOM acceptance
  remains pending. No production behavior changed in this test slice.
  Snapshot `f5859e5c` corrects the two SafeList accesses and passes independent
  delta review. Root run `19689` compiled and executed, then failed the sweep
  with `expected 221, found 224` (1/2 tests passed including aggregator; no
  stack trace). The exact assertion and failure index are not yet localized;
  do not label this a production rollback defect or relax the assertion based
  on that number alone. The next focused patch adds bounded diagnostics and
  the independently reviewed phase-aware pair/occurrence/witness suffix checks:
  no imported root means unchanged lengths, while a surviving root must own
  the complete new suffix of each pool. Root import precedes CommitProbe, so
  complete imported support may legitimately survive a later failure.
  Independent producer audit establishes that unconditional region-length
  equality is invalid in the surviving-root phase: the imported-root Probe
  commits `postProcessCopiedVars` / `fillInRegionsThrough` before the child
  CommitProbe. The 221-to-224 result is consistent with that three-variable
  root prefix, though its original assertion was not labeled. Replace the
  invalid unconditional check with exact phase-aware type/region bounds
  authenticated by the surviving root's raw destination occurrence prefix;
  absent root still requires unchanged lengths. A bounded failure-index/stage
  diagnostic will localize any remaining failure. No production fix is
  justified by this result.
  Snapshot `3767f007` implements the independently accepted phase-aware
  correction and exact three-pool suffix checks. The imported copy allocates
  one append-only destination prefix, and the speculative child allocates
  strictly afterward: bounding every root destination below by the baseline
  and equating its maximum-plus-one with both type and region ends excludes
  leaked child variables. This test does not re-prove interior occurrence
  completeness, which belongs to canonical copy-proof validation. Permanent
  error-only allocation-index/stage context replaces the TEMP diagnostic.
  Root focused run `93726` completed GREEN: 7/7 steps, 2/2 tests, 6 minutes
  test runtime / 24 MB. The finite routed-allocation sweep now passes its
  dynamic and independent review gates. No production behavior changed.
  That measured repeated-admission cost justifies the preapproved thin
  `TestEnv.initUncheckedWithAdmittedBuiltinForTesting` wrapper. Reviewed
  scratch patches create/admit one immutable Builtin source outside all
  consumers and use the same unchecked wrapper for calibration and every
  fresh failed checker; no consumer graph is reused and no admission is
  bypassed. Root caught and the implementer corrected a scratch local-name
  collision (`prepared_builtin_indices` versus the later checker-derived
  proof value) before application. Source is thawed only for this two-file
  test-setup optimization, followed by the sweep plus complete owner/
  recursive/default matrix. The next production task remains the exact
  historical invalid-import call-retirement regression.
  Snapshot `a69a5965` applies the reviewed shared-admission setup. The new
  unchecked wrapper delegates to the existing prepared-Builtin path with
  borrowed ownership; the outer test explicitly owns the view and validated
  capability, and every calibration/failure consumer is separately allocated
  and deinitialized. Applied delta review is clean. Root's combined 24-test
  sweep/owner/recursive/default gate `88439` completed GREEN: 7/7 steps,
  24/24 tests, 2 minutes test runtime / 33 MB. The bounded finite OOM slice
  and its setup optimization are complete and independently accepted. Agents
  now exchange roles for the historical invalid-import regression: first
  apply the exact saved source and bounded formal-owner diagnostics from
  `/private/tmp/invalid_import_call_retirement_diag.patch`, then identify the
  actual failing predicate before any production/schema correction. Source
  is thawed to that implementer; root continues to own all builds and jj.
  Snapshot `0656679f` restores the exact historical invalid-import source in
  `invalid imported where alias retires checked call formals without losing
  producer authority` and adds bounded read-only diagnostics at every failing
  call-formal replay stage, including guarded owner regions and token lookups.
  Independent review accepts this diagnostic-only increment; no predicate,
  schema, or typing rule changed. Root focused run `82818` reproduced the
  crash: owner node 25, source bytes 127..147 (`second(value, 1.U64)` inside
  `first`), is `.malformed`; root plan 4 / argument plan 5 retain
  `call_root` / `not_projected` / `call_shape_ready`, but the owner snapshot
  is absent and there are zero matching retirement or ambiguity rows. The
  first failed predicate is therefore now dynamically identified, not merely
  suspected from static code. The exact originating error branch/cause still
  needs tracing before production implementation; callee poisoning, argument
  relation failure, erroneous operands, and later enclosing retirement must
  not be conflated. Source is thawed for that bounded diagnosis/design work.
  Exact final Can/checker diagnostic counts remain intentionally
  unclaimed until observed; the current harness only checks the historical
  `Type Not Exposed` presence if initialization returns.
  Snapshot `933528f0` adds the independently reviewed causal discriminator:
  the existing ordinary-call error branch reports its exact callee/argument
  flags and failing relation slot, and the separate call-shape-error queue
  branch has its own label. Argument and owner-region reads remain bounded;
  no predicate or mutation order changes. Root same-filter run `40664`
  reproduces the crash and identifies the real branch: both owner 25
  (`second(value, 1.U64)`) and owner 53 (`first(value, 1.U64)`) have established
  call shapes, no call-shape problem, no erroneous callable type, and no
  argument-relation failure. Their `args_did_err` is true because slot 0's
  `value` lookup (node 23 / 51) is in both producer-authored error-flag channels;
  slot 1's typed integer is clean. The callee's returned typed status is
  established/inactive CauseOwner, even though its later dense flag is true.
  The erroneous-operand helper retires the call before its successful relation
  stamp; the subsequent owner replay still finds no retirement. The next
  bounded task traces the exact producer/cause of the erroneous lookup and
  declares the explicit ordinary-call operand-retirement mechanism before
  implementation. Neither a flag alone nor a solved `.err` supplies the
  durable cause. This is confirmed diagnosis, not a passing regression claim.
  The first bounded source-side repair is snapshot `464b5009`: the exact
  malformed where-alias target now goes through the existing
  `annotation_malformed_type` publisher with its unchanged `GenTypeAnnoCtx`
  before the same owning rigid is poisoned. It introduces no new schema or
  binder/call authority. Independent adversarial review found no issue in this
  bounded production/design delta. Root run `99667` completed with 8/9 tests
  passing: all seven prior annotation/suppression/sibling/transaction/causal-DAG
  regressions and the aggregator passed; the new no-call invalid-import
  fixture failed `expected 1, found 2`. The exact assertion/count must be
  localized before changing expectations; dynamic acceptance is pending.
  Test-only snapshot `5fa14e4e` adds bounded error-only count labels. Its
  same-test run `84696` completed red (1/2 including the aggregator) and
  identifies the first failed assertion as the canonical diagnostic count:
  the exact stream is `type_not_exposed`, then `undeclared_type`. No ledger
  cardinality assertion was reached. The fixture must pin both diagnostics
  and authenticate the malformed alias's own diagnostic rather than assuming
  it is the separate import-exposure diagnostic. No production change was
  made in response to this test failure. Corrected fixture snapshot `e21af699`
  pins both ordered diagnostic titles and the malformed alias's exact
  `undeclared_type` diagnostic; all publisher-local `TEMP` labels are removed.
  Independent review accepted that correction, and root run `92399` passed
  7/7 steps and 2/2 tests (7 seconds / 25 MB). With the unchanged production
  delta's seven existing named regression tests passing in `99667`, this
  bounded malformed-alias publisher slice is complete and accepted.
  The next authorized implementation is the explicit direct-binder path:
  select the exact published malformed-alias failure for the exact direct
  function formal and root assignment pattern, propagate it through a scoped
  binder status, reserve and complete a typed lookup retirement before that
  lookup is erased, and let the call-operand failure cite that lookup
  retirement. A distinct ordinary-call retirement preserves successful
  root/argument plans and ExpectedCallFormal rows with the original zero
  relation stamp; live and ambiguity-retired calls retain their nonzero stamp
  requirement. Canonicalization has two exact lambda-owner forms. A
  capture-free bare lambda is retired directly, so its annotation retirement's
  original lambda payload supplies the argument span. A captured closure's
  original payload names its still-live lambda: the closure checker
  unconditionally removes the lambda's queued retirement as the left operand
  of `and`, including checked-error outcomes. Validators must replay both
  closed source forms against fresh canonicalization, and tests must pin both
  lifetimes rather than assume every function has a closure wrapper.
  Multiple malformed aliases may own one direct formal, so transport must
  retain the exact selected upstream failure rather than require a unique
  matching cause in the annotation retirement. Nested/destructured binders
  cannot inherit this direct-root arm; clean sibling parameters and fields
  must remain unaffected. Implementation of this downstream slice is in
  progress; it has not reached a frozen build or independent acceptance.
  A second source-lifetime edge is now explicit: the malformed annotated
  binding's callee lookup is also erased, while call-instantiation replay
  requires its exact original binding pattern. Predeclared recursive calls
  can precede checking that binding's body, so their established lookup
  outcome must not borrow a future failure. The declared late
  `annotated_binding_lookup_checked_error` arm instead publishes at the
  existing destructive lookup-rewrite hook, after body sources exist and
  before erasing the exact payload. It cites the canonical body-malformed
  attachment and already-authored annotation failure/retirement, preserving
  checking order and requiring exact call-callee position authority. At
  producer time the annotation retirement may still be pending or may
  already be completed by an earlier sweep; terminal admission requires its
  complete cause range. This is an additional unaccepted part of the same
  historical regression repair, not a change to the original lookup's typed
  outcome. Shared call retirement completion must stage both owned-consumer
  and failure-reference ranges before committing either half, so allocation
  failure cannot strand a partial retirement.
  Driver recovery snapshot `954d63d8` records the still-unverified downstream
  edits (four-file delta from `e21af699`, including this plan). It is not a
  frozen build or acceptance checkpoint; implementation continued afterward.
  In-progress review requirements include: exact singleton lookup failure
  ownership; the source annotation retirement's closed lifecycle; reuse of an
  already-published late lookup draft after completion OOM; and explicit
  Kahn/readiness/remapping of the direct-binder subject's failure-index
  predecessor (other direct phases use different local-record namespaces).
  The driver caught a lambda-argument indirection defect: `args_start/len`
  select `NodeStore.index_data`, not a contiguous range of pattern nodes.
  That correction is now in the live edits, but untested. Fresh-source replay
  must also compare the referenced closure/lambda argument and annotation
  formal data, not only equal outer node payload words. New annotation and
  where-owner span reads need their own bounded access checks; existing
  attachment validation does not certify those ranges. The reviewer derived
  an authentic staged call-completion OOM test using a malformed-where
  annotated lambda and an immediately invoked identity lambda, with real
  lookup retirement before the live zero-stamp call's completion; this is a
  test recipe, not an executed result. A smaller lookup-only dynamic
  checkpoint is permitted if it can be isolated cleanly; the original
  historical call regression remains mandatory afterward.
  Lookup-only diagnostic snapshot `4f39ea7c` freezes the direct-binder
  producer, local/fresh admission, Kahn/remapping, Probe state, and new test
  `invalid where alias direct binder lookup retains its exact checked error`.
  Annotated-binding late poisoning and ordinary-call operand retirement remain
  unwired; their reserved schema/helpers are not accepted. Cache semantics
  advance to version 81; the hash golden still awaits an observed compile
  result. Root combined can/check run `41690` stopped before tests (1/10 build
  steps) with four compile errors: one untagged `Node.Payload` comparison,
  two incorrect `ExprLambda` type qualifications, and one missing failure-kind
  switch arm. The implementer is correcting those exact errors before a
  same-filter rerun. Formatting/AST success did not establish compilation or
  dynamic acceptance. Existing historical-call `TEMP` labels remain pending
  that separate repair and must be removed before production acceptance.
  Compile-corrected snapshot `4bcdf53f` replaces untagged-union comparisons
  with active typed payload comparisons and fixes the type qualifications and
  missing enum arm. Same-filter run `86128` still stops before tests (1/10
  steps), now with one error: the lambda-argument index-table entry is already
  `u32` and must not be passed through `@intFromEnum`. That exact correction is
  the next rerun; neither diagnostic run is a passing semantic gate.
  One-line corrected snapshot `142dffb8` passes compilation. Root run `85305`
  completes with 8/10 steps and 4/5 tests passing: can's exhaustive table and
  aggregator pass, as do the prior malformed-source fixture and check
  aggregator. The new direct-binder fixture aborts during initialization with
  `checked module produced invalid contextual W6b semantic data`. Source is
  thawed; no build is active at this checkpoint. Root and reviewer traced the
  lifecycle mismatch: cold `validateProducedW6bState` supplies the already
  rewritten module as `ImportResolution.resolution_env`, while the new
  snapshot comparison expects untouched canonical nodes. The approved bounded
  repair uses an explicit private replay-context tag: produced validation
  retains local failure/retirement/source checks without comparing original
  snapshots to final runtime-error nodes; independent fresh-canonical replay
  compares both source and lookup snapshots; admitted-cache replay rejects
  this malformed recovery family. The latter is already excluded by
  `validateCleanCacheW6bState`, which rejects malformed type/body-publication
  pools before either cache admission route. Do not invent an admitted
  snapshot reconstruction mechanism or use pointer identity as a blanket
  validation bypass. Import/provider checks remain active in every mode.
  Review also found a remaining local unchecked `annotation.anno` access:
  factor a guarded formal-at-slot helper covering root/tag, function type
  node/tag, argument index span, slot, and rigid formal node/tag, and pin
  out-of-range/wrong-tag/span corruption rejection. These fixes and further
  direct-binder coverage are not implemented or accepted at this checkpoint.
  The end-to-end bounds audit subsequently found that the common body-
  publication predicate calls `ModuleEnv.annotationOwnsMalformedTypePublication`
  before the new formal helper; its recursive annotation ownership traversal
  also trusts type-node and span coordinates. Guarding only the new helper
  would hide that earlier admission failure. The shared topology/ownership
  boundary and real entrypoint corruption tests must be addressed, including
  cycle safety without an arbitrary depth heuristic. Review is considering
  the exact role of the canonical joined publication versus contextual fresh
  replay before choosing the smallest sound repair. The private lifecycle
  tag is being wired, but generic `supplied`/`empty` constructors must not
  accidentally grant producer trust to artifact admission; in particular,
  `admitBuiltinOwned` is an untrusted entrypoint without the ordinary clean-
  cache gate. Neither this shared-boundary work nor the lifecycle repair has
  a new test result yet.
  The bounded implementation now targets stack-safe monotonic structural
  ascent, not a fresh global scan at each ancestor. Canonical TypeAnno
  structural children are emitted before their parents; identity references
  remain excluded. The producer invariant and malformed-coordinate rejection
  need independent review and tests before this replacement is accepted.
  Root and reviewer also confirmed that `WhereClauseOwner.owned_by_annotation`
  is a canonical transitive reachability result: main-annotation introductions
  seed the closure, and written method signatures or alias arguments extend
  it through locally declared rigids. Requiring every owned receiver to occur
  directly in the main type tree would reject legitimate constraint-only
  rigids. Preserve the published owner authority and clarify the narrower
  existing design wording. Any early artifact-admission preflight must remain
  allocation-free and avoid solved-type reads; the full clean-cache validator
  still belongs after structural validation because it dereferences raw type
  variables. These are review requirements, not new passing results.
  The live implementation now contains guarded structural-edge readers,
  monotonic membership ascent, bounded attachment/annotation storage reads,
  and shared malformed-alias selectors. It also names cold contexts
  `producedSupplied`, separates independent fresh replay, and uses
  `recovery_forbidden` for Builtin admission and admitted-cache republishing.
  Both public artifact entrypoints reject recovery publication pools before
  deep validation, while the full clean-cache check remains afterward.
  Terminal checking now asserts that both direct-formal source and active-
  binder transport lists are empty. These edits are still unbuilt. Remaining
  pre-freeze review findings concern preserving written clause order in the
  first-source selector, avoiding repeated quadratic owner-list membership
  counts, and ensuring short-circuit membership queries are not mistaken for
  certificates for later typed reads. A tooling interruption ended the
  implementation agent's turn; the driver resumed the same bounded task with
  all existing workspace edits preserved. No validation result or accepted
  checkpoint changed because of that interruption.
  Frozen diagnostic snapshot `8b367d9f` completed root checker-only run `64095`
  successfully: 7/7 build steps, 2/2 tests (the bare direct-binder regression
  and checker aggregator), with 7 seconds/25 MB for test execution. This fixes
  the cold contextual-validation crash observed in `85305`; it is not bounded
  lookup acceptance or whole-W6b acceptance. Independent review requires two
  follow-up fixes: `bodyAnnotationMalformedTypePublicationIsLocallyValid` must
  explicitly validate the actual attachment payload, not merely binary-search
  a matching ledger tuple; and checking each owner's written subsequence must
  not restart a full written-list scan, which is quadratic for many singleton
  owners. Use exact ordered membership and pin both contracts directly.
  A stale design sentence must distinguish the structural lookup occurrence
  from its non-structural payload reference. Source is thawed for these
  bounded fixes and straightforward exact-count/empty-transient assertions;
  the shared ownership regression gate and expanded direct-binder acceptance
  matrix remain to run afterward.
  Review-fix snapshot `b71614b8` restores the attachment helper's explicit
  payload replay, replaces per-owner full scans with ordered binary lookup,
  adds a direct coordinated-wrong-body rejection/restore check and ordered-
  membership cases, and pins the bare fixture's exact two failures, two
  retirements, two references, zero checker/type problems, and empty transient
  lists. Independent static review accepted that bounded correction. Root
  focused can/check run `41926` completed with 15/16 tests passing: all four
  can tests pass, and check has eleven passes and one failure in
  `malformed where clauses publish source ordinals and reject retargeted
  authority`. The strengthened bare fixture passes. This is not a green
  shared-boundary gate. Source is thawed for exact diagnosis; the next run must
  isolate the failing checker test. Root's leading hypothesis is its final
  mutation of an unrelated written clause: the existing test deliberately
  keeps the selected malformed failure locally exact while fresh replay
  rejects the changed complete span, but the new generic where-membership
  helper now enforces whole-span ordering. Preserve the declared local-versus-
  contextual contract rather than simply weakening the regression. Strict
  written-order authentication remains required for first-source selection.
  Narrow correction `3b9f7dd3` removes only whole-span ordering enforcement
  from generic selected-where membership; bounds/tag checks remain, as do
  strict ordering at every order-sensitive first-source/TypeAnno consumer.
  The original regression is unchanged. Independent review accepted this
  local-versus-fresh boundary. Isolated root run `53719` passes 7/7 steps and
  2/2 tests (14 seconds/24 MB); the subsequent unchanged-source full focused
  rerun `36886` passes 10/10 steps and 16/16 tests (can 4, check 12; checker
  execution 1 minute/26 MB). The lifecycle/shared-reader correction and bare
  direct-binder case are now an accepted bounded checkpoint, not completion
  of the complete direct-lookup slice or W6b.
  Source is thawed for the next bounded functional tests: an authentic captured
  annotation owner, an outer direct binder used inside a nested closure,
  repeated/destructured/clean-formal exclusions, and two malformed aliases
  selecting the exact source-ordered first cause while retaining both source
  failures. These fixtures contain no calls and must pin owner/pattern/formal
  topology, exact diagnostics and row counts, zero checker/type problems,
  local/fresh replay, and closed transient lists. Deep valid/corrupt-boundary
  cases, Probe/OOM/retry, canonical replay, and actual failure/retirement
  static/mutable roundtrips remain separate mandatory direct-slice gates.
  The existing compile-module W6b roundtrip test does not populate these
  failure/retirement tables; rerunning it alone cannot establish that new
  coverage. The version-81 hash golden still awaits an observed measurement.
  Functional-test snapshot `3ab74567` adds the four no-call fixtures and
  passed independent adversarial static review. Root run `17942` completed
  with 4/6 tests passing (the aggregator plus bare, nested-closure, and
  source-ordered multiple-alias cases). Captured-owner and exclusion cases
  stopped at their exact canonical-diagnostic counts: respectively expected
  2/found 3 and expected 5/found 7. Their subsequent ledger assertions have
  not run. Source is thawed only to identify the actual additional reports
  and correct the fixtures' diagnostic expectations without suppressing
  reports or changing their required topology; isolate those two tests next.
  The new functional slice is not yet accepted, and the last accepted
  bounded production checkpoint remains `3b9f7dd3`.
  Corrected snapshot `a42543f4` adds only the missing exact `Unused Variable`
  expectations for local `first` and formal `direct`/`bad`, plus actual-title
  reporting on diagnostic-count mismatch in the test helper. No Roc fixture
  or production behavior changed. Independent review accepted that correction.
  Isolated root run `83399` passes 7/7 steps and 3/3 tests (14 seconds/26 MB),
  proving the exact report order and all downstream provenance assertions.
  Unchanged-source full direct-binder group `7691` passes 7/7 steps and 6/6
  tests (34 seconds/26 MB). This functional slice is accepted; W6b remains
  unfinished. Source is thawed for the separate authentic direct-source,
  lookup publication, and lookup completion Probe/OOM/retry task. Prefer the
  existing `declareWhereAliasConstraints` producer seam; its final legal
  `markErroneous` has no injectable allocation at probe depth zero, so do
  not invent a post-publication mark failure. Lookup tests must calibrate a
  real append-capacity boundary using distinct canonical lookup expressions,
  not fabricate rows or mutate capacities. Completion is outside Probe.
  The minimal activation refactor now passes independent static review: the
  existing lambda call passes the same annotation, source-suffix start, and
  ownership fields through a private value scope; predicates and timing are
  unchanged. New transaction tests are still being written and are unbuilt.
  Incremental root review caught test-only confusion between a durable
  annotation failure's diagnostic cause and its transient result's retirement
  cause, an invalid generic comparison of opaque union payloads, a bounded
  allocation loop that must become a measured exhaustive sweep, and missing
  successful retry assertions on each failed instance. The live source test
  now corrects the cause distinction/comparison, measures allocation count,
  and retries/revisits every failed instance; these edits remain unbuilt and
  await final independent review. Exact consumer diagnostics, empty activation
  state in the source-only test, and post-retry erroneous content are also
  requested assertions. Lookup/Probe/completion tests are still being written.
  The second
  sol-ultra agent is implementing test-only deep/bounds coverage at EOF in
  parallel; the two agents will cross-review the disjoint tasks. Deep recovery
  validation must exercise actual local/fresh and TypeAnno ownership entry
  points: early clean-cache rejection cannot certify those deeper reads.
  The EOF deep/bounds task is now written: its 128-level fixture explicitly
  traverses fresh closures and both TypeAnno return chains, queries the deepest
  structural rigid-lookup occurrence, and restores after candidate-tag,
  fresh-index/span/attachment-coordinate, and deep-return-cycle mutations.
  The cycle is tested at the membership reader, not as a promise that failure
  replay validates unrelated syntax. Independent cross-review is active;
  `zig fmt --check` passes, but no build has run. The transaction slice now
  contains source and lookup publication tests; completion coverage remains
  in progress. A further fixture correction must distinguish the where-alias
  receiver's canonical lookup occurrence from the introducing formal identity
  and prove the exact authored owner entry. Both agents must freeze before
  root runs any gate.
  The deep test's independent review requests one accepted strengthening:
  invoke `validateExpectedFailureRetirementLocal` while the candidate formal
  tag is corrupt, before restoration. The proposed requirement that fresh
  failure context reject an unrelated deep-return cycle is not its declared
  contract; that negative belongs to the actual deepest-membership reader.
  B remains a bounded deep-positive/five-category corruption slice. Before
  complete direct-lookup acceptance, audit broader raw side-table coverage
  against existing tests: candidate function-argument spans, captured-owner
  closure-data/lambda coordinates, owner/clause/written tags and spans, and
  fresh lambda-argument/pattern coordinates. Add genuinely missing coverage
  rather than claiming the five new mutations exhaust those fields.
  Frozen snapshot `949c1e09` contains that independently reviewed B test plus
  unaccepted A source/lookup transaction work. Root B-only run `4747` crashes:
  1/2 tests pass (aggregator), and the deep test aborts with a segmentation
  fault before any `errdefer` stage label. It is not a passing deep checkpoint.
  The cached test binary's LLDB launch was rejected by macOS's non-interactive
  debugger permission policy; no stack was captured. Only temporary test-stage
  markers are thawed to distinguish parsing, canonicalization, checker entry,
  and the bounded validation mutations. Keep the 128-level source unchanged,
  remove the diagnostic markers before acceptance, and diagnose the actual
  crashing stage rather than reducing depth or inferring a stack overflow.
  Diagnostic run `24459` on frozen `31a3f09b` completes with 1/2 tests passing
  (aggregator) and the same abort. Its temporary markers prove parsing,
  canonicalization, and checker initialization complete; the crash is inside
  `checkFile`, before its return or fresh replay initialization. Static LLDB
  disassembly of that exact binary shows an 85,528-byte `checkExpr` stack frame;
  the shell stack limit is 8,192 KB. This establishes significant recursive
  stack pressure, not yet the precise dynamic crashing operation. The next
  diagnostic distinguishes recursive expression descent from earlier checking
  stages without lowering the fixture depth or changing stack limits.
  The second diagnostic increment adds exact-module-gated `checkFile` phase
  and `checkExpr` node/tag entry markers. Independent review accepts it as
  observation-only, and formatting/AST checks pass; all temporary markers
  remain removal-required before acceptance.
  Run `86549` on frozen `f52cae3a` is also red (1/2 tests, aggregator passes).
  Every setup phase and annotated predeclaration finishes; group 0 enters
  190 alternating lambda/closure `checkExpr` calls, ending at lambda 585 then
  closure 722 before aborting. No innermost lookup or terminal reader runs.
  That exact binary's prologue reserves 85,608 bytes per `checkExpr` call, and
  `otool` reports `LC_MAIN.stacksize = 16777216` (16 MiB). The earlier shell
  limit is therefore not the executable's actual budget. The observed
  recursion consumes about 16.26 MB in expression frames alone. Prepare a
  semantics-preserving expression-kind frame decomposition, preserving frame
  begin/finish, evaluation order, and cleanup scopes; do not lower fixture
  depth or raise the stack budget. The transaction test agent resumes its
  separate completion test during this bounded production refactor.
  The approved refactor extracts all 59 expression-kind bodies into exact
  typed, non-inlined helpers. `checkExpr` retains the original frame creation,
  cleanup, and finalization around a call-only typed dispatch. Each helper
  receives the existing frame and snapshotted payload; its original branch
  cleanup still precedes frame finalization. This is a general stack isolation
  change, not a special deep-lambda execution path. Independent review must
  compare all moved bodies, followed by unchanged-budget disassembly and the
  original depth-128 regression plus broader checker coverage. No intermediate
  extraction tranche certifies the deep test.
  A's implementation is now written and AST-clean: source publication,
  activation/lookup publication with Probe rollback and retry/revisit, and
  lookup-retirement completion. Each OOM sweep uses a measured allocation
  count and fresh authentic unchecked consumer per failure point. Completion
  injection targets only the real completion producer; failure preserves all
  logical prefixes and the live lookup, and retry uses the central executable
  rewrite. The upstream annotation retirement deliberately remains pending in
  this transaction test, so it does not assert whole-module terminal validity.
  Independent review of the complete tests and transaction contract is pending;
  no dynamic A run has occurred. A freezes its block while B performs the
  mechanical expression-kind extraction.
  Snapshot `40ae9ac0` records all three A tests before that extraction. Root
  runs the isolated three-test gate `4070` while B stages its transform only
  under `/private/tmp`, outside the frozen build inputs. The gate stops at
  compilation with two reported errors: lookup/completion assertions use
  nonexistent `cir.store.diagnostics` (seven occurrences in the new block).
  Published diagnostics are `ModuleEnv.diagnostics`; newly added diagnostics
  live in `NodeStore.scratch.diagnostics` until publication. The limited A fix
  must preserve diagnostic-state assertions against those actual stores, not
  merely count the already-published span. No A test has executed. B must
  preserve this newer A correction when applying its staged whole-file
  transform; both independent reviews and focused reruns remain required.
  The compile correction is test-only: `DirectBinderDiagnosticState` snapshots
  the published diagnostic span and an owned copy of pending scratch indices,
  and compares both after the transaction. Snapshot allocation occurs before
  failure injection. All seven nonexistent-field accesses are removed;
  formatting and AST checks pass. Rerun the isolated A gate before applying
  the staged stack refactor.
  Corrected snapshot `1cc4f7d8` compiles in run `45172`, but all three A tests
  abort with `reached unreachable code` (1/4 tests passes, aggregator only).
  The unchanged pre-extraction baseline isolates this from B's stack refactor.
  Root and A are auditing the shared unchecked fixture setup: `TestEnv`'s
  unchecked path only initializes `Check`, whereas the real `checkFile` fills
  raw CIR type slots before annotation generation, and other authentic staged
  tests explicitly perform that preparation. Prove the exact missing
  prerequisite and preserve measured OOM boundaries; do not rerun whole-file
  checking or construct substitute types. B's independent A/design review is
  statically accepted with no blockers, but these runtime crashes remain open.
  The bounded setup correction is now written and independently accepted:
  `prepareDirectBinderTransactionCheckerForTest` invokes the real
  `ensureTypeStoreIsFilled` before source calibration and every fresh failure
  instance, and before shared lookup/completion source staging. It asserts
  exact region/type-store size, owner/formal bounds, and the formal's root,
  outermost-flex initial state. No surrounding file check, Builtin copy,
  fabricated formal type, or injected-allocation boundary changes. Formatting
  and AST checks pass. The corrected gate `6400` on `5a890403` now passes
  7/7 build steps and 4/4 tests (three transactions plus aggregator), in
  21 seconds with 25 MB peak test RSS. With the independent review accepted,
  this bounded transaction task is complete; whole W6b remains open.
  B's stack extraction stays in `/private/tmp`. Root independently matches all
  59 moved bodies at token level (including quoted-string contents), while A's
  staged review catches old `&frame` arguments which would become double
  pointers inside the new helpers. The exact corrected count is six (one
  optional pending-predeclared frame and five direct callee calls); the initial
  count of seven included an unrelated site outside the extraction. Those
  six must become the existing pointer;
  wrapper calls still take `&frame`. Preserve original child-Expected creation
  timing and remove all deep-test TEMP markers before applying the final patch.
  Final independent adversarial review accepts the complete staged extraction:
  59 original arms map one-to-one to 59 non-inlined typed helpers, all original
  bodies match except the six required pointer conversions, and all cleanup
  scopes unwind before the unchanged wrapper finalization. The 22 consumers
  receive the original wrapper-time child Expected; only the three original
  consumers receive outer Expected. B applies exactly the reviewed files and
  freezes source. Both files pass formatting and AST checks; the accepted A
  block remains byte-identical (68,267 bytes, SHA256
  `679806b3e0e95fdedddb3927b5629c7ba36ec0d6b6d4557bec8dee988eb91f2b`).
  All deep-test temporary logging is removed; historical call diagnostics
  remain for the separate unresolved task. Root next runs unchanged depth 128
  with the three accepted A transaction tests, then inspects actual generated
  frame sizes and the unchanged executable stack budget.
  Snapshot `5f951c47` now passes that gate (`20681`: 7/7 steps, 5/5 tests,
  28 seconds/31 MB). Static disassembly of the exact test binary
  `.zig-cache/o/0f4299151582858a76978e286776cc57/check` shows `checkExpr`
  frame `0x1c60` (7,264 bytes), lambda helper `0x2dd0` (11,728 bytes), closure
  helper `0x320` (800 bytes), and `LC_MAIN.stacksize` still 16,777,216 bytes.
  The independent reviewer and implementer accept this bounded result.
  Broader gate `5300` on the same frozen source passes 10/10 steps and
  24/24 tests (can 4, check 20, including all direct-binder tests and the prior
  malformed-source/ownership/lifecycle matrix). The next implementation task
  is authentic populated failure/retirement serialization and canonical
  rebuild idempotence; late call retirement and the other W6b work remain open.
  That serialization task is now written by A and frozen for B's independent
  review. Test `invalid where alias direct binder recovery rebuilds and
  serializes canonically` uses the accepted bare hidden-Status fixture and a
  distinct fresh canonical module. It pins the real FA/FL/RA/RL/ref and source
  publication relationships, performs two serialized-byte rebuild comparisons,
  validates an aligned imported-module blob, checks six populated table headers,
  and replays exact semantics and complete bytes after readonly `deserializeInto`
  and mutable `deserializeWithMutableTypes`. Readonly cleanup frees only its
  owned import map/runtime map/environment; mutable cleanup uses the cache
  destructor. The independent reviewer requires and accepts explicit empty
  transient-list assertions after each rebuild, since serialized bytes cannot
  detect checker-local leaks. Final static review, formatting, and AST checks
  pass. Snapshot `b523751b` passes the real checker serialization test in run
  `83266` (2/2 including aggregator, 7 seconds/27 MB). The combined check/compile
  gate is red only at the old cache hash golden (26/28 steps, 3/4 tests):
  CACHE_VERSION 81 measures
  `372f9d28e8533a984821e46372ce1b12f4e58c11e474040aafe11bc7daeddfe3`.
  Root authorizes A to replace only the golden with those measured bytes;
  independent comparison now accepts the exact 32-byte edit with version 81
  unchanged. The targeted hash rerun `22649` on `31459e1d` passes 25/25 steps
  and 2/2 tests (1 second/1 MB test RSS). The bounded C serialization/idempotence
  task and measured golden update are accepted; the initial combined gate was
  not rerun wholesale after its isolated hash correction. No
  production changes were needed for the serialization/idempotence test.
  A separate raw-reader contract audit
  confirms one remaining selected-clause consistency obligation:
  `NodeStore.whereClauseSpanFrom` groups each clause exactly by its receiver
  declaration (direct rigid or lookup ref); alias arguments affect transitive
  `owned_by_annotation` reachability, never that clause's owner group. The
  selected malformed-alias replay must therefore compare the clause receiver's
  identity with its published `WhereClauseOwner.rigid_var`. This is validation
  of an explicit producer-authored inverse, not reconstruction of ownership.
  The current selector does not compare them. A bounded follow-on task must
  declare that inverse explicitly and test candidate/fresh disagreement, while
  retaining positive generic membership for identity-reference leaves.
  B has now declared the inverse in design.md and written the shared-reader
  helper `listedWhereClauseReceiverOwner`. It validates method/effectful or
  alias receiver encoding and reuses the guarded self-membership reader for
  rigid/lookup syntax, padding, and reference bounds; the identity comparison
  precedes the reachability `continue`. The initial patch now has canonical
  test `where clause owner rows preserve exact normalized receivers` and
  authentic two-formal test `invalid where alias direct binder owner inverse
  rejects sibling and bounded coordinates`; formatting and AST checks pass.
  A is reviewing the final declaration/helper/tests. Root also requires an
  explicit mapping of the agreed remaining raw-guard matrix to existing/new
  tests: the initial ten groups do not silently discharge untested pattern,
  owner, alias, attachment, or fresh-span cases. No runtime or final-review
  acceptance of this D patch is claimed.
  The mapping now reuses the prior deep/bare formal, written-span, attachment-
  body, and return-cycle checks. B is adding exactly the remaining selected
  pattern-index, owner flag/rigid/padding, written-entry tag, alias receiver/
  target, attachment node/annotation bounds, and fresh function-span cases to
  the same two-formal fixture. Each temporary edit is restored before assertions.
  Those additions are now complete. The independent reviewer also requires
  a distinct third rigid in the canonical alias-argument fixture, so an
  argument-as-owner implementation cannot pass accidentally; the separate
  detached unowned row remains intact. Final static review accepts the complete
  declaration, production helper, canonical fixture, and reconciled checked/
  fresh matrix. No solved-graph mutation, inferred owner, or global partition
  claim was added. The exact two-test dynamic gate is next.
  Snapshot `4631e7ad` passes that focused gate (`73577`: 10/10 steps, 4/4
  tests; can 2, check 2 including aggregators; checker 7 seconds/25 MB).
  The broader regression gate `33921` on the same frozen source passes
  10/10 steps and 33/33 tests (can 11 in 762 ms/5 MB, check 22 in
  2 minutes/33 MB). It includes the entire six-test canonical ownership file
  and the prior direct-binder/deep/serialization/transaction/malformed-source
  set. D is accepted; no remaining case from its reconciled matrix is being
  deferred implicitly.
  The next bounded task is call/callee retirement. A is preparing its explicit
  producer/draft-driven completion design read-only, with a bounded child
  producer audit; B remains the independent implementation reviewer. The call
  transaction must first be stated explicitly in design.md: both consumer and
  failure-reference ranges precede any call retirement stamp, draft removal,
  or erasure. Current sequential aggregate/ineligible completion is not that
  transaction, and the existing ineligible helper rejects the call producer.
  The declaration is an inverse only for clauses actually listed in an owner
  row, not a claim that all written clauses form a complete unique partition.
  Both values of `owned_by_annotation` retain the inverse; the bit remains
  canonicalization's explicit transitive reachability result.
  The historical call-retirement regression remains separate and unresolved.
  Remove every `TEMP default-source` and
  related selected-context label before production acceptance. The independent
  static audit established a separate ownership gap: selected-anchor move and
  terminal-evidence inverses are still selected-decision-only, and rebuilding
  likewise skips selected handles in generic movement closure. A retained
  default-owned chain can therefore be rejected or pruned despite retention
  of its default child/decision constraint. Immutable `DispatchSettlementSource`
  proves creation, not complete terminal consumption, so it cannot justify a
  validator exclusion. The declared normalized settlement event owns that
  complete inverse; any earlier terminal-consumer implementation would need
  equally explicit exact decision/offset and full movement closure. This
  structural gap is not yet the dynamically identified `92762` predicate.
  The zero-argument corruption test replaces one root with a duplicate other
  root; it tests exact uniqueness/order, not deletion of an otherwise
  unreferenced zero-argument root in isolation.
- Review also rejects the missing default-root first-attempt inverse: the
  current checks establish a compatible decision/offset and exact Builtin
  binding, but do not independently bind the root to the target that seeded
  it. A same-method sibling offset or later compatible decision can be
  substituted. The planned `DefaultMethodTarget`/`DefaultMethodInstantiation`
  needs an explicit cache-seed/target inverse that also represents a rejected
  first attempt with no instantiation. Read-only design review recommends
  keeping the existing cache-support root as seed identity: the six finite
  imported-method root kinds are produced exclusively by the cache-miss
  helper. That helper must return producer-authored `inserted` versus `reused`
  evidence; the target records a closed pre-import rejection, seeded root, or
  reused root outcome. A new parallel seed pool is not needed. Root uniqueness
  must compare the actual explicit provider/template authority even when one
  reference uses the Builtin namespace and another a semantic dependency.
  Generated, inspect, associated, and candidate first-consumer authorities
  still need exact attempt identities: predicted future SchemeUse/derivation
  rows may never be committed, and matching only MethodBinding is insufficient.
- Separate remaining integrity requirements are the exact committed child
  SchemeUse/constraint consumer inverse and read-only cache admission of
  default contributor ownership (fresh rebuilding checks it, current admission
  does not). None is certified by the provider/owner fix.
- Passing focused evidence includes corrected decision-owned selected anchors,
  real candidate-compatibility synthetic-binding rollback with a fresh consumer
  per failure index and forced resize failure, both codec import fixtures,
  copied-literal corruption checks, and resize-inclusive exhaustive boundary
  rebuild rollback. The latter fixture has no default root/child and therefore
  does not cover the new default-retention path. Add that allocation-failure
  coverage once an authentic default topology passes. The original
  `Discarded unpinned arithmetic specialization validates the default method
  type` regression now passes in the named combined run `98717`.
- Authentic accepted/rejected-first default lifecycle coverage remains open:
  the owner test has no expected default root, while the unchanged bad-then-good
  source is now correctly tested as nominal dispatch retirement, not as a
  default cache test. Review additionally requires a non-root decision-constraint
  identifier mutation, exact dispatch-target slot/constraint checks, proof
  that a rejected attempt leaked no SchemeUse, and exact candidate-probe
  coverage in the cache-free OOM fixture. No new typing-policy choice is needed.
- A bounded read-only fixture audit derived genuine default-path tests from
  the original numeric regression: change its `7.U64` RHS to `7.Dec` for the
  accepted case; use `-x` as an arithmetic trigger alongside independent
  dot-call `plus` constraints for rejected-first/reuse and same-decision
  sibling-offset cases. Dot calls alone do not trigger arithmetic defaulting,
  explaining the earlier failed fixture approaches. These proposed fixtures
  are dynamically unverified; assertions must follow exact producer records
  rather than fixed decision indices or constraint offsets.

Earlier implementation and verification history follows; passing intermediate
subsets below do not override this latest checkpoint.

- Implemented prerequisites include exact copy-step/source evidence, expected
  consumption and retirement evidence, call-formal and external-cache token
  ownership, method-output publication, loop/where/copied constraint sources,
  canonical boundary rebuilding, and serialization/cache admission checks.
  Expected-failure producer coverage and terminal settlement coverage are not
  yet complete. A read-only producer audit found four complete annotation
  failure paths (`malformed_type`, `malformed_where`, `invalid_tag_child`,
  `builtin_not_type`), one real aggregate-relation failure path still needing
  focused producer tests, and 21 schema kinds without production publication.
- The last completed copy-transaction matrix passed 11/11 checker tests and
  4/4 compile/cache tests. It covers generic, nominal, binding, and authentic
  platform copy allocation failures, cold first-import rollback, copied-source
  correctness, boundary allocation failures, and cold/warm admission. Public
  cross-module copies have three non-nesting entry points. Checker probes own
  Store state, both interners, identity displays, and the original variable-map
  allocation, as well as proof/cache journals.
- Copied-open-literal group/event schema, local and cross-module producers,
  and ModuleEnv lifecycle support are implemented. The stale provisional-step
  failure is fixed: predeclaration rebases group coordinates, and terminal
  reachability includes event handles and moves in its fixpoint. The focused
  `copy_import` run passed 7/7 build steps and 11/11 tests after those fixes.
  New admission, corruption, repeated-boundary, and OOM regressions are being
  integrated. Audit also disproved the new blanket prohibition on literals
  inside binding-codec component graphs: only the detached outer requirement
  is guaranteed nonliteral. Nested receiver/function groups now use their
  existing component tags, and an explicit mapping-origin record authenticates
  reuse from the binding root or an earlier component, including later
  requirement ordinals. A focused run passed 11/13 tests: structural
  binding-copy/OOM coverage passed, while both new real-source import fixtures
  reached a contributor-handle mismatch. Its exact cause was a scheme candidate
  retaining a by-value constraint before copy publication attached evidence.
  Candidates now retain only the producer-owned pool index. The narrow rerun
  passed 1/3 tests and advanced past that failure; it exposed a missing export
  in one fixture and a reused literal incorrectly treated as freshly copied
  in the other. The latter occurs while generated-codec finalization opens
  `Wrap`'s nominal backing: its formal `b` is substituted with the existing
  seed argument. The instantiator now records that actual substitution cut in
  a transient reuse ledger; checker bookkeeping consumes the ledger and leaves
  the seed's literal/dispatcher/rank/region ownership unchanged. No copy origin
  is invented, and no solved-graph probe reconstructs the substitution. The
  combined checker regression batch on snapshot `4c780d48` returned 23 tests:
  16 passed, 3 failed, 4 crashed. Four lifecycle fixtures registered literals
  after `initExpr` had finalized checking; they need genuine pre-check setup,
  not a reset of the freeze flag. The two provider fixtures used deprecated
  module headers and need modern type-module associated exports. The
  predeclaration fixture passed its source/group assertions but reused an OOM
  rollback snapshot after a successful rebuild. Its first failing assertion
  compares backing pointers, which a successful replacement is allowed to
  change. Keep exact pointer/capacity/byte preservation for rollback; test
  successful-rebuild idempotence separately through canonical contents and
  serialization. No production content drift is established by that pointer
  failure. Separate frozen-snapshot type tests passed 6/6 and copied-literal
  serialization tests passed 3/3. The append-invalidated deduplication iterator
  now uses the append-safe constraint iterator; final-codec requeue retains
  the exact source index and publishes evidence movement.
- The test-only correction snapshot `d14de740` preserves strict provider
  diagnostics, uses unchecked lifecycle setup for isolated producer tests, and
  compares complete canonical ModuleEnv serialization for successful rebuild
  idempotence. Its eight-target rerun returned 9 tests including the aggregator:
  5 passed, 1 failed, 3 crashed. `one local copied open receiver`, Probe
  rollback, boundary remapping, and predeclaration idempotence now pass. The
  corruption test omitted final BFS canonicalization before asking final-form
  admission; restore that setup and retain every negative mutation. Both modern
  type-module codec fixtures reach `one copy step repeated an open-literal
  destination occurrence`. The adversarial review confirmed a production
  blocker: raw destinations need not be unique across the complete occurrence
  relation. Defaulting registration must consume the explicit copied group's
  owning occurrence, not reconstruct it by searching destination equality.
  The boundary OOM fixture's synthetic graph
  insertion before real `checkFile` instead crashes with
  `Arrays out of sync: type_nodes=4 region_nodes=101`. It must use genuine
  source-produced literal copies/moves for its first successful check, then
  retain exhaustive pointer/capacity/byte-exact rebuild rollback assertions.
  The fresh sol-ultra adversarial review rejected snapshot `4c780d48` for the
  driver defect, found no other confirmed production blocker in its bounded
  pass, and confirmed that the synthetic mixed-literal callable is not a valid
  full-check fixture. The implementer is
  correcting these three red targets; source will be frozen again for the
  focused rerun and follow-up review. A clean full checker-module gate is still
  required before contributor/settlement work. Earlier passing matrices do not
  certify the current working tree.
- Corrective snapshot `ad47cf76` drives cross-module literal registration from
  the exact step-owned groups and their receiver occurrences, separately from
  allocation-range region bookkeeping. Both real codec fixtures now assert
  repeated destination occurrences with one exact owning group. The four-target
  rerun returned 5 tests including the aggregator: 4 passed, 1 crashed. Both
  codec fixtures and the corruption fixture passed. The OOM fixture now uses
  real `make_zero` instantiations, but its first check reaches
  `checked-boundary call formal lost its producer witness` before any OOM
  injection. Its headerless provider also needs an explicit valid main-type
  export; absence of printed diagnostics before a checker panic does not prove
  successful import checking. Diagnose the exact rejected formal and validate
  the source export before attributing this to the call sites. Do not remove
  valid calls merely to avoid the invariant. Follow-up adversarial review is
  active; this slice and full W6b remain unaccepted.
- The group-driven correction subsequently passed its bounded static review.
  Changing the OOM provider to the genuinely exported `Status` main type while
  preserving all three `make_zero` calls made the first terminal check complete.
  The OOM-only diagnostic run returned 2 tests including the aggregator: 1
  passed, 1 failed at an as-yet-unlocalized assertion. No production call-formal
  change was needed to check the valid program. Keep the unexposed-where-alias
  variant as a required error-retirement regression: its call-formal boundary
  panic is not an acceptable response to invalid source and must be resolved
  before W6b publication, even if its producer fix belongs to the later
  Expected-failure slice. The original failing input is preserved in snapshot
  `ad47cf76`: the provider named `BoundaryOomA` contains only
  `a.Status : where [a.status : a -> [Off, On]]`, without exporting `Status`;
  the consumer imports `BoundaryOomA exposing [Status]` and its mutually
  recursive `first`/`second` signatures use `where [a.Status]`. The valid OOM
  fixture instead names the provider `Status` and imports its main type. Keep
  those inputs distinct. The old panic comes from the preterminal
  `expectedCallFormalMatchesPlans` check, before canonical formal sorting;
  its exact rejected coordinate still requires dynamic diagnosis. A subsequent
  static audit established a separate call-retirement coverage gap: successful
  callable-shape checking publishes root/argument plans and formals before
  argument checking; an operand/type failure can then retire the call without
  publishing its checker relation or reserving the immutable call retirement
  recognized by `expectedCallOwnerSnapshot`. Ordinary call retirement currently
  lacks the snapshot authority that ambiguity retirement supplies. This is a
  real lifecycle gap, but its attribution to a particular call in the old
  invalid-import fixture remains unproved. The minimum dynamic discriminator
  is the failing formal's exact owner tag, optional relation field, and matching
  retirement rows, followed by the actual failed helper predicate. Do not
  replace that evidence with a solved-type or reachability guess. The valid
  OOM fixture still needs a clean exhaustive
  rollback run, followed by removal of temporary diagnostics and re-review.
- The assertion-localization run confirmed valid source and populated formal,
  token, cache-seed, loop, copied-group, and copied-event inventories, but zero
  copied-literal moves: independent `make_zero` calls preserve separate
  constraint occurrences. The next fixture uses four independent calls as
  elements of one list so ordinary element unification produces the required
  evidence movement. This is a test-coverage correction, not permission to
  fabricate movement or weaken the rollback checks.
- The homogeneous-list fixture produced nine real copied-literal moves, then
  the repeated rebuild reached `cross-module marker-copy step lost its exact
  producer provider`. Boundary provider lookup still consulted transient
  import/method caches for some root kinds, although their preterminal step ids
  are not rebased. The correction uses the existing typed durable
  `whereMarkerCrossRootAuthority` shared with admission and checks the exact
  raw source root. Cache-cleared repeated rebuild and exhaustive OOM coverage
  are being finalized; no transient-cache fallback may remain.
- Snapshot `f6c57bf8` failed the OOM fixture before injection. The traced rerun
  (`-Ddebug-gpa-traces`) located the failure at the assertion requiring a
  relocated default-method cache entry. The fixture's literal-only default
  does not establish such an entry; the earlier attribution of the provider
  panic to a default-method root was an inference, not an observed fact.
  Preserve the real imported where-alias authority and clear the transient
  caches, without asserting incidental default-cache topology. Independent
  review also found a genuine bug in the shared authority's `default_method`
  arm: `builtin_decl_index` is the owner declaration, not the method's
  `provider_type_node`. Validate the exact owner/module/method binding tuple
  and pin it with a real arithmetic default-method positive and wrong-owner
  corruption regression. The OOM test's successful-allocation cleanup must
  remain inside its loop-local failing allocator's lifetime even if a later
  assertion fails. These corrections are underway; the static acceptance of
  the provider change was withdrawn, and no full checker gate has passed yet.
- Snapshot `62e850ec` replaces the cross-domain equality with the exact
  Builtin owner/method lookup, validates decision targets with the shared
  default classifier and explicit Builtin indices, and guarantees successful
  OOM teardown within the failing allocator's lifetime. Its traced five-target
  batch returned 6 tests including the aggregator: 4 passed, 1 failed, 1
  crashed. Both codec fixtures and the corruption matrix passed. All produced
  cross-step providers resolved after cache clearing, but the OOM test then
  failed an unsupported assertion that a literal
  `external_where_alias_receiver` origin remained in the canonical inventory.
  Collect the actual origin kinds instead of guessing another tag. The new
  real arithmetic-default fixture crashed during its initial check at
  `default-method marker-copy origin escaped its exact decision anchors`.
  Audit found that a default root records a predicted SchemeUse index outside
  the compatibility probe, while rejection rolls back the only producer of
  that use; confirm the exact failing coordinates and correct the authority,
  never fabricate a use or retarget a sibling. Review also requires validating
  the selected constraint's identifier before the new cross-module name lookup
  so corrupt marker-free constraints return `CorruptArtifact`, not panic.
  The current slice remains unaccepted and no OOM-injection success is claimed.
- Diagnostic snapshot `f0c49c50` was rerun with captured output after the first
  diagnostic build's execution handle was lost. The captured run returned
  3 tests including the aggregator: 2 passed, 1 crashed. The exhaustive OOM
  test passed with real copied-literal movement, cleared transient caches,
  strict pointer/capacity/byte rollback, and successful final replacement.
  The actual former-cache-fallback origin is `candidate_probe_method_root`;
  the other produced cross roots were external cache seeds and selected-method
  roots. The default failure was exactly `decision=0/1, offset=0/1, use=1/1`,
  confirming the predicted use was rolled back. The correction will make a
  default cache root name its first target attempt, not a speculative SchemeUse;
  only a committed child use carries a SchemeUse. This matches the existing
  target-versus-instantiation distinction in design.md. All temporary prints
  must be removed before the next verification and review; this diagnostic
  snapshot is not publishable.
- The same audit found an independent probe rollback bug:
  `importedMethodSchemeFromSource` directly inserts a synthetic binding flag,
  bypassing `markBindingSchemeVar` and its mutation journal. A cold cache miss
  inside a candidate compatibility probe can therefore retain classification
  for a rolled-back type variable. Use the journaled producer within the
  existing transaction and pin an authentic outer-probe rollback, including
  allocation failure. No compatibility-probe/diagnostic redesign is needed.
- The default-terminal-authority slice must additionally prove the exact
  decision/constraint consumer inverse for every committed
  `default_method_use`. Existing checks only bound the nominated SchemeUse and
  compare its resolved scheme root with the cached template; sibling uses of
  that same template are not distinguished. Pin unrelated valid use retargets,
  same-template sibling use swaps, and decision/offset swaps. Provider/owner
  authentication does not establish this per-use relation. The rejected
  first-attempt root issue above may require its schema correction earlier;
  the complete terminal certification remains a W6b publication requirement.
- The contributor design audit found that a literal-only group domain is
  incomplete. An arithmetic-only copied candidate can default without literal
  events, and an ordinary nondefaultable copied candidate can later merge with
  a separate literal. Amend the design before migration to an exact
  `CopiedConstraintDriverGroup`: copy-time literal registration, instantiation
  candidate registration, or both, with a possibly empty literal-event range.
  This is not a copy-time defaultability classification or a claim of fresh
  receiver allocation; detached scheme requirements can reuse their receiver
  while copying the exact constraint. Preserve every producer token when
  ambiguity judgment candidates deduplicate. Pending group identity includes
  the component/ordinal and exact constraint ranges, not just the receiver
  pair. The initial nonliteral binding-codec outer ingress still has no such
  candidate; its later local scheme use owns the single-step requirement group.
  Admission needs a new explicit `flex_fresh_flex_copy` witness action at the
  instantiator's initial source-flex allocation: today's `traverse` also means
  memoized reuse and cannot certify creation. Attached candidate groups replay
  this action or `rigid_fresh_flex_cut`, the exact local policy, and their
  nonliteral constraint offsets. Detached candidate groups instead replay the
  paired requirement-ingress witnesses, retaining the exact ordinal through
  transient registration. Literal detached requirements are not produced
  today: ordinary requirement capture drops literals, and imported outer
  codec requirements are nonliteral. Do not broaden admission by guessing such
  an arm. This bounded design review is complete, without dynamic tests; the
  design amendment and contributor implementation have not begun.
- The follow-up read-only migration map identifies these concrete surfaces:
  `registerInstantiatedAttachedDispatch` must receive the explicit source and
  destination constraint ranges from `instantiateVarHelp` and predeclared
  replay. `InstantiatedSchemeDispatchRequirement` must retain the ordinal
  already available in `copySchemeDispatchRequirements`, so
  `registerInstantiatedSchemeRequirement` can author the exact detached token
  after its single constraint append. `recordAmbiguityCandidate` must union
  incoming tokens even on dedup and after assignment to a default draft;
  preserve its separate legacy driver for ambiguity/retirement authority.
  An append-only candidate-indexed token pool avoids unjournaled edits to
  contiguous ranges, but every assignment to pre-probe state still needs an
  exact rollback journal. In `instantiate.zig`, only the initial source-flex
  allocation authors `flex_fresh_flex_copy`; memoized `var_map` reuse remains
  `traverse`. Translate and admit that action in the closed policy/auxiliary
  switches. `publishLocalWhereMarkerCopyStep` publishes and binds exact groups;
  update both pending-driver bind helpers and their eight wrapper/replay
  callsites. Do not use receiver-only reverse lookup as group identity.
  `registerPublishedCopiedOpenLiteralGroups` registers only event-bearing
  groups and passes their authored ids. `registerDefaultDecisionDraft`,
  `judgeAmbiguityCandidate`, and `durableDefaultDecisionContributor` consume
  the complete token union. Local admission needs the exact attached-creation
  or detached-ingress converse; cross-provider admission remains literal-only.
  Candidate registration is exhaustive at those qualifying local producer
  hooks, so the amended design must state that converse explicitly rather
  than treating a registration bit or final shape as proof. Rebuild
  reachability follows contributor group to owning step; relocation consumes
  the existing `copied_literal_group_map` and re-sorts contributors afterward.
  Include predeclared in-place rebase, ModuleEnv static/mutable serde and
  savepoints, Check Probe, `CheckedBoundaryStateSnapshot`, and
  `CrossModuleCopyTransaction` in the migration and OOM audit. The map is
  preparation only; none of this migration is implemented or dynamically
  accepted yet.
- The read-only contributor design-amendment map is complete. Update both
  declarations (the copy-inventory prerequisite and the duplicate normalized
  schema), the exact ModuleEnv pool inventory, default-contributor prose,
  and publication/rebuild order together. The group/pool become
  `CopiedConstraintDriverGroup` / `copied_constraint_driver_groups`; literal
  event and disposition names stay unchanged. Empty event ranges retain the
  current gapless event cursor. Preserve literal-only settlement/pin rules;
  do not broaden event consumers to arithmetic-only candidate groups. The
  rigid-substitution exclusion must cover driver-group/candidate/event
  authorship, and the old blanket raw-receiver-identity exclusion must retain
  the exact detached-requirement exception. This is preparation, not a design
  amendment or implementation acceptance.
- A follow-up read-only sequencing audit identifies the smallest next code
  prerequisite after the normative amendment: append `flex_fresh_flex_copy`
  to both witness enums, emit it only for the initial local source-flex
  allocation, and retain `traverse` on memoized reuse. Its policy validation
  must cover every local policy whose flex branch actually allocates,
  including ranked variants, `all_fresh_rigid_preserve`, and
  `all_fresh_flex_preserve`; cross/share-leaf paths remain excluded. This
  witness-action domain must not be confused with the narrower qualifying
  candidate-registration converse. Pin allocation versus revisit and rejected
  policies before migrating groups. The rename, local producer/token union,
  and inverse admission must be combined wherever splitting them would
  contradict the declared group semantics; durable contributor group ids and
  rebuild remap/resort follow with their complete inverse. The missing
  requirement ordinal and attached source-constraint range already exist at
  their callers but are discarded by transient interfaces; no new semantic
  inference is needed. Rerun E's historical/formal/rebuild gates because the
  new action changes expected-call copy witnesses too. This is sequencing
  preparation only, not contributor implementation or acceptance.
- Full checker baseline `30286` remains pending on compiled snapshot
  `c124f1eb`; it is not a green gate. Read-only one-second stack samples show
  progress between ordinary integration tests, from the function-payload
  tag-union case to the recursive-nominal wrong-type case. Both samples were
  inside fresh Builtin admission. The first found source/anchor all-pairs
  validation; the second found branch-plan validation. Every ordinary
  `TestEnv` fixture currently owns and admits its fresh Builtin view. No
  validation has been skipped, no trust cache introduced, and no test process
  interrupted. The source/anchor audit found no existing global source order
  suitable for a linear merge: only anchors are currently sorted, while
  source rebuilding retains input order. A temporary source-order schema
  migration was therefore not authorized; preserve the exact inverse when
  implementing the planned normalized settlement structure.
- Independent review requested corrections to the read-only contributor
  proposal before implementation: explicit step-local static/detached
  primary-witness joining with the composite outer binding-codec exclusion;
  literal classification from source constraints rather than output
  registration; exact six-word contributor serialization; and explicit
  pending-token binding and rollback. Root additionally pinned preservation
  of existing non-traverse root-cut witnesses and the timing of pending
  tokens before group publication. The author has supplied revised normative
  text, including the existing rigid-cut behavior, and final proposal review
  is pending. No witness or contributor migration is implemented yet.
  After approval, preparation may overlap execution of the already-compiled
  baseline binary: its result remains attributed only to `c124f1eb`, never
  to subsequent edits. Only the driver may start builds. The initial plan
  serialized all gates; the later user-requested incremental-loop experiment
  below permits one narrow watcher beside this already-compiled test run.
- The independent proposal review still requests the following contributor
  contract corrections: intern one pending token per complete key (repeated
  insertion of that token is idempotent); immediately propagate tokens to
  already-assigned candidate/literal drafts and transfer the whole union on
  later assignment; keep pending/bound tokens checker-local, separate from
  serialized group/event/contributor rows; permit only bound ids at quiescent
  rebuilding; and retain the complete group slice of every retained step.
  These are not waived and contributor migration remains deferred.
  The independently valid witness-only prerequisite is now authorized and
  in progress alongside the old compiled baseline gate. Its scope is only
  the new action, its normative policy/root-action rule, translation and
  admission, exact present/in-range/distinct raw endpoint validation, and
  focused tests. It requires its own next cache-version and measured hash
  gate; the later contributor migration requires another. No new witness
  implementation has yet passed a dynamic gate or adversarial code review.
- Under the new per-task WIP-push authorization, the driver duplicated the
  accepted `ed49d30b` snapshot as unique change `ssvqsxro`, described it as
  `WIP: checkpoint W6b checked-boundary recovery and metadata`, and pushed
  `cc8ace35bc0973fae4da465b199fe21f7e537cf3` to `jared/polarity`. The duplicate
  has an identical tree to `ed49d30b`; comparison with tested `c8b4a737` shows
  only progress-document differences. This excludes every in-progress
  witness edit. The driver fetched the exact remote bookmark before the
  fast-forward push, updated PR #10434 with current verification and explicit
  unfinished scope, and verified the remote head and retained draft status.
  The driver rebased the frozen working change onto that checkpoint. The
  resulting six-file conflict was resolved against the saved pre-rebase
  snapshot `37ea50fb`; the complete resulting tree `24f9c13b` is byte-identical
  to that snapshot. Only witness/progress changes remain above the pushed
  parent. Formatting, AST checks, and conflict-marker checks pass; dynamic
  verification of this task is not yet green. The full checker gate still
  evaluates old `c124f1eb`.
  Installed Zig help confirms `--watch`, `-fincremental`, and
  `--error-style minimal`. The driver started narrowly filtered watcher
  `44170` on frozen `24f9c13b` alongside the already-compiled baseline test
  execution, with separate revision attribution; this is not a second full
  suite. Stop that watcher before conflicting builds or cache pruning.
  Compact diagnostics do not replace full saved output, test counts, or a
  completed final verification gate. Its first cycle is red: 8/9 tests pass
  (types 4/4, check 4/5). The sole failure is `local fresh-flex copy witness
  survives exact validation rebuild and serde`, with equal serialized lengths
  of 146868 and a first byte difference at offset 352 (expected a u64 value
  of 784, actual zero). Independent source audits identify that exact word
  as `TypeStore.Serialized.record_fields.capacity`: empty `SafeMultiList`
  serialization writes no backing extent but retains reserved runtime
  capacity, whereas both deserializers return zero capacity. Empty `tags`
  has the same inconsistency later. The approved separate prerequisite is
  canonical zero capacity on empty serialization and rejection of empty
  serialized lists with nonzero capacity, preserving the current cursor
  offset and all nonempty SoA layout. Keep whole-byte regression assertions;
  no caller-state clearing, masking, or weakened equality is authorized.
  During intermediate enum edits, types recompilation took 2–4 seconds and
  compact diagnostics exposed missing exhaustive cases. The experiment then
  twice failed in the generated Builtin compiler object with `string not
  null terminated in '__TEXT,__cstring'`, followed by a compiler restart
  `BrokenPipe`. Root stopped watcher `44170` (exit 1 after Ctrl-C). These
  intermediate results are not source acceptance evidence. Continue final
  focused gates in regular nonincremental mode with minimal diagnostics;
  no compiler-source workaround or cache pruning was performed. This was
  not a disk-space error, and old full checker `30286` remains separate.
- Independent static review found that virtual requirement ingress overrides
  can erase the new fresh-flex allocation action. A virtual component may
  itself own attached constraints, so a wording-only exception would lose
  authority needed for their creation. Root approved the exact design-first
  combined `requirement_component_fresh_flex_copy` correction, preserving
  both the ingress role and first allocation from a source flex or rigid.
  It requires exact endpoint, allocating-policy, and virtual-edge validation;
  virtual polarity opening remains ordinary ingress and creates no attached
  constraints. The author is implementing it before the separate serializer
  prerequisite. Neither correction is accepted yet; the witness checkpoint
  remains unaccepted and unpushed.
- A preliminary regular (nonincremental) types-only gate `78293` passes
  4/4 steps and 5/5 tests: fresh constrained copy/memoized reuse, forced
  shared-root creation, and the new virtual receiver/function/interpolation
  part/item creation-plus-memoized-role case. Compilation took 4 seconds
  (294 MB); tests took 288 ms (1 MB). `instantiate.zig` had identical pre/post
  SHA-256 `5939115fed3242c8dcc9be03e92a931946ba9c6cf2313b10f32b4e0e7e31d6f7`.
  This is only the low-level slice, not final checker/serializer acceptance.
  Version-ownership review found that production `SafeMultiList.Serialized`
  persistence goes through `TypeStore.Serialized` and `ModuleEnv.Serialized`;
  the pending cache version 83 covers the empty-capacity narrowing too.
  Builtin blobs must be regenerated, but no independently versioned
  checked-artifact or LirImage format embeds that layout.
- Checker-fixture review rejected a synthetic topology that reused the same
  source constraint as both an attached row and a detached requirement while
  making its receiver independently copyable. That would author two
  destinations for one source, which the existing functional-pair invariant
  correctly rejects; it is not a confirmed compiler defect. The approved
  typed/local-publication fixture must use real detached registration,
  `.scheme_copy` candidate capture, and the separately appended occurrence,
  retaining its exact structural origin alongside a distinct attached row.
  Pin both source/destination pairs and attached registration. This tests
  local producer/validator authority, not parsed whole-module admission;
  no pair-functionality relaxation or hand-swapped index is authorized.
  The fixture's deferred-component input remains deliberately controlled:
  manually supplied codec flags are not authenticated by registration or
  capture, which only propagate them. Review requires that explicit scope
  and rejects calling capture an upstream codec authenticity boundary. The
  real imported binding-codec producer and parsed cross-provider tests are
  separate evidence; they do not make this synthetic local input a genuine
  generated-codec classification.
- Adversarial admission review also found that a memoized revisit could be
  relabeled as a second fresh allocation with otherwise correct raw endpoints.
  Root approved a design-first allocation-free uniqueness fence: at most one
  witness per exact raw source/destination mapping may use any of
  `flex_fresh_flex_copy`, `requirement_component_fresh_flex_copy`, or
  `rigid_fresh_flex_cut`. After occurrence validation, the unique raw child
  occurrence is the equivalent key; the solved canonical pair is not, because
  distinct historical mappings may later unify. Add ordinary repeated-tuple
  and virtual-revisit forgery/restoration tests. This does not infer exact-one
  existence, firstness from BFS order, or allocation from solved shape, and
  does not broaden the rule to historical sharing/reuse/substitution actions.
  The correction and independent final review remain pending.
- The old full checker baseline `30286` has now completed, superseding every
  earlier pending-status note above. It is red on compiled `c124f1eb`, the code
  now pushed in `cc8ace35`: 1,257/1,383 passed, 6 failed, 120 crashed; 5/7
  build steps passed. These results do not cover the newer witness changes.
  The complete 39,770-character report is saved in session artifact
  `/private/tmp/polarity-witness-gates.MlM03v/full-checker-c124f1eb-30286.txt`.
  Reproducible grouping (all 120 crashes accounted for):

  | Failure family | Crashes |
  |---|---:|
  | Call formal lost producer witness | 62 |
  | Local copy witness absent raw occurrence | 24 |
  | Aggregate consumer retirement erased-plan replay | 10 |
  | Invalid call Expected topology | 7 |
  | Projected aggregate child rejection changed producer plan | 4 |
  | Preexisting runtime-error missing canonicalization authority | 4 |
  | Invalid branch Expected topology | 3 |
  | Disconnected copy pair | 2 |
  | Invalid external lookup producer inventory | 1 |
  | Projected aggregate child suppression changed producer plan | 1 |
  | Unclassified cross-module preseed cut | 1 |
  | Root alias substitution missing platform binding row | 1 |

  The six ordinary failures are the Builtin Num lookup/apply authority test,
  four branch Expected-plan assertions (`if`, `match`, refined accumulator,
  and nested-plan preservation; expected `anchored`, found `related`), and
  the declarative static-dispatch representative repeated-merge assertion
  (expected variable 5, found 6). These are not approved expectation updates.
  First reproduce targeted representatives on the next frozen source, then
  fix and rerun each affected section; do not use the full checker or minici
  as the inner retry loop. The independent witness task continues separately.
  The draft PR description was updated and read back with the completed red
  result, unchanged head `cc8ace35`, and retained draft status. Its earlier
  green focused gates do not certify this broader checkpoint.
- Witness snapshot `9474febf` completed its independent full-diff review.
  Static verdict is REJECT solely for a missing interpolation action/primary
  inverse: an attached structural interpolation allocation could be relabeled
  as virtual combined ingress. The approved design-first correction binds
  both interpolation edge kinds to the unique primary for their exact
  constraint pair. Ingress actions are required exactly for a local detached
  `scheme_requirement_function` with no auxiliary origin and the same root
  parent; static primaries and composite binding-codec function-component
  metadata require non-ingress actions. Add isolated structural-to-virtual
  and reverse corruption/restoration checks. This correction is in progress.
- The separate canonical-empty `SafeMultiList` prerequisite is frozen at
  `55aa0ec4` (only its file and the Cache Boundary paragraph changed after
  witness freeze). Formatting and AST checks pass. Collections gate `63893`
  passes 4/4 steps and 7/7 tests, covering the expanded empty-capacity oracle
  and all existing `SafeMultiList CompactWriter` tests plus SafeList edge
  cases (2-second compile/215 MB; 345-ms run/1 MB). Independent static review
  accepts this serializer slice with no blocker. The original ModuleEnv byte
  oracle and measured version-83 hash verification are still required before
  checkpoint acceptance. No new witness/serializer
  changes have been pushed.
- Interpolation correction `db3919ec` is frozen, formatted, AST-checked, and
  independently STATIC ACCEPTED. It closes the sole remaining witness review
  blocker, including both isolated relabel directions beside non-traverse
  roots. The original binding-codec split-component/collapsed-raw-mapping
  positive is unchanged. Combined regular focused gate `44474` is running on
  this exact code, covering all instantiator-proof tests, new witness and
  serializer tests, historical call-formal recovery, and that binding-codec
  positive. This is not a green dynamic gate yet. The author remains read-only
  during verification. Cache version 83's measured golden, native/wasm size
  gate, and ModuleEnv round trips still precede the next WIP publication.
- Remaining after that slice: contributor migration; exact terminal target,
  rejection, and generalization authorities; normalized settlement events;
  option (e)'s deferred readiness and pre-unification nested-row rejection;
  checked-to-Boxy/Monotype transport; direct-result/Try adapters at template
  completion; missing call-formal positives; final schema/version updates;
  full verification and independent adversarial review. W2b, W7, and W8 have
  not started.

2026-09-10 verification continuation:

- Focused gate `44474` on `db3919ec` completed with 11/13 build steps and
  23/24 tests passing: types 8/8, collections 7/7, check 8/9. The sole failure
  is the new virtual-requirement local-copy fixture's policy assertion:
  expected `ranked_fresh_flex_close_scheme`, observed
  `ranked_fresh_flex_close`. The author is checking the exact producer and
  declared fixture intent before correcting anything. The failing check
  section will be rerun first; static acceptance is not dynamic acceptance.
  Full output: `/private/tmp/polarity-witness-gates.MlM03v/witness-db3919-focused-44474.txt`.
- Cache version 83's comment now accounts for both allocation/virtual-ingress
  witness semantics and canonical zero-capacity empty SafeMultiList cache
  descriptors. Its measured golden and schema gates remain outstanding.
- The sole fixture failure was traced to missing binding-scheme publication,
  not the policy producer. The explicitly generalized synthetic root now
  passes through the real `publishBindingScheme` operation after requirement
  capture, with an immediate classification assertion; the original
  `_scheme` expectation is retained. Side-table presence alone intentionally
  does not authorize a forced scheme-root copy. Independent review ACCEPTS
  this test-only correction and its controlled typed-local scope. Single-test
  retry `14615` is running on `cf4398a2` before the schema/hash gate.
- Retry `14615` completed GREEN: 7/7 steps, 2/2 tests. Schema gate `57855`
  then passed native and wasm32 serialization-size builds and the native
  size runner, plus all selected ModuleEnv round trips. Its only failure
  (4/5 tests, 30/32 steps) was the intentionally stale version-82 golden.
  The measured version-83 hash is
  `95bf7a59b6ec0ede80052b639aa29aceaf79f1a0add795c6a8670980309796e5`.
  Root applied exactly those bytes, with independent review ACCEPT and
  formatting green; targeted golden retry `16537` is running on `53c83296`.
  A combined final focused/schema gate will precede publication. Full
  measured output is saved as `schema-measured-version83-57855.txt` in the
  witness gate artifact directory above.
- Targeted golden retry `16537` completed GREEN (2/2 tests, 25/25 steps).
  Final combined gate `42891` completed GREEN on compiler source `53c83296`
  (only progress documentation changed afterward): 29/29 tests, 41/41 steps;
  types 8, checker 9, collections 7, compile/serde 5. This includes both new
  allocation actions, raw allocation uniqueness, the interpolation iff
  corruption/restoration checks, exact local rebuild and readonly/mutable
  byte oracles, historical malformed-import call authority, the original
  binding-codec split-component positive, and native/wasm32 size checks.
  All changed Zig files pass formatting. The historical fixture's Roc source
  is byte-for-byte unchanged; no hunk in this cohort touches that test.
- Root accepts this bounded witness/cache task after independent adversarial
  acceptance of the full witness slice, serializer, fixture lifecycle
  correction, and measured golden update. WIP change `qpmsttws` is described
  as `WIP: preserve W6b fresh-flex copy authority and canonical cache bytes`,
  with a Codex coauthor trailer, and is the next fast-forward publication on
  `jared/polarity` / draft PR #10434. The final report is
  `witness-publication-combined-42891.txt` in the artifact directory above.
- Next bounded task: reproduce the old record-update absent-occurrence
  failure on this checkpoint, then declare and implement explicit root-request
  versus resolved-share authority. Static audit found that a raw base request
  can redirect to a shared root whose authentic occurrence is `R -> R`, while
  publication incorrectly requests `V -> R`. Preserve actual sharing; do not
  fabricate an occurrence, force a copy merely to satisfy the validator, or
  choose a root through final canonical equality. Any new durable semantics
  belong to a separate next cache-version checkpoint. An independent audit
  of the 62 old call-formal witness crashes is also in progress. Neither
  failure group is yet dynamically certified fixed on this newer source.
- WIP publication completed and read back: `qpmsttws` is Git commit
  `e67aaeb8a89e56e40474fe27e7d24f88956c0c77`, fast-forwarded from `cc8ace35`
  on `jared/polarity`. Draft PR #10434's description matches the prepared
  body and reports both the new 29-test gate and the old full-checker red
  result. Root opened `zkmyrwqu` for the separate root-request task.
  Gate `81384` is running the unmodified pushed source with the minimal
  `record - update 1` case and the independently audited
  `literal strict demands flow through polymorphic called function summaries`
  call-retirement case. No new implementation is part of those reproductions.
- Current-source reproduction `81384` completed RED on pushed `e67aaeb8`:
  1/3 tests passed, 2 crashed (5/7 steps). `record - update 1` reproduces
  `local marker-copy witness named an absent raw occurrence`; the strict-
  demand call case reproduces `checked-boundary call formal lost its producer
  witness`. Both are current completion blockers. The first task preserves
  the authentic shared root with an explicit producer-authored request bridge;
  successful-call retirement after later poisoning remains a separate task.
  Report: `root-share-call-retirement-repro-e67-81384.txt` in the artifact
  directory above. No claim is made that all 24 or all 62 cases have the same
  cause until their targeted groups have been rerun.
- Root-request bridge preflight is accepted and implementation is underway,
  with design changes written first. Only record-update copies opt into an
  initial-request `ProofRootSelection`; child/detached walks cannot replace
  it and OOM clears it. The origin uses dense `direct_request=0` and
  `redirected_identity_share=1`, reusing a reserved word. Raw occurrence
  identity survives rebuild independently of current canonical roots.
  Other local origins and cross-module copying keep their existing behavior.
  This is the separate cache-version-84 task, not a rewrite of pushed v83.
- Driver audit corrected a preflight overclaim: admission has no whole-CIR
  fresh-byte comparison for record updates. A small contextual base-edge
  inverse is therefore included and declared before implementation. At the
  exact record owner index, candidate and supplied resolution environments
  must each contain a record update whose actual decoded base child is
  `origin.base_expr`. Bounds and expression tags are checked before reading
  each `span_with_node_data` backing row. Merely comparing `ExprRecord`
  payload indices is insufficient. The check runs in produced, fresh-cache,
  and admitted-republication contexts; self-context is checked, not skipped.
  It claims the exact record-to-base edge, not equality of all record fields
  or prevention of arbitrary coordinated proof-row permutations. Tests must
  distinguish this contextual authority from local byte-roundtrip oracles.
- Preliminary root-selection gate is GREEN: all 10 `instantiator proof:`
  tests passed (4/4 steps), including direct/redirected root selection,
  child/detached non-overwrite, zero-child structural publication, and
  allocation-failure clearing. The tested `instantiate.zig` SHA-256 is
  `6b4d4b304551a08b0f12ab43063db0534aaf69911ff1ae67f549ac40fdd26c39`.
  A syntax preflight correction preceded the gate; the first build attempt
  was blocked by shared Zig-cache permissions and the permitted retry passed.
  No disk-space error or cache pruning occurred. Report:
  `record-root-types-preliminary-permitted.txt` in the artifact directory
  above. Checker integration fixtures, full bridge review, schema-84 golden
  measurement, and the old 24-case group remain pending; this is not yet a
  completed or publishable bridge task.
- Independent adversarial review accepted the frozen types-only delta at
  that same digest: exact one-shot producer selection, structural/detached
  non-overwrite, synthesized-root exclusion, and allocation-failure cleanup
  have no static blocker. This does not yet accept the checker/durable
  integration. Its first real-source fixture and fresh-context negative are
  written; representative-change and local-publication OOM tests are being
  completed before the combined gate and full task review.
- Interim checker review found a concrete untrusted-input blocker: local
  record-update proof validation called `getExpr` before bounding the record's
  backing-table index, and that local pass precedes contextual admission.
  The already-declared bounds-first canonical-edge helper must also guard
  the local pass. The task adds a forged out-of-range backing-index test at
  both local validation and actual admission; rejection must return failure,
  never panic. The production fix is written and its independent static
  re-review accepts the bounds-first ordering.
- Early checker gate `97010` completed on frozen `e56344e7`: 2/3 tests pass,
  with the original `record - update 1` crash regression now passing. The new
  fixture fails a numeric assertion (`expected 0, found 1`) at its authentic
  producer-proof stage, before its corruption/rebuild/admission checks run.
  This is not a green integration gate. The fixture oracle is being audited
  against the exact field-projection authority chain: a fresh-shape support
  may use the intermediate projection root as its parent, rather than name
  the base-copy root directly. The source stayed frozen throughout the run.
  Report: `record-root-first-checker.txt` in the artifact directory above.
- The fixture-only correction is frozen at `e689ca2c` and independently
  accepted statically. It now checks the exact base-to-projection-to-field
  or fresh-shape-support chain, with source slot/site and support range
  preserved. Each raw selected root is checked against its own canonical
  projection; the two updates share their final canonical class, not
  necessarily the same raw occurrence. At least one genuine later
  representative change is required, and both rebuilds preserve the exact
  raw identities. Retry `24959` is running the same new fixture and original
  record-update regression. No production code changed in this retry.
- Retry `24959` stopped at semantic compilation: the three test-only
  diagnostic sentinel locals needed explicit `u32` types. That correction
  is the only subsequent code delta. The next normal gate, `26189`, is
  GREEN on frozen `d4537534`: 3/3 tests, 7/7 steps (14 seconds, 25 MB test
  runtime). Thus the first real-source proof/corruption/fresh-context/
  rebuild/readonly-mutable-serde/full-admission fixture and original
  record-update crash regression now pass. Check.zig SHA-256 is
  `db5edc4ea4bbda5cd74a463572a65948475cd0d6ed01273d8745fa4c47b4a9f5`.
  Report: `record-root-first-checker-oracle-u32-retry.txt` in the artifact
  directory above. The historical 24-case gate `37050` is now running.
  Remaining task work is the complete corruption matrix, an exhaustive
  measured OOM/retry sweep of the authentic pre-generalization redirected
  base producer, schema-84 verification, and final adversarial review.
- Historical 24-case gate `37050` completed on the same frozen compiler
  source: 19/25 tests passed (including the aggregator), six crashed, and
  5/7 steps succeeded. None of the 24 cases retains the original absent-raw-
  occurrence panic. Two now fail call-formal witness replay (generalized
  unset rejects required row; issue 10576), two fail erased record-update
  plan replay (empty base; missing field), and two fail local marker-copy
  proof validation (field mismatch 3; wrong optional payload). The last two
  have an independent exact-predicate audit in progress; they are not
  silently waived as unrelated baseline failures. The six downstream
  failures still block whole-W6b acceptance. Report: `record-root-all24.txt`.
  Frozen binary SHA-256:
  `9ae65f7b34298019c62a23912019e7e5abb8fb30bc7ee3604ca2c34e6369b15b`.
- Native and wasm serialization-size gate `70064` is GREEN (29/29 steps).
  `ModuleEnv.Serialized` remains 3520 bytes, NodeStore 528, Node.Payload 16,
  and Node 20. Report: `record-root-version84-serialization-sizes.txt`.
  Schema-84 golden measurement and compile-module serde tests remain pending.
  The driver also verified a non-emitting semantic-only checker preflight
  using the exact normal-build compiler arguments and generated Builtin
  inputs; this is an additional fast diagnostic step for test-only edits,
  not a replacement for normal builds or executed acceptance tests.
- Complete matrix gate `68818` finished on frozen compiler `49cc5cce`:
  18/19 tests passed and 36/38 steps succeeded. Types passed 10/10; checker
  passed 4/4, including direct/redirected origin corruption, genuinely fresh
  contextual admission, readonly/mutable serialization, two exact rebuilds,
  and the exhaustive staged redirected-publication allocation-failure sweep.
  Each failed allocation rolls back semantic state and permits same-instance
  retry; map capacity is additionally checked on same-instance rollback.
  Native/wasm serialization sizes passed. Compile/serde passed 4/5: its sole
  failure was the intentionally stale version-83 golden. The measured
  version-84 hash is
  `9006bb901cb33ee2d114390a05259a8fe5bb3416d3393cb93a681f1b420f2a15`;
  the driver updated only those golden bytes and started targeted retry
  `47558`. Reports: `record-root-full-matrix-combined.txt` and
  `record-root-version84-golden-retry.txt` in the artifact directory above.
  The checker digest is
  `aabdd481bd247f24abcf0b0af9782d109c32982f357cede3416f08a1647bbd2b`.
- Final adversarial review is resolving two explicit acceptance boundaries:
  retained record-update origins still require a live owner (the old local
  validator already required `expr_record`), and the new source-edge bridge
  proves each row's owner/base edge rather than a whole-owner bijection.
  The four downstream record-update failures require explicit early/late
  retirement authority; coordinated owner-plus-base substitution needs an
  exact projection/owner inverse audit. Neither is waived for whole-W6b
  acceptance. The author is preparing the next bounded retirement task
  read-only while the current compiler source remains frozen.
- Golden retry `47558` is GREEN: 2/2 tests and 25/25 steps. The sole schema
  golden edit copies the exact measured version-84 bytes above. Changed Zig
  files pass formatting. Final combined gate `99548` adds the existing
  shared-helper consumers (rebuild OOM, method-output OOM/Probe, selected-method
  rollback, external-cache OOM, and nested Probe ownership) to the focused
  bridge and compile/serde/size tests.
- Final static verdict: the root-selection/redirect transaction mechanics
  are accepted only as an explicitly incomplete WIP checkpoint. Complete
  checked-boundary admission has a REQUEST CHANGES finding: changing just
  the first redirected step's `origin.{record_expr,base_expr}` to the second
  update's owner/base leaves both authentic backing rows untouched and can
  pass local plus fresh per-row validation. Its projection chain still
  belongs to the first owner, which is orphaned while the sibling is
  duplicated. The next task must implement the already-declared mandatory
  base Expected plan and exact live/retired owner/base-step/projection
  converses, including zero-field updates. A field-only equality check is
  insufficient. The four downstream record-update failures additionally
  require early/late failure retirement without restamping successful plans.
  The reviewer found no other static blocker to the mechanical WIP scope;
  publishing that checkpoint does not waive this admission finding.
- Final gate `99548` is GREEN: 26/26 tests and 38/38 steps (types 10,
  checker 11, compile/serde 5), including native/wasm serialization sizes and
  all seven selected shared-helper regressions. Compiler digests stayed
  identical to the reviewed freeze; only the measured golden changed after
  `68818`. Report: `record-root-final-shared-helper-combined.txt` in the
  artifact directory above. The driver prepared the full jj description
  `WIP: preserve record-update root-selection and rollback mechanics` with
  coauthor trailer and a PR body carrying the exact admission finding and
  remaining failures. This is publication of accepted lower-level mechanics,
  not completion of record-update admission, W6b, or the feature.
- Publication completed: `zkmyrwqu` is Git commit
  `1f69432c86d3945b48623b97fed51b7da880eb64`, fast-forwarded from `e67aaeb8`
  on `jared/polarity`. Draft PR #10434's body and head were read back and
  matched exactly. The new child `wpvvswsl` owns the mandatory base-plan and
  live/retired ownership repair. Its preflight must explicitly define base
  plan storage (the existing `producer_root` surface is call-argument-only),
  arbitrary-index field registrations (nested checks interleave plan rows),
  and exact early/late error causes without rewriting successful projection
  plans. Zero-field and unset/final-relation failure paths require explicit
  treatment; a field-only inverse cannot discharge the review finding.
- Current-source next-task baseline `15948` reproduced all four retirement
  failures on pushed `1f69432c`: 1/5 tests passed (aggregator), four crashed,
  5/7 steps. Empty-record and missing-field updates fail exact erased-plan
  replay; field mismatch 3 and wrong optional payload fail local copy-proof
  validation after owner erasure. Report:
  `record-ownership-retirement-four-baseline.txt`. The baseline compiler is
  unchanged from the accepted mechanical checkpoint. Design preflight is
  still required before the next implementation; this is diagnostic evidence,
  not a new regression introduced by the pending ownership task.
- The next implementation is deliberately staged. First, `wpvvswsl` adds
  the live-owner inverse with mandatory base plans, including zero-field
  updates. The declared `source_root_copy` and
  `source_root_copy_checked_error` outcomes retain the existing actual base
  copy, exact syntax request, and produced destination root; neither reuses
  call-only `producer_root`. A checked-error base carries its exact typed
  cause, and skipped fields point upstream to the earlier base plan while
  retaining its copy endpoint. Producer registrations preserve arbitrary
  plan indices, while cache admission independently enumerates durable rows.
  The author wrote the design declaration before compiler edits; independent
  preflight review is completing.
  Required follow-on slices are R1 (the four reproduced retirement cases and
  exact live/retired group converse), R2 (immediate unset and final-relation
  outcomes), and R3 (annotation/enclosing-producer retirement). Successful
  field projections remain immutable; late failures need separate typed
  failure rows, not an `anchored_checked_error` restamp. Deferred unset kind
  checks currently emit diagnostics only and are not falsely claimed as
  rewrite causes. This first live-owner slice alone will not certify complete
  record-update admission or W6b.
- Independent final design preflight is ACCEPT: the complete base semantic
  key, inactive/reserved fields, exact produced root, closed cause-arm
  ownership replay, skipped-field upstream edge, and transient/durable
  distinction are declared. The driver authorized implementation of the
  bounded live-owner slice. Both endpoint-bearing outcomes must participate
  in legality, ordering, root retention, reachability, remapping and serde;
  they must not inherit call-only `producer_root` behavior or silently vanish
  through an anchored-only switch. The author will freeze an early compilable
  delta before expanding the full adversarial matrix.
- First implementation diagnostic freeze `af654f08` contains the new outcome
  vocabulary, transient registration init/deinit/Probe rollback, mandatory
  base plan/copy/registration transaction, field registrations and upstream
  base-cause links, and partial exhaustive-switch integration. Live admission
  inverse, new tests and cache-version-85 bump are not yet implemented.
  Non-emitting checker preflight `7759` completed in 22.78 seconds and found
  one reported compiler error: the direct-source inner reason switch must
  explicitly reject `record_update_field_base_checked_error` now that its
  outer relation is upstream. The author was thawed to correct it and
  continue. No runtime gate was run on this partial implementation.
  Frozen Check digest:
  `19aa786ba256ee2078c41ba1bb4f69f02705eaaa084421e21e943003f33664a0`.
  New task artifacts are in `/private/tmp/polarity-live-owner-gates.JLEzuh/`:
  `first-diagnostic-freeze.diff` and `first-semantic-diagnostic.txt`.
- Targeted semantic retry `54769` is GREEN (exit 0, 23.96 seconds) on frozen
  `49047994`, after only that explicit impossible-reason switch correction.
  Check digest is unchanged; ModuleEnv digest is
  `729dc5f78cfa919dd8f55f32497c9161053a7dcaf2327844394b65adde35d4f9`.
  Report: `first-semantic-switch-retry.txt` in the new task artifact directory.
  This is a non-emitting compile check, not executed tests. The author resumed
  live admission, exact cause replay/ownership-count integration, tests and
  the separate cache-version bump. The old copy-only OOM fixture remains a
  lower-level test; the new outer transaction needs its own authentic staged
  plan/copy/registration failure sweep.
- Driver/adversarial review sharpened two integration requirements before
  the next freeze. A base may propagate a nested child's cause through its
  own exact, syntax-valid Expected plan; direct `failure.owner_node == base`
  alone is too narrow, while index validity or an arbitrary descendant is
  insufficient. Such a base plan is a causal reference, not a second owner
  in either ExpectedFailure ownership counter. The transient registration
  list also needs an explicit recorded/consumed lifecycle: first rebuild
  validates complete membership even for an empty list, failure preserves
  incoming state and handles, and successful commit clears/consumes it
  infallibly. Repeated rebuild must not leave stale plan indices or use
  list emptiness as evidence of consumption. These are being implemented and
  reviewed; the new live inverse and these additions have no executed gate
  yet. Per-slot uniqueness/coverage and genuine structural base copies are
  explicit requirements for the upcoming adversarial matrix.
- A further read-only audit confirmed that the existing independent fresh
  context checks the record extension/base edge, not supplied-field or unset
  topology. The first live-owner slice therefore claims the exact base
  owner/plan/step converse and candidate-local field-slot membership; it must
  not claim fresh-authenticated field topology. Coordinated candidate field
  payload plus plan retarget is a required R1/full-admission follow-up, to be
  closed with exact semantic topology checks rather than whole-CIR payload
  bytes. Canonical ordering does preserve base-before-field for the same
  owner: the base has no plan-valued dependencies and its role sorts first.
  It does not prove an arbitrary nested-cause support plan has a lower final
  index than the outer base plan. Causal support must use a valid authored
  route without that unsupported cross-owner index assumption, and multiple
  valid routes carrying the same cause must remain accepted.
- The coherent live-owner freeze now wires the base/plan/step inverse into
  both produced and rehydrated validation, checks exact candidate-local field
  slots with no extra roles, and implements commit-only registration
  consumption. Cache version is 85; its measured hash golden is still pending.
  The early semantic gate found two local `fresh` naming collisions; both
  were corrected. Retry `68319` is GREEN (exit 0, 24.32 seconds) on frozen
  `0a733218`, Check digest
  `004c2d8baa086ea7b8a0713232a6b818d7c3c8195776cd632835fda8bc87709b`.
  Normal positive runtime gate `86253` is running against that frozen code;
  no new live-ownership tests have been added yet. Reports are
  `first-positive-semantic-all-renames.txt` and `first-positive-runtime.txt`
  in the live-owner artifact directory above.
  Adversarial review requires an explicit clean-cache rejection of every
  checked-error/reserved Expected plan and every consumer retirement; the
  current clean-cache helper omits these pools. It also confirmed that a
  propagated cause supported by an aggregate plan inherits the existing lack
  of a durable live aggregate eligibility converse. That remains a required
  full-admission dependency, not independent fresh-authenticated authority.
  Eligibility of unvisited updates beneath rejected canonical parents is
  under audit. The author remains frozen during the runtime gate; the next
  implementation step includes the cache gate and an authentic production
  helper for exhaustive base-plan/copy/registration OOM tests.
- Review found a current-slice blocker in the new checked-error base arm:
  `ExprCheckFrame.finish` records every checked-error expression for terminal
  poisoning, which changes the base child to `.malformed`, while both current
  record-update base decoders require an `expr_*` child. The outer update may
  remain live, so this is not the deferred record-owner retirement problem.
  The arm needs an explicit, typed base-child retirement/lifecycle contract
  and an authentic checked-error positive through the complete checker tail.
  The driver assigned design-first repair and independent preflight; blanket
  acceptance of malformed children or solved-type reconstruction is forbidden.
  The current runtime gate is positive-only and cannot discharge this finding.
- Positive runtime gate `86253` is GREEN: 6/6 tests and 7/7 build steps on
  the frozen `004c2d8b` Check source. This covers both existing root-authority
  tests, ordinary update 1/2, nominal-extension lifting, and the test
  aggregator. The author was thawed for the required base-child retirement
  contract, clean-cache gate, production transaction helper and new tests.
  An early coherent freeze must include an authentic checked-error positive
  before expanding the complete corruption/OOM matrix. These passing positive
  controls do not close the error-path review blocker or certify admission.
- A second concrete review finding concerns the domain of the reverse
  syntax-to-plan inventory. Chained-range canonicalization retains already
  stored operands when replacing the outer expression with a malformed root.
  A record update in that discarded operand subtree is never checked, yet
  the current all-node scan requires a base plan for it. The call inventory
  already documents this class of unvisited canonical nodes. This cannot be
  repaired by a solved-type or shape-based exemption. Before acceptance, the
  task must either consume exact earlier-authored retained/reached authority
  or narrow its WIP claim and checks to the plan/step/live-owner converse,
  leaving missing-whole-bundle detection as an explicit mandatory authority
  dependency. The driver requested an audit of existing explicit canonical
  roots and traversal data before choosing; no eligibility workaround is
  authorized. The authentic failed-base repair remains the next small gate.
- The failed-base lifecycle design received independent exact-text ACCEPT
  for `design.md`'s Expected-consumption declaration and Rewrite Inventory.
  The plan's existing trailing word becomes a closed plus-one source-retirement
  coordinate, active only for the checked-error base outcome. It preserves the
  original status cause separately from the later destructive rewrite.
  Preexisting malformed bases reuse their exact completed publication and
  retirement; live bases reuse an explicit owning producer or reserve the
  dedicated `checker_rewrite_record_update_base` kind only when the base owns
  no Expected plans or retirement-producing drafts/groups. The dedicated
  draft completes at central replacement, with exact original payload,
  Probe/OOM ownership, rebuild remapping and serde preservation. Scalar
  retirement validation precedes the nonrecursive plan/retirement converse.
  The driver and reviewer read the complete declaration before authorizing
  compiler edits. The next freeze must include one authentic full-tail error
  test; the separate all-node eligibility finding is not waived by this
  preflight acceptance.
- The driver resolved the eligibility scope after the independent audit:
  existing module roots, demand traversal and hoist walkers do not encode the
  checker's reached/short-circuited decision. A new late structural walk would
  not supply that missing producer authority. This WIP will therefore validate
  the plan/base-copy-step/live-owner converse and exact candidate-local field
  membership for published owners, not require a plan for every raw record
  node. The author must declare that staged scope before removing both local
  and fresh-context all-node scans, and add the genuine discarded chained-range
  subtree regression. A missing base plan with a remaining step is still
  corrupt, including a zero-field update; coordinated deletion of the complete
  plan-and-step bundle cannot yet be detected. A durable earlier-authored
  eligibility/reached token and the complete source-site bijection remain
  mandatory full-admission work. This narrows the checkpoint claim, not the
  final W6b requirement, and must be explicit in review and publication.
- The first coherent base-lifecycle implementation freeze is `3d4af296`.
  Non-emitting semantic gate `22114` is GREEN (exit 0, 24.49 seconds), with
  Check digest
  `0515343358286a06370792253d7e1a69c24901fd185f32e32487feb907ccbf9f`
  and ModuleEnv digest
  `2736425ebb4a17559d4162e2f90f6cc831dad4d24cf7642ac058a19b7e2613fc`.
  It includes the plus-one coordinate, dedicated retirement kind/draft,
  central completion guarded outside a Probe only when that draft exists,
  closed pending producer selection, phase-aware fresh replay, relocation,
  clean-cache rejection and the scoped published-owner inventory. The
  accidental intermediate ExternalLookupToken rename was restored before
  this freeze; the coordinate exists only on ExpectedConsumptionPlan.
  Reports: `base-lifecycle-first-freeze.diff` and
  `base-lifecycle-first-semantic.txt` in the live-owner artifact directory.
  This is compile readiness only: no new runtime fixture or complete matrix
  exists at this freeze. Independent frozen-code review is in progress.
  The next small task is an authentic direct-binder error fixture: that
  existing producer emits a checked-error status without itself marking the
  lookup's raw type erroneous, so it may retain a live outer update. It must
  reuse the existing ineligible lookup retirement, not fabricate the new
  dedicated kind. A malformed raw base, by contrast, can poison the entire
  update even with an outer annotation; normal poisoning must not be changed
  to force this bounded fixture to pass.
- The authentic direct-binder fixture is added in frozen `dbdb36b3`
  (Check digest
  `45e0511e4b7497122d0defcfe99f9f94c7161ae5cd7cf70a2e23cb800fbcd179`).
  Its semantic gate `69154`, now including the new live-ownership test body,
  is GREEN (exit 0, 23.22 seconds). Normal runtime gate `27042` is running
  that fixture alongside the six existing controls. Reports:
  `direct-binder-error-fixture-freeze.diff`,
  `direct-binder-error-fixture-semantic.txt`, and
  `direct-binder-error-fixture-runtime.txt`. No runtime result is claimed yet.
  Frozen review found another current-slice obligation: a nested child cause
  must replay through the retired base's exact retained-consumer membership
  and original payload/slot once the aggregate base is malformed. The current
  fallback to live-only base syntax rejects that authentic route. A
  phase-aware, exact retired-group join and a genuine nested aggregate-base
  fixture are queued after this gate; generic malformed acceptance remains
  forbidden. The reviewer separately verified that current non-retirement
  status producers have no pending-versus-terminal validation blocker.
- Runtime `27042` is RED: 6/7 tests passed and the new direct-binder fixture
  crashed in `validateExpectedRecordUpdatePlans` during produced validation
  (5/7 build steps). All six existing controls remained green. The driver
  assigned targeted diagnosis of the exact local predicate before extending
  the matrix. A separate confirmed contextual blocker follows it: the new
  record-update context helper currently drops `ImportResolution`'s replay
  mode, then treats the producer's already-poisoned environment as fresh
  canonical input. The repair must consume the existing explicit produced /
  fresh-canonical / recovery-forbidden mode, not infer it from pointer
  equality or skip contextual validation globally. Only the failing
  live-ownership section will be rerun until it passes; the nested fixture
  remains queued behind this direct-case gate.
- The temporary, test-only diagnostic freeze `d1cf2d94` passed semantic
  gate `58638`. Targeted runtime `41977` is RED (1/2 tests passed, one
  crash): both the outer record owner (node 17) and base (node 14) are
  malformed, while the supplied field remains live. The base plan's legal
  tags, status cause, source-retirement join, cardinality and the skipped
  field's base authority/upstream cause all pass. The first rejection is
  therefore the live-outer-owner precondition, not a broken base-retirement
  reference. This authentic fixture enters the deferred R1 outer-record
  retirement domain. Author and reviewer are investigating whether a genuine
  live-outer checked-error source exists; normal poisoning must not change
  to manufacture one. If that boundary is not independently reachable, the
  task must integrate the exact outer retirement lifecycle before claiming
  a runtime-positive checked-error path. The separate explicit context-mode
  repair is still pending. Reports: `direct-binder-local-diagnostic-freeze.diff`,
  `direct-binder-local-diagnostic-semantic.txt`, and
  `direct-binder-local-diagnostic-runtime.txt` in the live-owner artifact
  directory. The diagnostic is temporary and must be removed before acceptance.
- Independent producer tracing ruled out a zero-field direct-binder variant
  before any fixture edit or additional runtime build: malformed where-alias
  checking marks the formal's owner type erroneous; the lookup unifies with
  that formal, and even an empty update performs the final base-to-owner
  relation. Ordinary expression completion therefore poisons the outer owner.
  The author accepted the trace. The driver advanced the exact outer-record
  retirement dependency into the current work, with the existing one-field
  source retained as the first authentic acceptance target. Its design must
  consume arbitrary-index producer registrations, preserve completed plans,
  retain exact independent failure and rewrite causes, and authenticate the
  complete retired group and fresh source topology. The context-mode design
  amendment is written and under independent preflight review; compiler code
  for that amendment and R1 is not yet implemented at this milestone.
- Context replay is now implemented in freeze `9daf9c0b` (Check digest
  `204a9c94ff7c0e0d26e88f96c722768938b765d698234dad52c966bed318b5cc`).
  Independent static review ACCEPTS the exact declared live-owner scope;
  semantic gate `98033` is GREEN. Full `ImportResolution` reaches the helper,
  with explicit produced/fresh/recovery-forbidden branches and no inferred
  pointer freshness or self-snapshot comparison. R1 owner-phase extension
  and runtime-positive coverage remain pending. Reports:
  `context-mode-first-freeze.diff` and `context-mode-first-semantic.txt`.
  A broader producer audit found no authentic current live-outer checked-base
  path and no current producer for the dedicated
  `checker_rewrite_record_update_base` kind: reachable checked-base statuses
  already own preexisting, ineligible, call or aggregate retirement authority.
  The driver approved design-first removal of that unused kind and its draft
  machinery, retaining the exact source-retirement coordinate and reuse of
  existing producer lifecycles. Missing future producer authority must remain
  an invariant failure until its own declaration, implementation and positive
  test exist; no generic retirement may stand in for it.
- The exact-existing-retirement simplification is implemented in freeze
  `a8980892`: Check digest
  `61c45df812458db04ab4b181afb9d8448c2ed82026000793cf57c8a017e27c47`,
  ModuleEnv digest
  `4c063053d58ca91984174c31ad9a177db2d9357effca04216978d52d5d1c4de1`.
  Independent static review ACCEPTS this removal; semantic `13107` is GREEN.
  The source-retirement coordinate and exact existing producer selection
  remain; the unused enum kind, draft, completion, allocation, Probe and
  validation branches are removed. Direct diagnostic `20988` reproduces the
  same outer-owner failure with unchanged valid base-cause/source-retirement
  and field-chain predicates. This direct compiler loop uses the actual
  schema-85 inputs captured from the normal build and took approximately
  one minute; it is diagnostic evidence only, not acceptance with regenerated
  built-ins. The normal producer-regenerated gate remains required. Reports:
  `existing-retirement-only-freeze.diff`,
  `existing-retirement-only-semantic.txt`, and
  `existing-retirement-only-direct-diagnostic.txt`. R1 design is the next
  implementation prerequisite; no new checkpoint is publishable yet.
- The bounded checked-base outer-retirement design now has exact-text
  ACCEPT from both the driver and independent reviewer, including its
  Rewrite Inventory entry. Implementation is authorized. The existing
  `checker_rewrite_expected` retirement `R_U` owns the exact arbitrary-index
  base/field consumer set; the unchanged base plan is its sole typed trigger
  and keeps its distinct base retirement `R_B`. No duplicate outer
  ExpectedFailure is introduced. The new reason is retirement-row-only and
  cannot legalize an unproduced base-plan outcome. Depth-zero completion
  validates and reserves before invalidation, then appends/sorts only the new
  suffix and publishes infallibly. Scalar snapshot decoding is nonrecursive
  and separate from the complete group/cause inverse; nested pending source
  lifecycles and both poison orders are explicit. Existing checked-boundary
  rebuilding repacks disjoint completion-order ranges before terminal
  admission. Fresh replay checks decoded base, field and unset topology,
  not backing-table indices alone. The authentic one-field fixture will use
  the `record-update owner retirement:` test prefix and require both outer
  and base retirements. Its targeted semantic/direct/normal gate commands
  include that prefix. This is design acceptance only; runtime, corruption,
  both-order/interleaved-retirement, zero-field, OOM, rebuild, serde, cache
  golden and final frozen review remain required. Established-base later
  failures (R2/R3) and whole-bundle reachability authority remain separate
  mandatory completion work.
- R1's first implementation scaffold passes the short schema-85 semantic
  diagnostic at frozen revision `21aba3f6993517f8359ba64b9c825c38adfa8fe7`
  (session `13250`, no compiler diagnostics). It includes the owner draft and
  Probe lifecycle, producer reservation, canonical pending-row checks,
  depth-zero completion, pending-draft admission guards, and initial scalar
  snapshot/group validation. The author deliberately froze before context
  replay, retirement-only rebuild routing, and the authentic fixture oracle
  were integrated; no runtime acceptance is claimed for this scaffold.
  Reports: `r1-scaffold-semantic.txt` and `r1-scaffold-freeze.diff` in
  `/private/tmp/polarity-live-owner-gates.JLEzuh/`. Review also requires
  prepare-time lifecycle exclusivity and an infallible R1 completion branch
  after invalidation. The driver resumed implementation after this diagnostic;
  the published checkpoint remains `1f69432c`.
- First coherent R1 integration at `032844440141508e4e8672f38c0d7ea3767650d6`
  passes semantic preflight (`49697`) and the direct runtime diagnostic
  (`98542`, 2/2 tests). The authentic one-field direct-binder fixture now
  completes the real checker tail, preserves distinct outer/base retirements
  and the exact two-member outer consumer set, and passes local and fresh
  admission. Independent adversarial review gives scoped STATIC ACCEPT:
  produced/fresh owner phases, decoded topology comparison, retirement-only
  rebuild/remapping, and enum-domain disjointness are sound for this slice.
  Reports: `r1-first-coherent-{freeze.diff,semantic.txt,direct-diagnostic.txt}`
  in the same artifact directory. Normal producer-regenerated gate `67011`
  is running; the direct diagnostic alone does not replace that acceptance
  path. Zero-field/nested/interleaved owners, both poison orders, exhaustive
  OOM/retry, corruption, repeated rebuild, serialization and measured cache
  golden remain unverified. This is not complete ownership or W6b acceptance,
  and no new checkpoint has been pushed.
- Continued adversarial review of `03284444` found a pre-invalidation blocker
  after that initial scoped acceptance: completion rechecks equality among
  the draft, base-plan and field-plan causes but does not revalidate semantic
  ownership of that cause at the current base phase. Coordinated corruption
  of all those carriers could survive until terminal rejection after the
  live node was erased. The author must replay exact pending/completed
  base-cause validation before invalidation and pin coordinated corruption
  as a no-op. The positive runtime result remains valid, but this revision
  is not accepted for publication while that finding is open.
- Normal producer-regenerated gate `67011` passes at `03284444`: 7/7 steps,
  2/2 tests, including Builtin regeneration and the authentic owner-retirement
  test (`r1-first-coherent-normal-runtime.txt`). The subsequent cause-check
  fix at `69db20112cd7756e1089becdf3ea13d51c88c7e0` replays the existing
  pending-aware/terminal base-cause predicate before invalidation. Independent
  review accepts that exact fix and confirms nested pending retirement checks
  remain nonrecursive. Semantic regression `15436` passes; its direct runtime
  regression is running. The coordinated-cause corruption/no-op test remains
  mandatory in the broader matrix. No new publication yet.
- Cause-fix direct regression `3082` passes (2/2 tests); both the reviewed
  fix and its authentic positive remain green. The driver froze source while
  preparing the minimum verified, explicitly incomplete WIP publication
  requested by the user. Existing canonicalization/types/checker/compile,
  shared-helper, serialization and size regression gate `71234` is running;
  it also measures the intentionally stale cache-84 golden against schema 85.
  The author is read-only while planning the next real-producer zero-field,
  nested and two-poison-order test slice. Publication scope remains subject
  to independent review; neither a W6b-completion claim nor a new push has
  occurred.
- The reviewer initially allowed that narrow WIP scope, then withdrew it on
  a concrete nested-order trace: pending inner `R_U` authentication chooses
  its base phase from `R_B.kind`. A completed checker rewrite retains that
  kind even after the base becomes malformed, so leaf-base-first followed by
  outer-before-inner retirement wrongly replays a live base. This violates
  the already-declared order-independent R1 lifecycle, rather than merely
  lacking coverage. Publication is blocked until the producer authenticates
  the actual phase through the exact pending/completed `R_B` lifecycle and
  the real nested-order regression passes. The driver and author accepted
  the finding without narrowing the design. Gate `71234` continues as useful
  existing-regression/cache evidence; no checkpoint has been pushed.
- Existing combined gate `71234` finishes with 36/37 tests and 39/41 steps:
  canonicalization 8/8, types 10/10, checker 14/14, compile/serde 4/5. Native
  and wasm serialization-size checks pass. The sole failure is the expected
  stale version-84 cache golden. The driver replaced it with measured
  schema-85 hash
  `aae0cef39269365ef15dce3bfc3708977e1820d516d17fb482ca3dd73c8e685b`
  and started only the failing golden section (`5453`), preserving the frozen
  compiler implementation. Reports: `r1-checkpoint-existing-regression-first.txt`
  and `r1-version85-golden-retry.txt`. The known nested-lifecycle defect still
  blocks publication despite these passing existing tests; fixing it and
  adding its authentic order regression is the next author task.
- Focused schema-85 golden retry `5453` passes (2/2 tests, 25/25 steps) with
  the measured bytes above. The driver thawed the author for the nested
  pending-lifecycle repair and authentic order tests, plus removal of the
  temporary local-plan diagnostic helper. Zero-field coverage must use an
  unset-only update because canonicalization collapses the no-field/no-unset
  spelling; arbitrary-index coverage must keep the nested supplied field
  healthy, distinct from a nested checked base. The known defect still blocks
  publication, and the combined gate must be rerun after its fix.
- Nested-identity scaffold `2efca5dae1c82270a948f4ba5be5a7b61b83f187` passes
  semantic preflight `51557`. The author split shallow pending identity from
  the immediate-source check, authenticated actual pending/completed phases,
  validated inactive fields and unique owner rows, and removed the temporary
  diagnostic helper. The driver read and accepted the precise normative
  clarification in `design.md`. Review accepts that core structure but found
  a remaining zero-plan aggregate lifecycle condition: only its declared
  empty-range/reason combination is legal. That is being fixed before the
  authentic nested-order test; no nested runtime result or publication is
  claimed yet. Reports: `r1-nested-identity-scaffold-{freeze.diff,semantic.txt}`.
- The focused nested-order implementation and real-producer regression are
  now green at `fc78e9a157db3fed84c39fe8e2c3d3282230dd2e`. The first fixture
  semantic check (`52804`) caught incorrect access to the unset-span wrapper;
  both sites were corrected and semantic retry `25217` passes. Direct runtime
  `18062` passes 3/3 tests: the original one-field case and the nested case
  exercising both outer/inner retirement orders after the real leaf lookup
  sweep. The nested test pins the exact three-retirement graph, resumes the
  unmodified checker tail, validates fresh context, and compares canonical
  serialized bytes across orders with stable preallocated diagnostics.
  Independent review accepts this bounded WIP scope contingent on final
  combined gate `76513`, which is running. The known nested phase defect is
  repaired; unverified zero/unset, preexisting-base, arbitrary interleaving,
  OOM/corruption, active serde/rebuild, complete source eligibility, R2/R3 and
  call-formal surfaces remain open. No push or whole-W6b acceptance yet.
  Reports: `r1-nested-orders-span-fix-{freeze.diff,semantic.txt}`,
  `r1-nested-orders-first-direct-diagnostic.txt`, and
  `r1-nested-checkpoint-final-regression.txt`.
- Final combined gate `76513` passes at the unchanged reviewed compiler
  freeze: 38/38 tests and 41/41 steps (canonicalization 8, types 10, checker 15,
  compile/serde 5), including regenerated Builtins, the measured schema-85
  golden and native/wasm serialization sizes. Independent adversarial review
  confirms final scoped ACCEPT for the bounded R1 checkpoint only. The jj
  description is `WIP: bind record-update copies to checked-base owner
  retirements`; the driver is preparing its branch/PR publication. The
  compiler source hashes are Check
  `fbbacdbc95095a6656f435fe6177b7b98270f463949e29f81ac9f32ae00ba9e0`,
  ModuleEnv `5dc3cc45ff0abbaa579a0ea06f2c520630fd92adbdc7ecc223cc0c0791ba9be4`,
  and cache golden file
  `ed55216bf942ca23e27a0e003cbd557f18708cd5b326bba8a0f5a4f71094ddc1`.
  Later documentation/description edits do not alter that tested source.
  Complete ownership, the remaining negative/OOM/active-serde matrix, R2/R3,
  call-formal failures, option-(e) rejection and adapters remain open.
- Publication is complete and read-back verified: `wpvvswsl` is Git commit
  `95dfea5a272b2c0263b15248a4a328e558799285`, pushed by fast-forward from
  `1f69432c` to `jared/polarity`. Draft PR #10434 has that exact head and the
  exact updated body, with `isDraft == true`. Artifacts in
  `/private/tmp/polarity-live-owner-gates.JLEzuh/` include
  `r1-checkpoint-description.txt`, `pr-body-r1-checkpoint.md`, and
  `r1-pr-readback.json`. The driver created clean child
  `txopxlzxxsymywntzkktrlpvkyxvyxpl` and dispatched the same sol-ultra author
  for real zero-supplied-field/unset-only, healthy nested-field arbitrary-index,
  and retirement-only reason-domain coverage; independent sol-ultra review
  follows. New-producer OOM/corruption/active-serde, complete admission, R2/R3,
  and the rest of W6b remain separate mandatory tasks.

- The early boundary slice in `txopxlzx`, frozen at `7b0637e5`, passes
  semantic preflight (session 95275), direct checker diagnostics (session
  89661, 4/4 tests), and the normal reason-domain gate (session 14157, 9/9
  tests, 7/7 steps, regenerated Builtins). Independent adversarial review
  accepts only its numeric-half reason rule and authentic unset-only
  base-plan-only retirement fixture. The healthy nested-field interleaving
  fixture and real reason-corruption/restoration tests are still being added;
  this child is not yet published. Its numeric-half guard strengthens the
  future-extension invariant without changing current legal values or the
  schema-85 layout. The inactive optional plan-reason `none` sentinel is
  explicitly exempt from the undeclared-reason prohibition.

- The complete boundary-test slice is frozen at `9e1bf6d8`: semantic
  preflight 16551 and direct runtime 50549 pass (5/5 tests), including healthy
  nested-field interleaving and authentic consumer/plan reason corruption
  with restoration. Normal combined gate 7662 passes 41/41 tests and 41/41
  steps: canonicalization 9, types 10, checker 17, compile/serde 5, regenerated
  Builtins, measured schema-85 golden, and native/wasm sizes. Independent
  adversarial review accepts this bounded slice. Check SHA-256 is
  `7e3ab156d98a1546267fa15d4d2e2b18267f41144bc1106c6211d040e559787f`;
  ModuleEnv remains
  `b6d2a1bf372b4b85352f77f805361a95a8e36632567338a911590c535c0d1de6`.
  Documentation edits do not change the frozen compiler source. The driver
  is describing and publishing `txopxlzx` as `WIP: verify checked-base
  record-update ownership boundaries`, then continuing with actual-producer
  allocation-failure and rollback coverage. No broader R1/W6b completion is
  claimed. Exact local artifacts are `boundary-complete-final-regression.txt`,
  `boundary-checkpoint-description.txt`, and `pr-body-boundary-checkpoint.md`
  under `/private/tmp/polarity-live-owner-gates.JLEzuh/`.

- Publication of the boundary slice is complete: `txopxlzx` is Git commit
  `10f613d6181a759ce665b9ce399fa405350dee6f`, fast-forwarded from `95dfea5a`
  to `jared/polarity`. Draft PR #10434 has that exact head and exact updated
  body, with draft state preserved. Readback is saved as
  `boundary-pr-readback.json` in the artifact directory above. New child
  `onmovlvrxsxtqptkpykqlqnvznskwsur` owns the next bounded task: extract the
  actual base-plan/copy/registration publication transaction into a helper
  used by `checkExprRecord`, and test exhaustive established-base allocation
  failure, same-instance retry and outer local-transaction rollback. The outer
  `LocalMarkerCopyTransaction` owns the Env rank suffixes and consume-once
  inputs in addition to its nested Probe's ledgers; bare `Probe` does not own
  Env rank-pool rewind. Preserve the existing
  lower-level redirected-copy OOM test. Extend the logical snapshot to include
  private registration/draft rows and the consumed flag. Root and independent
  review confirmed those are omitted from the old test snapshot, although
  Probe already rolls the private list lengths back. Capacity retention is not
  logical publication; clean-vs-retry compares semantic state. Checked-base
  publication and R_U reservation/completion OOM remain later bounded tasks.
  No authority-positive fixture may manufacture rows or delete previously
  produced rows to simulate an earlier producer boundary.

- The actual base-publication helper extraction in `onmovlvr` is frozen at
  `b8ba18a3`, with semantic preflight 89725 passing. Production
  `checkExprRecord` calls `publishRecordUpdateBasePlan` immediately after the
  real base check, carrying its returned status and exact raw occurrence. The
  helper owns the unchanged plan/copy/registration transaction and returns
  its committed coordinates by value. Existing direct runtime regression
  69805 passes 5/5 tests. The author is adding the new snapshot and allocation
  tests; independent extraction review is in progress. Check SHA-256 is
  `a3ee5716499cb044a832bb0efd07d010256d1599d06df4a12164e343807303ce`.

- The coherent full `onmovlvr` slice is frozen at `bb6874ba`, Check hash
  `1d8e9381f77f191822042de5bd9a2379aa39d5a374b31ce14bd40e50794e8c71`.
  Semantic preflight 10244 and direct runtime gate 2534 pass (7/7 tests).
  Independent adversarial review is in progress. The tests call the actual extracted
  producer, sweep allocation failures from an empty publication prefix with
  same-instance retry, and roll back a second real base publication while
  preserving the first under an outer local transaction. Snapshot equality
  includes exact private registration/draft rows and consumed phase. This
  intentionally partial base-only seam does not assert terminal full-owner
  admission. The old lower-level copy test remains in the regression matrix.

- Four additional zero-capacity assertions pin the actual base-plan and
  registration allocation sites on calibration and every injected-failure
  fixture. At freeze `14737117`, Check hash
  `369af7429d9d3c07b94165e7276ba33f4fee75af505d534dffd6882cd5818641`,
  semantic preflight 51327 and the targeted atomicity runtime 98803 pass
  (2/2 tests). Normal combined gate 72381 passes 43/43 tests and 41/41 steps:
  canonicalization 9, types 10, checker 19, compile/serde 5, regenerated
  Builtins, shared-helper regressions, measured schema-85 golden, and
  native/wasm serialization sizes. Independent adversarial review gives final
  bounded ACCEPT. No compiler behavior changed after the earlier 7/7 direct
  run. The driver is describing and publishing `onmovlvr` as `WIP: verify
  atomic record-update base-plan publication`, then continuing with real
  checked-base outer-retirement completion allocation/retry coverage. The
  actual finished-code review, distinct from its earlier planning audit,
  inspected the entire delta and the four capacity assertions. No checked-
  error/R_U OOM, complete-owner/fresh-inverse/active-serde/eligibility, R2/R3,
  call-formal or whole-W6b acceptance is claimed. Local artifacts include
  `base-producer-final-regression.txt`,
  `base-producer-checkpoint-description.txt`, and
  `pr-body-base-producer-checkpoint.md` in the artifact directory above.

- Publication of `onmovlvr` is complete: Git commit
  `92741c582452e8a3136cd95bce9264c47321fa87` was fast-forwarded from `10f613d6`
  to `jared/polarity`. Draft PR #10434 has that exact head and exact updated
  body, with draft status preserved; readback is saved as
  `base-producer-pr-readback.json` in the artifact directory above. The driver
  opened clean child `mpqnvuvqoonktrzzuyrytxrnpytpxyml` for actual central R_U
  completion allocation/retry tests. The same sol-ultra author and independent
  sol-ultra planning auditor were explicitly dispatched with `followup_task`;
  the finished-code review will be separately dispatched after a code freeze.
  This avoids confusing a completed planning turn with a running code review.
  The next fixture must use real pre-poison checking and the real leaf sweep
  to complete R_B, leaving the outer record and pending R_U intact. Create the
  real outer diagnostic before the failing-allocator window, then exercise
  `replaceExprWithRuntimeError` itself across every allocation, including
  preflight consumer growth and transactional subtree invalidation. Verify
  exact no-op on failure and same-instance completion followed by the real
  checker tail and distinct fresh admission. Capture both durable/private
  transaction state and affected invalidation metadata; no test-side repairs,
  fabricated rows or weakened terminal admission. Checked-error base
  publication and R_U reservation OOM remain later bounded tasks.

- The independent completion planning audit finds three demanded allocation
  families in the real central path: the retired-consumer suffix, offside
  descendant discovery, and durable invalidation membership. Discovery starts
  with the outer record's children, not the outer record itself; selected-root
  bodies are not cloned or mutated by this operation. The test must pin fresh
  consumer and invalidation capacity and supplement the existing transaction
  snapshot with invalidation metadata, exact node tags/payloads and diagnostics.
  The author is first building the authentic calibration-success seam before
  the exhaustive failure/retry loop. This is planning evidence only: no new
  test result or finished-code acceptance is claimed.

- The early calibration seam passes semantic retry 30906 after two test-local
  shadow-name fixes. Direct runtime 58903 passes 7/8 tests: only the new fixture
  fails at pre-completion staging. Focused diagnostic 73906 pins the failure to
  direct-binder/owner identity checking, before the copy-root assertions. The
  fixture called a terminal lookup proof that requires the upstream annotation
  retirement to have completed; this intentional seam has completed only the
  base lookup R_B. The author is correcting the test to authenticate that
  intermediate R_B through the existing phase-appropriate identity checks,
  retaining terminal proof after the unmodified full tail. This is a test
  staging correction, not authority fabrication or weakened terminal admission.
  The exhaustive failure loop is deferred until calibration passes.

- Calibration now passes: focused runtime 92287 is 2/2 at freeze `05ad7608`,
  Check SHA-256
  `50f2882be3126abe8c1721a94ff4477181601bd0620ec8747259a2b4f9fae64a`.
  Two further targeted failures exposed test-phase assumptions: the historical
  selected root was compared to its later union-find representative, and a
  full canonical proof validator was invoked before rebuilding. The fixture
  now preserves exact copy-time coordinates and checks their current
  equivalence; all full local/fresh validation remains after the real tail.
  No production semantics or validator were changed. Independent actual-code
  review accepts this calibration only. The author is now adding the composite
  state snapshot, exhaustive central-completion allocation failures and
  same-instance retry/terminal comparison; that loop is not yet certified.

- The full completion-only slice is frozen at `e6dd4e01`, Check SHA-256
  `4deaae392adea75c6fa700f40705ecf349dc04f487c822638c760f15743ab500`.
  Semantic 96831 passes, and direct runtime 35568 passes 9/9 tests, including
  the retained calibration and exhaustive actual central-completion OOM/retry
  test. The latter independently measures ordered child discovery and requires
  exactly two additional allocation sites for consumer and durable invalidation
  reserves. Every failure preserves logical transaction state, exact nodes,
  diagnostics, metadata and the still-live upstream annotation draft; retry on
  the same instance matches committed state and canonical terminal bytes after
  the real tail and distinct fresh admission. Final actual-code review accepts
  those mechanics but requests one explicit assertion that the sole field's
  literal plan is unresolved before finalization. That narrow addition and the
  normal producer-regenerated combined gate remain before publication.

- Final code freeze `e5d98d6f`, Check SHA-256
  `95bcdd179c3c6747415358ce2381fc50537c3b4de477d01c43b4451c578de537`,
  adds only the requested unresolved-literal assertion. The independent
  sol-ultra reviewer gives final bounded ACCEPT for the entire completion-only
  delta, including live R_A lifecycle coverage, exact immutable row oracles,
  allocation-site accounting, every OOM no-op, same-allocator retry, real-tail
  fresh admission and canonical bytes. Normal combined gate 11291 is running
  against that final source. Its result and publication are still pending;
  the earlier 9/9 direct gate is not substituted for producer regeneration.

- Normal combined gate 11291 passes **45/45 tests and 41/41 steps** on the
  final reviewed source: canonicalization 9, types 10, checker 21, compile/serde
  5, regenerated Builtins, shared-helper regressions, measured schema-85 golden
  and native/wasm serialization sizes. Formatting passes and compiler hashes
  match the accepted freeze. The driver is describing and publishing
  `mpqnvuvq` as `WIP: verify checked-base owner retirement completion atomicity`.
  Artifacts are `ru-completion-final-regression.txt`,
  `ru-completion-checkpoint-description.txt`, and
  `pr-body-ru-completion-checkpoint.md` in the artifact directory above.
  This completes the bounded central-completion allocation task only; checked-
  error base publication and R_U reservation OOM, broader R1 admission and
  active serialization, R2/R3, call-formal failures, option (e), runtime adapters
  and full W6b verification remain open.

- Publication of `mpqnvuvq` is complete: Git commit
  `9c2958760ad72b819f301f75e5146b7fc4f303e7` was fast-forwarded from `92741c58`
  to `jared/polarity`. PR #10434's exact head, full updated body and draft status
  were verified; `ru-completion-pr-readback.json` records that readback. The
  driver opened clean child `mwttkmrxotzsqxsmoyuunuxtrlyrlryl` and explicitly
  dispatched the same sol-ultra author plus independent sol-ultra planning
  reviewer for checked-error P_B publication allocation coverage. The new seam
  must retain one real Env and the actual base `checkExpr` return status, stop
  before `publishRecordUpdateBasePlan`, preserve authentic pending R_B and its
  upstream annotation authority, then exercise the shared producer transaction
  with exact rollback and same-instance retry. No fabricated/deleted authority,
  solved-state status reconstruction, or premature terminal validator is allowed.
  R_U reservation and the broader remaining work stay separate; no new-child
  verification is claimed yet.

- The checked-error P_B planning audit confirms that source-retirement binding
  itself is allocation-free: both live-pending and preexisting-malformed paths
  validate and reuse an existing R_B. The new sweep therefore targets the
  surrounding publication/copy transaction while preserving the authentic
  R_A/F_A to R_B/F_B recovery prefix. Its initial calibration uses actual
  annotated-lambda and record expression frames with one caller-owned Env,
  checks the base once, and publishes P_B while those frames remain live.
  This is an isolated producer seam, not a full checkFile/checkDef chronology;
  it does not claim predeclaration or terminal artifact admission. Exact new-
  copy and registration assertions must not inherit an inappropriate global
  canonical proof check from an earlier test helper. The stage is being
  implemented; no calibration or allocation-test result is claimed yet.

- The isolated checked-error P_B calibration is frozen at `7512d489`, Check
  SHA-256 `b4a56e583c755d286de9eb6686bdaf199a605cb3c2516f9b3facc27df12c661c`.
  Semantic 72226 passes on the first run. Focused runtime 85114 and a separately
  dispatched actual-code review are in progress. The driver paused the author
  at this coherent landed boundary for early feedback; the exhaustive snapshot
  and allocation-failure loop have not been implemented in this slice yet.

- Checked-error P_B calibration runtime 85114 passes 2/2 on that same source.
  The driver resumed the author for exact recovery/context snapshots,
  exhaustive measured publication failures, same-instance retry and a bounded
  outer LocalMarkerCopyTransaction rollback case. The actual base check's
  returned cause and live frame/Env context are retained throughout. The normal
  gate for this task will additionally run the existing direct-binder source,
  lookup-publication and lookup-completion transaction tests affected by common
  test-site helper generalization. No full-slice acceptance is claimed yet.

- The actual calibration review gives bounded ACCEPT at `7512d489` /
  `b4a56e58`. The full checked-error P_B slice is now frozen at `ab828f96`,
  Check SHA-256
  `9c78317fe05471165d62b98cb135df2b733d285df752610328287d9cc93ade12`.
  Semantic 26688 passes; direct runtime 1659 and a separately dispatched
  full-delta adversarial review are running. The exhaustive fixtures construct
  their stable Env using the same disabled FailingAllocator subsequently used
  for publication, so retained VarPool allocation sites remain in the sweep.
  Snapshots pin the actual recovery prefix, durable/private transaction state,
  Env and checker context, source nodes, value lookup and error bits. A separate
  outer LocalMarkerCopyTransaction test checks rollback over that nonempty
  prefix and same-instance retry. Publication and full-slice acceptance remain
  pending; no terminal artifact or complete R1 acceptance is claimed.

- Full checked-error P_B direct runtime 1659 passes 12/12 on the frozen
  `9c78317f` source, including all three new tests and the preceding owner,
  established-publication and completion-allocation controls. The normal
  producer-regenerated combined gate 69757 is running with the three added
  direct-binder helper regressions. Independent full-delta review and
  publication remain pending.

- Actual full-delta review identified an oracle robustness issue: allocator
  handle identity belongs only in same-instance rollback, not the semantic
  comparison between calibration and failure fixtures. Current TestEnv uses
  the shared testing allocator, so the passing runtime did not expose this
  coupling. The driver stopped normal gate 69757 with exit 130 before editing;
  it has no completed gate result. The author is narrowly thawed to move those
  three handle checks to `expectSameInstanceRollback`, followed by a fresh
  source freeze, validation and review.

- The sole review correction is frozen at `69a8ab9d`, Check SHA-256
  `a451728dbc9173074f847d3a4055854a2a6f0db2e971a66de0ad900d36f9982b`.
  Formatting is unchanged. All three allocator-handle comparisons now live
  only in the same-instance rollback oracle; semantic cross-fixture equality
  remains independent of allocation identity. Semantic 21251 is running, and
  the reviewer has the exact new hash for the final full-delta verdict.

- Semantic 21251 passes on `a451728d`. The independent full-delta adversarial
  reviewer gives final bounded static ACCEPT for `69a8ab9d` and that exact
  source hash, including the allocator-oracle correction. Review accepts the
  authentic retained status and live frames, exhaustive measured allocation
  sweep, full logical rollback with same-instance ownership checks, retry
  equivalence and outer local-transaction prefix preservation. Normal combined
  gate 6063 is running on the final source. This verdict does not claim that
  gate passed, full checkDef/predeclaration fidelity, R_U reservation, terminal
  R1 admission or option-(e) completion. Both agents are independently planning
  a subsequent reservation-only slice without editing the frozen source.

- The final normal gate 6063 passes 51/51 tests and 41/41 steps on the exact
  independently reviewed `a451728d` compiler source: canonicalization 9,
  types 10, checker 27 and compile/serde 5. It regenerates Builtins and includes
  the three older direct-binder source/lookup/completion helper regressions,
  schema-85 cache golden, serialization and native/wasm sizes. Formatting and
  all four tracked compiler hashes match the freeze. The driver is preparing
  the full jj description, WIP push and exact draft-PR head/body readback.
  No production or schema behavior changed in this task. Next is a separately
  reviewed reservation-only test slice using the authentic unset-only
  pre-unset producer cut; no next-task source edits have begun.

The remaining option-(e) checker work has an audited integration route:
replace the target-wait boolean with a closed `none` / `target_def` /
`where_settlement` reason; wait for readiness of an entire deferred constraint
range before processing any member; drain after all owners in a recursive SCC
have finalized widening and before generalization. Inspect the exact selected
method's output paths immediately before target instantiation/unification in
nominal/alias selection and candidate/default compatibility. Candidate checks
must propagate waiting, not turn it into rejection. The four annotation-owner
finish paths need the post-finalization drain. Use the existing output-row
enumerator and guarded semantic paths; terminal `MethodOutputPublication` is
unavailable for same-SCC rejection. Add the dedicated diagnostic and rejected
dispatch recovery. The semantic check can be implemented independently, but
its durable attribution must use the planned normalized settlement events.

A read-only lowering audit also confirmed that `MethodOutputPublication` and
`ResultRowWideningUse` have no downstream consumers yet. Project exact selected
binding capability into method targets and exact per-use authorization into
call/iterator plans; shared evidence nodes are not per-use authority. Validate
authorization before any specialization or worker-cache lookup. Preserve the
planned one-adapter-per-`(template, requested type)` sharing: the audit did not
find a valid collision requiring per-use proof ids in cache keys. Generalize
direct/Try-ok/Try-err/Try-both relations and use request-derived payload types.
Preserve capture-aware local targets and explicit provenance through constant
storage/restoration. Local attached-method capture reachability is already
pinned by `eval_tests.zig`'s `generic dispatch preserves each capturing local
method context` fixture (`make` declares `Local.get` capturing `offset`), and
`static_dispatch_registry.zig` publishes its exact local binder, expression,
and declaration-context anchor. A widened closed-result variant must retain
that context. Boxy's `plan.zig` `dispatchMethodTargetLookup` still rejects
`local_proc` for direct dispatch (the ordinary method-worker helper already
supports nested-expression sources; that does not establish this direct-call
ingress). Both direct call and iterator analysis use the rejecting helper.
Thus nested-worker ingress is a concrete W6b prerequisite to verify and support,
not an unreachable-target assumption. No new runtime test of that variant has
run yet. Adapter-value reachability through ConstStore still needs concrete
verification. Test both
lowering strategies (`--specialize=yes` and `--specialize=no`) as well as both
interpreter/dev execution backends; testing execution backends alone does not
cover both lowerers. No lowering implementation or test run was performed by
that audit.

The three existing W6b CLI fixtures remain unregistered in the parallel CLI
runner. Their presence is not execution coverage. In particular,
`QuestionClosedImpl.roc` returns only `Ok("hit")`; its current assertion cannot
demonstrate re-tagging of an actual closed error-row value. Adapter acceptance
must add executed `Err` cases as well as the planned Try-ok/Try-err/Try-both
width matrix, and register the cases for both lowerers and execution backends.

Transport producer coordinates reconfirmed on `6e869f7f`: in
`static_dispatch_registry.zig`, `MethodRegistry.build` still has the exact
`entry.value` MethodBinding when it creates `MethodTarget`, before discarding
the binding's type-node coordinate. `StaticDispatchPlanTable` construction
still has each raw `constraint_fn_var` when it creates `StaticDispatchCallPlan`;
iterator calls have their separate plan records. These are the projection
points for implementation capability and per-use authorization, respectively.
`checked_artifact.zig` still publishes only `hosted_try_adapter` on hosted
procedure templates. Monotype's `instantiateTargetFromPlanNode` and
`methodTargetNodeFromPlan` still apply exact interface relations, and template
completion adapts only the hosted arm. The new normative Result-Row Widening
Adapter section remains to be written before that implementation; today's
Polarity lowering note is forward-looking. No downstream code was changed by
this reconfirmation.

### 8.2 The working agreement Jared set (binding)

- This session's driver owned jj; subagents never ran state-changing jj
  commands. One scoped commit per item, created with `jj new -m` BEFORE
  the first edit, described up front, finalized with `jj describe` when
  done. Never merge; rebase only.
- Per item: one implementer, then one adversarial reviewer on the diff,
  then the implementer applies the review's fixes, then finalize. The
  reviewer does not edit source.
- Continue routine implementation and proof-design decisions autonomously.
  Ask Jared only if an implementer or reviewer finds a typing or lowering
  policy not covered by the accepted plan. The later autonomy instruction
  supersedes the original session's stop-for-every-unanticipated-finding rule.
- Production quality, not a prototype; long-term compiler health over a
  local optimum. Every behaviour change is a declared rule in
  `design.md` and is pinned by tests at each level it touches.
- Jared's answers so far are recorded in §7 and in each item's "Landed"
  note. Question 3 is closed: Jared chose option (e) and explicitly gave the
  driver the green light to continue the planned work autonomously. Ask only
  if implementation exposes a typing or lowering policy not covered by this plan.

### 8.3 What is next, in order

W6a is implemented and its focused verification is complete. Finish W6b,
complete independent review, update the docs and publish, then pause for Jared
before W2b (also owns the optional-field stored-codec fixtures, Appendix A).
The remaining order is W2b → W7 → W8. Each
section above is the specification; the "Landed" notes on W2a/W3/W4
show the level of detail expected in a commit and what the reviewers
looked for. Verification matrix in §4.

Facts that were only in the lost scratchpad and matter for W6:
- Where-method widening lowered for OPEN implementations in the original
  diagnosis through a same-name match. W6a removes that route. The deliberate
  route is the exact raw `SchemeUseRecord.where_method_use` plus any serialized
  `GeneralizedDispatchTargetShare` needed when generalized dominance omitted
  the callable named by the plan. A share never comes from a solved-class scan
  or a method-name match.
- A shape-only share synthesizes nested evidence; an exact raw where-use share
  reuses the retained slot's nested evidence. `requires_record` needs the latter
  and is pinned by the direct Monotype accepted/rejected gate plus the combined
  W6a LIR fixture.
- In generalized deduplication, `deferred_generated_codec = true` dominates
  `false` in either encounter order; choose the final representative before
  emitting any directed share edge.
- Focused verification: `zig build run-test-zig-module-check --summary
  all -- --test-filter "scheme use"` passed 7/7 steps and 2/2 tests. Putting
  `--summary all` after `--` is rejected by the test runner, so the literal
  trailing-option form is not a valid verification command. The direct
  producer/lifecycle/codec/relation/roundtrip/cache gates—including cold/warm
  preservation of an exact where-use target-share row—accepted/rejected
  cross-module codec gate, Monotype `requires_record` gate, and combined W6a LIR
  regression are all green; see W6a's verification paragraph for counts.
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
  `--ignore-working-copy` shows the LAST SNAPSHOT: ask the driver to snapshot
  before diff-based review. Subagents do not run plain `jj status` either.
- Builds are slow (`zig build roc` and full suites can exceed 10
  minutes). Coordinate one long Zig build at a time on stable source. Use a
  foreground execution session and poll it in waits of at most 60 seconds;
  keep the driver informed. Do not edit while a build is consuming source,
  and do not end a task with a build still running. Report only actual results.
  A sandbox `PermissionDenied` is not disk exhaustion: retry the unchanged
  command through the approval mechanism. Only an actual disk-use error
  authorizes the driver to remove the exact workspace `.zig-cache` directory.
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
