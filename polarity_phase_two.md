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
fallback intended for a different case. Use the scheme-use mechanism that
exists rather than a parallel pair table: a body use is a scheme
instantiation (`instantiateWhereMethodForUse` ~6198 copies the signature's
structure over shared leaves), and `SchemeUseRecord` (`ModuleEnv.zig` ~685)
already models one — a slot kind, a `slot_data` unique per constraint
instantiation (the use's constraint fn var, as `dispatch_target` does), the
scheme root (the where-clause signature var), and copy pairs. Add
`Slot.where_method_use`; record it in `checkStaticDispatchConstraints`'
rigid branch (~29961) when a copy is minted, pairs from the instantiator's
`var_map`; have `paramIndexFor` (~17512) resolve the plan's
`constraint_fn_var` through that record to the signature root before
matching and produce `evidence_dependent{ independent_callable = true }` by
rule; the same-name fallback becomes an invariant. The evidence pass's
walks over `scheme_uses` (`emitSchemeUseSiteEvidence` ~18469,
`evidenceRefsForRecord`) skip the new slot: a body use has no obligations
of its own. Old-branch lessons: keyed by the two explicit vars (use fn var,
signature fn var), never by name; `evidenceNodeForTarget` memoization is
unaffected because per-use copies never reach a `record_idx`. Update the
`independent_callable` doc (`static_dispatch_registry.zig` ~1502), which
today describes only the shared-slot case.

Risk, to be settled in this item: an independent callable's nested evidence
is `.synthesize`, which lowering maps to `.from_callable`
(`appendConstFnEvidence` ~4346). An implementation whose evidence schema is
`requires_record` (`procedureEvidenceSchemaFromSlices` ~17963: a param
sourced from `constraint_callable`, `use_site_only`, `erased_row_remainder`,
or a pathed `explicit_default`) cannot be synthesized from the callable.
W6a either reuses the obligation's `.resolved` vector for target identity
and nested evidence while keeping the callable independent, or pins such
targets unreachable with an invariant and a fixture. Tests: the scratch
programs from the investigation as `lir_inline`/CLI fixtures (widen; widen
with a tag that sorts between `Err`/`Ok` to prove the implementation is
specialized at the wider row; `?` into a wider row; one closing use and one
widening use; an implementation with its own where-clause, exercising
`.synthesize` nested evidence; and an implementation with a
`requires_record` schema, constructed from one of the sources above).

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
possible; Jared decided (e) on 2026-09-03 (open question 3): (d) per-use opening is
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
(W2b). Nothing in this plan is a Rewrite Inventory entry: that inventory
classifies solver-mutating rewrites in checking, and `groundRowDefaults`
(deleted by W2b) and the adapter are Monotype mechanisms declared in the
Monotype sections. If W6b's nested-position decision adds a checker
rejection, that rule is declared in the Polarity section. `kupupkyt`
already bumped the checked-artifact cache version (`CACHE_VERSION` 72 → 73,
`src/compile/cache_config.zig`) because published `row_default`s and
weak-value grounding changed the artifact's meaning; W6a's new
`SchemeUseRecord` slot kind and any W3 checker-published flag change the
artifact again and bump it once more. Refresh the PR description's
verification section.

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

Bookmark `jared/polarity` still points at the plan commit `wlzsxolu`;
nothing after the original plan commit has been pushed. To publish: move
the bookmark to the top (`jj bookmark set jared/polarity -r vrpryvko`,
`--allow-backwards` is not needed for a forward move) and `jj git push`
— only at Jared's explicit direction. Then refresh the PR description
(W7).

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
  note. Open question 3 (W6b nested positions) is still open; the
  driver's recommendation to Jared was option (e). Ask before W6b.

### 8.3 What is next, in order

W6a → W6b (needs the §7 question 3 answer first) → W2b (also owns the
optional-field stored-codec fixtures, Appendix A) → W7 → W8. Each
section above is the specification; the "Landed" notes on W2a/W3/W4
show the level of detail expected in a commit and what the reviewers
looked for. Verification matrix in §4.

Facts that were only in the lost scratchpad and matter for W6:
- Where-method widening ALREADY lowers for OPEN implementations: the
  artifact plan carries the use's copy as `callable_ty`, `paramIndexFor`
  cannot match the copy's fresh root to the where-clause var, falls back
  to a same-name match with `independent_callable = true`, and Monotype
  instantiates the implementation's scheme against the use's callable.
  Verified on both backends with the Appendix B programs marked "passes".
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
