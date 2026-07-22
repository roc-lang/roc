# reunify Slice 0 — measurement and audit report

Companion to `reunify.md` §13 (Slice 0). Records what the declared
measurements and audits found, and the disposition of each finding as
**publication gap** / **algebra rule** / **deletable bookkeeping**.

Status: static audits complete; Debug census instrumentation landed; corpus
census numbers below.

---

## 1. Static audits

### 1.1 Head-multiplicity canonicalization (reunify.md §8.4)

**Verdict: already total at translation, for both records and tag unions.**
The Monotype node has exactly one record head and one tag head
(`monotype/type.zig:210`); the checked-side multiplicity
(`record`+`empty_record`-ext vs `record_unbound` vs `empty_record`, and the
tag-union equivalents) collapses in the translation funnel: both the direct
route (`lowerType`/`lowerRecordRow`/`lowerTagUnionRow`) and the graph route
(`instNodeContent` → `GraphTypeFinals.sealContent` → `flatten*Row`) end in
`addRecordFields`/`addTagVariants` (`monotype/type.zig:338/:356`), which sort
by label and normalize empties to `Span.empty()`. Store-wide `verify()`
enforces sortedness; digest and equality walk span content, not offsets.

**Gap for interning:** none specific to head multiplicity. The residual
difference between equal-shape encodings is span-offset allocation identity —
exactly what content-addressed interning removes. Slice 3 work item: route
construction through the interner and add an invariant test that
`empty_record`, `record_unbound{[]}`, and `record{[], ext=empty_record}`
intern to one id (and the tag-union equivalents).

### 1.2 Nominal construction/destruction totality (reunify.md §6.1, §10.3, §10.5)

**Verdict: backing access is total over the declared edge kinds with exactly
one exception.** Sanctioned and verified: construction edges
(`nominalConstructionLayer` + constructor expr/pattern wrapping), destruction
(pattern) edges, inspection edges (field access through backing, checked
inspect authority, iterator `.next`/`try` results, serialization-schema
`backing.use`), runtime-layout/representation authority (durable backing on
the node, generated-opaque-evidence backing reads, seal-time backing,
iterator minted-join backing, nominal-backing node cache), plus alias
transparency as a separate operation.

**The one finding:** `unifyThroughBacking`'s nominal branch
(`monotype/solve.zig:938–947`), reached from ~13 `unifyConcrete` arms when a
structural head meets a `.named` nominal — the generic
"try-the-backing-on-head-mismatch" path that reunify.md §10.5 bans from the
representation algebra.
**Disposition: deletable bookkeeping** — it is intrinsic to the logical
`InstGraph` unifier deleted in Slice 7 and must not be ported into the
Slice 4 descriptor policy, where nominal-backing relation is a distinct API
gated on construction/destruction/inspection/layout edges. The census
counter `nominal_backing_root_join` quantifies production firing frequency.

### 1.3 TypeId-keyed consumer classification (reunify.md §8.5, risk §15.5)

**Verdict: one live occurrence-identity coupling, already scheduled.**

| map | meaning | status |
|---|---|---|
| `lambda_solved/solve.zig:1929` `TypeCloner.map` | occurrence | BREAKS under interning; fixed by Slice 1 before interning |
| `monotype/solve.zig:237/:234` `mono_nodes`/`node_monos` | occurrence (graph views) | deleted with refill API before interning (Slice 3 sequencing) |
| `monotype/solve.zig:226` + `lower.zig:628` `unsolved_monos` | provenance flag | deleted with graph (Slice 7) |
| interner `by_digest`, seal memos, visited guards, transform memos, `evidence_cache` | structural | safe |
| specialization store | digest-keyed (`SpecLookupAddress`), not TypeId-keyed | immune |
| monotype_lifted / lambda_mono / solved_lir_lower / lir / layout | own id spaces (lambda_mono ids, LocalIds, FnIds) | immune |

No ARC, snapshot, or generated-symbol-naming map keys on Monotype TypeId.
This confirms §15.5's expectation: lambda callable slots are the one known
case; no additional hidden occurrence coupling exists.

**Slice 3 addenda from the audit:** retire the `evidence_cache`
digest-generation/`type_epoch` invalidation guards once the store is
immutable (they exist only for mutable-view refill); record the
specialization store and downstream stages as "verified not coupled".

### 1.4 Lambda Solved census re-verification (reunify.md §12.4 item 5)

All eleven census claims verified against `lambda_solved/solve.zig` (exact
sites now pinned by `ci/check_reunify_manifest.pl`, category
`lambda-solved-census`). Census corrections found and adopted:

- `unifyIteratorOwnerStampedPublic` is gated by owner-stamp identity
  (`sameMonoTypeDef` + `builtin_owner` inequality), not by
  `MonoType.iteratorRelation` — only three of the four iterator joins sit
  under the shared relation.
- Two distinct scoring functions back score selection:
  `generatedBackingScore` (pattern side) and `generatedOpaqueEvidenceScore`
  (expression side, which gates then wraps the former).
- Two additional census entries the earlier draft missed:
  `expectGeneratedIteratorBackingExpr`/`expectExprAtTypeEvenIfDone` (the
  expression-walk minted-iterator backing re-solve — a third iterator-backing
  mechanism) and `structuralBackingForNamed` (clone-time structural backing
  selection, with the generated-opaque-evidence raw-backing exception).
- Seam-assertion note for Slice 1: `.named` left-wins linking discards the
  loser's `declared_order`/`named_type` without agreement checks (layout-only
  fields); the §12.6 seam assertion must exempt them explicitly.

---

## 2. Debug census over the corpora

Counters live in `monotype/census.zig` (env `ROC_REUNIFY_CENSUS`) and
`check/reunify_census.zig` (env `ROC_REUNIFY_CHECK_CENSUS`). Corpora: the
full snapshot suite (2026-07-22, snapshot regeneration left every tracked
snapshot byte-identical with census enabled) and the eval suite (1497
passed; the 2 crashes are the pre-existing issue-8754 recursive-tag cases
and reproduce identically without the census). Counters are cumulative per
process; values are the per-run maxima. The eval runner uses 24 worker
processes appending to one file, so eval values are per-process maxima —
order-of-magnitude and existence evidence, not exact totals.

| counter | snapshots | eval |
|---|---|---|
| deferred requests sealed with changed type id | 23308 | 496 |
| ... of which direct-recursive | 0 | 0 |
| request_refined (refineRequest calls) | 20317 | 246 |
| request_refined_digest_changed | **8** | 1 |
| solved_digest_differs_from_request | **8** | 14 |
| import_tag_ext_kept_open | 37438 | 12052 |
| import_record_ext_kept_open | 53785 | 19669 |
| **import_ext_widened** | **0** | **0** |
| two_sided_tag_row_merge / record | **0 / 0** | **0 / 0** |
| one_sided_tag_row_merge / record | 0 / 0 | 44 / 4 |
| expected_return_constraint_bound | 17 | 8 |
| empty_tag_union_yield | **0** | **0** |
| plain_variable_to_empty_tag_union | 776 | 26 |
| nominal_backing_root_join | 8438 | 416 |
| iter_public_minted | 1118 | 1126 |
| iter_forced_dynamic | 0 | 70 |
| iter_minted_join | 0 | 4 |
| numeric_default_applied | 88 | 16 |
| row_default_applied | 1753 | 2566 |
| generated_opaque_evidence_gate | 21513 | 398 |
| builtin_owned_alias_created | **0** | **0** |
| lambda_alias_unwrap_builtin_owned | **0** | **0** |
| lambda_generated_backing_equal_score | **0** | **0** |

Checking census (full snapshot corpus, final run):
`scheme_use_duplicate_edges=41303`, `scheme_use_duplicates_equivalent=41303`,
`scheme_use_duplicates_divergent=0` under binder-aligned comparison (see
disposition 10 — the 2 initially-divergent cases were pair-order noise),
`err_reachable_in_lowerable_module=0`. Duplicates occur only under REPL
accumulative re-checking; all non-REPL directories measure zero.

Notes on counter semantics: "sealed with changed type id" fires whenever
sealing maps a deferred request's graph-era type id to a different final id,
which is near-universal; the digest-level signals are the two
digest-changed counters.

---

## 3. Dispositions (per reunify.md §13 Slice 0)

1. **Request-vs-solved digest differences are rare and real** (8 per full
   snapshot corpus, ≤14 per eval process). The counterexamples exist, so the
   requested/solved distinction cannot be deleted untested; each surviving
   case must be classified during Slices 5–6 shadow comparison. Until then:
   machinery stays. Expected home per §6.7: **publication gap** (fuller
   checked rows / published actuals) or **deletable bookkeeping**; no case
   may become an algebra rule without a genuinely postcheck-created
   representation input.
2. **Row solving is not two-sided in practice**: two-sided merges never
   fire; import extensions are kept open ~90k times but never widened.
   Disposition: **deletable bookkeeping** (the openness machinery defends
   against a flow that does not occur); the one-sided merges (44+4, eval
   only) are directed closure of a checked row against ground content —
   replaced by directed substitution over published rows.
3. **Expected-return back-constraints bind real variables** (17 + 8).
   Disposition: **publication gap** — §7.2's published actuals cover this by
   construction (checking saw the whole relation); these counts prove the
   argument-only matcher rejected in §9.5 would have been wrong.
4. **Empty-tag-union yield never fires**; unconstrained variables do
   materialize as empty tag unions (776 + 26). Disposition: the yield path
   is **deletable bookkeeping**; the materializations are §7.4 residual
   dispositions (checking must classify each as bottom or contextual —
   Slice 2's phase-one recording, with the semantically-bottom proof
   outstanding).
5. **Nominal backing joins fire heavily** (8438 + 416) through the one
   generic mismatch path (§1.2 above). Disposition: **deletable
   bookkeeping** mechanically, but the *frequency* proves the replacement
   must handle nominal-meets-structural by design: the §10.3 nominal
   wrapper with explicit backing edges, never a peer join.
6. **Iterator relations are live in all three non-ordinary arms**
   (public_minted, forced_dynamic, minted_join). Disposition: **algebra
   rules** (§10.3), as planned.
7. **Generated-evidence equal-score ties never occur in the corpora**, so
   today's traversal-order tie-break is unexercised. Disposition: **algebra
   rule** with the declared deterministic tie-break; no behavior to
   preserve.
8. **Builtin-owned aliases are never created in production corpora** and
   never reach the Lambda Solved unwrap. Disposition: keep §8.2's declared
   three-way alias split (the form is still constructible), but Slice 3 can
   treat the builtin-owned-alias story as a low-risk edge with direct unit
   tests rather than corpus-driven risk.
9. **Numeric/row defaulting applications are common** (88+16 / 1753+2566)
   and already route through checked evidence. Disposition: directed
   application under §7.4 — no probing observed, none permitted.
10. **Scheme-use duplicate records: all equivalent as mappings; recorded
    pair order is nondeterministic.** The initial position-wise comparison
    reported 2 of 36271 duplicates divergent; investigation (localized to
    `test/snapshots/repl/dict_nested_key.md`, `dispatch_target` records of
    the same raw constraint fn var re-checked across REPL passes) showed
    both records contain the identical five (binder → actual) pairs —
    {Dec, Dec eq, Str, Str eq, Str hash} — in different stored order.
    Re-comparing with binder-aligned, order-insensitive digests: zero
    divergent duplicates. Two conclusions feed Slice 2 directly: (a) the
    exactly-one-equivalent-record invariant is achievable — every corpus
    duplicate is equivalent as a mapping, so no first/last-write policy is
    needed; (b) today's recorded pair order is hash-map iteration order
    and varies between re-checks of one edge, confirming §7.2's
    deterministic-projection discipline (walk `scheme.binders` in binder
    order; never trust record order) is mandatory, not stylistic.
    Duplicates only occur at all under REPL accumulative re-checking —
    every non-REPL corpus directory measures zero duplicate edges.
11. **No lowerable module reaches `.err`** across both corpora — the §5.4
    contract holds; Slice 2's boundary verifier can assert it as a hard
    invariant.
