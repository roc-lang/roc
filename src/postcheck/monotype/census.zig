//! Debug-only counters for what the Monotype instantiation graph decides.
//!
//! Slice 0 measures the current post-check meaning before any of it moves.
//! Each counter answers one classification question about the graph's work:
//! symmetric and directional row merges, import extension widening, checked
//! defaulting, deferred procedure requests, iterator relations, request
//! refinement, backed aliases owned by a builtin, and the Lambda Solved
//! score ties. Every counter is inert outside Debug builds, so release
//! lowering keeps its exact behavior.

const std = @import("std");
const builtin = @import("builtin");

/// All counting and the dump are compiled out unless this is a Debug build
/// on a 64-bit non-wasm target: the counters are 64-bit atomics and the dump
/// reads an env var, neither of which the wasm builds support.
pub const enabled = builtin.mode == .Debug and
    !builtin.target.cpu.arch.isWasm() and
    builtin.target.ptrBitWidth() >= 64 and
    builtin.os.tag != .freestanding;

const Counter = std.atomic.Value(u64);

/// One atomic u64 per classification question. Each field name is the text
/// the dump writes on its line, so a corpus run reads the names directly.
pub const Census = struct {
    two_sided_tag_row_merge: Counter = Counter.init(0),
    two_sided_record_row_merge: Counter = Counter.init(0),
    one_sided_tag_row_merge: Counter = Counter.init(0),
    one_sided_record_row_merge: Counter = Counter.init(0),
    import_tag_ext_kept_open: Counter = Counter.init(0),
    import_record_ext_kept_open: Counter = Counter.init(0),
    import_ext_widened: Counter = Counter.init(0),
    plain_variable_to_empty_tag_union: Counter = Counter.init(0),
    empty_tag_union_yield: Counter = Counter.init(0),
    nominal_backing_root_join: Counter = Counter.init(0),
    iter_public_minted: Counter = Counter.init(0),
    iter_forced_dynamic: Counter = Counter.init(0),
    iter_minted_join: Counter = Counter.init(0),
    numeric_default_applied: Counter = Counter.init(0),
    row_default_applied: Counter = Counter.init(0),
    expected_return_constraint_bound: Counter = Counter.init(0),
    deferred_request_sealed_shape_changed: Counter = Counter.init(0),
    deferred_request_recursive: Counter = Counter.init(0),
    deferred_request_nonrecursive: Counter = Counter.init(0),
    generated_opaque_evidence_gate: Counter = Counter.init(0),
    request_refined: Counter = Counter.init(0),
    request_refined_digest_changed: Counter = Counter.init(0),
    solved_digest_differs_from_request: Counter = Counter.init(0),
    builtin_owned_alias_created: Counter = Counter.init(0),
    lambda_alias_unwrap_builtin_owned: Counter = Counter.init(0),
    lambda_generated_backing_equal_score: Counter = Counter.init(0),
    // reunify.md 7.1, Slice 2: how a procedure binding's source scheme root was
    // resolved. `by_id` is the dense scheme id carried on the binding; `by_content_digest`
    // is the content-key lookup used only when the binding stored no id.
    scheme_lookup_by_id: Counter = Counter.init(0),
    scheme_lookup_by_content_digest: Counter = Counter.init(0),
    // reunify.md 8.1, Slice 3: how often a type digest walk exhausts its fixed
    // visiting stack and digests the content shape instead of recursing. The
    // count measures whether any corpus type is deep enough to reach the cap;
    // the shape digest uses the content variant, never an allocation id.
    digest_stack_depth_exceeded: Counter = Counter.init(0),
    // reunify.md 8.1, Slice 3: interner outcomes. `intern_hit` reuses an existing
    // id after an exact-equality bucket match; `intern_miss` adds a fresh id.
    intern_hit: Counter = Counter.init(0),
    intern_miss: Counter = Counter.init(0),
    // reunify.md section 9, Slice 7 Stage A: the directed stored-form translation
    // probe. `direct_probe_population` counts every distinct root the probe
    // translates — the widened population is every type the graph seals into the
    // program per specialization (the GraphTypeFinals commit path, deduped by
    // sealed id), not only the concrete checked roots lowering translated.
    //
    // For each root the directed translation's stored digest is compared with the
    // graph's stored digest. `match` counts equal stored digests; `mismatch`
    // counts unequal, split by whether the graph type carries iterator/generated
    // representation content (`mismatch_representation`, bounded by what Stage B's
    // engine must supply) or does not (`mismatch_logical`, which must be zero — an
    // unequal representation-free stored form is a translation bug).
    //
    // `mismatch_representation` plus `skip_engine_input_needed` together bound the
    // representation content step (b)'s closure engine must supply: the first is a
    // type translated with derivable content that still differs by graph-minted
    // representation; the second is a position whose content the checked data
    // cannot dictate at all, skipped instead of emitting wrong output.
    //
    // The remaining skip counters record roots outside the translatable subset,
    // one per direct_translate.SkipReason. `skip_recursive` should stay near zero
    // now that recursive groups are built through the store's recursive-group
    // builder; a nonzero count is a recursion the builder could not close.
    direct_probe_population: Counter = Counter.init(0),
    direct_stored_match: Counter = Counter.init(0),
    direct_stored_mismatch: Counter = Counter.init(0),
    direct_stored_mismatch_representation: Counter = Counter.init(0),
    direct_stored_mismatch_logical: Counter = Counter.init(0),
    direct_stored_skip_recursive: Counter = Counter.init(0),
    direct_stored_skip_open_row: Counter = Counter.init(0),
    direct_stored_skip_engine_input_needed: Counter = Counter.init(0),
    direct_stored_skip_pending_or_err: Counter = Counter.init(0),
    direct_stored_skip_numeric_default: Counter = Counter.init(0),
    direct_stored_skip_malformed_arity: Counter = Counter.init(0),
    direct_stored_skip_binder_not_found: Counter = Counter.init(0),
    direct_stored_skip_missing_backing: Counter = Counter.init(0),
    // A widened-population entry the graph sealed from an instantiated scheme:
    // its `named_type` provenance is the scheme template node (carrying rigid
    // binders), not a concrete checked instance, so a ground directed translation
    // cannot reproduce it — the dense scheme binding is exactly what step (b)'s
    // directed instantiation supplies. Counted rather than compared, so a
    // template's defaulted ground shape never registers as a logical mismatch.
    direct_stored_skip_uninstantiated_template: Counter = Counter.init(0),
    // The subset of the uninstantiated-template skips whose sealed graph type
    // carries iterator/generated representation content. This is the step (b)
    // representation bound inside the population the ground probe cannot compare:
    // once directed instantiation supplies their dense scheme binding, the
    // section 10 closure engine must supply exactly this representation content.
    direct_stored_uninstantiated_carries_representation: Counter = Counter.init(0),
    // A widened-population sealed variant whose checked source IS a concrete
    // type_cache key (so its ground translation is faithful, proven by that key's
    // own authoritative comparison) but which the graph sealed to a distinct id
    // that differs logically: the seal used a different residual-disposition
    // context than the module-body-owner ground walk. It is not a translation bug
    // — the ground translation is faithful to its own context — so it is counted
    // here, keeping `mismatch_logical` reserved for the authoritative type_cache
    // comparison, which must stay zero.
    direct_stored_skip_context_variant: Counter = Counter.init(0),
    // reunify.md section 10, Slice 7 Stage B: the representation closure engine
    // driven from the graph as an inert shadow. Wherever the graph applies a
    // representation decision, the same relation is mirrored into engine slots,
    // sealed at the graph's seal point, and the engine's sealed representation
    // descriptor (tier/kind/depth/owner) is compared against the graph-sealed
    // node's representation content. `match` counts equal descriptors; `mismatch`
    // counts unequal — an engine rule gap the flip must not carry. The per-rule
    // counters split both by the section 10.3 rule that placed the slot in its
    // class. Every mismatch is measured, never a panic.
    representation_mirror_match: Counter = Counter.init(0),
    representation_mirror_mismatch: Counter = Counter.init(0),
    representation_mirror_match_public_minted: Counter = Counter.init(0),
    representation_mirror_mismatch_public_minted: Counter = Counter.init(0),
    representation_mirror_match_forced_dynamic: Counter = Counter.init(0),
    representation_mirror_mismatch_forced_dynamic: Counter = Counter.init(0),
    representation_mirror_match_minted_join: Counter = Counter.init(0),
    representation_mirror_mismatch_minted_join: Counter = Counter.init(0),
    representation_mirror_match_evidence: Counter = Counter.init(0),
    representation_mirror_mismatch_evidence: Counter = Counter.init(0),
    // A mirrored `relate` refused its operands as logically unequal. Because the
    // graph only relates logically-equal nodes at these sites, a rejection is a
    // mirror-side token or slot-shape imprecision, recorded rather than asserted.
    representation_mirror_relate_rejected: Counter = Counter.init(0),
    // The sanctioned nominal-backing relation (two equal-identity nominals whose
    // backings the graph relates) mirrored into the engine as a component
    // equality of the two nominal wrappers. Counts the applied relations.
    representation_mirror_nominal_backing_related: Counter = Counter.init(0),
    // The generic try-the-backing-on-head-mismatch path, reunify.md section
    // 10.5: dying bookkeeping the flip deletes, not a section 10.3 edge. It is
    // counted when it fires rather than mirrored into the engine.
    nominal_generic_mismatch_path_fired: Counter = Counter.init(0),
    // reunify.md section 11.1, Slice 7 Stage B: the interface reservation trial.
    // For each specialization the graph lowers, argument and result
    // representation slots are reserved in the mirror before body lowering;
    // `gained_info` counts positions whose representation tier moved up during
    // body discovery, and `gained_info_nonrecursive` restricts that to
    // specializations that made no recursive self-request (the section 11
    // openness measurement on live data, at slot granularity).
    interface_slots_reserved: Counter = Counter.init(0),
    interface_slots_gained_info: Counter = Counter.init(0),
    gained_info_nonrecursive: Counter = Counter.init(0),
    // reunify.md section 11.1/11.5, Slice 7 Stage C: the parallel FinalSpecId
    // computed on the production spec builder at `markReady`. `computed` counts a
    // record whose request type reduced to a FinalSpecId; `skipped` counts one
    // whose request left the representation-reducible subset (recursive, open
    // row, zero sized). The collision counters key by FinalSpecId: two records
    // sharing one are the same specialization and must reduce to structurally
    // equal solved skeletons — `equivalent` counts a matching repeat, `divergent`
    // a mismatch (a red flag the flip must not carry; expected zero),
    // `solved_skipped` a repeat whose solved witness could not be compared. This
    // is the production port of the shadow's `spec_seal_*`/`spec_collisions_*`
    // sealing census, driven on live records instead of a read-only post-pass.
    final_spec_id_computed: Counter = Counter.init(0),
    final_spec_id_skipped: Counter = Counter.init(0),
    final_spec_id_collisions_equivalent: Counter = Counter.init(0),
    final_spec_id_collisions_divergent: Counter = Counter.init(0),
    final_spec_id_collisions_solved_skipped: Counter = Counter.init(0),
    // reunify.md sections 9/10/11, Slice 7 flip-prep step (b): the
    // per-specialization rehearsal. For every specialization the graph lowers,
    // the rehearsal resolves that specialization's dense binder environment from
    // the requesting edge's instantiation site and emits, from checked data
    // alone, the type at every position the graph sealed.
    //
    // `spec_attempted` counts specializations reached; `spec_compared` counts
    // those whose environment resolved, and `env_resolved` is the same event seen
    // from the environment side. The `skip_*` counters name every way an edge did
    // not resolve, one class per cause, so an unresolved edge is never an
    // assumption: a root has no requesting edge (`root_edge`), a compiler-generated
    // request records none (`generated_edge`), and the rest are missing site,
    // ambiguous site, unresolved scheme, absent module, arity, an actual the
    // checker did not reach, and an actual outside the translatable subset.
    rehearsal_spec_attempted: Counter = Counter.init(0),
    rehearsal_spec_compared: Counter = Counter.init(0),
    rehearsal_env_resolved: Counter = Counter.init(0),
    // The subset of resolved environments whose scheme carries an EMPTY binder
    // vector: the site's arity agrees (zero actuals for zero binders), so the
    // environment resolves with nothing bound.
    rehearsal_env_resolved_without_binders: Counter = Counter.init(0),
    // The empty-binder population split by the question reunify.md 7.1 asks of
    // it: is the empty vector exact (the scheme is genuinely monomorphic, so its
    // root reaches no checked variable at all) or does the root reach one that no
    // binder names? The owner split says which owner kind an unnamed one belongs
    // to, and `imported` says whether the scheme was read out of another module's
    // checked data. `root_variable` is the only class 7.1 forbids.
    rehearsal_env_no_binders_root_ground: Counter = Counter.init(0),
    rehearsal_env_no_binders_root_variable: Counter = Counter.init(0),
    rehearsal_env_no_binders_owner_top_level: Counter = Counter.init(0),
    rehearsal_env_no_binders_owner_nested: Counter = Counter.init(0),
    rehearsal_env_no_binders_owner_required: Counter = Counter.init(0),
    rehearsal_env_no_binders_owner_synthetic: Counter = Counter.init(0),
    rehearsal_env_no_binders_snapshot_present: Counter = Counter.init(0),
    rehearsal_env_no_binders_snapshot_absent: Counter = Counter.init(0),
    rehearsal_env_no_binders_imported: Counter = Counter.init(0),
    // Which environments the unbound-residual mismatches actually come from:
    // an empty binder vector, or a populated one that still failed to name the
    // position's variable.
    rehearsal_unbound_residual_env_without_binders: Counter = Counter.init(0),
    rehearsal_unbound_residual_env_with_binders: Counter = Counter.init(0),
    // Why the free checked variable under a mismatching position was not bound:
    // it belongs to a different checked scheme, it carries a residual
    // disposition (and under which owner), it carries none at all, or the
    // position reaches no free variable and the empty tag union came from
    // somewhere else.
    rehearsal_unbound_other_scheme_binder: Counter = Counter.init(0),
    // The other-scheme-binder class split by whether that scheme's environment is
    // on this frame's lexical chain. `on_chain` means the level is present but its
    // own binding did not name the variable; `off_chain` means no checked
    // relation put that scheme on the chain at all — reunify.md 7.1 derives a
    // nested scheme's captured set from its ROOT, so a binder that only its body
    // reaches leaves no captured pair to link through.
    rehearsal_unbound_other_scheme_binder_on_chain: Counter = Counter.init(0),
    rehearsal_unbound_other_scheme_binder_off_chain: Counter = Counter.init(0),
    // The off-chain split by direction: `inside_frame` is a scheme whose checked
    // captured pairs name this frame's own scheme, so it is nested INSIDE the
    // specialized body and its binders are bound at its own use sites through its
    // own binder list (reunify.md 7.3) rather than by anything this frame carries;
    // `unrelated` is a scheme with no checked relation to this frame at all.
    rehearsal_unbound_binder_scheme_inside_frame: Counter = Counter.init(0),
    rehearsal_unbound_binder_scheme_unrelated: Counter = Counter.init(0),
    rehearsal_unbound_disposed_contextual: Counter = Counter.init(0),
    rehearsal_unbound_disposed_uninhabited: Counter = Counter.init(0),
    rehearsal_unbound_disposed_module_body: Counter = Counter.init(0),
    rehearsal_unbound_disposed_other_owner: Counter = Counter.init(0),
    rehearsal_unbound_undisposed: Counter = Counter.init(0),
    rehearsal_unbound_no_free_variable: Counter = Counter.init(0),
    // Whether the value a binder was bound to is itself a residual
    // materialization — the requesting edge's checked actual did not translate
    // to real content — split by whether the requesting body had an environment
    // of its own to resolve a symbolic actual under, and by whether the checked
    // actual is a bare variable or a structure containing one.
    rehearsal_actual_residual_with_caller_env: Counter = Counter.init(0),
    rehearsal_actual_residual_without_caller_env: Counter = Counter.init(0),
    rehearsal_actual_residual_bare_variable: Counter = Counter.init(0),
    rehearsal_actual_residual_structure: Counter = Counter.init(0),
    rehearsal_actual_residual_is_scheme_binder: Counter = Counter.init(0),
    // Whether the requesting body's environment already carried this actual as
    // one of its own binders — so the residual value was inherited from the
    // caller's binding rather than produced here.
    rehearsal_actual_residual_inherited: Counter = Counter.init(0),
    rehearsal_actual_residual_unbound_here: Counter = Counter.init(0),
    rehearsal_actual_residual_disposed_here: Counter = Counter.init(0),
    rehearsal_actual_residual_disposed_elsewhere: Counter = Counter.init(0),
    rehearsal_actual_residual_undisposed: Counter = Counter.init(0),
    // Whether a request carried the requesting body's own binding with it, and
    // when it did not, why: no active frame, an active frame whose own
    // environment never resolved, or one binding ids in another module.
    // `captured_chained` counts the captures that carried more than the
    // requesting body's own level, so the edge travels with the enclosing
    // environments a symbolic actual resolves through (reunify.md 7.3).
    rehearsal_caller_env_captured: Counter = Counter.init(0),
    rehearsal_caller_env_captured_chained: Counter = Counter.init(0),
    rehearsal_caller_env_no_frame: Counter = Counter.init(0),
    rehearsal_caller_env_frame_not_ready: Counter = Counter.init(0),
    rehearsal_caller_env_other_module: Counter = Counter.init(0),
    // reunify.md 7.1/9.1: one specialization's lexical parent link, built by
    // reading the caller's environment chain at the callee scheme's checked
    // `(outer scheme, binder index)` captured pairs. `parent_linked`
    // counts environments that gained at least one enclosing level;
    // `captured_binder` counts the pairs read, `bound` those a live caller level
    // supplied, and the rest name exactly why a pair supplied none: the checker
    // attributed the pair to no outer scheme, its outer scheme is not in the
    // defining store, its index is outside the outer scheme's binders, the outer
    // scheme is not on the caller's chain, or the active level and the checked
    // scheme disagree about which checked binder sits at that index.
    rehearsal_env_parent_linked: Counter = Counter.init(0),
    rehearsal_env_parent_absent: Counter = Counter.init(0),
    // The owner kind of every scheme a resolved environment binds. A scheme with
    // no enclosing scheme has nothing to link a lexical parent to, so this says
    // whether the corpus ever asks a nested scheme to bind under a caller: a
    // specialization whose callee scheme is a top-level owner can only ever
    // report `parent_absent`.
    rehearsal_env_owner_top_level: Counter = Counter.init(0),
    rehearsal_env_owner_nested: Counter = Counter.init(0),
    rehearsal_env_owner_required: Counter = Counter.init(0),
    rehearsal_env_owner_synthetic: Counter = Counter.init(0),
    // Whether the callee scheme a resolved environment binds carries any checked
    // captured binder at all.
    rehearsal_env_scheme_captures_present: Counter = Counter.init(0),
    rehearsal_env_scheme_captures_absent: Counter = Counter.init(0),
    rehearsal_captured_binder: Counter = Counter.init(0),
    rehearsal_captured_binder_bound: Counter = Counter.init(0),
    rehearsal_captured_binder_outer_unattributed: Counter = Counter.init(0),
    rehearsal_captured_binder_outer_unresolved: Counter = Counter.init(0),
    rehearsal_captured_binder_outer_not_active: Counter = Counter.init(0),
    rehearsal_captured_binder_index_out_of_range: Counter = Counter.init(0),
    rehearsal_captured_binder_identity_disagrees: Counter = Counter.init(0),
    // The callee scheme's own module was not in the lowering input's module
    // index, but it is the very module whose frozen store this specialization's
    // template body is already being read through, so the scheme resolves from
    // that cursor — the same module identity, hence the same frozen store.
    rehearsal_defining_module_from_template_cursor: Counter = Counter.init(0),
    // Whether the resolved edge's callee is defined by the very module this
    // specialization's template body reads from. The `differs` class is an edge
    // the request seam recorded that reached a different template: it names
    // another scheme's actuals, so it is refused before it can bind anything and
    // the specialization is resolved from its own template instead.
    rehearsal_edge_defining_module_matches_template: Counter = Counter.init(0),
    rehearsal_skip_edge_defining_module_differs: Counter = Counter.init(0),
    // A specialization no requesting edge named — a root request has no
    // requesting site and a compiler-generated one records none (reunify.md
    // 9.6) — still has an exact environment when its own template's scheme is
    // ground: no binders, no captures, and a root that reaches no checked
    // variable, so the empty binding is the whole binding. `edgeless_ground`
    // counts those; the rest name exactly why the template's scheme did not
    // supply one, and `scheme_has_binders` is the class that genuinely needs a
    // declared generated-edge binding before it can resolve.
    // `template_scheme_absent` is the template that names no owning scheme id at
    // all: its owner is a checked type rather than a defining-module node, which
    // is the synthesized wrapper kinds.
    rehearsal_env_resolved_edgeless_ground: Counter = Counter.init(0),
    rehearsal_edgeless_template_scheme_absent: Counter = Counter.init(0),
    rehearsal_edgeless_scheme_unresolved: Counter = Counter.init(0),
    rehearsal_edgeless_scheme_has_binders: Counter = Counter.init(0),
    rehearsal_edgeless_scheme_captures: Counter = Counter.init(0),
    rehearsal_edgeless_scheme_root_variable: Counter = Counter.init(0),
    // The `scheme_has_binders` population — the specializations that genuinely
    // still have no binding — split three ways: why the requesting edge supplied
    // none, which owner kind the template's scheme has, and which target kind the
    // template is. Together they say whether the missing binding is a root's
    // requested type (reunify.md 7.2), a declared generated edge (reunify.md
    // 9.6), or an unindexed dispatch site.
    rehearsal_edgeless_binders_root_request: Counter = Counter.init(0),
    rehearsal_edgeless_binders_generated_request: Counter = Counter.init(0),
    rehearsal_edgeless_binders_no_site: Counter = Counter.init(0),
    rehearsal_edgeless_binders_site_ambiguous: Counter = Counter.init(0),
    rehearsal_edgeless_binders_module_differs: Counter = Counter.init(0),
    rehearsal_edgeless_binders_edge_unusable: Counter = Counter.init(0),
    rehearsal_edgeless_binders_owner_top_level: Counter = Counter.init(0),
    rehearsal_edgeless_binders_owner_nested: Counter = Counter.init(0),
    rehearsal_edgeless_binders_owner_required: Counter = Counter.init(0),
    rehearsal_edgeless_binders_owner_synthetic: Counter = Counter.init(0),
    rehearsal_edgeless_binders_target_roc: Counter = Counter.init(0),
    rehearsal_edgeless_binders_target_hosted: Counter = Counter.init(0),
    rehearsal_edgeless_binders_target_intrinsic: Counter = Counter.init(0),
    rehearsal_edgeless_binders_target_entry: Counter = Counter.init(0),
    rehearsal_edgeless_binders_target_comptime: Counter = Counter.init(0),
    // The request seam's edge association (reunify.md 7.2, 11.3). A use site's
    // edge is visible only inside the request scope that site opened: `claimed`
    // counts the reservations that took it, so the specialization lowered under
    // that reserved id reads the edge that actually requested it; `unclaimed`
    // counts scopes that closed with their edge untaken, which is a request that
    // bound no new specialization. `claim_without_scope` and
    // `claim_without_edge` count reservations made outside any request scope and
    // inside one whose use named no edge — both are edgeless by construction.
    // `claim_repeated` counts a second claim for one reserved id, which would
    // mean two distinct use sites reserved one specialization body.
    // Binding such an edge from the template's OWN scheme, which is the checked
    // output of the same definition whose binder ids the specialized body names.
    // `owner_node_agrees` is the identity precondition — both checked outputs
    // name one defining CIR node as the scheme's owner — and the witness
    // counters are what accepts the binding: the callee scheme root emitted
    // under it must be the same type as the requesting site's own instantiated
    // root (reunify.md 7.5). A binding whose witness is absent or disagrees is
    // released, so no specialization is bound on an unproven alignment of two
    // independently written binder orders.
    rehearsal_foreign_scheme_owner_node_agrees: Counter = Counter.init(0),
    rehearsal_foreign_scheme_owner_node_differs: Counter = Counter.init(0),
    rehearsal_foreign_scheme_arity_differs: Counter = Counter.init(0),
    rehearsal_foreign_witness_agrees: Counter = Counter.init(0),
    rehearsal_foreign_witness_agrees_under_rerooting: Counter = Counter.init(0),
    rehearsal_foreign_witness_absent: Counter = Counter.init(0),
    rehearsal_foreign_witness_differs: Counter = Counter.init(0),
    rehearsal_env_resolved_foreign_scheme: Counter = Counter.init(0),
    // reunify.md 9.6: the declared compiler-generated instantiation rules. A
    // generated edge names the rule it was emitted under; the rule states where
    // its callee scheme's binder values come from. `declared_unbound` counts an
    // edge whose rule declares no binder source at all — the rule's missing
    // datum is stated on its enum member and in design.md, never guessed from
    // the concrete callable. The `receiver_*` counters bound the ways a
    // declared source failed to supply the mapping, and the `witness_*`
    // counters are what accepts a binding: the callee scheme root emitted under
    // it must be the same type as the checked callable the request names
    // (reunify.md 7.5). A binding whose witness is absent or disagrees is
    // released. The per-rule split is dumped as `rehearsal_generated_rule`
    // lines, because summing rules that bind with rules that stay unbound would
    // hide exactly the distinction section 9.6 requires.
    rehearsal_generated_rule_declared_unbound: Counter = Counter.init(0),
    rehearsal_generated_rule_caller_module_absent: Counter = Counter.init(0),
    rehearsal_generated_rule_scheme_captures: Counter = Counter.init(0),
    rehearsal_generated_rule_receiver_not_named: Counter = Counter.init(0),
    rehearsal_generated_rule_receiver_arity_differs: Counter = Counter.init(0),
    rehearsal_generated_rule_argument_untranslatable: Counter = Counter.init(0),
    rehearsal_generated_rule_witness_agrees: Counter = Counter.init(0),
    rehearsal_generated_rule_witness_agrees_under_rerooting: Counter = Counter.init(0),
    rehearsal_generated_rule_witness_absent: Counter = Counter.init(0),
    rehearsal_generated_rule_witness_differs: Counter = Counter.init(0),
    rehearsal_env_resolved_generated_rule: Counter = Counter.init(0),
    rehearsal_request_edge_claimed: Counter = Counter.init(0),
    rehearsal_request_edge_unclaimed: Counter = Counter.init(0),
    rehearsal_request_edge_claim_without_scope: Counter = Counter.init(0),
    rehearsal_request_edge_claim_without_edge: Counter = Counter.init(0),
    rehearsal_request_edge_claim_repeated: Counter = Counter.init(0),
    rehearsal_skip_root_edge: Counter = Counter.init(0),
    rehearsal_skip_generated_edge: Counter = Counter.init(0),
    rehearsal_skip_no_site: Counter = Counter.init(0),
    // The `no_site` class split by whether the use expression carries recorded
    // sites at all: `owned_elsewhere` is a use whose sites all name other
    // definitions as their scheme owner, and `unrecorded` is a use no site
    // names, which is an edge outside the section 7.2 coverage table.
    rehearsal_no_site_use_owned_elsewhere: Counter = Counter.init(0),
    rehearsal_no_site_use_unrecorded: Counter = Counter.init(0),
    rehearsal_skip_site_ambiguous: Counter = Counter.init(0),
    rehearsal_skip_scheme_unresolved: Counter = Counter.init(0),
    // The specialization's own template names no owning scheme, so the
    // `scheme_owner_node` half of its requesting edge's identity is unknown and
    // no site can be selected by it.
    rehearsal_skip_template_owner_unresolved: Counter = Counter.init(0),
    rehearsal_skip_caller_module_absent: Counter = Counter.init(0),
    rehearsal_skip_defining_module_absent: Counter = Counter.init(0),
    rehearsal_skip_arity_mismatch: Counter = Counter.init(0),
    rehearsal_skip_unreached_actual: Counter = Counter.init(0),
    rehearsal_skip_actual_untranslatable: Counter = Counter.init(0),
    // Per compared position. `type_compared` counts one (position, sealed id)
    // pair; `type_match` counts equal stored digests. `type_mismatch_logical` is
    // the required-zero counter: neither side carries iterator or generated
    // representation content, so an unequal stored form is a directed-emission
    // bug. `type_mismatch_representation` is a difference on a position where one
    // side does carry that content, which together with
    // `type_skip_engine_input_needed` bounds exactly the representation content
    // the flip's body discovery must supply.
    rehearsal_type_compared: Counter = Counter.init(0),
    rehearsal_type_match: Counter = Counter.init(0),
    // A position whose two stored forms are the same rooted graph reached
    // through different entry paths: their unfoldings agree, while their stored
    // digests encode the recursive back reference at different visiting-stack
    // positions (reunify.md section 8.3). The graph roots such a knot wherever
    // unification joined two nodes, which differs between call sites of one
    // nominal; the directed emission roots it at the nominal every time. The
    // count is the population whose emitted stored form the flip deliberately
    // re-roots, not a content difference.
    rehearsal_type_equal_under_rerooting: Counter = Counter.init(0),
    rehearsal_type_mismatch_logical: Counter = Counter.init(0),
    // The subset of logical mismatches whose emitted type is the empty tag union
    // — the stored form an undisposed, undefaulted residual variable
    // materializes to — where the graph sealed real content. The `unbound_*`
    // counters below say which position and which binding produced it; the
    // corpus attributes almost all of them to a binder whose bound value was
    // itself a residual, not to a scheme carrying no binders.
    rehearsal_type_mismatch_unbound_residual: Counter = Counter.init(0),
    // The logical mismatches that are NOT a residual materialization, split by
    // what actually differs at the deepest disagreeing pair: two different
    // content heads, two rows of the same head with different widths, two named
    // heads whose declared identity differs, and anything else.
    rehearsal_type_mismatch_head_tag: Counter = Counter.init(0),
    rehearsal_type_mismatch_row_width: Counter = Counter.init(0),
    rehearsal_type_mismatch_named_identity: Counter = Counter.init(0),
    // Two recursive types that agree on every head the bounded parallel walk
    // reaches and whose bounded unfoldings still differ: the difference sits
    // deeper in the cycle than the walk localizes, so the pair is named as such
    // rather than counted as a difference nobody looked at.
    rehearsal_type_mismatch_recursive_beyond_depth: Counter = Counter.init(0),
    rehearsal_type_mismatch_unclassified: Counter = Counter.init(0),
    rehearsal_type_mismatch_representation: Counter = Counter.init(0),
    // A position whose checked source lives outside the specialization's own
    // scheme module, so no binder of this environment is in scope there and it
    // emits as a ground type.
    rehearsal_type_outside_environment: Counter = Counter.init(0),
    // One checked position that the graph sealed to more than one id within a
    // single specialization: a second occurrence with its own representation
    // flow, counted rather than compared against the one emitted type.
    rehearsal_type_skip_other_occurrence: Counter = Counter.init(0),
    rehearsal_type_skip_module_absent: Counter = Counter.init(0),
    rehearsal_type_skip_engine_input_needed: Counter = Counter.init(0),
    rehearsal_type_skip_open_row: Counter = Counter.init(0),
    rehearsal_type_skip_recursive: Counter = Counter.init(0),
    rehearsal_type_skip_pending_or_err: Counter = Counter.init(0),
    rehearsal_type_skip_numeric_default: Counter = Counter.init(0),
    rehearsal_type_skip_malformed_arity: Counter = Counter.init(0),
    rehearsal_type_skip_binder_not_found: Counter = Counter.init(0),
    rehearsal_type_skip_missing_backing: Counter = Counter.init(0),
    // The representation side (reunify.md sections 10.2/10.6, 11.1). Every
    // representation-bearing position the rehearsal emits gets its own slot;
    // `interface_relate_*` is the explicit caller-to-callee representation edge
    // between the request context's emission of the requesting edge and the
    // callee's scheme root emitted under the binding, related through the
    // section 10.3 closure engine (rejected when the two are not logically
    // equal). `seal_positions` counts slots sealed at the specialization's end,
    // `relations_applied` those a relation moved into another class, and
    // `seal_descriptor_moved` a sealed descriptor that no longer matches the one
    // emitted at that position — the case where emission must be re-materialized
    // from the sealed slot instead of kept.
    rehearsal_slots_created: Counter = Counter.init(0),
    rehearsal_interface_relate_applied: Counter = Counter.init(0),
    rehearsal_interface_relate_rejected: Counter = Counter.init(0),
    rehearsal_interface_already_related: Counter = Counter.init(0),
    rehearsal_seal_positions: Counter = Counter.init(0),
    rehearsal_relations_applied: Counter = Counter.init(0),
    rehearsal_seal_descriptor_moved: Counter = Counter.init(0),
};

/// The single process-wide census. A corpus run accumulates into it and the
/// pipeline dumps it once lowering finishes.
pub var global: Census = .{};

/// Add one to the named counter. Inert outside Debug builds. `name` is a
/// field of `Census`, checked at compile time.
pub inline fn bump(comptime name: []const u8) void {
    if (!enabled) return;
    _ = @field(global, name).fetchAdd(1, .monotonic);
}

/// Render every counter as a `name value` line. Inert outside Debug builds.
/// The caller owns and frees the returned bytes.
pub fn dumpText(allocator: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    if (enabled) {
        inline for (@typeInfo(Census).@"struct".fields) |field| {
            const value = @field(global, field.name).load(.monotonic);
            const line = try std.fmt.allocPrint(allocator, "{s} {d}\n", .{ field.name, value });
            defer allocator.free(line);
            try out.appendSlice(allocator, line);
        }
    }
    return out.toOwnedSlice(allocator);
}

/// When `ROC_REUNIFY_CENSUS` names a file, append the counter dump to it.
/// The census owns this write directly through libc — it is Debug-only
/// measurement plumbing, deliberately outside the compiler's file-system
/// abstraction, and every failure is silent so lowering is never affected.
pub fn appendDumpToEnvPath(allocator: std.mem.Allocator) void {
    if (comptime !enabled) return;
    const raw_path = std.c.getenv("ROC_REUNIFY_CENSUS") orelse return;
    const path = raw_path[0..std.mem.len(raw_path)];
    if (path.len == 0) return;
    const text = dumpText(allocator) catch return;
    defer allocator.free(text);
    if (text.len == 0) return;
    appendToFile(raw_path, text);
}

/// Append bytes to the named file through libc with `O_APPEND`, so multiple
/// processes measuring one corpus interleave whole writes rather than
/// clobbering each other's offsets.
pub fn appendToFile(path: [*:0]const u8, bytes: []const u8) void {
    if (comptime !enabled) return;
    const fd = std.c.open(path, .{ .ACCMODE = .WRONLY, .CREAT = true, .APPEND = true }, @as(std.c.mode_t, 0o644));
    if (fd < 0) return;
    defer _ = std.c.close(fd);
    var remaining = bytes;
    while (remaining.len > 0) {
        const written = std.c.write(fd, remaining.ptr, remaining.len);
        if (written <= 0) return;
        remaining = remaining[@intCast(written)..];
    }
}
