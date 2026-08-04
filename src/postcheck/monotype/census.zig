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

/// Every constraint-replay site in Monotype body lowering (reunify.md sections
/// 9, 13 Slice 7). Body lowering states most of its type relations through four
/// shared relaters — `relateRequestComponent`, `relateFunctionRequestInterface`,
/// `checkedMonoRequestNode` and its walk `relateCheckedMonoRequestNodeAt` — and
/// states the rest as a bare `graph.unify`. A relater carries several distinct
/// relations, so it gets one member per relation rather than one member for the
/// relater, and each member is named by the relater that carries the relation
/// plus the relation it states. The identifiers are stable, so a corpus table
/// reads per site rather than only in total.
///
/// The question each site answers is whether its two sides are ALREADY the same
/// type under directed translation — a site that is, is a relation the flip can
/// simply delete; a site that is not, is a place the flip must supply the
/// information the relation carried.
pub const UnifySite = enum {
    // --- Relations `relateRequestComponent` carries ---

    /// `unifyFormalWithCallerArgNode`: a callee formal joins the caller's own
    /// checked argument position.
    request_component_call_arg_formal_to_actual,
    /// `instantiateCallNodeFromCallerAtNode`: a callee formal joins the public
    /// interface of an argument whose evidence is generated-private.
    request_component_call_arg_formal_to_public_evidence,
    /// `instantiateCallNodeFromCallerAtNode`: a callee formal joins the exact
    /// producer evidence the caller's argument expression lowered to.
    request_component_call_arg_formal_to_evidence,
    /// `instantiateCallNodeFromCallerAtNode`: the callee's checked return joins
    /// the caller's checked result position.
    request_component_call_ret_callee_to_caller,
    /// `instantiateCallableDispatchPlanCallNodeFromCallerAtNode`: the plan's
    /// dispatcher position joins the callee formal it dispatches on.
    request_component_dispatch_receiver_to_formal,
    /// `instantiateCallableDispatchPlanCallNodeFromCallerAtNode`: a callee
    /// formal joins the caller's own checked operand position.
    request_component_dispatch_arg_formal_to_actual,
    /// `instantiateCallableDispatchPlanCallNodeFromCallerAtNode`: a callee
    /// formal joins the public interface of a generated-private operand.
    request_component_dispatch_arg_formal_to_public_evidence,
    /// `instantiateCallableDispatchPlanCallNodeFromCallerAtNode`: a callee
    /// formal joins the exact producer evidence its operand lowered to.
    request_component_dispatch_arg_formal_to_evidence,
    /// `instantiateCallableDispatchPlanCallNodeFromCallerAtNode`: the callee's
    /// checked return joins the caller's checked result position.
    request_component_dispatch_ret_callee_to_caller,
    /// `relateFormalToOperand`: a numeral plan's callee formal joins the
    /// caller's checked operand position.
    request_component_numeral_formal_to_operand,
    /// `instantiateIteratorPlanCallNodeFromCaller`: an iterator plan's callee
    /// formal joins the caller's checked operand position.
    request_component_iterator_formal_to_actual,
    /// `fieldAccessTypeNode`: the field read joins the Monotype the request
    /// expects at that position.
    request_component_field_access_to_expected,

    // --- Relations `relateFunctionRequestInterface` carries ---

    /// `instantiateTargetFromPlanNode`: the dispatch target's checked function
    /// interface joins the dispatch plan's.
    function_request_interface_target_to_plan,

    // --- Relations `checkedMonoRequestNode` carries ---

    /// `instantiateCallNodeFromCallerAtNode`: the callee's checked return is
    /// related to the Monotype the request expects.
    checked_mono_request_call_ret_to_expected,
    /// `instantiateTargetCallNodeFromMonoArgs`, per formal and for the return.
    checked_mono_request_target_formal_to_mono_arg,
    checked_mono_request_target_ret_to_mono,
    /// `instantiateTargetCallNodeFromMonoArgAtIndex`: the formal at the
    /// requested index is related to the Monotype supplied for it.
    checked_mono_request_target_indexed_formal_to_mono_arg,
    /// `instantiateIteratorPlanCallNodeFromCaller`: the caller's checked
    /// argument position is related to the exact producer evidence it lowered
    /// to, the loop's iterator state is related to its callee formal, and the
    /// callee's checked return is related to the expected Monotype.
    checked_mono_request_iterator_arg_to_evidence,
    checked_mono_request_iterator_formal_to_loop_state,
    checked_mono_request_iterator_ret_to_expected,

    // --- Relations stated as a bare `graph.unify` ---

    /// `lowerDraftTemplateFromContext`: a recursive root request, a repeated
    /// template request, and a root's own function node each join the request
    /// function node the use site built.
    draft_template_recursive_root_to_request,
    draft_template_spec_to_request,
    draft_template_root_to_request,
    /// `lowerDraftNestedFromContext`: a capture anchor joins the nested
    /// specialization it anchors, a nested specialization joins the request
    /// function node, and an owned nested root joins its own body's type.
    draft_nested_capture_anchor_to_spec,
    draft_nested_spec_to_request,
    draft_nested_root_to_body,
    /// `constrainTypeToMono` / `constrainTypeToCell` / `constrainCheckedTypeRelations`.
    constrain_checked_to_mono,
    constrain_checked_to_cell,
    constrain_checked_to_checked,
    /// `instNode`: the placeholder standing for a checked address joins the
    /// content built for it. Node construction, not a relation between two
    /// independently derived types.
    inst_node_placeholder_to_content,
    /// `instNominalDeclarationBackingNode`: the placeholder standing for a
    /// nominal's backing joins the instantiated declaration backing. Node
    /// construction.
    inst_nominal_backing_placeholder_to_content,
    /// `relateCheckedNodeToProducedValueInner`: a checked request position joins
    /// the runtime value a body produced for it.
    checked_to_produced_value,
    /// `lowerExprInner`: a string literal's checked position joins the `Str`
    /// primitive.
    str_literal_expr_to_primitive,
    /// `restoredHoistedConstAtNode`: a hoisted const's checked type joins the
    /// request node its use site built.
    hoisted_const_checked_to_request,
    /// `lowerPendingCallableEvalBindingValueAtNode`: the entry wrapper's checked
    /// root joins the nullary wrapper function node, and the template's and the
    /// compile-time root's checked roots each join the request function node.
    callable_eval_wrapper_root_to_wrapper_fn,
    callable_eval_template_root_to_request,
    callable_eval_comptime_root_to_request,
    /// `instantiateNumeralPlanCallNode`: the caller's and the callee's checked
    /// results each join the plan's converted target type.
    numeral_caller_ret_to_target,
    numeral_callee_ret_to_target,
    /// `localCallArgumentEvidenceNode`: a local's own value node joins the
    /// Monotype the call expects for that argument.
    local_argument_evidence_to_expected,
    /// `restoreConstUseAtNode`: an eval-template const use's requested checked
    /// type joins the request node.
    const_use_requested_to_request,
    /// `lowerConstEvalTemplateUseAtNode`: the entry template's checked root
    /// joins the nullary wrapper function node, and the evaluated body's checked
    /// type joins the request node.
    const_eval_entry_root_to_wrapper_fn,
    const_eval_body_to_request,
    /// `relateLookupExprAtNode`: the request node joins the looked-up
    /// expression's checked position.
    lookup_expr_expected_to_checked,
    /// `relateExprAtNode`: the request node joins the type the expression
    /// lowered to, for the forms with no shape-specific relation.
    expr_expected_to_lowered,
    /// `relateEvidenceTargetRootToRequest`: a checked dispatch target's root
    /// joins its live call request.
    evidence_target_root_to_request,
    /// `lowerStructuralEqualityAtNode` / `lowerStructuralHashAtNode`: the
    /// derivation's two operands are one type, and the hasher operand and the
    /// result are one type.
    structural_equality_operands_equal,
    structural_hash_hasher_to_result,
    /// `constrainStructuralEqualityOperandNode`: an operand node joins the other
    /// side's checked position, and then the node that side lowered to.
    structural_equality_operand_to_checked,
    structural_equality_operand_to_result,
    /// `relateRecordRestNodeToSource`: each field of a rest row joins the same
    /// field read off the source record.
    record_rest_field_to_source_field,
    /// `recordRestNodeForPattern`: the rest pattern's checked type joins the row
    /// of the fields its destructs left over.
    record_rest_pattern_to_row,
    /// `includeControlFlowResult`: two branches' generated-private results join.
    control_flow_private_results_join,
    /// `lowerIteratorOperandAtNode`: the loop's iterator state joins the callee
    /// formal the `.next` dispatch instantiated for it.
    iterator_loop_state_to_formal,
    /// `fieldAccessTypeNode` through `constrainCheckedInterfaceToFieldCell`: the
    /// field read joins the checked position the access sits at.
    field_access_to_checked,
};

/// What one execution of a constraint-replay site contributed.
pub const UnifySiteOutcome = enum {
    /// The unify decided nothing the flip has to reproduce, which happens two
    /// ways. Either directed translation already makes the two sides one type,
    /// or every position they differ at is one the graph-built side leaves
    /// open: the graph's own Monotype import reads an empty tag union in an
    /// immutable type as a slot no value reached and turns it back into an
    /// unresolved node, so the constraint fills a hole in the graph's own node
    /// from content the other side already holds. `open_on_import` counts the
    /// second way apart from the first.
    redundant,
    /// The two sides differed, so the unify carried information the directed
    /// translation of its operands did not already hold. Every one of these is a
    /// place the flip cannot simply delete the call.
    informative,
    /// The two sides are ONE logical type that differ only in the
    /// representation content reunify.md section 10.3's rules move, and the
    /// shared representation policy covers the pair. What the unify moves here
    /// is a representation decision the closure engine owns, not logical
    /// information the checked data has to carry, so the flip reproduces it
    /// through that engine rather than through a constraint.
    representation_decision,
    /// At least one side had no directed answer at this execution;
    /// `UnifySiteBlocker` says which.
    unmeasurable,
    /// Not a relation between two independently derived types at all: the
    /// graph's own node-building step, which the flip deletes together with the
    /// node it builds.
    construction,
};

/// What an `informative` execution's two sides actually disagree about — the
/// question "what does this unify contribute that directed translation lacks?"
/// answered per execution rather than per total.
pub const UnifySiteInformation = enum {
    /// At least one side carries iterator or generated-evidence representation
    /// content, so what the unify moves is a representation decision that
    /// reunify.md section 10's closure engine owns.
    representation,
    /// One side is the empty tag union — the stored form an unbound residual
    /// variable materializes to — where the other carries content, and the first
    /// checked variable that side reaches IS a generalized binder of a checked
    /// scheme. What the unify moved is that scheme's binder value, which
    /// reunify.md section 9's directed instantiation takes from the checker's
    /// recorded substitution instead of deriving by matching.
    scheme_binder_unbound,
    /// The same empty-tag-union materialization where the free variable is not a
    /// checked scheme's binder, so no recorded substitution names a value for
    /// that position.
    unbound_residual,
    /// Two different content heads.
    head_tag,
    /// One head with two different row or argument widths.
    row_width,
    /// Two named heads whose declared identity differs.
    named_identity,
    /// A difference the bounded parallel walk did not localize into one of the
    /// classes above.
    unclassified,
};

/// Why one execution came out `unmeasurable`. Every way a directed answer can
/// be absent has its own member, so an unmeasurable execution always names
/// which operand had no answer and exactly why.
pub const UnifySiteBlocker = enum {
    /// No specialization binding environment was active for the operand's
    /// module, so a checked operand carrying binders has no value to take.
    no_environment,
    /// The operand's module is not in this lowering input, so no checked store
    /// holds the position at all.
    operand_module_absent,
    /// The remaining members are the directed translator's own skip classes,
    /// one per `direct_translate.SkipReason`: the operand's checked type left
    /// the translatable subset at a recursion the walk could not close, an open
    /// row, a position whose representation content the checked data cannot
    /// dictate, a pending or erroneous checked type, an unresolved numeric
    /// default, a builtin whose declared arity did not match, or a nominal
    /// whose backing the checked data does not carry.
    operand_recursive,
    operand_open_row,
    operand_engine_input_needed,
    operand_pending_or_err,
    operand_numeric_default,
    operand_malformed_arity,
    operand_missing_backing,
    operand_undisposed_residual,
    /// The translation reported an error rather than a skip, which stops the
    /// rehearsal; expected zero.
    operand_untranslatable,
    /// The site hands the graph a node with no checked address and no immutable
    /// type behind it, so nothing names what the directed side would compute.
    operand_undescribed,
    /// The operand is a record field read, and the receiver's own translation
    /// is not a record — unwrapping named backings did not reach one. The
    /// receiver translated; the read off it did not.
    field_receiver_not_a_record,
    /// The operand is a record field read whose receiver translated to a
    /// record that carries no such label.
    field_label_absent,
};

/// What one measured operand is. The empty tag union is the stored shape both
/// the graph and directed translation use for a position no value reached, so
/// an informative execution's classification turns on which kind of operand
/// carried it: a directed answer, or a type the graph sealed while one of its
/// own nodes was still unresolved — which the graph's own Monotype import
/// reopens as unresolved when the site imports it back in.
pub const UnifySiteOperandOrigin = enum {
    /// A checked position, translated under the environment in force.
    checked_position,
    /// A record field read off such a position's translation.
    field_of_checked,
    /// An immutable type the graph sealed, which the site imports.
    graph_sealed,
};

/// What names the value of the checked variable one informative execution's
/// residual materialization came from.
pub const UnifySiteResidualState = enum {
    /// The residual side is graph-sealed, so no checked position stands behind
    /// it and nothing in the checked data was consulted for it.
    not_a_checked_position,
    /// The difference sits under a head whose children the checked side orders
    /// differently from the emission (a row), so the walk declines to name a
    /// checked position rather than name the wrong one.
    position_not_followed,
    /// The checked position at the difference holds no variable: its empty tag
    /// union is content the checked data states, not a residual materialization.
    checked_content,
    /// The variable at that position is a generalized binder of a checked
    /// scheme, so a binding names its value.
    scheme_binder,
    /// A residual disposition under this body's own scheme owner names it.
    disposed_contextual,
    disposed_uninhabited,
    /// A module-body-scoped residual disposition names it.
    disposed_module_body,
    /// A residual disposition names it only under another scheme owner.
    disposed_other_owner,
    /// No residual disposition names it.
    undisposed,
};

/// How many constraint-replay sites reunify.md's Slice 7 flip has to account
/// for. Every one is declared in `UnifySite`.
pub const unify_site_count = @typeInfo(UnifySite).@"enum".fields.len;
const unify_outcome_count = @typeInfo(UnifySiteOutcome).@"enum".fields.len;
const unify_blocker_count = @typeInfo(UnifySiteBlocker).@"enum".fields.len;
const unify_information_count = @typeInfo(UnifySiteInformation).@"enum".fields.len;
const unify_origin_count = @typeInfo(UnifySiteOperandOrigin).@"enum".fields.len;
const unify_residual_state_count = @typeInfo(UnifySiteResidualState).@"enum".fields.len;

/// One atomic u64 per classification question. Each field name is the text
/// the dump writes on its line, so a corpus run reads the names directly.
pub const Census = struct {
    two_sided_tag_row_merge: Counter = Counter.init(0),
    two_sided_record_row_merge: Counter = Counter.init(0),
    one_sided_tag_row_merge: Counter = Counter.init(0),
    one_sided_record_row_merge: Counter = Counter.init(0),
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
    // The lowering seam (reunify.md section 9): how many checked positions the
    // directed instantiation answered, and how many it could not name a type
    // for. The second must stay zero; every checked position a body reads is one
    // the checked data describes.
    seam_direct: Counter = Counter.init(0),
    seam_direct_absent: Counter = Counter.init(0),
    // The two exits by which body lowering obtains a Monotype from the graph
    // (reunify.md 13.2 step 2a). Their sum is the denominator seam coverage is
    // a fraction of.
    graph_exit_active_type_view: Counter = Counter.init(0),
    graph_exit_seal_node: Counter = Counter.init(0),
    graph_exit_seal_entry: Counter = Counter.init(0),
    seal_exit_diverged: Counter = Counter.init(0),
    // What supplied a diverging node's value: another checked position joined
    // into its class, only itself, an unnamed member, or nothing at all.
    supplier_class_holds_another_position: Counter = Counter.init(0),
    supplier_class_only_same_position: Counter = Counter.init(0),
    supplier_class_has_no_named_member: Counter = Counter.init(0),
    supplier_class_is_alone: Counter = Counter.init(0),
    // Asked position-first: is the diverging position a declared parameter that
    // some recorded nominal instance already supplies an argument for?
    nominal_args_not_a_declared_parameter: Counter = Counter.init(0),
    nominal_args_no_free_variable: Counter = Counter.init(0),
    // Whether a diverging nominal parameter is already supplied by directed
    // translation binding a declaration's formals to an instance's args.
    nominal_param_supplied_by_walk_into_backing: Counter = Counter.init(0),
    nominal_param_no_read_reaches_an_instance: Counter = Counter.init(0),
    // For a free variable no nominal declaration declares: whether a scheme's
    // binding frame supplies it during a compositional walk.
    scheme_frame_supplies_the_position: Counter = Counter.init(0),
    scheme_binds_it_but_no_read_enters_the_scheme: Counter = Counter.init(0),
    free_variable_no_scheme_binds_it: Counter = Counter.init(0),
    free_variable_unbound_no_scheme_lists_any_binder: Counter = Counter.init(0),
    // Divergences measured away from the seal exit, classified by the binding
    // their free variable waits on. A ground one waits on nothing.
    outside_exit_divergence_no_cursor: Counter = Counter.init(0),
    outside_exit_divergence_is_ground: Counter = Counter.init(0),
    outside_exit_divergence_nominal_param: Counter = Counter.init(0),
    outside_exit_divergence_scheme_binder: Counter = Counter.init(0),
    outside_exit_divergence_free_but_unbound: Counter = Counter.init(0),
    // Why a both-content divergence could not be judged against checking.
    sealed_vs_checked_nominal_lowered_by_representation: Counter = Counter.init(0),
    sealed_vs_checked_same_nominal_other_position: Counter = Counter.init(0),
    // Whether the frame's own scheme root - the root production enters - is
    // what reaches a diverging position, rather than some other recorded read.
    nominal_param_reached_from_frame_scheme_root: Counter = Counter.init(0),
    scheme_reached_from_frame_scheme_root: Counter = Counter.init(0),
    // Framing-independent coverage: the position lies inside the very root
    // whose binder the walk binds, so any entry into it supplies the position.
    nominal_param_inside_the_backing_that_binds_it: Counter = Counter.init(0),
    scheme_position_inside_its_own_scheme_root: Counter = Counter.init(0),
    // Where the last free variables no generalized list names actually live.
    unlisted_variable_is_a_captured_binder: Counter = Counter.init(0),
    unlisted_variable_inside_an_unlisting_scheme_root: Counter = Counter.init(0),
    unlisted_variable_inside_a_nominal_backing: Counter = Counter.init(0),
    unlisted_variable_disposed_uninhabited: Counter = Counter.init(0),
    unlisted_variable_disposed_contextual: Counter = Counter.init(0),
    unlisted_variable_under_a_recorded_root: Counter = Counter.init(0),
    // Whether a position this specialization emitted unbound is one a walk
    // supplies, asked the same way as the seam's divergences.
    mismatch_unbound_position_is_ground: Counter = Counter.init(0),
    mismatch_unbound_inside_the_backing_that_binds_it: Counter = Counter.init(0),
    mismatch_unbound_inside_its_own_scheme_root: Counter = Counter.init(0),
    mismatch_unbound_inside_an_imported_scheme_root: Counter = Counter.init(0),
    position_inside_an_imported_scheme_root: Counter = Counter.init(0),
    mismatch_unbound_no_frame_will_bind_it: Counter = Counter.init(0),
    // Whether a diverging position is one this lowering instantiated, or a
    // template's own position that a use edge's actuals would make concrete.
    diverged_position_is_instantiated: Counter = Counter.init(0),
    diverged_position_is_a_template: Counter = Counter.init(0),
    // The same question settled once lowering has finished and the cache is complete.
    settled_diverged_position_is_instantiated: Counter = Counter.init(0),
    settled_diverged_position_is_a_template: Counter = Counter.init(0),
    unbound_no_frame_position_is_instantiated: Counter = Counter.init(0),
    unbound_no_frame_position_is_a_template: Counter = Counter.init(0),
    unlisted_variable_no_root_reaches_it: Counter = Counter.init(0),
    ground_divergence_judged_against_checking: Counter = Counter.init(0),
    // Whether directed emission reproduces the graph's dispatch returns, the
    // certification the representation-movement class never had.
    iter_declare_attempt: Counter = Counter.init(0),
    iter_declare_not_iterator_rule: Counter = Counter.init(0),
    iter_declare_no_source: Counter = Counter.init(0),
    iter_declare_no_procedure: Counter = Counter.init(0),
    iter_declare_nonminting_procedure: Counter = Counter.init(0),
    iter_declare_root_not_function: Counter = Counter.init(0),
    iter_declare_no_caller_cursor: Counter = Counter.init(0),
    iter_declare_no_topology_lookup: Counter = Counter.init(0),
    iter_declare_no_topology_ids: Counter = Counter.init(0),
    iter_declare_topology_intern_failed: Counter = Counter.init(0),
    iter_declare_declared: Counter = Counter.init(0),
    iter_declare_caller_ret_declared: Counter = Counter.init(0),
    iter_declare_witness_not_function: Counter = Counter.init(0),
    iter_declare_witness_receiver_only: Counter = Counter.init(0),
    spec_root_parity_agree: Counter = Counter.init(0),
    spec_root_parity_diverge: Counter = Counter.init(0),
    spec_root_parity_declined: Counter = Counter.init(0),
    spec_root_parity_error: Counter = Counter.init(0),
    spec_root_parity_eql_not_digest: Counter = Counter.init(0),
    dispatch_return_directed_cell: Counter = Counter.init(0),
    dispatch_return_directed_declined_at_cell: Counter = Counter.init(0),
    // The generated dispatch return's production route.
    direct_stored_skip_undisposed_residual: Counter = Counter.init(0),
    rehearsal_type_skip_undisposed_residual: Counter = Counter.init(0),
    rehearsal_authoritative_unstated: Counter = Counter.init(0),
    type_cell_graph_route: Counter = Counter.init(0),
    pattern_uninhabited_directed: Counter = Counter.init(0),
    pattern_uninhabited_graph: Counter = Counter.init(0),
    spec_record_producer_recorded: Counter = Counter.init(0),
    spec_record_producer_declared: Counter = Counter.init(0),
    spec_record_request_seen: Counter = Counter.init(0),
    spec_record_position_not_named: Counter = Counter.init(0),
    spec_record_position_not_generated: Counter = Counter.init(0),
    iter_declare_primary_component_untranslated: Counter = Counter.init(0),
    iter_declare_primary_state_missing: Counter = Counter.init(0),
    rehearsal_frame_rule_declared: Counter = Counter.init(0),
    rehearsal_frame_request_deep_declared: Counter = Counter.init(0),
    rehearsal_rule_path_mint_declared: Counter = Counter.init(0),
    rehearsal_body_root_mint_declared: Counter = Counter.init(0),
    iter_declare_adopted_enclosing_mint: Counter = Counter.init(0),
    iter_chain_link_declared: Counter = Counter.init(0),
    rehearsal_use_mint_recorded: Counter = Counter.init(0),
    rehearsal_use_mint_propagated: Counter = Counter.init(0),
    rehearsal_request_root_body_final: Counter = Counter.init(0),
    spec_root_declined_env_unready: Counter = Counter.init(0),
    spec_root_declined_root_request: Counter = Counter.init(0),
    spec_root_declined_generated_request: Counter = Counter.init(0),
    spec_root_declined_no_site: Counter = Counter.init(0),
    spec_root_declined_site_ambiguous: Counter = Counter.init(0),
    spec_root_declined_module_differs: Counter = Counter.init(0),
    spec_root_declined_edge_unusable: Counter = Counter.init(0),
    rehearsal_nested_claim_without_scope: Counter = Counter.init(0),
    rehearsal_nested_claim_top_not_checked: Counter = Counter.init(0),
    rehearsal_nested_claim_edge_differs: Counter = Counter.init(0),
    rehearsal_nested_claim_matched: Counter = Counter.init(0),
    rehearsal_nested_spec_attempted: Counter = Counter.init(0),
    rehearsal_nested_spec_resolved: Counter = Counter.init(0),
    rehearsal_nested_scheme_by_root: Counter = Counter.init(0),
    rehearsal_nested_scheme_absent: Counter = Counter.init(0),
    rehearsal_site_identity_instantiation: Counter = Counter.init(0),
    rehearsal_site_actual_routes_to_rule: Counter = Counter.init(0),
    deferred_identity_directed_agrees: Counter = Counter.init(0),
    deferred_identity_directed_differs: Counter = Counter.init(0),
    deferred_identity_directed_eql_not_digest: Counter = Counter.init(0),
    deferred_identity_directed_absent: Counter = Counter.init(0),
    deferred_identity_keyed_directed: Counter = Counter.init(0),
    deferred_identity_keyed_sealed: Counter = Counter.init(0),
    dispatch_result_directed_final: Counter = Counter.init(0),
    dispatch_result_directed_declined: Counter = Counter.init(0),
    emission_joinable_slots_opened: Counter = Counter.init(0),
    rehearsal_request_provisional_emitted: Counter = Counter.init(0),
    rehearsal_joinable_slots_opened: Counter = Counter.init(0),
    rehearsal_provisional_slot_joined: Counter = Counter.init(0),
    value_sealed_by_drafts: Counter = Counter.init(0),
    value_sealed_by_reemission: Counter = Counter.init(0),
    value_reemission_shared_position: Counter = Counter.init(0),
    value_declined_no_scheme_root: Counter = Counter.init(0),
    value_declined_env_unready: Counter = Counter.init(0),
    value_declined_reemission_skip: Counter = Counter.init(0),
    value_parity_agree: Counter = Counter.init(0),
    value_parity_eql_not_digest: Counter = Counter.init(0),
    value_parity_diverge: Counter = Counter.init(0),
    value_parity_declined: Counter = Counter.init(0),
    spec_root_declined_emission_null: Counter = Counter.init(0),
    iter_chain_link_underived: Counter = Counter.init(0),
    iter_declare_identity_emit_failed: Counter = Counter.init(0),
    iter_declare_identity_not_named: Counter = Counter.init(0),
    iter_declare_identity_stamped: Counter = Counter.init(0),
    intern_hit_occurrence_held: Counter = Counter.init(0),
    generated_dispatch_return_directed: Counter = Counter.init(0),
    generated_dispatch_return_node: Counter = Counter.init(0),
    // What a nested-local-scheme frame would need at the variable-headed
    // leaves the flip left on the graph.
    variable_headed_leaf_kept_on_graph: Counter = Counter.init(0),
    nested_leaf_no_free_variable: Counter = Counter.init(0),
    nested_leaf_active_frame_binds_it: Counter = Counter.init(0),
    nested_leaf_active_frame_does_not_bind_it: Counter = Counter.init(0),
    nested_leaf_no_active_frame: Counter = Counter.init(0),
    nested_leaf_no_scheme_owns_it: Counter = Counter.init(0),
    nested_leaf_scheme_owned: Counter = Counter.init(0),
    nested_leaf_scheme_captures_enclosing: Counter = Counter.init(0),
    nested_leaf_request_scope_open: Counter = Counter.init(0),
    nested_leaf_no_request_scope: Counter = Counter.init(0),
    nested_leaf_scheme_has_no_recorded_site: Counter = Counter.init(0),
    nested_leaf_scheme_has_one_site: Counter = Counter.init(0),
    nested_leaf_scheme_has_many_sites: Counter = Counter.init(0),
    nested_leaf_innermost_edge_names_site: Counter = Counter.init(0),
    nested_leaf_outer_edge_names_site: Counter = Counter.init(0),
    nested_leaf_no_open_edge_names_site: Counter = Counter.init(0),
    sealed_vs_checked_position_is_a_variable: Counter = Counter.init(0),
    sealed_vs_checked_position_is_an_alias: Counter = Counter.init(0),
    sealed_vs_checked_position_unresolved: Counter = Counter.init(0),
    // Whether a diverging position is reachable from another position the same
    // specialization reads, so a compositional walk would cover it and the
    // standalone comparison asks something production never asks.
    position_reachable_from_another_read: Counter = Counter.init(0),
    position_only_read_standalone: Counter = Counter.init(0),
    // What a seal-exit divergence disagrees about, classified the same way the
    // constraint census classifies its informative executions.
    seal_diverged_direct_unbound: Counter = Counter.init(0),
    seal_diverged_graph_unbound: Counter = Counter.init(0),
    seal_diverged_head_tag: Counter = Counter.init(0),
    seal_diverged_row_width: Counter = Counter.init(0),
    seal_diverged_named_identity: Counter = Counter.init(0),
    seal_diverged_unclassified: Counter = Counter.init(0),
    // For a content-vs-content divergence, whether the sealed type still
    // agrees with the head checking recorded. A contradiction is the graph's
    // error, since checking is the authority on logical types.
    sealed_agrees_with_checked_head: Counter = Counter.init(0),
    sealed_contradicts_checked_head: Counter = Counter.init(0),
    sealed_vs_checked_inconclusive: Counter = Counter.init(0),
    // Of the reads at both exits, whether the node stands for a checked
    // position at all. A derived node - a record's field, a list's element -
    // names none, so no per-position measurement can ever cover it.
    graph_exit_read_names_checked: Counter = Counter.init(0),
    graph_exit_read_derived: Counter = Counter.init(0),
    // Why a read at a graph exit was not measured against directed translation.
    exit_read_no_contexted_provenance: Counter = Counter.init(0),
    // Measured even though the node was made under a different binding
    // context than the read holds; refusing these suppressed coverage without
    // protecting the comparison.
    exit_read_context_differed_measured: Counter = Counter.init(0),
    exit_read_module_differed_measured: Counter = Counter.init(0),
    exit_read_unreadable: Counter = Counter.init(0),
    // Of reads that DIVERGED, whether the node was made inside an edge-naming
    // request scope. This decides whether carrying the binding from that edge
    // can close the gap, or whether no recorded route reaches the position.
    diverged_node_inside_request_edge: Counter = Counter.init(0),
    diverged_node_outside_request_edge: Counter = Counter.init(0),
    // For a diverging position, whether the checked data names its unbound
    // variable's owning definition at the request edge it entered through.
    position_bound_by_edge_level: Counter = Counter.init(0),
    // A level built from the one site a module records for a definition, where
    // no entering edge named one.
    level_from_unique_site: Counter = Counter.init(0),
    unique_site_ambiguous: Counter = Counter.init(0),
    unique_site_absent: Counter = Counter.init(0),
    divergence_site_at_request_edge: Counter = Counter.init(0),
    divergence_no_site_at_request_edge: Counter = Counter.init(0),
    divergence_no_request_edge: Counter = Counter.init(0),
    // For an edgeless divergence, whether the checked data records its
    // definition's instantiation anywhere in the position's module.
    edgeless_owner_has_site_somewhere: Counter = Counter.init(0),
    edgeless_owner_has_no_site_anywhere: Counter = Counter.init(0),
    edgeless_no_free_variable: Counter = Counter.init(0),
    edgeless_free_var_unowned: Counter = Counter.init(0),
    // What checking holds for a variable no scheme generalizes: whether it is
    // still a variable there, what it carries, and whether checking classified
    // it as a residual at all.
    // Whether a rigid no binder list holds is reachable from the root of the
    // definition being specialized, which says whether its binder list omits a
    // parameter of its own signature.
    // Whether the rigid appears in ANY definition's signature in the module.
    unowned_rigid_in_some_scheme_root: Counter = Counter.init(0),
    unowned_rigid_in_no_scheme_root: Counter = Counter.init(0),
    // Whether a rigid in no LOCAL signature is a parameter of an IMPORTED
    // definition: already among the projected binders, present in a projected
    // root but missing from its binder list, or in no signature anywhere.
    unowned_rigid_is_imported_binder: Counter = Counter.init(0),
    unowned_rigid_in_imported_root_not_binders: Counter = Counter.init(0),
    unowned_rigid_in_no_root_at_all: Counter = Counter.init(0),
    // Whether the parameter stands in a scheme's PRISTINE snapshot root even
    // though the final recorded root no longer mentions it.
    unowned_rigid_in_pristine_root_only: Counter = Counter.init(0),
    unowned_rigid_in_no_pristine_root_either: Counter = Counter.init(0),
    // Whether the parameter is a nominal TYPE declaration's formal parameter,
    // which a nominal's arguments bind at each use rather than any scheme.
    unowned_rigid_is_nominal_declaration_parameter: Counter = Counter.init(0),
    unowned_rigid_in_nothing_at_all: Counter = Counter.init(0),
    unowned_rigid_reachable_from_frame_scheme: Counter = Counter.init(0),
    unowned_rigid_outside_frame_scheme: Counter = Counter.init(0),
    unowned_frame_owner_has_no_scheme: Counter = Counter.init(0),
    unowned_no_frame_for_module: Counter = Counter.init(0),
    unowned_var_is_flex: Counter = Counter.init(0),
    unowned_var_is_rigid: Counter = Counter.init(0),
    unowned_var_is_not_a_variable: Counter = Counter.init(0),
    unowned_var_has_constraints: Counter = Counter.init(0),
    unowned_var_has_numeric_default: Counter = Counter.init(0),
    unowned_var_has_row_default: Counter = Counter.init(0),
    unowned_var_has_disposition: Counter = Counter.init(0),
    unowned_var_has_no_disposition: Counter = Counter.init(0),
    divergence_no_free_variable: Counter = Counter.init(0),
    divergence_free_var_unowned: Counter = Counter.init(0),
    // What a read naming no checked position is, by node shape.
    derived_read_redirect: Counter = Counter.init(0),
    derived_read_unresolved: Counter = Counter.init(0),
    derived_read_primitive: Counter = Counter.init(0),
    derived_read_list: Counter = Counter.init(0),
    derived_read_box: Counter = Counter.init(0),
    derived_read_tuple: Counter = Counter.init(0),
    derived_read_func: Counter = Counter.init(0),
    derived_read_tag_union: Counter = Counter.init(0),
    derived_read_record: Counter = Counter.init(0),
    derived_read_other: Counter = Counter.init(0),
    // Whether a composite read's own components name checked positions, which
    // says whether it is correct by construction or owes a declared rule.
    derived_components_all_nameable: Counter = Counter.init(0),
    derived_components_partly_nameable: Counter = Counter.init(0),
    derived_components_none_nameable: Counter = Counter.init(0),
    derived_components_none: Counter = Counter.init(0),
    derived_components_not_composite: Counter = Counter.init(0),
    // Whether a derived read's whole structure bottoms out in checked-named
    // nodes, asked recursively rather than one level deep.
    derived_structure_grounded: Counter = Counter.init(0),
    derived_structure_ungrounded: Counter = Counter.init(0),
    derived_structure_unallocatable: Counter = Counter.init(0),

    // Whether a graph-built function node still carries the argument count the
    // checked function type recorded (reunify.md 15.1b). A contradiction names
    // a defect in the solver being deleted, not a gap in the data replacing it.
    graph_arity_matches_checked: Counter = Counter.init(0),
    graph_arity_contradicts_checked: Counter = Counter.init(0),
    graph_arity_not_a_function_node: Counter = Counter.init(0),

    seam_direct_diverged: Counter = Counter.init(0),

    direct_probe_population: Counter = Counter.init(0),
    direct_stored_match: Counter = Counter.init(0),
    /// A root whose two stored forms are the same rooted graph reached through
    /// different entry paths: their unfoldings agree while their stored digests
    /// encode the recursive back reference at different visiting-stack
    /// positions (reunify.md section 8.3). This is the probe's copy of the
    /// rehearsal's `rehearsal_type_equal_under_rerooting` class, so the
    /// required-zero `direct_stored_mismatch_logical` counts only content
    /// differences.
    direct_stored_equal_under_rerooting: Counter = Counter.init(0),
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
    // The engine refused a representation the producer finalized at a mirrored
    // position, because that position's slot models no producer representation
    // or the incoming tier sits below the one the class already carries. Like a
    // refused relate, this is mirror-side slot-shape imprecision and is recorded
    // rather than asserted.
    representation_mirror_adopt_rejected: Counter = Counter.init(0),
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
    // Where each residual-materialization mismatch's empty tag union entered the
    // binding, followed back through every inherited binding to the head of its
    // cascade. `unresolved_context` is a request made under a specialization
    // whose own environment never resolved (the `edgeless_binders_*` classes
    // name which); `scheme_binder` is an actual naming a binder no chain level
    // binds; `disposed_here`, `disposed_elsewhere`, and `undisposed` are the
    // reunify.md 7.4 disposition classes, split by whether the disposition the
    // checked module records for that variable belongs to the requesting body
    // context; `closed_empty_row` is an actual whose empty row is
    // checked content rather than a materialization; `absent` is a binding no
    // residual actual reached, so the position produced its own.
    rehearsal_unbound_origin_absent: Counter = Counter.init(0),
    rehearsal_unbound_origin_unresolved_context: Counter = Counter.init(0),
    rehearsal_unbound_origin_scheme_binder: Counter = Counter.init(0),
    rehearsal_unbound_origin_disposed_here: Counter = Counter.init(0),
    rehearsal_unbound_origin_disposed_elsewhere: Counter = Counter.init(0),
    rehearsal_unbound_origin_undisposed: Counter = Counter.init(0),
    rehearsal_unbound_origin_closed_empty_row: Counter = Counter.init(0),
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
    // An actual whose translated value carries the empty tag union while the
    // actual itself reaches no checked variable: the empty row is content the
    // requesting body really names, not a residual materialization.
    rehearsal_actual_residual_closed_empty_row: Counter = Counter.init(0),
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
    // it must be the same type as the checked callable the request names, or —
    // where the request names only a receiver — the scheme root's own dispatch
    // argument must be the same type as that receiver (reunify.md 7.5). A
    // binding whose witness is absent or disagrees is
    // released. The per-rule split is dumped as `rehearsal_generated_rule`
    // lines, because summing rules that bind with rules that stay unbound would
    // hide exactly the distinction section 9.6 requires.
    rehearsal_generated_rule_declared_unbound: Counter = Counter.init(0),
    rehearsal_generated_rule_caller_module_absent: Counter = Counter.init(0),
    rehearsal_generated_rule_scheme_captures: Counter = Counter.init(0),
    rehearsal_generated_rule_receiver_untranslatable: Counter = Counter.init(0),
    rehearsal_generated_rule_receiver_path_absent: Counter = Counter.init(0),
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
    // A specialization recorded while its requesting body lowered and reserved
    // only after that body's graph froze carries its request scope on the
    // record. `held_checked` and `held_generated` are the scopes that named an
    // edge, `held_without_edge` a recording request whose use named none, and
    // `hold_without_scope` a record made under no request scope at all.
    rehearsal_request_held_checked: Counter = Counter.init(0),
    rehearsal_request_held_generated: Counter = Counter.init(0),
    rehearsal_request_held_without_edge: Counter = Counter.init(0),
    rehearsal_request_hold_without_scope: Counter = Counter.init(0),
    // A dispatch target the binding step reaches, by whether the checked data
    // names a callee scheme for it. `procedure_scheme` is a procedure target
    // whose template carries one; `local_proc_scheme` is a locally defined
    // target whose own nested definition owns a scheme the owner index names;
    // `local_proc_no_scheme` is one no such scheme owns;
    // `local_proc_captures` is one whose scheme captures enclosing binders, so a
    // call-site binding cannot state it and the section 7.3 lexical chain owns
    // it; `structural` is a structural target, which names no callee body.
    rehearsal_dispatch_target_procedure_scheme: Counter = Counter.init(0),
    rehearsal_dispatch_target_procedure_no_scheme: Counter = Counter.init(0),
    rehearsal_dispatch_target_local_proc_scheme: Counter = Counter.init(0),
    rehearsal_dispatch_target_local_proc_no_scheme: Counter = Counter.init(0),
    rehearsal_dispatch_target_local_proc_captures: Counter = Counter.init(0),
    rehearsal_dispatch_target_structural: Counter = Counter.init(0),
    // Why one opened callee binding did or did not resolve to a dense binding
    // of the callee scheme's own binders. `resolved_by_site` is the recorded
    // instantiation site the requesting body named; `resolved_by_rule` is a
    // declared generated-edge rule standing in where checking recorded no site.
    // The unresolved classes are the ones whose binders are then read under no
    // binding at all, which is what an informative `scheme_binder_unbound`
    // execution reports downstream.
    // Why one declared generated-edge rule failed to state its callee binding.
    // `receiver_argument_free` is the rule's receiver emitting to a head that
    // takes no arguments, so no argument of it can supply a binder value.
    rehearsal_rule_receiver_untranslatable: Counter = Counter.init(0),
    rehearsal_rule_receiver_path_absent: Counter = Counter.init(0),
    rehearsal_rule_receiver_not_applied: Counter = Counter.init(0),
    rehearsal_rule_receiver_argument_free: Counter = Counter.init(0),
    rehearsal_rule_receiver_argument_free_one_binder: Counter = Counter.init(0),
    rehearsal_rule_receiver_arity_differs: Counter = Counter.init(0),
    rehearsal_rule_witness_disagreed: Counter = Counter.init(0),
    rehearsal_callee_resolved_by_site: Counter = Counter.init(0),
    rehearsal_callee_resolved_by_rule: Counter = Counter.init(0),
    rehearsal_callee_scheme_without_binders: Counter = Counter.init(0),
    rehearsal_callee_site_absent: Counter = Counter.init(0),
    rehearsal_callee_site_absent_both_present_unpaired: Counter = Counter.init(0),
    rehearsal_callee_site_absent_use_owned_elsewhere: Counter = Counter.init(0),
    rehearsal_callee_site_absent_owner_used_elsewhere: Counter = Counter.init(0),
    rehearsal_callee_site_absent_unrecorded: Counter = Counter.init(0),
    // The unresolved classes split by whether the callee scheme generalizes
    // anything. Only a scheme with binders can leave one unbound, so the
    // with-binders counts are the population that reaches the informative
    // constraint executions; the rest need no binding at all.
    // Where an informative `scheme_binder_unbound` execution came from: under
    // an unresolved callee binding, under a resolved one, or with no callee
    // binding open at all.
    // Whether the unnamed binder belongs to the scheme the operand was
    // translated under, or to another scheme it reaches from outside.
    rehearsal_unbound_binder_of_translating_scheme: Counter = Counter.init(0),
    rehearsal_unbound_binder_of_caller_frame_scheme: Counter = Counter.init(0),
    rehearsal_unbound_binder_of_third_scheme: Counter = Counter.init(0),
    // The third scheme by the kind of definition that owns it.
    rehearsal_unbound_binder_third_top_level: Counter = Counter.init(0),
    rehearsal_unbound_binder_third_nested: Counter = Counter.init(0),
    rehearsal_unbound_binder_third_required: Counter = Counter.init(0),
    rehearsal_unbound_binder_third_synthetic: Counter = Counter.init(0),
    rehearsal_unbound_binder_third_multi_binder: Counter = Counter.init(0),
    // Whether the third definition is specializing in an outer frame whose
    // binding already holds the value the position needs.
    rehearsal_unbound_binder_third_in_outer_frame: Counter = Counter.init(0),
    rehearsal_unbound_binder_third_no_frame_anywhere: Counter = Counter.init(0),
    // Whether a recorded instantiation site in the position's own module names
    // the third definition, which says whether the value exists and only its
    // selecting key is missing, or no edge states it at all.
    rehearsal_unbound_binder_third_has_recorded_site: Counter = Counter.init(0),
    rehearsal_unbound_binder_third_has_no_recorded_site: Counter = Counter.init(0),
    // Whether one use expression carries a site for both the third definition
    // and the scheme the operand translates under, so a single key selects both.
    rehearsal_unbound_binder_third_co_located_use: Counter = Counter.init(0),
    rehearsal_unbound_binder_third_separate_use: Counter = Counter.init(0),

    rehearsal_unbound_binder_no_frame: Counter = Counter.init(0),
    // Whether the variable is generalized by exactly one scheme in the view.
    rehearsal_unbound_binder_owned_by_one_scheme: Counter = Counter.init(0),
    rehearsal_unbound_binder_owned_by_many_schemes: Counter = Counter.init(0),

    rehearsal_binder_unbound_under_unready_callee: Counter = Counter.init(0),
    rehearsal_binder_unbound_callee_ready: Counter = Counter.init(0),
    rehearsal_binder_unbound_no_callee_open: Counter = Counter.init(0),
    rehearsal_callee_site_absent_scheme_with_binders: Counter = Counter.init(0),
    rehearsal_callee_site_absent_scheme_without_binders: Counter = Counter.init(0),
    rehearsal_callee_unresolved_no_rule_with_binders: Counter = Counter.init(0),
    rehearsal_callee_unresolved_rule_failed_with_binders: Counter = Counter.init(0),

    rehearsal_callee_site_bind_failed: Counter = Counter.init(0),
    rehearsal_callee_unresolved_defining_module_absent: Counter = Counter.init(0),
    rehearsal_callee_unresolved_scheme_absent: Counter = Counter.init(0),
    rehearsal_callee_unresolved_captures: Counter = Counter.init(0),
    rehearsal_callee_unresolved_no_site_no_rule: Counter = Counter.init(0),
    rehearsal_callee_unresolved_rule_bind_failed: Counter = Counter.init(0),
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
    // reunify.md section 10: directed translation EMITTING representation rather
    // than reading it. `emission_drafts_built` counts roots rerun as a draft
    // because the eager walk reached a position whose runtime encoding the
    // checked data does not dictate (or a recursive cycle);
    // `emission_positions_opened` counts those positions, and
    // `emission_slots_sealed` the ones whose slot sealed with its
    // representation-erased identity intact. `emission_seal_identity_lost` is the
    // required-zero counter: a relation that moved a position's identity rather
    // than only its runtime encoding. `emission_seal_moved` counts a position
    // whose sealed encoding is not the declared one, which is exactly the
    // content a producer supplied. The `emission_input_*` counters cover the
    // declared producer inputs: how many were stated, and the refusals — a tier
    // the declared order will not move back down to, an operand the engine holds
    // to be a different identity, and a position whose shape carries no
    // descriptor to adopt one into.
    emission_drafts_built: Counter = Counter.init(0),
    emission_positions_opened: Counter = Counter.init(0),
    emission_slots_sealed: Counter = Counter.init(0),
    emission_seal_identity_lost: Counter = Counter.init(0),
    emission_seal_moved: Counter = Counter.init(0),
    emission_input_declared: Counter = Counter.init(0),
    emission_input_refused_tier: Counter = Counter.init(0),
    emission_input_refused_identity: Counter = Counter.init(0),
    emission_input_not_modelled: Counter = Counter.init(0),
    // The emitted population inside the directed stored-form probe: the roots
    // whose content includes representation the section 10 layer emitted rather
    // than read out of the checked module. Each is classified by the same
    // comparison against the graph's sealed type as every other root, so
    // `emission_root_match` plus `emission_root_equal_under_rerooting` against
    // `emission_root_mismatch` says whether a sealed slot's stored type is the
    // type the graph seals at that position.
    emission_root_compared: Counter = Counter.init(0),
    emission_root_match: Counter = Counter.init(0),
    emission_root_equal_under_rerooting: Counter = Counter.init(0),
    emission_root_mismatch: Counter = Counter.init(0),
    // reunify.md sections 9 and 13 Slice 7: the constraint-replay totals over
    // every `UnifySite`. `unify_site_informative` is the number that decides how
    // much of body lowering's unification the flip must replace rather than
    // delete; the per-site table in the dump says exactly where those sit.
    unify_site_redundant: Counter = Counter.init(0),
    // How many of the redundant executions were decided by the graph-built
    // side's open positions rather than by the two sides being equal outright.
    unify_site_redundant_open_on_import: Counter = Counter.init(0),
    unify_site_informative: Counter = Counter.init(0),
    unify_site_representation_decision: Counter = Counter.init(0),
    unify_site_unmeasurable: Counter = Counter.init(0),
    unify_site_construction: Counter = Counter.init(0),
};

/// The single process-wide census. A corpus run accumulates into it and the
/// pipeline dumps it once lowering finishes.
pub var global: Census = .{};

/// Per constraint-replay site, how its executions classified.
pub var unify_site_outcomes = [_][unify_outcome_count]Counter{
    [_]Counter{Counter.init(0)} ** unify_outcome_count,
} ** unify_site_count;

/// Per constraint-replay site, why its unmeasurable executions were blocked.
pub var unify_site_blockers = [_][unify_blocker_count]Counter{
    [_]Counter{Counter.init(0)} ** unify_blocker_count,
} ** unify_site_count;

/// Per constraint-replay site, what its informative executions disagreed about.
pub var unify_site_information = [_][unify_information_count]Counter{
    [_]Counter{Counter.init(0)} ** unify_information_count,
} ** unify_site_count;

/// Per constraint-replay site, how many of its redundant executions were
/// decided by the graph-built side's open positions rather than by the two
/// sides being equal outright.
pub var unify_site_open_on_import = [_]Counter{Counter.init(0)} ** unify_site_count;

/// Per constraint-replay site, what kind of operand carried the empty tag union
/// in its informative executions.
pub var unify_site_residual_origins = [_][unify_origin_count]Counter{
    [_]Counter{Counter.init(0)} ** unify_origin_count,
} ** unify_site_count;

/// Per constraint-replay site, what named the value of that residual's checked
/// variable in its informative executions.
pub var unify_site_residual_states = [_][unify_residual_state_count]Counter{
    [_]Counter{Counter.init(0)} ** unify_residual_state_count,
} ** unify_site_count;

/// Add one to the named counter. Inert outside Debug builds. `name` is a
/// field of `Census`, checked at compile time.
pub inline fn bump(comptime name: []const u8) void {
    if (!enabled) return;
    _ = @field(global, name).fetchAdd(1, .monotonic);
}

/// Record how one execution of one constraint-replay site classified, both in
/// that site's own row and in the corresponding total. Inert outside Debug.
pub fn bumpUnifySite(site: UnifySite, outcome: UnifySiteOutcome) void {
    if (comptime !enabled) return;
    _ = unify_site_outcomes[@intFromEnum(site)][@intFromEnum(outcome)].fetchAdd(1, .monotonic);
    switch (outcome) {
        .redundant => bump("unify_site_redundant"),
        .informative => bump("unify_site_informative"),
        .representation_decision => bump("unify_site_representation_decision"),
        .unmeasurable => bump("unify_site_unmeasurable"),
        .construction => bump("unify_site_construction"),
    }
}

/// Record why one unmeasurable execution of one constraint-replay site had no
/// directed answer. Inert outside Debug.
pub fn bumpUnifySiteBlocker(site: UnifySite, blocker: UnifySiteBlocker) void {
    if (comptime !enabled) return;
    _ = unify_site_blockers[@intFromEnum(site)][@intFromEnum(blocker)].fetchAdd(1, .monotonic);
}

/// Record what one informative execution of one constraint-replay site
/// disagreed about. Inert outside Debug.
pub fn bumpUnifySiteInformation(site: UnifySite, information: UnifySiteInformation) void {
    if (comptime !enabled) return;
    _ = unify_site_information[@intFromEnum(site)][@intFromEnum(information)].fetchAdd(1, .monotonic);
}

/// Record that one redundant execution was decided by the graph-built side's
/// open positions rather than by outright equality. Inert outside Debug.
pub fn bumpUnifySiteOpenOnImport(site: UnifySite) void {
    if (comptime !enabled) return;
    _ = unify_site_open_on_import[@intFromEnum(site)].fetchAdd(1, .monotonic);
    bump("unify_site_redundant_open_on_import");
}

/// Record which kind of operand carried one informative execution's empty tag
/// union, and what named that residual's checked variable. Inert outside Debug.
pub fn bumpUnifySiteResidual(
    site: UnifySite,
    origin: UnifySiteOperandOrigin,
    state: UnifySiteResidualState,
) void {
    if (comptime !enabled) return;
    _ = unify_site_residual_origins[@intFromEnum(site)][@intFromEnum(origin)].fetchAdd(1, .monotonic);
    _ = unify_site_residual_states[@intFromEnum(site)][@intFromEnum(state)].fetchAdd(1, .monotonic);
}

/// Append one formatted fragment of the dump to `out`.
fn appendCounterLine(
    allocator: std.mem.Allocator,
    out: *std.ArrayList(u8),
    comptime format: []const u8,
    args: anytype,
) std.mem.Allocator.Error!void {
    const text = try std.fmt.allocPrint(allocator, format, args);
    defer allocator.free(text);
    try out.appendSlice(allocator, text);
}

/// Append one `name=value` column per member of a classification enum, in
/// declaration order, so the dump names every class the code declares.
fn appendClassColumns(
    allocator: std.mem.Allocator,
    out: *std.ArrayList(u8),
    comptime Class: type,
    comptime prefix: []const u8,
    counters: []const Counter,
) std.mem.Allocator.Error!void {
    inline for (@typeInfo(Class).@"enum".fields) |field| {
        const value = counters[@intFromEnum(@field(Class, field.name))].load(.monotonic);
        try appendCounterLine(allocator, out, " " ++ prefix ++ field.name ++ "={d}", .{value});
    }
}

/// Render every counter as a `name value` line. Inert outside Debug builds.
/// The caller owns and frees the returned bytes.
pub fn dumpText(allocator: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    if (enabled) {
        @setEvalBranchQuota(4000);
        inline for (@typeInfo(Census).@"struct".fields) |field| {
            const value = @field(global, field.name).load(.monotonic);
            const line = try std.fmt.allocPrint(allocator, "{s} {d}\n", .{ field.name, value });
            defer allocator.free(line);
            try out.appendSlice(allocator, line);
        }
        // One row per constraint-replay site, including sites the corpus never
        // reached: an unexecuted site is a finding of its own, so the table
        // states every declared site rather than only the ones that ran. Each
        // column is named by the enum member it counts, with the residual
        // origin and state columns prefixed so no two classifications collide.
        for (unify_site_outcomes, unify_site_blockers, unify_site_information, 0..) |outcomes, blockers, information, index| {
            const site: UnifySite = @enumFromInt(index);
            try appendCounterLine(allocator, &out, "unify_site name={s}", .{@tagName(site)});
            try appendClassColumns(allocator, &out, UnifySiteOutcome, "", &outcomes);
            try appendClassColumns(allocator, &out, UnifySiteBlocker, "", &blockers);
            try appendClassColumns(allocator, &out, UnifySiteInformation, "", &information);
            try appendCounterLine(allocator, &out, " open_on_import={d}", .{unify_site_open_on_import[index].load(.monotonic)});
            try appendClassColumns(allocator, &out, UnifySiteOperandOrigin, "residual_origin_", &unify_site_residual_origins[index]);
            try appendClassColumns(allocator, &out, UnifySiteResidualState, "residual_state_", &unify_site_residual_states[index]);
            try out.append(allocator, '\n');
        }
    }
    return out.toOwnedSlice(allocator);
}

/// When `ROC_REUNIFY_CENSUS` names a file, append the counter dump to it.
/// The census owns this write directly through libc — it is Debug-only
/// measurement plumbing, deliberately outside the compiler's file-system
/// abstraction, and every failure is silent so lowering is never affected.
///
/// Each counter is a running total for the one process that holds it, and a
/// corpus run fans out across many processes that all append here. The block
/// therefore opens with the writing process id, so a reader takes each
/// writer's last block as that writer's total and adds those, instead of
/// adding a writer's running totals to each other. The whole block is one
/// write, so the id and the counters it labels cannot be separated.
pub fn appendDumpToEnvPath(allocator: std.mem.Allocator) void {
    if (comptime !enabled) return;
    // The dump channel appends through libc with O_APPEND; that interface is
    // not present here, and the census is a local measurement tool.
    if (comptime @import("builtin").os.tag == .windows) return;
    const raw_path = std.c.getenv("ROC_REUNIFY_CENSUS") orelse return;
    const path = raw_path[0..std.mem.len(raw_path)];
    if (path.len == 0) return;
    const text = dumpText(allocator) catch return;
    defer allocator.free(text);
    if (text.len == 0) return;
    const block = std.fmt.allocPrint(allocator, "census_writer {d}\n{s}", .{ std.c.getpid(), text }) catch return;
    defer allocator.free(block);
    appendToFile(raw_path, block);
}

/// Append bytes to the named file through libc with `O_APPEND`, so multiple
/// processes measuring one corpus interleave whole writes rather than
/// clobbering each other's offsets.
pub fn appendToFile(path: [*:0]const u8, bytes: []const u8) void {
    if (comptime !enabled) return;
    if (comptime @import("builtin").os.tag == .windows) return;
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
