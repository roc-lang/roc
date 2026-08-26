//! Boxy representation planner.
//!
//! The planner records explicit data consumed by the boxy lowerer: how each
//! checked type is represented internally, which descriptor data dynamic
//! positions require, and which static-dispatch constraints require dictionary
//! arguments. It only consumes checked type data.

const std = @import("std");
const can = @import("can");
const check = @import("check");
const collections = @import("collections");
const Common = @import("../common.zig");
const test_fixtures = @import("test_fixtures.zig");

/// Shared Boxy stage-test fixtures, aliased so every stage builds the same
/// synthetic checked payloads from one definition.
const fixtureTableIndex = test_fixtures.tableIndex;
const builtinNominal = test_fixtures.builtinNominal;

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const checked_names = check.CanonicalNames;
const static_dispatch = check.StaticDispatchRegistry;
const RecordFieldLabelId = @TypeOf(@as(checked.CheckedRecordField, undefined).name);
const TagLabelId = @TypeOf(@as(checked.CheckedTag, undefined).name);
const MethodNameId = @TypeOf(@as(checked.CheckedStaticDispatchConstraint, undefined).fn_name);
const StaticDispatchOrigin = @TypeOf(@as(checked.CheckedStaticDispatchConstraint, undefined).origin);
const NumeralInfo = std.meta.Child(@TypeOf(@as(checked.CheckedStaticDispatchConstraint, undefined).numeralInfo()));

const empty_interface_capabilities = checked.ModuleInterfaceCapabilities{};
const empty_resolved_value_refs = checked.ResolvedValueRefTable{};
const empty_checked_procedure_templates = checked.CheckedProcedureTemplateTable{};
const empty_top_level_procedure_bindings = checked.TopLevelProcedureBindingTable{};
const empty_compile_time_roots = checked.CompileTimeRootTable{};
const empty_nested_proc_sites = checked.NestedProcSiteTable{};
const empty_entry_wrappers = checked.EntryWrapperTable{};
const empty_intrinsic_wrappers = checked.IntrinsicWrapperTable{};
const empty_hosted_procs = checked.HostedProcTable{};
const empty_static_dispatch_plans = static_dispatch.StaticDispatchPlanTable{};
const empty_method_registry = static_dispatch.MethodRegistry{};

/// Stable index of a checked type's planned Boxy representation.
pub const TypeRepId = enum(u32) { _ };
/// Stable index of a requested root in a program plan.
pub const RootPlanId = enum(u32) { _ };
/// Stable index of a lowered worker in a program plan.
pub const WorkerPlanId = enum(u32) { _ };
/// Stable index of an explicit runtime descriptor requirement.
pub const DescriptorRequirementId = enum(u32) { _ };
/// Stable index of an explicit runtime dictionary requirement.
pub const DictionaryRequirementId = enum(u32) { _ };

/// Module-qualified checked type reference used throughout planning.
pub const CheckedTypeIdentity = struct {
    module: checked.ModuleId = .{},
    ty: checked.CheckedTypeId,
};

/// Module-qualified identity of producer-owned monomorphic type evidence in a
/// checked module's `ConstStore`.
pub const StoredTypeIdentity = struct {
    module: checked.ModuleId,
    ty: check.ConstStore.ConstTypeId,
};

/// Module-qualified checked expression reference used throughout planning.
pub const CheckedExprIdentity = struct {
    module: checked.ModuleId = .{},
    expr: checked.CheckedExprId,
};

const CheckedPatternIdentity = struct {
    module: checked.ModuleId = .{},
    pattern: checked.CheckedPatternId,
};

const CheckedStatementIdentity = struct {
    module: checked.ModuleId = .{},
    statement: checked.CheckedStatementId,
};

/// Compact start and length pair into an append-only plan table.
pub const Span = extern struct {
    start: u32 = 0,
    len: u32 = 0,

    pub fn empty() Span {
        return .{};
    }
};

/// Whether a dynamic type variable is flexible or rigid.
pub const DynamicKind = enum {
    flex,
    rigid,
};

/// Representation policy for a nominal checked type.
pub const NominalKind = enum {
    transparent,
    opaque_nominal,
    builtin_other,
};

/// Complete planned representation category for one checked type.
pub const RepresentationKind = union(enum) {
    in_progress,
    dynamic: DynamicKind,
    primitive: checked.CheckedPrimitive,
    bool_tag_union,
    erased_callable: checked.CheckedFunctionKind,
    alias,
    record,
    record_unbound,
    tuple,
    nominal: NominalKind,
    list,
    box,
    generated_field,
    generated_field_names,
    generated_tag_union_spec,
    empty_record,
    tag_union,
    empty_tag_union,
};

/// Structural relationship between a representation and one child.
pub const ChildRole = union(enum) {
    alias_backing,
    alias_arg: u32,
    nominal_backing,
    nominal_arg: u32,
    nominal_padding_field: u32,
    record_field: RecordFieldLabelId,
    record_ext,
    tuple_elem: u32,
    function_arg: u32,
    function_ret,
    tag_payload: struct {
        tag: TagLabelId,
        index: u32,
    },
    tag_ext,
    list_elem,
    box_payload,
};

/// Whether a child is part of the runtime value represented by its parent.
/// Function signatures and alias/nominal type arguments are type-level
/// relations; erased callables and their result descriptors own those
/// boundaries independently.
pub fn childCarriesRuntimeDescriptor(role: ChildRole) bool {
    return switch (role) {
        .alias_backing,
        .nominal_backing,
        .record_field,
        .record_ext,
        .tuple_elem,
        .tag_payload,
        .tag_ext,
        .list_elem,
        .box_payload,
        => true,
        .alias_arg,
        .nominal_arg,
        .nominal_padding_field,
        .function_arg,
        .function_ret,
        => false,
    };
}

/// One explicitly analyzed child of a type representation.
pub const RepChild = struct {
    role: ChildRole,
    source_type: CheckedTypeIdentity,
    rep: TypeRepId,
    record_field_kind: checked.CheckedFieldKind = .required,
};

/// One tag name and its planned payload representation span.
pub const TagVariant = struct {
    name: TagLabelId,
    name_module: checked.ModuleId = .{},
    payloads: Span = .{},
};

/// Source and representation metadata for one declared aggregate field.
pub const DeclaredField = struct {
    index: u16,
    source_type: CheckedTypeIdentity,
    rep: TypeRepId,
    is_padding: bool = false,
};

/// Mapping from a module-qualified checked type to its representation id.
pub const TypeRepBinding = struct {
    source_type: CheckedTypeIdentity,
    rep: TypeRepId,
};

/// Mapping from exact stored monomorphic type evidence to its Boxy
/// representation. Stored representations are separate from checked-type
/// bindings because one generalized checked type may have several concrete
/// `ConstStore` instantiations.
pub const StoredTypeRepBinding = struct {
    source_type: StoredTypeIdentity,
    rep: TypeRepId,
};

/// Exact substitution from one nominal declaration backing parameter to the
/// corresponding argument on this nominal use.
pub const NominalBackingArgSubstitution = struct {
    arg_index: u32,
    formal_rep: TypeRepId,
    actual_rep: TypeRepId,
};

/// Runtime field-order policy selected from checked nominal metadata.
pub const RecordFieldOrder = enum(u8) {
    structural,
    declared,
};

/// Planner output describing one checked type's complete representation.
pub const TypeRepresentation = struct {
    source_type: CheckedTypeIdentity,
    kind: RepresentationKind,
    children: Span = .{},
    tag_variants: Span = .{},
    /// Explicit nominal field metadata used by aggregate descriptor lowering.
    /// `record_field_order` independently controls its runtime layout.
    declared_fields: Span = .{},
    record_field_order: RecordFieldOrder = .structural,
    nominal_backing_arg_substitutions: Span = .{},
    dictionaries: Span = .{},
    descriptor: ?DescriptorRequirementId = null,
    /// Set when this planned representation transitively stores at least one
    /// dynamic child whose ownership is descriptor-defined. Lowering consumes
    /// `contains_dynamic` directly; committed layouts are not an input.
    contains_dynamic: bool = false,
    /// This representation is the checker-defined `[#Missing, #Present(a)]`
    /// storage convention used for an optional or still-parametric field kind.
    /// The discriminant is explicit so later stages never infer the field's
    /// presence kind from tag names or representation shape.
    presence_slot_present_discriminant: ?u16 = null,
    /// The source nominal type declared itself opaque: inspect must not
    /// reveal the backing structure.
    inspect_opaque: bool = false,
};

/// Reason a representation must carry an explicit runtime descriptor.
pub const DescriptorReason = enum {
    dynamic_payload,
    aggregate_contains_dynamic,
    list_element_dynamic,
    box_payload_dynamic,
};

/// Explicit runtime descriptor demanded by one planned representation.
pub const DescriptorRequirement = struct {
    source_type: CheckedTypeIdentity,
    rep: TypeRepId,
    reason: DescriptorReason,
};

/// Hidden worker parameter that supplies a runtime type descriptor.
pub const HiddenDescriptorParam = struct {
    source_type: CheckedTypeIdentity,
    rep: TypeRepId,
    desc: DescriptorRequirementId,
};

/// A non-dictionary checked evidence slot backed by one worker descriptor
/// parameter. `hidden_desc_index` is relative to `WorkerPlan.hidden_descs`.
pub const WorkerEvidenceDescriptorParam = struct {
    evidence_index: u32,
    hidden_desc_index: u32,
};

/// Hidden worker parameter that supplies one or more method dictionaries.
pub const HiddenDictionaryParam = struct {
    source_type: CheckedTypeIdentity,
    rep: TypeRepId,
    dictionaries: Span,
};

/// Descriptor argument mapping for one direct worker call.
pub const DirectCallHiddenDescriptorArg = struct {
    worker_desc: DescriptorRequirementId,
    worker_rep: TypeRepId,
    source_type: CheckedTypeIdentity,
    rep: TypeRepId,
    source_arg_index: ?u32 = null,
    source_value_rep: ?TypeRepId = null,
};

/// Exact runtime source for one dictionary method worker descriptor.
pub const DictionaryMethodHiddenDescriptorSource = union(enum) {
    slot: u32,
    call: u32,
    argument: u32,
};

/// Exact runtime source for one descriptor in a checked dictionary method
/// requirement, in requirement traversal order.
pub const DictionaryMethodDescriptorSource = struct {
    rep: TypeRepId,
    source: Source,

    pub const Source = union(enum) {
        static_rep,
        argument: u32,
        call: u32,
    };
};

/// Dictionary argument mapping for one direct worker call.
pub const DirectCallHiddenDictionaryArg = struct {
    worker_dictionaries: Span,
    source_type: CheckedTypeIdentity,
    rep: TypeRepId,
    method_evidence: Span = .{},
    source: Source,

    pub const Source = union(enum) {
        bound_dictionaries: Span,
        static_rep: TypeRepId,
    };
};

/// Exact checked implementation and callable type for one static dictionary slot.
pub const DictionaryMethodEvidence = struct {
    requirement_type: CheckedTypeIdentity,
    callable_type: CheckedTypeIdentity,
    resolution: Resolution,
    nested_dict_args: Span = .{},
    worker_desc_args: Span = .{},
    requirement_desc_sources: Span = .{},
    requirement_desc_args: Span = .{},
    hidden_desc_sources: Span = .{},

    pub const Resolution = union(enum) {
        worker: WorkerPlanId,
        structural: static_dispatch.StructuralKind,
        constraint,
        checked_error,
        unreachable_value,
    };
};

/// One checked call-site instantiation of a worker type position.
///
/// `operand_type` is the argument expression's checked type. `call_type` is
/// the contextual type from the checker's instantiated function type for the
/// call. The corresponding reps make both checked relations explicit before
/// the final boundary into `worker_rep`.
pub const CallTypeSubstitution = struct {
    operand_type: CheckedTypeIdentity,
    operand_rep: TypeRepId,
    call_type: CheckedTypeIdentity,
    call_rep: TypeRepId,
    worker_rep: TypeRepId,
};

/// Storage role of one field in an erased callable capture.
pub const ErasedCaptureKind = enum {
    captured_value,
    hidden_desc,
    hidden_dict,
};

/// Planned value, descriptor, or dictionary captured by an erased callable.
pub const ErasedCapture = struct {
    kind: ErasedCaptureKind,
    source_type: CheckedTypeIdentity,
    rep: TypeRepId,
    desc: ?DescriptorRequirementId = null,
    body_descriptor: bool = false,
    dictionaries: Span = .{},
    body_dictionary: bool = false,
    capture_id: ?checked.CaptureId = null,
};

/// Exact checked source and worker selected for a stored function value.
pub const StaticFnPlan = struct {
    store_module: checked.ModuleId,
    fn_id: checked.ConstFnId,
    rep: TypeRepId,
    worker: WorkerPlanId,
    capture_sources: Span = .{},
};

/// One checked static-dispatch constraint requiring a runtime dictionary slot.
pub const DictionaryRequirement = struct {
    source_type: CheckedTypeIdentity,
    constraint_index: u32,
    /// Program-wide runtime slot for this checked method spelling. The slot is
    /// stable across dictionaries with different requirement subsets.
    slot: u32,
    fn_name: MethodNameId,
    fn_ty: CheckedTypeIdentity,
    origin: StaticDispatchOrigin,
    binop_negated: bool,
    num_literal: ?NumeralInfo,
};

/// One program-wide dictionary slot identity. Method ids are module-local, so
/// consumers compare their checked source spelling while planning and use only `slot`
/// after LIR lowering begins.
pub const DictionaryMethodSlotIdentity = struct {
    module: checked.ModuleId,
    method: MethodNameId,
};

/// Whether a root needs only a worker or also a host-shaped ABI wrapper.
pub const RootWrapperKind = enum {
    private_worker_only,
    host_shaped_wrapper,
};

/// Compiler-generated codec worker role used to select its exact lowering.
pub const GeneratedCodecKind = enum {
    parser_constructor,
    encoder_constructor,
    parser_runtime,
    encoder_runtime,
    encoder_record_fields,
    encoder_dict_fields,
    encoder_sequence_elements,
    encoder_tag_field,
    encoder_tag_payload_thunk,
    encoder_tag_payload_elements,
    encoder_value_thunk,
};

/// Checked source metadata for one compiler-generated codec worker.
pub const GeneratedCodecSource = struct {
    kind: GeneratedCodecKind,
    shape: CheckedTypeIdentity,
    value_type: ?CheckedTypeIdentity = null,
    optional_missing: bool = false,
    optional_null: bool = false,
    runtime_type: ?CheckedTypeIdentity = null,
    capture_type: ?CheckedTypeIdentity = null,
    contract_derivation: ?static_dispatch.GeneratedCodecDerivationId = null,
    contract_worker: ?WorkerPlanId = null,
    contract_expr: ?CheckedExprIdentity = null,
};

/// Exact checked method edge emitted by a compiler-generated codec worker.
pub const GeneratedCodecCallPlan = struct {
    caller: WorkerPlanId,
    dispatch_type: CheckedTypeIdentity,
    subject_type: ?CheckedTypeIdentity,
    method_module: checked.ModuleId,
    method: MethodNameId,
    worker: WorkerPlanId,
    arg_types: Span,
    ret_type: CheckedTypeIdentity,
    checked_evidence: Span = .{},
    hidden_desc_args: Span = .{},
    hidden_dict_args: Span = .{},
};

/// Exact runtime worker selected for one generated codec constructor.
pub const GeneratedCodecRuntimeLink = struct {
    constructor: WorkerPlanId,
    runtime: WorkerPlanId,
};

/// Exact checked schema consumed by one generated parser runtime body.
pub const GeneratedParserRuntimePlan = struct {
    worker: WorkerPlanId,
    schema_type: CheckedTypeIdentity,
};

/// Exact checked parse_tag_union subject selected for a parser-visible checked
/// shape, including aliases and transparent nominal wrappers.
pub const GeneratedParserTagCallLink = struct {
    contract_worker: WorkerPlanId,
    shape_type: CheckedTypeIdentity,
    call_shape_type: CheckedTypeIdentity,
};

/// Exact generated-parser contract consumed by one specialized
/// ParseTagUnionSpec.parse intrinsic worker.
pub const GeneratedParserTagUnionPlan = struct {
    runtime_id: u32,
    intrinsic_worker: WorkerPlanId,
    contract_worker: WorkerPlanId,
    shape_type: CheckedTypeIdentity,
    encoding_type: CheckedTypeIdentity,
    state_type: CheckedTypeIdentity,
    record_types: Span,
};

/// Which generated FieldNames iteration contract a worker implements.
pub const GeneratedFieldIteratorMode = enum {
    all,
    for_size,
};

/// One generated step worker over the fixed FieldNames runtime representation.
pub const GeneratedFieldIteratorSource = struct {
    mode: GeneratedFieldIteratorMode,
    field_names_type: CheckedTypeIdentity,
    iter_type: CheckedTypeIdentity,
    index_type: CheckedTypeIdentity,
    size_type: ?CheckedTypeIdentity = null,
};

/// First generated step worker selected for a FieldNames intrinsic wrapper.
pub const GeneratedFieldIteratorLink = struct {
    intrinsic: WorkerPlanId,
    first_step: WorkerPlanId,
};

/// One zero-argument step worker for a compiler-generated interpolation Iter.
pub const GeneratedInterpolationStepSource = struct {
    step_type: CheckedTypeIdentity,
    one_payload_type: ?CheckedTypeIdentity = null,
};

/// Exact generated workers selected for one interpolation operand use.
pub const GeneratedInterpolationPlan = struct {
    interpolation: CheckedExprIdentity,
    caller: WorkerPlanId,
    iter_rep: TypeRepId,
    one_step: WorkerPlanId,
    done_step: WorkerPlanId,
};

/// One renamed record field captured by a generated parser runtime worker.
pub const GeneratedParserFieldCapture = struct {
    worker: WorkerPlanId,
    record_type: CheckedTypeIdentity,
    field_module: checked.ModuleId,
    field_name: RecordFieldLabelId,
    source_type: CheckedTypeIdentity,
    parse_type: CheckedTypeIdentity,
    parser_wrap_ok: bool = false,
    optional_error_type: ?CheckedTypeIdentity,
    optional_missing: bool = false,
    optional_null: bool = false,
};

/// Exact checked JSON-style Try handling consumed by a generated parser.
pub const GeneratedParserTryPlan = struct {
    worker: WorkerPlanId,
    try_type: CheckedTypeIdentity,
    ok_type: CheckedTypeIdentity,
    error_type: CheckedTypeIdentity,
    missing: bool,
    null: bool,
};

/// Checked strategy selected for a generated dictionary field parser.
pub const GeneratedParserDictionaryFieldStrategy = union(enum) {
    method: struct {
        module: checked.ModuleId,
        name: MethodNameId,
    },
    unit_tags,
};

/// Checked dictionary-field parser selection for one generated Dict parser.
pub const GeneratedParserDictionaryFieldSelection = struct {
    worker: WorkerPlanId,
    key_type: CheckedTypeIdentity,
    strategy: GeneratedParserDictionaryFieldStrategy,
};

/// Exact checked Try handling consumed by a generated encoder.
pub const GeneratedEncoderTryPlan = struct {
    worker: WorkerPlanId,
    try_type: CheckedTypeIdentity,
    ok_type: CheckedTypeIdentity,
    missing: bool,
    null: bool,
};

/// Checked checked_module from which a worker body is lowered.
pub const WorkerSource = union(enum) {
    procedure_template: checked_names.ProcedureTemplateRef,
    procedure_binding: checked.ArtifactTopLevelProcedureBindingRef,
    procedure_use: checked.ProcedureUseTemplate,
    nested_expr: CheckedExprIdentity,
    generated_codec: GeneratedCodecSource,
    generated_field_iterator: GeneratedFieldIteratorSource,
    generated_interpolation_step: GeneratedInterpolationStepSource,
};

/// Const-store identity of a function value persisted into runtime code.
pub const StoredFnSource = struct {
    module: checked.ModuleId,
    fn_id: checked.ConstFnId,
};

/// Complete checked source and hidden-input plan for one worker.
pub const WorkerPlan = struct {
    id: WorkerPlanId,
    root_request: ?checked.RootRequest = null,
    source: WorkerSource,
    checked_type: CheckedTypeIdentity,
    rep: TypeRepId,
    stored_fn: ?StoredFnSource = null,
    hidden_descs: Span = .{},
    body_hidden_descs: Span = .{},
    evidence_only_descs: Span = .{},
    evidence_descs: Span = .{},
    hidden_dicts: Span = .{},
    body_hidden_dicts: Span = .{},
    erased_captures: Span = .{},
};

/// Exact producer for one explicit call argument.
pub const CallOperand = union(enum) {
    checked_expr: checked.CheckedExprId,
    generated_interpolation_iter: checked.CheckedExprId,
    generated_numeral: can.ModuleEnv.NumeralLiteral,
    generated_quote: checked.CheckedStringLiteralId,
};

/// Explicit worker, substitutions, and hidden arguments for one direct call.
pub const DirectCallPlan = struct {
    call: CheckedExprIdentity,
    caller: WorkerPlanId,
    worker: WorkerPlanId,
    source_fn_type: CheckedTypeIdentity,
    operands: Span,
    arg_substitutions: Span = .{},
    ret_substitution: ?CallTypeSubstitution = null,
    hidden_desc_args: Span = .{},
    hidden_dict_args: Span = .{},
};

/// Exact generic dictionary owner for one unresolved checked dispatch site.
pub const DictionaryDispatchPlan = struct {
    call: CheckedExprIdentity,
    caller: WorkerPlanId,
    dispatcher_rep: TypeRepId,
    method: MethodNameId,
    source_fn_type: CheckedTypeIdentity,
    operands: Span,
    arg_substitutions: Span = .{},
    ret_substitution: ?CallTypeSubstitution = null,
};

/// Exact checked use, caller, worker, and contextual callable type for one
/// nested callable expression.
pub const NestedCallableUsePlan = struct {
    use: CheckedExprIdentity,
    caller: WorkerPlanId,
    worker: WorkerPlanId,
    callable_ty: CheckedTypeIdentity,
    hidden_desc_args: Span = .{},
    hidden_dict_args: Span = .{},
};

/// Exact hidden dictionaries captured when a checked procedure lookup becomes
/// an erased callable value.
pub const CallableUsePlan = struct {
    use: CheckedExprIdentity,
    caller: WorkerPlanId,
    worker: WorkerPlanId,
    callable_ty: CheckedTypeIdentity,
    stored_fn: ?StoredFnSource = null,
    stored_capture_sources: Span = .{},
    hidden_desc_args: Span = .{},
    hidden_dict_args: Span = .{},
};

/// Exact checked producer expression for a polymorphic callable binding that
/// cannot be evaluated as a concrete compile-time root.
pub const RuntimeCallableEvalUsePlan = struct {
    use: CheckedExprIdentity,
    caller: WorkerPlanId,
    source: CheckedExprIdentity,
    callable_ty: CheckedTypeIdentity,
};

/// Producer-selected value source for one capture of a finalized callable.
pub const StoredCallableCaptureSource = struct {
    capture_id: checked.CaptureId,
    source: Source,

    pub const Source = union(enum) {
        const_node: struct {
            store_module: checked.ModuleId,
            node: checked.ConstNodeId,
            stored_type: check.ConstStore.ConstTypeId,
        },
        checked_expr: CheckedExprIdentity,
    };
};

/// Custom inspect worker selected for one source representation.
pub const InspectMethodPlan = struct {
    source_rep: TypeRepId,
    worker: WorkerPlanId,
    method_module: checked.ModuleId,
    method: MethodNameId,
};

/// Which protocol operation an iterator call performs.
pub const IteratorCallKind = enum {
    iter,
    next,
};

/// Worker and substitutions for one checked iterator protocol call.
pub const IteratorCallPlan = struct {
    module: checked.ModuleId,
    for_plan: static_dispatch.IteratorForPlanId,
    kind: IteratorCallKind,
    caller: WorkerPlanId,
    worker: WorkerPlanId,
    source_fn_type: CheckedTypeIdentity,
    arg_substitutions: Span = .{},
    ret_type: CheckedTypeIdentity,
    ret_substitution: CallTypeSubstitution,
    hidden_desc_args: Span = .{},
    hidden_dict_args: Span = .{},
};

/// Worker and return substitution for one compile-time-evaluated call.
pub const ConstEvalCallPlan = struct {
    worker: WorkerPlanId,
    ret_type: CheckedTypeIdentity,
    ret_substitution: CallTypeSubstitution,
    hidden_desc_args: Span = .{},
    hidden_dict_args: Span = .{},
};

/// Host and worker representation plan for one requested root.
pub const RootPlan = struct {
    id: RootPlanId,
    request: checked.RootRequest,
    worker: WorkerPlanId,
    wrapper_kind: RootWrapperKind,
    host_type: CheckedTypeIdentity,
    host_rep: TypeRepId,
    source_type: CheckedTypeIdentity,
    source_rep: TypeRepId,
    worker_rep: TypeRepId,
    hidden_desc_args: Span = .{},
    hidden_dict_args: Span = .{},
};

/// Target-independent Boxy representation and call plan for a checked program.
pub const ProgramPlan = struct {
    allocator: Allocator,
    roots: std.ArrayList(RootPlan),
    workers: std.ArrayList(WorkerPlan),
    direct_calls: std.ArrayList(DirectCallPlan),
    call_operands: std.ArrayList(CallOperand),
    dictionary_dispatches: std.ArrayList(DictionaryDispatchPlan),
    nested_callable_uses: std.ArrayList(NestedCallableUsePlan),
    callable_uses: std.ArrayList(CallableUsePlan),
    runtime_callable_eval_uses: std.ArrayList(RuntimeCallableEvalUsePlan),
    stored_callable_capture_sources: std.ArrayList(StoredCallableCaptureSource),
    inspect_methods: std.ArrayList(InspectMethodPlan),
    const_eval_calls: std.ArrayList(ConstEvalCallPlan),
    iterator_calls: std.ArrayList(IteratorCallPlan),
    generated_codec_calls: std.ArrayList(GeneratedCodecCallPlan),
    generated_codec_call_types: std.ArrayList(CheckedTypeIdentity),
    generated_codec_runtime_links: std.ArrayList(GeneratedCodecRuntimeLink),
    generated_parser_runtime_plans: std.ArrayList(GeneratedParserRuntimePlan),
    generated_parser_tag_call_links: std.ArrayList(GeneratedParserTagCallLink),
    generated_parser_tag_union_plans: std.ArrayList(GeneratedParserTagUnionPlan),
    generated_parser_tag_union_record_types: std.ArrayList(CheckedTypeIdentity),
    generated_field_iterator_links: std.ArrayList(GeneratedFieldIteratorLink),
    generated_interpolations: std.ArrayList(GeneratedInterpolationPlan),
    generated_parser_field_captures: std.ArrayList(GeneratedParserFieldCapture),
    generated_parser_try_plans: std.ArrayList(GeneratedParserTryPlan),
    generated_parser_dictionary_field_selections: std.ArrayList(GeneratedParserDictionaryFieldSelection),
    generated_encoder_try_plans: std.ArrayList(GeneratedEncoderTryPlan),
    root_reps: std.ArrayList(TypeRepId),
    type_reps: std.ArrayList(TypeRepBinding),
    stored_type_reps: std.ArrayList(StoredTypeRepBinding),
    representations: std.ArrayList(TypeRepresentation),
    children: std.ArrayList(RepChild),
    tag_variants: std.ArrayList(TagVariant),
    declared_fields: std.ArrayList(DeclaredField),
    nominal_backing_arg_substitutions: std.ArrayList(NominalBackingArgSubstitution),
    descriptors: std.ArrayList(DescriptorRequirement),
    hidden_descriptor_params: std.ArrayList(HiddenDescriptorParam),
    worker_evidence_descriptor_params: std.ArrayList(WorkerEvidenceDescriptorParam),
    hidden_dictionary_params: std.ArrayList(HiddenDictionaryParam),
    direct_call_hidden_desc_args: std.ArrayList(DirectCallHiddenDescriptorArg),
    direct_call_hidden_dict_args: std.ArrayList(DirectCallHiddenDictionaryArg),
    dictionary_method_evidence: std.ArrayList(DictionaryMethodEvidence),
    dictionary_method_desc_sources: std.ArrayList(DictionaryMethodDescriptorSource),
    dictionary_method_hidden_desc_sources: std.ArrayList(DictionaryMethodHiddenDescriptorSource),
    call_type_substitutions: std.ArrayList(CallTypeSubstitution),
    erased_captures: std.ArrayList(ErasedCapture),
    dictionaries: std.ArrayList(DictionaryRequirement),
    dictionary_method_slots: std.ArrayList(DictionaryMethodSlotIdentity),
    static_fns: std.ArrayList(StaticFnPlan),

    pub fn init(allocator: Allocator) ProgramPlan {
        return .{
            .allocator = allocator,
            .roots = .empty,
            .workers = .empty,
            .direct_calls = .empty,
            .call_operands = .empty,
            .dictionary_dispatches = .empty,
            .nested_callable_uses = .empty,
            .callable_uses = .empty,
            .runtime_callable_eval_uses = .empty,
            .stored_callable_capture_sources = .empty,
            .inspect_methods = .empty,
            .const_eval_calls = .empty,
            .iterator_calls = .empty,
            .generated_codec_calls = .empty,
            .generated_codec_call_types = .empty,
            .generated_codec_runtime_links = .empty,
            .generated_parser_runtime_plans = .empty,
            .generated_parser_tag_call_links = .empty,
            .generated_parser_tag_union_plans = .empty,
            .generated_parser_tag_union_record_types = .empty,
            .generated_field_iterator_links = .empty,
            .generated_interpolations = .empty,
            .generated_parser_field_captures = .empty,
            .generated_parser_try_plans = .empty,
            .generated_parser_dictionary_field_selections = .empty,
            .generated_encoder_try_plans = .empty,
            .root_reps = .empty,
            .type_reps = .empty,
            .stored_type_reps = .empty,
            .representations = .empty,
            .children = .empty,
            .tag_variants = .empty,
            .declared_fields = .empty,
            .nominal_backing_arg_substitutions = .empty,
            .descriptors = .empty,
            .hidden_descriptor_params = .empty,
            .worker_evidence_descriptor_params = .empty,
            .hidden_dictionary_params = .empty,
            .direct_call_hidden_desc_args = .empty,
            .direct_call_hidden_dict_args = .empty,
            .dictionary_method_evidence = .empty,
            .dictionary_method_desc_sources = .empty,
            .dictionary_method_hidden_desc_sources = .empty,
            .call_type_substitutions = .empty,
            .erased_captures = .empty,
            .dictionaries = .empty,
            .dictionary_method_slots = .empty,
            .static_fns = .empty,
        };
    }

    pub fn deinit(self: *ProgramPlan) void {
        self.static_fns.deinit(self.allocator);
        self.dictionary_method_slots.deinit(self.allocator);
        self.dictionaries.deinit(self.allocator);
        self.erased_captures.deinit(self.allocator);
        self.call_type_substitutions.deinit(self.allocator);
        self.dictionary_method_hidden_desc_sources.deinit(self.allocator);
        self.dictionary_method_desc_sources.deinit(self.allocator);
        self.dictionary_method_evidence.deinit(self.allocator);
        self.direct_call_hidden_dict_args.deinit(self.allocator);
        self.direct_call_hidden_desc_args.deinit(self.allocator);
        self.hidden_dictionary_params.deinit(self.allocator);
        self.worker_evidence_descriptor_params.deinit(self.allocator);
        self.hidden_descriptor_params.deinit(self.allocator);
        self.descriptors.deinit(self.allocator);
        self.nominal_backing_arg_substitutions.deinit(self.allocator);
        self.declared_fields.deinit(self.allocator);
        self.tag_variants.deinit(self.allocator);
        self.children.deinit(self.allocator);
        self.representations.deinit(self.allocator);
        self.type_reps.deinit(self.allocator);
        self.stored_type_reps.deinit(self.allocator);
        self.root_reps.deinit(self.allocator);
        self.generated_codec_call_types.deinit(self.allocator);
        self.generated_codec_runtime_links.deinit(self.allocator);
        self.generated_parser_runtime_plans.deinit(self.allocator);
        self.generated_parser_tag_call_links.deinit(self.allocator);
        self.generated_parser_tag_union_plans.deinit(self.allocator);
        self.generated_parser_tag_union_record_types.deinit(self.allocator);
        self.generated_field_iterator_links.deinit(self.allocator);
        self.generated_interpolations.deinit(self.allocator);
        self.generated_codec_calls.deinit(self.allocator);
        self.generated_parser_field_captures.deinit(self.allocator);
        self.generated_parser_try_plans.deinit(self.allocator);
        self.generated_parser_dictionary_field_selections.deinit(self.allocator);
        self.generated_encoder_try_plans.deinit(self.allocator);
        self.iterator_calls.deinit(self.allocator);
        self.const_eval_calls.deinit(self.allocator);
        self.inspect_methods.deinit(self.allocator);
        self.stored_callable_capture_sources.deinit(self.allocator);
        self.runtime_callable_eval_uses.deinit(self.allocator);
        self.callable_uses.deinit(self.allocator);
        self.nested_callable_uses.deinit(self.allocator);
        self.dictionary_dispatches.deinit(self.allocator);
        self.call_operands.deinit(self.allocator);
        self.direct_calls.deinit(self.allocator);
        self.workers.deinit(self.allocator);
        self.roots.deinit(self.allocator);
        self.* = ProgramPlan.init(self.allocator);
    }

    pub fn childSlice(self: *const ProgramPlan, span: Span) []const RepChild {
        return self.children.items[span.start .. span.start + span.len];
    }

    /// Dictionary parameters and checked evidence share the scheme order
    /// defined by `dispatch_evidence`: alias/nominal arguments precede their
    /// backing type. Runtime representation children keep backing-first order
    /// where layout lowering needs it, so dictionary traversals use this
    /// explicit view instead of inheriting layout order.
    fn dictionaryChildAt(self: *const ProgramPlan, rep_id: TypeRepId, visit_index: usize) ?RepChild {
        const rep = self.representations.items[@intFromEnum(rep_id)];
        const children = self.childSlice(rep.children);
        const class_count: usize = if (rep.kind == .alias)
            2
        else if (rep.kind == .nominal)
            3
        else
            return if (visit_index < children.len) children[visit_index] else null;

        var seen: usize = 0;
        var class: usize = 0;
        while (class < class_count) : (class += 1) {
            for (children) |child| {
                const child_class: usize = if (rep.kind == .alias)
                    if (child.role == .alias_arg)
                        0
                    else if (child.role == .alias_backing)
                        1
                    else
                        boxyPlanInvariant("boxy alias representation had a non-alias child")
                else if (rep.kind == .nominal)
                    if (child.role == .nominal_arg)
                        0
                    else if (child.role == .nominal_backing)
                        1
                    else if (child.role == .nominal_padding_field)
                        2
                    else
                        boxyPlanInvariant("boxy nominal representation had a non-nominal child")
                else
                    unreachable;
                if (child_class != class) continue;
                if (seen == visit_index) return child;
                seen += 1;
            }
        }
        return null;
    }

    pub fn tagVariantSlice(self: *const ProgramPlan, span: Span) []const TagVariant {
        return self.tag_variants.items[span.start .. span.start + span.len];
    }

    pub fn declaredFieldSlice(self: *const ProgramPlan, span: Span) []const DeclaredField {
        return self.declared_fields.items[span.start .. span.start + span.len];
    }

    pub fn nominalBackingArgSubstitutionSlice(
        self: *const ProgramPlan,
        span: Span,
    ) []const NominalBackingArgSubstitution {
        return self.nominal_backing_arg_substitutions.items[span.start .. span.start + span.len];
    }

    pub fn dictionarySlice(self: *const ProgramPlan, span: Span) []const DictionaryRequirement {
        return self.dictionaries.items[span.start .. span.start + span.len];
    }

    pub fn hiddenDescriptorParamSlice(self: *const ProgramPlan, span: Span) []const HiddenDescriptorParam {
        return self.hidden_descriptor_params.items[span.start .. span.start + span.len];
    }

    pub fn workerEvidenceDescriptorParamSlice(self: *const ProgramPlan, span: Span) []const WorkerEvidenceDescriptorParam {
        return self.worker_evidence_descriptor_params.items[span.start .. span.start + span.len];
    }

    pub fn hiddenDictionaryParamSlice(self: *const ProgramPlan, span: Span) []const HiddenDictionaryParam {
        return self.hidden_dictionary_params.items[span.start .. span.start + span.len];
    }

    pub fn directCallHiddenDescriptorArgSlice(self: *const ProgramPlan, span: Span) []const DirectCallHiddenDescriptorArg {
        return self.direct_call_hidden_desc_args.items[span.start .. span.start + span.len];
    }

    pub fn directCallHiddenDictionaryArgSlice(self: *const ProgramPlan, span: Span) []const DirectCallHiddenDictionaryArg {
        return self.direct_call_hidden_dict_args.items[span.start .. span.start + span.len];
    }

    pub fn dictionaryMethodEvidenceSlice(self: *const ProgramPlan, span: Span) []const DictionaryMethodEvidence {
        return self.dictionary_method_evidence.items[span.start .. span.start + span.len];
    }

    pub fn dictionaryMethodDescriptorSourceSlice(self: *const ProgramPlan, span: Span) []const DictionaryMethodDescriptorSource {
        return self.dictionary_method_desc_sources.items[span.start .. span.start + span.len];
    }

    pub fn dictionaryMethodHiddenDescriptorSourceSlice(
        self: *const ProgramPlan,
        span: Span,
    ) []const DictionaryMethodHiddenDescriptorSource {
        return self.dictionary_method_hidden_desc_sources.items[span.start .. span.start + span.len];
    }

    pub fn callTypeSubstitutionSlice(self: *const ProgramPlan, span: Span) []const CallTypeSubstitution {
        return self.call_type_substitutions.items[span.start .. span.start + span.len];
    }

    pub fn callOperandSlice(self: *const ProgramPlan, span: Span) []const CallOperand {
        return self.call_operands.items[span.start .. span.start + span.len];
    }

    pub fn erasedCaptureSlice(self: *const ProgramPlan, span: Span) []const ErasedCapture {
        return self.erased_captures.items[span.start .. span.start + span.len];
    }

    pub fn generatedCodecCallTypeSlice(self: *const ProgramPlan, span: Span) []const CheckedTypeIdentity {
        return self.generated_codec_call_types.items[span.start .. span.start + span.len];
    }

    pub fn directWorkerForCall(
        self: *const ProgramPlan,
        call: CheckedExprIdentity,
        caller: WorkerPlanId,
    ) ?WorkerPlanId {
        return if (self.directCallPlanForCall(call, caller)) |plan| plan.worker else null;
    }

    pub fn directCallPlanForCall(
        self: *const ProgramPlan,
        call: CheckedExprIdentity,
        caller: WorkerPlanId,
    ) ?DirectCallPlan {
        for (self.direct_calls.items) |direct| {
            if (direct.caller == caller and exprRefEql(direct.call, call)) return direct;
        }
        return null;
    }

    pub fn dictionaryDispatchPlanForCall(
        self: *const ProgramPlan,
        call: CheckedExprIdentity,
        caller: WorkerPlanId,
    ) ?DictionaryDispatchPlan {
        for (self.dictionary_dispatches.items) |dispatch| {
            if (dispatch.caller == caller and exprRefEql(dispatch.call, call)) return dispatch;
        }
        return null;
    }

    pub fn callableUsePlan(
        self: *const ProgramPlan,
        use: CheckedExprIdentity,
        caller: WorkerPlanId,
    ) ?CallableUsePlan {
        for (self.callable_uses.items) |planned| {
            if (planned.caller == caller and exprRefEql(planned.use, use)) return planned;
        }
        return null;
    }

    pub fn runtimeCallableEvalUsePlan(
        self: *const ProgramPlan,
        use: CheckedExprIdentity,
        caller: WorkerPlanId,
    ) ?RuntimeCallableEvalUsePlan {
        for (self.runtime_callable_eval_uses.items) |planned| {
            if (planned.caller == caller and exprRefEql(planned.use, use)) return planned;
        }
        return null;
    }

    pub fn storedCallableCaptureSourceSlice(
        self: *const ProgramPlan,
        span: Span,
    ) []const StoredCallableCaptureSource {
        return self.stored_callable_capture_sources.items[span.start .. span.start + span.len];
    }

    pub fn uniqueNestedCallableUseType(self: *const ProgramPlan, worker: WorkerPlanId) ?CheckedTypeIdentity {
        var found: ?CheckedTypeIdentity = null;
        for (self.nested_callable_uses.items) |use| {
            if (use.worker != worker) continue;
            if (found) |existing| {
                if (!typeRefEql(existing, use.callable_ty)) return null;
                continue;
            }
            found = use.callable_ty;
        }
        return found;
    }

    pub fn nestedCallableUsePlan(
        self: *const ProgramPlan,
        use: CheckedExprIdentity,
        caller: WorkerPlanId,
        callable_ty: CheckedTypeIdentity,
    ) ?NestedCallableUsePlan {
        for (self.nested_callable_uses.items) |planned| {
            if (planned.caller != caller or
                !exprRefEql(planned.use, use) or
                !typeRefEql(planned.callable_ty, callable_ty))
            {
                continue;
            }
            return planned;
        }
        return null;
    }

    pub fn inspectMethodForRep(self: *const ProgramPlan, source_rep: TypeRepId) ?InspectMethodPlan {
        for (self.inspect_methods.items) |method| {
            if (method.source_rep == source_rep) return method;
        }
        return null;
    }

    pub fn constEvalCallFor(self: *const ProgramPlan, worker: WorkerPlanId, ret_type: CheckedTypeIdentity) ?ConstEvalCallPlan {
        for (self.const_eval_calls.items) |call| {
            if (call.worker == worker and typeRefEql(call.ret_type, ret_type)) return call;
        }
        return null;
    }

    pub fn repForSourceType(self: *const ProgramPlan, source_type: CheckedTypeIdentity) ?TypeRepId {
        for (self.type_reps.items) |binding| {
            if (typeRefEql(binding.source_type, source_type)) return binding.rep;
        }
        return null;
    }

    pub fn repForStoredType(self: *const ProgramPlan, source_type: StoredTypeIdentity) ?TypeRepId {
        for (self.stored_type_reps.items) |binding| {
            if (moduleKeyEqual(binding.source_type.module, source_type.module) and binding.source_type.ty == source_type.ty) {
                return binding.rep;
            }
        }
        return null;
    }

    pub fn workerForSourceType(self: *const ProgramPlan, source: WorkerSource, checked_type: CheckedTypeIdentity) ?WorkerPlanId {
        for (self.workers.items) |worker| {
            if (!workerSourceEql(worker.source, source)) continue;
            if (source == .nested_expr or typeRefEql(worker.checked_type, checked_type)) return worker.id;
        }
        return null;
    }

    pub fn generatedCodecRuntimeWorker(self: *const ProgramPlan, constructor: WorkerPlanId) ?WorkerPlanId {
        for (self.generated_codec_runtime_links.items) |link| {
            if (link.constructor == constructor) return link.runtime;
        }
        return null;
    }

    pub fn generatedParserRuntimeSchema(self: *const ProgramPlan, worker: WorkerPlanId) ?CheckedTypeIdentity {
        for (self.generated_parser_runtime_plans.items) |runtime| {
            if (runtime.worker == worker) return runtime.schema_type;
        }
        return null;
    }

    pub fn generatedParserTagUnionPlanForContract(
        self: *const ProgramPlan,
        worker: WorkerPlanId,
        shape_type: CheckedTypeIdentity,
    ) ?GeneratedParserTagUnionPlan {
        for (self.generated_parser_tag_union_plans.items) |plan| {
            if (plan.contract_worker == worker and typeRefEql(plan.shape_type, shape_type)) return plan;
        }
        return null;
    }

    pub fn generatedParserTagCallShape(
        self: *const ProgramPlan,
        worker: WorkerPlanId,
        shape_type: CheckedTypeIdentity,
    ) ?CheckedTypeIdentity {
        for (self.generated_parser_tag_call_links.items) |link| {
            if (link.contract_worker == worker and typeRefEql(link.shape_type, shape_type)) return link.call_shape_type;
        }
        return null;
    }

    pub fn generatedParserTagUnionRecordTypes(self: *const ProgramPlan, span: Span) []const CheckedTypeIdentity {
        return self.generated_parser_tag_union_record_types.items[span.start..][0..span.len];
    }

    pub fn generatedParserTryPlan(
        self: *const ProgramPlan,
        worker: WorkerPlanId,
        try_type: CheckedTypeIdentity,
    ) ?GeneratedParserTryPlan {
        for (self.generated_parser_try_plans.items) |plan| {
            if (plan.worker == worker and typeRefEql(plan.try_type, try_type)) return plan;
        }
        return null;
    }

    pub fn generatedParserDictionaryFieldSelection(
        self: *const ProgramPlan,
        worker: WorkerPlanId,
        key_type: CheckedTypeIdentity,
    ) ?GeneratedParserDictionaryFieldSelection {
        for (self.generated_parser_dictionary_field_selections.items) |selection| {
            if (selection.worker == worker and typeRefEql(selection.key_type, key_type)) return selection;
        }
        return null;
    }

    pub fn generatedFieldIteratorFirstStep(self: *const ProgramPlan, intrinsic: WorkerPlanId) ?WorkerPlanId {
        for (self.generated_field_iterator_links.items) |link| {
            if (link.intrinsic == intrinsic) return link.first_step;
        }
        return null;
    }

    pub fn generatedInterpolationPlan(
        self: *const ProgramPlan,
        interpolation: CheckedExprIdentity,
        caller: WorkerPlanId,
    ) ?GeneratedInterpolationPlan {
        for (self.generated_interpolations.items) |plan| {
            if (plan.caller == caller and exprRefEql(plan.interpolation, interpolation)) return plan;
        }
        return null;
    }

    pub fn iteratorCallPlanFor(
        self: *const ProgramPlan,
        module: checked.ModuleId,
        for_plan: static_dispatch.IteratorForPlanId,
        kind: IteratorCallKind,
        caller: WorkerPlanId,
    ) ?IteratorCallPlan {
        for (self.iterator_calls.items) |call| {
            if (moduleKeyEqual(call.module, module) and
                call.for_plan == for_plan and
                call.kind == kind and
                call.caller == caller)
            {
                return call;
            }
        }
        return null;
    }
};

/// Configuration for target-independent Boxy planning.
pub const AnalyzeOptions = struct {};

/// Checked module data required by the Boxy representation planner.
pub const ModuleView = struct {
    key: checked.ModuleId = .{},
    canonical_names: ?*const checked_names.CanonicalNameStore = null,
    checked_types: checked.CheckedTypeStoreView,
    checked_bodies: checked.CheckedBodyStoreView = .{},
    compile_time_roots: *const checked.CompileTimeRootTable = &empty_compile_time_roots,
    entry_wrappers: *const checked.EntryWrapperTable = &empty_entry_wrappers,
    intrinsic_wrappers: *const checked.IntrinsicWrapperTable = &empty_intrinsic_wrappers,
    hosted_procs: *const checked.HostedProcTable = &empty_hosted_procs,
    resolved_value_refs: *const checked.ResolvedValueRefTable = &empty_resolved_value_refs,
    static_dispatch_plans: *const static_dispatch.StaticDispatchPlanTable = &empty_static_dispatch_plans,
    method_registry: *const static_dispatch.MethodRegistry = &empty_method_registry,
    checked_procedure_templates: *const checked.CheckedProcedureTemplateTable = &empty_checked_procedure_templates,
    nested_proc_sites: *const checked.NestedProcSiteTable = &empty_nested_proc_sites,
    top_level_procedure_bindings: *const checked.TopLevelProcedureBindingTable = &empty_top_level_procedure_bindings,
    callable_eval_templates: checked.CallableEvalTemplateTableView = .{},
    exported_procedure_bindings: checked.ExportedProcedureBindingView = .{},
    interface_capabilities: *const checked.ModuleInterfaceCapabilities = &empty_interface_capabilities,
    const_store: ?*const check.ConstStore.ConstStore = null,
    const_templates: ?*const checked.ConstTemplateTable = null,
};

/// Checked modules, roots, and standalone layout requests to analyze together.
pub const ProgramInput = struct {
    checked_types: checked.CheckedTypeStoreView = .{},
    root_view: ?ModuleView = null,
    extra_module_views: []const ModuleView = &.{},
    root_module: ?checked.LoweringModuleView = null,
    imports: []const checked.ImportedModuleView = &.{},
    roots: []const checked.RootRequest = &.{},
    layout_requests: []const checked.CheckedTypeId = &.{},
    static_data_requests: []const Common.StaticDataRequest = &.{},
};

/// Analyze a checked program into explicit Boxy representations and call plans.
pub fn analyzeProgram(
    allocator: Allocator,
    input: ProgramInput,
    _: AnalyzeOptions,
) Allocator.Error!ProgramPlan {
    var builder = Builder.init(allocator, input);
    defer builder.deinit();

    for (input.roots) |root| {
        try builder.analyzeRoot(root);
    }
    for (input.layout_requests) |layout_request| {
        try builder.plan.root_reps.append(allocator, try builder.analyzeType(builder.root_view, layout_request));
    }
    for (input.static_data_requests) |request| {
        try builder.analyzeStaticDataRequest(request);
    }

    try builder.analyzePlannedEvidenceTypes();
    builder.propagateDynamicRequirements();
    try builder.materializeDictionaryCallPlans();
    try builder.materializeGeneratedParserTagUnionPlans();
    // The dictionary phases above analyze new types (static dictionary
    // workers), so representations created there need the dynamic-content
    // propagation re-run before descriptor requirements are derived from it.
    builder.propagateDynamicRequirements();
    try builder.materializeDescriptorRequirements();
    try builder.materializeWorkerHiddenDescriptorParams();
    try builder.materializeCallableUseHiddenDescriptorArgs();
    try builder.materializeDictionaryMethodDescriptorSources();
    try builder.materializeWorkerHiddenDictionaryParams();
    try builder.materializeWorkerErasedCaptures();
    try builder.materializeStoredCallableCaptureSources();
    try builder.materializeRootHiddenDescriptorArgs();
    try builder.materializeDirectCallHiddenDescriptorArgs();
    try builder.materializeGeneratedCodecCallHiddenDescriptorArgs();
    try builder.materializeConstEvalCallHiddenDescriptorArgs();
    try builder.materializeIteratorCallHiddenDescriptorArgs();
    // Matching hidden call arguments against concrete use-site types can
    // analyze new dynamic representations; give any that were created a
    // descriptor requirement so their worker-mode layout is well defined.
    try builder.materializeDescriptorRequirements();
    builder.propagateDynamicRequirements();

    const out = builder.plan;
    builder.plan = ProgramPlan.init(allocator);
    return out;
}

/// Analyze standalone checked types when no executable program roots are needed.
pub fn analyzeCheckedTypes(
    allocator: Allocator,
    checked_types: checked.CheckedTypeStoreView,
    roots: []const checked.CheckedTypeId,
    options: AnalyzeOptions,
) Allocator.Error!ProgramPlan {
    return analyzeProgram(allocator, .{
        .checked_types = checked_types,
        .layout_requests = roots,
    }, options);
}

const Builder = struct {
    const BodyExprVisit = struct {
        expr: CheckedExprIdentity,
        worker: WorkerPlanId,
    };

    const BodyPatternVisit = struct {
        pattern: CheckedPatternIdentity,
        worker: WorkerPlanId,
    };

    const BodyStatementVisit = struct {
        statement: CheckedStatementIdentity,
        worker: WorkerPlanId,
    };

    const WorkerDictionaryUse = struct {
        worker: WorkerPlanId,
        rep: TypeRepId,
    };

    const StaticConstVisit = struct {
        node: checked.ConstNodeId,
        rep: TypeRepId,
    };

    const GeneratedCodecShapeVisit = struct {
        worker: WorkerPlanId,
        shape: CheckedTypeIdentity,
    };

    allocator: Allocator,
    root_module: ?checked.LoweringModuleView,
    root_view: ModuleView,
    extra_module_views: []const ModuleView,
    imports: []const checked.ImportedModuleView,
    relation_modules: []const checked.ImportedModuleView,
    plan: ProgramPlan,
    by_type: std.AutoHashMap(CheckedTypeIdentity, TypeRepId),
    optional_slots: std.AutoHashMap(CheckedTypeIdentity, TypeRepId),
    by_stored_type: std.AutoHashMap(StoredTypeIdentity, TypeRepId),
    body_exprs_seen: std.AutoHashMap(BodyExprVisit, void),
    body_patterns_seen: std.AutoHashMap(BodyPatternVisit, void),
    body_statements_seen: std.AutoHashMap(BodyStatementVisit, void),
    generated_codec_shapes_seen: std.AutoHashMap(GeneratedCodecShapeVisit, void),
    worker_dictionary_uses: std.ArrayList(WorkerDictionaryUse),
    active_worker: ?WorkerPlanId,

    fn init(allocator: Allocator, input: ProgramInput) Builder {
        const root_view = if (input.root_view) |root_view|
            root_view
        else if (input.root_module) |root_module|
            moduleViewFromCheckedModule(root_module.module)
        else
            ModuleView{ .checked_types = input.checked_types };

        return .{
            .allocator = allocator,
            .root_module = input.root_module,
            .root_view = root_view,
            .extra_module_views = input.extra_module_views,
            .imports = if (input.root_module != null) input.imports else &.{},
            .relation_modules = if (input.root_module) |root_module| root_module.relation_modules else &.{},
            .plan = ProgramPlan.init(allocator),
            .by_type = std.AutoHashMap(CheckedTypeIdentity, TypeRepId).init(allocator),
            .optional_slots = std.AutoHashMap(CheckedTypeIdentity, TypeRepId).init(allocator),
            .by_stored_type = std.AutoHashMap(StoredTypeIdentity, TypeRepId).init(allocator),
            .body_exprs_seen = std.AutoHashMap(BodyExprVisit, void).init(allocator),
            .body_patterns_seen = std.AutoHashMap(BodyPatternVisit, void).init(allocator),
            .body_statements_seen = std.AutoHashMap(BodyStatementVisit, void).init(allocator),
            .generated_codec_shapes_seen = std.AutoHashMap(GeneratedCodecShapeVisit, void).init(allocator),
            .worker_dictionary_uses = .empty,
            .active_worker = null,
        };
    }

    fn deinit(self: *Builder) void {
        self.worker_dictionary_uses.deinit(self.allocator);
        self.generated_codec_shapes_seen.deinit();
        self.body_statements_seen.deinit();
        self.body_patterns_seen.deinit();
        self.body_exprs_seen.deinit();
        self.optional_slots.deinit();
        self.by_type.deinit();
        self.by_stored_type.deinit();
        self.plan.deinit();
    }

    /// The module data the shared label-comparing queries need.
    pub fn moduleNames(self: *Builder, module_id: checked.ModuleId) ModuleNames {
        const view = self.moduleForId(module_id);
        return .{ .key = view.key, .canonical_names = view.canonical_names };
    }

    /// Shared read-only queries over the plan built so far.
    fn repQuery(self: *Builder) RepQuery {
        return .{ .plan = &self.plan, .allocator = self.allocator };
    }

    /// Shared label-comparing queries over the plan built so far.
    fn namedQuery(self: *Builder) NamedRepQuery(*Builder) {
        return .{ .query = self.repQuery(), .modules = self };
    }

    fn moduleForId(self: *Builder, module_id: checked.ModuleId) ModuleView {
        if (moduleKeyEqual(module_id, self.root_view.key)) return self.root_view;
        for (self.extra_module_views) |view| {
            if (moduleKeyEqual(module_id, view.key)) return view;
        }
        for (self.imports) |imported| {
            if (moduleKeyEqual(module_id, imported.key)) return moduleViewFromImported(imported);
        }
        for (self.relation_modules) |relation| {
            if (moduleKeyEqual(module_id, relation.key)) return moduleViewFromImported(relation);
        }
        boxyPlanInvariant("checked nominal representation referenced a module outside boxy planner input");
    }

    fn moduleForCheckedModuleId(self: *Builder, checked_module: anytype) ModuleView {
        return self.moduleForId(.{ .bytes = checked_module.bytes });
    }

    fn analyzeRoot(self: *Builder, root: checked.RootRequest) Allocator.Error!void {
        const host_type = typeRef(self.root_view, root.checked_type);
        const host_rep = try self.analyzeType(self.root_view, root.checked_type);
        const source = workerSourceForRoot(root, self.root_view.key) orelse
            boxyPlanInvariant("boxy root request had no checked procedure worker source");
        const worker_id = try self.ensureWorker(source, host_type, root);
        const worker = self.plan.workers.items[@intFromEnum(worker_id)];
        const source_type = self.workerCheckedTypeForSource(source, worker.checked_type);
        const source_rep = self.plan.repForSourceType(source_type) orelse
            boxyPlanInvariant("boxy root source type was not analyzed");

        const id: RootPlanId = @enumFromInt(@as(u32, @intCast(self.plan.roots.items.len)));
        try self.plan.roots.append(self.allocator, .{
            .id = id,
            .request = root,
            .worker = worker_id,
            .wrapper_kind = if (rootRequiresHostWrapper(root)) .host_shaped_wrapper else .private_worker_only,
            .host_type = host_type,
            .host_rep = host_rep,
            .source_type = source_type,
            .source_rep = source_rep,
            .worker_rep = worker.rep,
        });
        try self.plan.root_reps.append(self.allocator, host_rep);

        if (host_rep != worker.rep) {
            boxyPlanInvariant("boxy root worker representation disagreed with root representation");
        }
    }

    fn analyzeStaticDataRequest(self: *Builder, request: Common.StaticDataRequest) Allocator.Error!void {
        const store_view = self.moduleForId(checked.constModuleId(request.const_locator));
        const templates = store_view.const_templates orelse
            boxyPlanInvariant("static data request module had no const templates");
        const stored = switch (templates.get(request.const_locator).state) {
            .stored_const => |stored| stored,
            .reserved, .eval_template => boxyPlanInvariant("static data request const was not stored before boxy planning"),
            .unimplemented => boxyPlanInvariant("static data request reached a declaration with no implementation"),
        };
        try self.plan.root_reps.append(
            self.allocator,
            try self.analyzeStoredType(store_view, stored.root_type, typeRef(self.root_view, request.checked_type)),
        );
        const node = request.node orelse stored.node;
        const root_rep = self.plan.root_reps.items[self.plan.root_reps.items.len - 1];
        var visited = std.AutoHashMap(StaticConstVisit, void).init(self.allocator);
        defer visited.deinit();
        try self.analyzeStaticConstNode(store_view, node, root_rep, stored.root_type, &visited);
    }

    fn analyzeStaticConstNode(
        self: *Builder,
        store_view: ModuleView,
        node: checked.ConstNodeId,
        rep_id: TypeRepId,
        const_type: ?check.ConstStore.ConstTypeId,
        visited: *std.AutoHashMap(StaticConstVisit, void),
    ) Allocator.Error!void {
        const entry = try visited.getOrPut(.{ .node = node, .rep = rep_id });
        if (entry.found_existing) return;
        const store = store_view.const_store orelse
            boxyPlanInvariant("static data request module had no ConstStore");
        const rep = self.plan.representations.items[@intFromEnum(rep_id)];
        switch (rep.kind) {
            .alias => {
                const backing_type = if (const_type) |stored_type| switch (store.type_store.get(stored_type)) {
                    .named => |named| (named.backing orelse
                        boxyPlanInvariant("stored alias type had no backing")).ty,
                    .primitive, .record, .tuple, .tag_union, .list, .box, .func, .erased, .zst => boxyPlanInvariant("stored alias representation had a non-named stored type"),
                } else null;
                return try self.analyzeStaticConstNode(
                    store_view,
                    node,
                    requiredSingleChildOf(&self.plan, rep_id, .alias_backing).rep,
                    backing_type,
                    visited,
                );
            },
            .nominal => |kind| {
                if (kind != .opaque_nominal) {
                    const backing = requiredSingleChildOf(&self.plan, rep_id, .nominal_backing).rep;
                    const backing_node = switch (store.get(node)) {
                        .nominal => |nominal| nominal.backing,
                        .pending, .zst, .scalar, .str, .list, .box, .tuple, .record, .crash, .tag, .fn_value => node,
                    };
                    const backing_type = if (const_type) |stored_type| switch (store.type_store.get(stored_type)) {
                        .named => |named| (named.backing orelse
                            boxyPlanInvariant("stored nominal type had no backing")).ty,
                        .primitive, .record, .tuple, .tag_union, .list, .box, .func, .erased, .zst => boxyPlanInvariant("stored nominal representation had a non-named stored type"),
                    } else null;
                    return try self.analyzeStaticConstNode(store_view, backing_node, backing, backing_type, visited);
                }
            },
            .in_progress,
            .dynamic,
            .primitive,
            .bool_tag_union,
            .erased_callable,
            .record,
            .record_unbound,
            .tuple,
            .list,
            .box,
            .generated_field,
            .generated_field_names,
            .generated_tag_union_spec,
            .empty_record,
            .tag_union,
            .empty_tag_union,
            => {},
        }
        switch (store.get(node)) {
            .pending => boxyPlanInvariant("pending ConstStore node reached static data planning"),
            .zst, .scalar, .str, .crash => {},
            .box => |child| try self.analyzeStaticConstNode(
                store_view,
                child,
                requiredSingleChildOf(&self.plan, rep_id, .box_payload).rep,
                if (const_type) |stored_type| switch (store.type_store.get(stored_type)) {
                    .box => |payload_type| payload_type,
                    .primitive, .named, .record, .tuple, .tag_union, .list, .func, .erased, .zst => boxyPlanInvariant("stored box node had a non-box stored type"),
                } else null,
                visited,
            ),
            .nominal => |nominal| {
                if (rep.kind == .nominal) {
                    if (rep.kind.nominal != .opaque_nominal) {
                        boxyPlanInvariant("transparent static nominal node was not unwrapped before traversal");
                    }
                } else {
                    const nominal_view = self.moduleForId(.{ .bytes = nominal.named_type.module.bytes });
                    const nominal_rep = try self.analyzeType(nominal_view, nominal.named_type.ty);
                    try self.analyzeStaticConstNode(store_view, node, nominal_rep, null, visited);
                }
            },
            .list => |list_value| {
                const elem_rep = requiredSingleChildOf(&self.plan, rep_id, .list_elem).rep;
                const elem_type = if (const_type) |stored_type| switch (store.type_store.get(stored_type)) {
                    .list => |element| element,
                    .primitive, .named, .record, .tuple, .tag_union, .box, .func, .erased, .zst => boxyPlanInvariant("stored list node had a non-list stored type"),
                } else null;
                switch (list_value) {
                    // Packed scalar elements carry no nested ConstStore nodes, so
                    // there is nothing further to analyze for representation.
                    .scalar_bytes => {},
                    .nodes => |children| for (children) |child| try self.analyzeStaticConstNode(store_view, child, elem_rep, elem_type, visited),
                }
            },
            .tuple, .record => |children| {
                var child_index: usize = 0;
                for (self.plan.childSlice(rep.children)) |rep_child| {
                    const is_value_child = rep_child.role == .tuple_elem or rep_child.role == .record_field;
                    if (!is_value_child) continue;
                    if (child_index >= children.len) boxyPlanInvariant("static aggregate ConstStore node had too few children");
                    try self.analyzeStaticConstNode(
                        store_view,
                        children[child_index],
                        rep_child.rep,
                        if (const_type) |stored_type| switch (store.type_store.get(stored_type)) {
                            .tuple => |items| store.type_store.typeSpan(items)[child_index],
                            .record => |fields| store.type_store.fieldSpan(fields)[child_index].ty,
                            .primitive, .named, .tag_union, .list, .box, .func, .erased, .zst => boxyPlanInvariant("stored aggregate node had a non-aggregate stored type"),
                        } else null,
                        visited,
                    );
                    child_index += 1;
                }
                if (child_index != children.len) boxyPlanInvariant("static aggregate ConstStore node had too many children");
            },
            .tag => |tag| {
                var selected: ?TagVariant = null;
                for (self.plan.tagVariantSlice(rep.tag_variants)) |variant| {
                    const name_view = self.moduleForId(variant.name_module);
                    const canonical_names = name_view.canonical_names orelse
                        boxyPlanInvariant("static tag representation module had no checked name store");
                    if (std.mem.eql(u8, tag.tag_name, canonical_names.tagLabelText(variant.name))) {
                        selected = variant;
                        break;
                    }
                }
                const variant = selected orelse {
                    var extension: ?RepChild = null;
                    for (self.plan.childSlice(rep.children)) |child| {
                        if (child.role != .tag_ext) continue;
                        if (extension != null) boxyPlanInvariant("static tag representation had duplicate row extensions");
                        extension = child;
                    }
                    const ext = extension orelse {
                        if (@import("builtin").mode == .Debug) {
                            std.debug.panic(
                                "boxy plan invariant violated: static tag {s} was absent from {s} representation {d} for checked type {d}",
                                .{ tag.tag_name, @tagName(rep.kind), @intFromEnum(rep_id), @intFromEnum(rep.source_type.ty) },
                            );
                        }
                        unreachable;
                    };
                    return try self.analyzeStaticConstNode(store_view, node, ext.rep, const_type, visited);
                };
                const payload_reps = self.plan.childSlice(variant.payloads);
                if (payload_reps.len != tag.payloads.len) boxyPlanInvariant("static tag payload count disagreed with its representation");
                var stored_payload_types: ?[]const check.ConstStore.ConstTypeId = null;
                if (const_type) |stored_type| {
                    const stored_tags = switch (store.type_store.get(stored_type)) {
                        .tag_union => |tags| store.type_store.tagSpan(tags),
                        .primitive, .named, .record, .tuple, .list, .box, .func, .erased, .zst => boxyPlanInvariant("stored tag node had a non-tag-union stored type"),
                    };
                    const names = store_view.canonical_names orelse
                        boxyPlanInvariant("stored tag type had no resolved tag labels");
                    for (stored_tags) |stored_tag| {
                        if (std.mem.eql(u8, tag.tag_name, names.tagLabelText(stored_tag.checked_name))) {
                            stored_payload_types = store.type_store.typeSpan(stored_tag.payloads);
                            break;
                        }
                    }
                    if (stored_payload_types == null) {
                        boxyPlanInvariant("stored tag node was absent from its stored type");
                    }
                }
                for (tag.payloads, payload_reps, 0..) |child, payload_rep, index| {
                    try self.analyzeStaticConstNode(
                        store_view,
                        child,
                        payload_rep.rep,
                        if (stored_payload_types) |types| types[index] else null,
                        visited,
                    );
                }
            },
            .fn_value => |fn_id| {
                const function = (self.repQuery().functionChildren(rep_id)) orelse
                    boxyPlanInvariant("static function ConstStore node had a non-callable representation");
                const worker = try self.analyzeStaticFnValue(store_view, fn_id, function.rep, const_type);
                const fn_value = store.getFn(fn_id);
                try self.analyzeConstFnCaptures(store_view, fn_value, worker, visited);
            },
        }
    }

    fn analyzeStaticFnValue(
        self: *Builder,
        store_view: ModuleView,
        fn_id: checked.ConstFnId,
        requested_rep: TypeRepId,
        const_type: ?check.ConstStore.ConstTypeId,
    ) Allocator.Error!WorkerPlanId {
        for (self.plan.static_fns.items) |planned| {
            if (moduleKeyEqual(planned.store_module, store_view.key) and planned.fn_id == fn_id and planned.rep == requested_rep) {
                return planned.worker;
            }
        }
        const store = store_view.const_store orelse
            boxyPlanInvariant("static function value had no ConstStore");
        const fn_value = store.getFn(fn_id);
        const requested_type = self.plan.representations.items[@intFromEnum(requested_rep)].source_type;
        const source = if (const_type) |stored_type|
            try self.workerSourceForConstFnValueAtStoredType(store_view, fn_value, stored_type)
        else
            self.workerSourceForConstFnValue(fn_value, requested_type);
        const checked_type = switch (source) {
            .procedure_template => |template| self.checkedTypeForTemplate(template),
            .nested_expr => self.workerCheckedTypeForSource(source, typeRef(store_view, fn_value.source_fn_ty)),
            .generated_codec => |codec| codec.runtime_type orelse typeRef(store_view, fn_value.source_fn_ty),
            .procedure_binding,
            .procedure_use,
            .generated_field_iterator,
            .generated_interpolation_step,
            => unreachable,
        };
        const worker = try self.ensureWorker(source, checked_type, null);
        try self.plan.static_fns.append(self.allocator, .{
            .store_module = store_view.key,
            .fn_id = fn_id,
            .rep = requested_rep,
            .worker = worker,
        });
        return worker;
    }

    fn analyzeConstFnCaptures(
        self: *Builder,
        store_view: ModuleView,
        fn_value: check.ConstStore.ConstFn,
        worker: WorkerPlanId,
        visited: *std.AutoHashMap(StaticConstVisit, void),
    ) Allocator.Error!void {
        const fn_view = switch (fn_value.fn_def) {
            .nested => |nested| self.moduleForId(.{ .bytes = checked_names.procTemplateModuleDigest(nested.owner).bytes }),
            .local_template,
            .imported_template,
            .local_hosted,
            .imported_hosted,
            .checked_generated,
            .parser_runtime,
            .encoder_for_runtime,
            => store_view,
        };
        for (fn_value.captures) |capture| {
            const source_type = if (capture.id.isCanonical())
                typeRef(fn_view, self.checkedBinderType(fn_view, capture.id.binder()))
            else
                self.generatedWorkerCaptureType(worker, capture.id);
            const capture_rep = try self.analyzeStoredType(store_view, capture.ty, source_type);
            try self.analyzeStaticConstNode(store_view, capture.value, capture_rep, capture.ty, visited);
        }
    }

    fn generatedWorkerCaptureType(
        self: *Builder,
        worker_id: WorkerPlanId,
        capture_id: checked.CaptureId,
    ) CheckedTypeIdentity {
        if (capture_id.isCanonical()) {
            boxyPlanInvariant("source capture reached generated worker capture type lookup");
        }
        const worker = self.plan.workers.items[@intFromEnum(worker_id)];
        const codec = switch (worker.source) {
            .generated_codec => |codec| switch (codec.kind) {
                .parser_runtime, .encoder_runtime => codec,
                .parser_constructor,
                .encoder_constructor,
                .encoder_record_fields,
                .encoder_dict_fields,
                .encoder_sequence_elements,
                .encoder_tag_field,
                .encoder_tag_payload_thunk,
                .encoder_tag_payload_elements,
                .encoder_value_thunk,
                => boxyPlanInvariant("generated stored capture referenced a non-runtime codec worker"),
            },
            .procedure_template,
            .procedure_binding,
            .procedure_use,
            .nested_expr,
            .generated_field_iterator,
            .generated_interpolation_step,
            => boxyPlanInvariant("generated stored capture referenced a non-codec worker"),
        };
        if (capture_id == checked.CaptureId.generatedCheck(0)) {
            return codec.capture_type orelse
                boxyPlanInvariant("generated runtime codec had no encoding capture type");
        }
        var next_capture: u32 = 1;
        for (self.plan.generated_parser_field_captures.items) |field_capture| {
            if (field_capture.worker != worker_id) continue;
            if (capture_id == checked.CaptureId.generatedCheck(next_capture)) return field_capture.source_type;
            next_capture += 1;
        }
        boxyPlanInvariant("generated stored capture id was absent from its worker capture plan");
    }

    fn workerSourceForConstFnValue(
        self: *Builder,
        fn_value: check.ConstStore.ConstFn,
        requested_type: CheckedTypeIdentity,
    ) WorkerSource {
        return switch (fn_value.fn_def) {
            .local_template,
            .imported_template,
            .checked_generated,
            .local_hosted,
            .imported_hosted,
            => |template| .{ .procedure_template = template },
            .nested => |nested| blk: {
                const view = self.moduleForId(.{ .bytes = checked_names.procTemplateModuleDigest(nested.owner).bytes });
                var site_expr: ?checked.CheckedExprId = null;
                for (view.nested_proc_sites.sites) |site| {
                    if (site.site == nested.site and checked_names.procedureTemplateRefEql(site.owner_template, nested.owner)) {
                        site_expr = site.checked_expr orelse
                            boxyPlanInvariant("stored nested function had no checked expression site");
                        break;
                    }
                }
                break :blk .{ .nested_expr = .{
                    .module = view.key,
                    .expr = site_expr orelse boxyPlanInvariant("stored nested function referenced a missing checked nested site"),
                } };
            },
            .parser_runtime => |runtime| blk: {
                const view = self.moduleForId(.{ .bytes = checked_names.procTemplateModuleDigest(runtime.owner).bytes });
                break :blk .{ .generated_codec = self.storedGeneratedCodecSource(
                    view,
                    runtime.expr,
                    requested_type.ty,
                    .parser_runtime,
                    .parser,
                ) };
            },
            .encoder_for_runtime => |runtime| blk: {
                const view = self.moduleForId(.{ .bytes = checked_names.procTemplateModuleDigest(runtime.owner).bytes });
                break :blk .{ .generated_codec = self.storedGeneratedCodecSource(
                    view,
                    runtime.expr,
                    requested_type.ty,
                    .encoder_runtime,
                    .encoder,
                ) };
            },
        };
    }

    fn workerSourceForConstFnValueAtStoredType(
        self: *Builder,
        store_view: ModuleView,
        fn_value: check.ConstStore.ConstFn,
        stored_type: check.ConstStore.ConstTypeId,
    ) Allocator.Error!WorkerSource {
        const selection: ?struct {
            owner: checked_names.ProcedureTemplateRef,
            expr: checked.CheckedExprId,
            kind: GeneratedCodecKind,
            derivation_kind: static_dispatch.GeneratedCodecDerivationKind,
        } = switch (fn_value.fn_def) {
            .parser_runtime => |runtime| .{
                .owner = runtime.owner,
                .expr = runtime.expr,
                .kind = .parser_runtime,
                .derivation_kind = .parser,
            },
            .encoder_for_runtime => |runtime| .{
                .owner = runtime.owner,
                .expr = runtime.expr,
                .kind = .encoder_runtime,
                .derivation_kind = .encoder,
            },
            .local_template,
            .imported_template,
            .nested,
            .local_hosted,
            .imported_hosted,
            .checked_generated,
            => null,
        };
        const selected = selection orelse return self.workerSourceForConstFnValue(
            fn_value,
            typeRef(store_view, fn_value.source_fn_ty),
        );
        const view = self.moduleForId(.{ .bytes = checked_names.procTemplateModuleDigest(selected.owner).bytes });
        var found: ?static_dispatch.GeneratedCodecDerivation = null;
        for (view.static_dispatch_plans.generated_codec_derivations) |derivation| {
            if (derivation.kind != selected.derivation_kind) continue;
            if (!try self.storedTypeMatchesCheckedType(store_view, stored_type, view, derivation.source_runtime_ty)) continue;
            if (found) |existing| {
                if (existing.constructor_ty != derivation.constructor_ty or
                    existing.runtime_ty != derivation.runtime_ty or
                    existing.shape_ty != derivation.shape_ty or
                    existing.encoding_ty != derivation.encoding_ty or
                    existing.state_ty != derivation.state_ty or
                    existing.error_ty != derivation.error_ty)
                {
                    boxyPlanInvariant("stored generated codec type matched multiple checked derivations");
                }
                continue;
            }
            found = derivation;
        }
        const derivation = found orelse
            boxyPlanInvariant("stored generated codec type had no checked derivation");
        return .{ .generated_codec = .{
            .kind = selected.kind,
            .shape = typeRef(view, derivation.shape_ty),
            .runtime_type = typeRef(view, derivation.runtime_ty),
            .capture_type = typeRef(view, derivation.encoding_ty),
            .contract_expr = .{ .module = view.key, .expr = selected.expr },
        } };
    }

    const StoredCheckedTypePair = struct {
        stored: check.ConstStore.ConstTypeId,
        checked_module: checked.ModuleId,
        checked_ty: checked.CheckedTypeId,
    };

    const CheckedNamedTypeIdentity = struct {
        kind: check.ConstStore.TypeNamedKind,
        name: checked_names.TypeNameId,
        origin_module: checked_names.ModuleIdentityId,
        source_decl: ?u32,
        args: []const checked.CheckedTypeId,
        backing: TypeSource,
    };

    fn storedTypeMatchesCheckedType(
        self: *Builder,
        store_view: ModuleView,
        stored_type: check.ConstStore.ConstTypeId,
        checked_view: ModuleView,
        checked_ty: checked.CheckedTypeId,
    ) Allocator.Error!bool {
        var visited = std.AutoHashMap(StoredCheckedTypePair, void).init(self.allocator);
        defer visited.deinit();
        return try self.storedTypeMatchesCheckedTypeInner(store_view, stored_type, checked_view, checked_ty, &visited);
    }

    fn storedTypeMatchesCheckedTypeInner(
        self: *Builder,
        store_view: ModuleView,
        stored_type: check.ConstStore.ConstTypeId,
        checked_view: ModuleView,
        checked_ty: checked.CheckedTypeId,
        visited: *std.AutoHashMap(StoredCheckedTypePair, void),
    ) Allocator.Error!bool {
        const visit = try visited.getOrPut(.{
            .stored = stored_type,
            .checked_module = checked_view.key,
            .checked_ty = checked_ty,
        });
        if (visit.found_existing) return true;

        const store = store_view.const_store orelse
            boxyPlanInvariant("stored type comparison had no ConstStore");
        const stored = store.type_store.get(stored_type);
        const payload = checked_view.checked_types.payload(checked_ty);
        if (payload == .alias and stored != .named) {
            return try self.storedTypeMatchesCheckedTypeInner(
                store_view,
                stored_type,
                checked_view,
                payload.alias.backing,
                visited,
            );
        }

        return switch (stored) {
            .func => |function| switch (payload) {
                .function => |checked_fn| blk: {
                    const stored_args = store.type_store.typeSpan(function.args);
                    if (stored_args.len != checked_fn.args.len) break :blk false;
                    for (stored_args, checked_fn.args) |stored_arg, checked_arg| {
                        if (!try self.storedTypeMatchesCheckedTypeInner(store_view, stored_arg, checked_view, checked_arg, visited)) {
                            break :blk false;
                        }
                    }
                    break :blk try self.storedTypeMatchesCheckedTypeInner(
                        store_view,
                        function.ret,
                        checked_view,
                        checked_fn.ret,
                        visited,
                    );
                },
                .pending,
                .err,
                .flex,
                .rigid,
                .alias,
                .record,
                .record_unbound,
                .tuple,
                .nominal,
                .empty_record,
                .tag_union,
                .empty_tag_union,
                => false,
            },
            .primitive => |primitive| switch (payload) {
                .nominal => |nominal| if (nominal.builtin) |builtin|
                    storedPrimitiveMatchesBuiltin(primitive, builtin)
                else
                    false,
                .pending,
                .err,
                .flex,
                .rigid,
                .alias,
                .record,
                .record_unbound,
                .tuple,
                .function,
                .empty_record,
                .tag_union,
                .empty_tag_union,
                => false,
            },
            .named => |named| try self.storedNamedTypeMatchesChecked(
                store_view,
                named,
                checked_view,
                payload,
                visited,
            ),
            .record => |range| switch (payload) {
                .record => |record| try self.storedRecordTypeMatchesChecked(
                    store_view,
                    store.type_store.fieldSpan(range),
                    checked_view,
                    record,
                    visited,
                ),
                .empty_record => range.len == 0,
                .pending, .err, .flex, .rigid, .alias, .record_unbound, .tuple, .nominal, .function, .tag_union, .empty_tag_union => false,
            },
            .tuple => |range| switch (payload) {
                .tuple => |items| blk: {
                    const stored_items = store.type_store.typeSpan(range);
                    if (stored_items.len != items.len) break :blk false;
                    for (stored_items, items) |stored_item, item| {
                        if (!try self.storedTypeMatchesCheckedTypeInner(store_view, stored_item, checked_view, item, visited)) {
                            break :blk false;
                        }
                    }
                    break :blk true;
                },
                .pending, .err, .flex, .rigid, .alias, .record, .record_unbound, .nominal, .function, .empty_record, .tag_union, .empty_tag_union => false,
            },
            .tag_union => |range| switch (payload) {
                .tag_union => |tag_union| try self.storedTagTypeMatchesChecked(
                    store_view,
                    store.type_store.tagSpan(range),
                    checked_view,
                    tag_union,
                    visited,
                ),
                .empty_tag_union => range.len == 0,
                .pending, .err, .flex, .rigid, .alias, .record, .record_unbound, .tuple, .nominal, .function, .empty_record => false,
            },
            .list => |element| switch (payload) {
                .nominal => |nominal| if (nominal.builtin == .list and nominal.args.len == 1)
                    try self.storedTypeMatchesCheckedTypeInner(store_view, element, checked_view, nominal.args[0], visited)
                else
                    false,
                .pending,
                .err,
                .flex,
                .rigid,
                .alias,
                .record,
                .record_unbound,
                .tuple,
                .function,
                .empty_record,
                .tag_union,
                .empty_tag_union,
                => false,
            },
            .box => |element| switch (payload) {
                .nominal => |nominal| if (nominal.builtin == .box and nominal.args.len == 1)
                    try self.storedTypeMatchesCheckedTypeInner(store_view, element, checked_view, nominal.args[0], visited)
                else
                    false,
                .pending,
                .err,
                .flex,
                .rigid,
                .alias,
                .record,
                .record_unbound,
                .tuple,
                .function,
                .empty_record,
                .tag_union,
                .empty_tag_union,
                => false,
            },
            .erased => |digest| std.meta.eql(digest, checked_view.checked_types.rootKey(checked_ty)),
            .zst => payload == .empty_record or payload == .empty_tag_union,
        };
    }

    fn storedNamedTypeMatchesChecked(
        self: *Builder,
        store_view: ModuleView,
        stored: anytype,
        checked_view: ModuleView,
        payload: checked.CheckedTypePayload,
        visited: *std.AutoHashMap(StoredCheckedTypePair, void),
    ) Allocator.Error!bool {
        const checked_def: CheckedNamedTypeIdentity = switch (payload) {
            .alias => |alias| .{
                .kind = check.ConstStore.TypeNamedKind.alias,
                .name = alias.name,
                .origin_module = alias.origin_module,
                .source_decl = alias.source_decl,
                .args = alias.args,
                .backing = .{ .view = checked_view, .ty = alias.backing },
            },
            .nominal => |nominal| .{
                .kind = if (nominal.is_opaque)
                    check.ConstStore.TypeNamedKind.@"opaque"
                else
                    check.ConstStore.TypeNamedKind.nominal,
                .name = nominal.name,
                .origin_module = nominal.origin_module,
                .source_decl = nominal.source_decl,
                .args = nominal.args,
                .backing = try self.nominalBackingSource(checked_view, nominal),
            },
            .pending,
            .err,
            .flex,
            .rigid,
            .record,
            .record_unbound,
            .tuple,
            .function,
            .empty_record,
            .tag_union,
            .empty_tag_union,
            => return false,
        };
        if (stored.kind != checked_def.kind or stored.def.source_decl != checked_def.source_decl) return false;
        const store_names = store_view.canonical_names orelse
            boxyPlanInvariant("stored named type comparison had no ConstStore names");
        const checked_names_store = checked_view.canonical_names orelse
            boxyPlanInvariant("stored named type comparison had no checked names");
        if (!std.mem.eql(
            u8,
            store_names.moduleIdentityBytes(stored.def.module),
            checked_names_store.moduleIdentityBytes(checked_def.origin_module),
        )) return false;
        if (!std.mem.eql(
            u8,
            store_names.typeNameText(stored.def.type_name),
            checked_names_store.typeNameText(checked_def.name),
        )) return false;

        const store = store_view.const_store orelse
            boxyPlanInvariant("stored named type comparison had no ConstStore");
        const stored_args = store.type_store.typeSpan(stored.args);
        if (stored_args.len != checked_def.args.len) return false;
        for (stored_args, checked_def.args) |stored_arg, checked_arg| {
            if (!try self.storedTypeMatchesCheckedTypeInner(store_view, stored_arg, checked_view, checked_arg, visited)) {
                return false;
            }
        }
        if (stored.backing) |backing| {
            return try self.storedTypeMatchesCheckedTypeInner(
                store_view,
                backing.ty,
                checked_def.backing.view,
                checked_def.backing.ty,
                visited,
            );
        }
        return true;
    }

    fn storedRecordTypeMatchesChecked(
        self: *Builder,
        store_view: ModuleView,
        stored_fields: []const check.ConstStore.TypeField,
        checked_view: ModuleView,
        root: checked.CheckedRecordType,
        visited: *std.AutoHashMap(StoredCheckedTypePair, void),
    ) Allocator.Error!bool {
        var checked_fields = std.ArrayList(checked.CheckedRecordField).empty;
        defer checked_fields.deinit(self.allocator);
        try checked_fields.appendSlice(self.allocator, root.fields);
        var extension = root.ext;
        while (true) {
            switch (checked_view.checked_types.payload(extension)) {
                .alias => |alias| extension = alias.backing,
                .record => |record| {
                    try checked_fields.appendSlice(self.allocator, record.fields);
                    extension = record.ext;
                },
                .record_unbound => |fields| {
                    try checked_fields.appendSlice(self.allocator, fields);
                    break;
                },
                .empty_record => break,
                .flex, .rigid => |variable| {
                    if (variable.row_default != .empty_record) return false;
                    break;
                },
                .pending, .err, .tuple, .nominal, .function, .tag_union, .empty_tag_union => return false,
            }
        }
        if (stored_fields.len != checked_fields.items.len) return false;
        const store_names = store_view.canonical_names orelse
            boxyPlanInvariant("stored record type comparison had no ConstStore names");
        const checked_names_store = checked_view.canonical_names orelse
            boxyPlanInvariant("stored record type comparison had no checked names");
        for (stored_fields) |stored_field| {
            const stored_name = store_names.recordFieldLabelText(stored_field.name);
            var matched: ?checked.CheckedRecordField = null;
            for (checked_fields.items) |checked_field| {
                if (std.mem.eql(u8, stored_name, checked_names_store.recordFieldLabelText(checked_field.name))) {
                    if (matched != null) boxyPlanInvariant("checked record type had duplicate field labels");
                    matched = checked_field;
                }
            }
            const checked_field = matched orelse return false;
            if (!try self.storedTypeMatchesCheckedTypeInner(
                store_view,
                stored_field.ty,
                checked_view,
                checked_field.ty,
                visited,
            )) return false;
        }
        return true;
    }

    fn storedTagTypeMatchesChecked(
        self: *Builder,
        store_view: ModuleView,
        stored_tags: []const check.ConstStore.TypeTag,
        checked_view: ModuleView,
        root: checked.CheckedTagUnionType,
        visited: *std.AutoHashMap(StoredCheckedTypePair, void),
    ) Allocator.Error!bool {
        var checked_tags = std.ArrayList(checked.CheckedTag).empty;
        defer checked_tags.deinit(self.allocator);
        try checked_tags.appendSlice(self.allocator, root.tags);
        var extension = root.ext;
        while (true) {
            switch (checked_view.checked_types.payload(extension)) {
                .alias => |alias| extension = alias.backing,
                .tag_union => |tag_union| {
                    try checked_tags.appendSlice(self.allocator, tag_union.tags);
                    extension = tag_union.ext;
                },
                .empty_tag_union => break,
                .flex, .rigid => |variable| {
                    if (variable.row_default != .empty_tag_union) return false;
                    break;
                },
                .pending, .err, .record, .record_unbound, .tuple, .nominal, .function, .empty_record => return false,
            }
        }
        if (stored_tags.len != checked_tags.items.len) return false;
        const store = store_view.const_store orelse
            boxyPlanInvariant("stored tag type comparison had no ConstStore");
        const store_names = store_view.canonical_names orelse
            boxyPlanInvariant("stored tag type comparison had no ConstStore names");
        const checked_names_store = checked_view.canonical_names orelse
            boxyPlanInvariant("stored tag type comparison had no checked names");
        for (stored_tags) |stored_tag| {
            const stored_name = store_names.tagLabelText(stored_tag.name);
            var matched: ?checked.CheckedTag = null;
            for (checked_tags.items) |checked_tag| {
                if (std.mem.eql(u8, stored_name, checked_names_store.tagLabelText(checked_tag.name))) {
                    if (matched != null) boxyPlanInvariant("checked tag type had duplicate tag labels");
                    matched = checked_tag;
                }
            }
            const checked_tag = matched orelse return false;
            const stored_payloads = store.type_store.typeSpan(stored_tag.payloads);
            const checked_payloads = checked_tag.argsSlice(checked_view.checked_types);
            if (stored_payloads.len != checked_payloads.len) return false;
            for (stored_payloads, checked_payloads) |stored_payload, checked_payload| {
                if (!try self.storedTypeMatchesCheckedTypeInner(
                    store_view,
                    stored_payload,
                    checked_view,
                    checked_payload,
                    visited,
                )) return false;
            }
        }
        return true;
    }

    fn storedGeneratedCodecSource(
        self: *Builder,
        view: ModuleView,
        expr_id: checked.CheckedExprId,
        stored_runtime_ty: checked.CheckedTypeId,
        kind: GeneratedCodecKind,
        derivation_kind: static_dispatch.GeneratedCodecDerivationKind,
    ) GeneratedCodecSource {
        const dispatch = self.dispatchPlanForGeneratedRuntime(view, expr_id);
        const constructor = checkedFunctionPayload(view, dispatch.callable_ty);
        if (constructor.args.len != 1) {
            boxyPlanInvariant("stored generated codec constructor did not have one encoding argument");
        }
        var found: ?static_dispatch.GeneratedCodecDerivation = null;
        for (view.static_dispatch_plans.generated_codec_derivations) |derivation| {
            if (derivation.kind != derivation_kind or
                derivation.source_runtime_ty != stored_runtime_ty or
                derivation.source_encoding_ty != constructor.args[0])
            {
                continue;
            }
            if (found) |existing| {
                if (!generatedCodecDerivationsEql(view.static_dispatch_plans, existing, derivation)) {
                    boxyPlanInvariant("stored generated codec runtime matched multiple checked derivations");
                }
                continue;
            }
            found = derivation;
        }
        const derivation = found orelse
            boxyPlanInvariant("stored generated codec runtime had no checked derivation");
        return .{
            .kind = kind,
            .shape = typeRef(view, derivation.shape_ty),
            .runtime_type = typeRef(view, derivation.runtime_ty),
            .capture_type = typeRef(view, derivation.encoding_ty),
            .contract_expr = .{ .module = view.key, .expr = expr_id },
        };
    }

    fn dispatchPlanForGeneratedRuntime(
        _: *Builder,
        view: ModuleView,
        expr_id: checked.CheckedExprId,
    ) static_dispatch.StaticDispatchCallPlan {
        const expr = view.checked_bodies.expr(expr_id);
        const plan_id = if (expr.data == .dispatch_call)
            expr.data.dispatch_call orelse
                boxyPlanInvariant("stored serialization dispatch expression had no dispatch plan")
        else if (expr.data == .type_dispatch_call)
            expr.data.type_dispatch_call orelse
                boxyPlanInvariant("stored serialization type dispatch expression had no dispatch plan")
        else
            boxyPlanInvariant("stored serialization runtime function did not reference a dispatch expression");
        const raw = @intFromEnum(plan_id);
        if (raw >= view.static_dispatch_plans.plans.len) {
            boxyPlanInvariant("stored serialization dispatch plan was outside its checked table");
        }
        return view.static_dispatch_plans.plans[raw];
    }

    fn ensureWorker(
        self: *Builder,
        source: WorkerSource,
        checked_type: CheckedTypeIdentity,
        root_request: ?checked.RootRequest,
    ) Allocator.Error!WorkerPlanId {
        const selected_source = self.workerSourceForCallableEvalSource(source);
        if (!workerSourceEql(selected_source, source)) {
            return self.ensureWorker(selected_source, checked_type, root_request);
        }
        const worker_type = if (source == .nested_expr)
            self.workerCheckedTypeForSource(source, checked_type)
        else
            checked_type;
        const rep = try self.analyzeType(self.moduleForId(worker_type.module), worker_type.ty);
        const definition_type = self.workerCheckedTypeForSource(source, worker_type);
        if (!typeRefEql(definition_type, worker_type)) {
            _ = try self.analyzeType(self.moduleForId(definition_type.module), definition_type.ty);
        }
        for (self.plan.workers.items) |worker| {
            if (workerSourceEql(worker.source, source) and (source == .nested_expr or typeRefEql(worker.checked_type, worker_type))) {
                if (root_request) |request| {
                    if (worker.root_request == null) {
                        self.plan.workers.items[@intFromEnum(worker.id)].root_request = request;
                    }
                }
                return worker.id;
            }
        }

        const worker_id: WorkerPlanId = @enumFromInt(@as(u32, @intCast(self.plan.workers.items.len)));
        const body = if (self.root_module != null and
            source != .generated_codec and
            source != .generated_field_iterator and
            source != .generated_interpolation_step)
            self.rootWorkerBody(source)
        else
            null;
        try self.plan.workers.append(self.allocator, .{
            .id = worker_id,
            .root_request = root_request,
            .source = source,
            .checked_type = worker_type,
            .rep = rep,
            .stored_fn = if (body) |resolved_body| switch (resolved_body) {
                .checked_expr => |checked_body| checked_body.stored_fn,
                .intrinsic_wrapper, .hosted_proc, .unimplemented => null,
            } else null,
        });

        switch (source) {
            .generated_codec => |codec| switch (codec.kind) {
                .parser_constructor,
                .encoder_constructor,
                => {
                    const function = (self.repQuery().functionChildren(rep)) orelse
                        boxyPlanInvariant("generated codec constructor did not have a function representation");
                    if (function.arg_count != 1) {
                        boxyPlanInvariant("generated codec constructor did not have one encoding argument");
                    }
                    const children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(function.rep)].children);
                    const encoding_type = children[function.args_start].source_type;
                    const checked_runtime_type = self.plan.representations.items[@intFromEnum(function.ret)].source_type;
                    const contract = self.generatedCodecContractForConstructor(
                        codec,
                        worker_type,
                        encoding_type,
                        checked_runtime_type,
                    );
                    const runtime_type = typeRef(contract.view, contract.derivation.runtime_ty);
                    const runtime_worker = try self.ensureWorker(
                        .{ .generated_codec = .{
                            .kind = switch (codec.kind) {
                                .parser_constructor => .parser_runtime,
                                .encoder_constructor => .encoder_runtime,
                                .parser_runtime,
                                .encoder_runtime,
                                .encoder_record_fields,
                                .encoder_dict_fields,
                                .encoder_sequence_elements,
                                .encoder_tag_field,
                                .encoder_tag_payload_thunk,
                                .encoder_tag_payload_elements,
                                .encoder_value_thunk,
                                => unreachable,
                            },
                            .shape = codec.shape,
                            .runtime_type = runtime_type,
                            .capture_type = encoding_type,
                            .contract_derivation = codec.contract_derivation,
                            .contract_expr = codec.contract_expr,
                        } },
                        runtime_type,
                        null,
                    );
                    try self.plan.generated_codec_runtime_links.append(self.allocator, .{
                        .constructor = worker_id,
                        .runtime = runtime_worker,
                    });
                },
                .parser_runtime,
                => {
                    const encoding_type = codec.capture_type orelse
                        boxyPlanInvariant("generated parser runtime had no encoding capture type");
                    _ = try self.analyzeType(self.moduleForId(codec.shape.module), codec.shape.ty);
                    try self.planGeneratedParserShape(worker_id, codec.shape, encoding_type);
                    try self.plan.generated_parser_runtime_plans.append(self.allocator, .{
                        .worker = worker_id,
                        .schema_type = try self.generatedParserRuntimeSchema(codec.shape),
                    });
                },
                .encoder_runtime => {
                    const encoding_type = codec.capture_type orelse
                        boxyPlanInvariant("generated encoder runtime had no encoding capture type");
                    _ = try self.analyzeType(self.moduleForId(codec.shape.module), codec.shape.ty);
                    const function = (self.repQuery().functionChildren(rep)) orelse
                        boxyPlanInvariant("generated encoder runtime did not have a function representation");
                    if (function.arg_count != 2) {
                        boxyPlanInvariant("generated encoder runtime did not have value and state arguments");
                    }
                    const children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(function.rep)].children);
                    const value_type = children[function.args_start].source_type;
                    try self.planGeneratedEncoderShape(worker_id, worker_id, value_type, value_type, encoding_type);
                },
                .encoder_record_fields,
                .encoder_dict_fields,
                .encoder_sequence_elements,
                .encoder_tag_field,
                .encoder_tag_payload_thunk,
                .encoder_tag_payload_elements,
                .encoder_value_thunk,
                => {},
            },
            .generated_field_iterator => {},
            .generated_interpolation_step => {},
            .procedure_template,
            .procedure_binding,
            .procedure_use,
            .nested_expr,
            => {},
        }

        if (body) |resolved_body| {
            const previous_worker = self.active_worker;
            self.active_worker = worker_id;
            defer self.active_worker = previous_worker;
            try self.analyzeWorkerBodyTypes(resolved_body);
        }

        return worker_id;
    }

    fn ensureGeneratedCodecCall(
        self: *Builder,
        caller: WorkerPlanId,
        dispatch_type: CheckedTypeIdentity,
        method_text: []const u8,
        subject_type: ?CheckedTypeIdentity,
    ) Allocator.Error!GeneratedCodecCallPlan {
        const contract = self.generatedCodecContractForWorker(caller);
        const contract_names = contract.view.canonical_names orelse
            boxyPlanInvariant("generated codec contract module had no checked names");
        var checked_call: ?static_dispatch.GeneratedCodecCall = null;
        for (contract.derivation.callsSlice(contract.view.static_dispatch_plans)) |candidate| {
            if (!std.mem.eql(u8, contract_names.methodNameText(candidate.method), method_text)) continue;
            if (!moduleKeyEqual(dispatch_type.module, contract.view.key) or candidate.dispatcher_ty != dispatch_type.ty) continue;
            const candidate_subject = if (candidate.subject_ty) |ty| typeRef(contract.view, ty) else null;
            if (!optionalTypeRefEql(candidate_subject, subject_type)) continue;
            if (checked_call) |existing| {
                if (!std.meta.eql(
                    contract.view.checked_types.rootKey(existing.callable_ty),
                    contract.view.checked_types.rootKey(candidate.callable_ty),
                ) or !std.meta.eql(
                    contract.view.checked_types.rootKey(existing.dispatcher_ty),
                    contract.view.checked_types.rootKey(candidate.dispatcher_ty),
                )) {
                    boxyPlanInvariant("generated codec contract had ambiguous method call metadata");
                }
                continue;
            }
            checked_call = candidate;
        }
        const exact_call = checked_call orelse
            boxyPlanInvariant("generated codec contract was missing a required method call");
        const exact_dispatch_type = typeRef(contract.view, exact_call.dispatcher_ty);
        if (!typeRefEql(exact_dispatch_type, dispatch_type)) {
            boxyPlanInvariant("generated codec contract dispatcher disagreed with the planned call");
        }

        for (self.plan.generated_codec_calls.items) |planned| {
            if (planned.caller != caller or !typeRefEql(planned.dispatch_type, dispatch_type)) continue;
            if (!optionalTypeRefEql(planned.subject_type, subject_type)) continue;
            const planned_view = self.moduleForId(planned.method_module);
            const planned_names = planned_view.canonical_names orelse
                boxyPlanInvariant("planned generated codec method module had no checked names");
            if (std.mem.eql(u8, planned_names.methodNameText(planned.method), method_text)) return planned;
        }

        const owner = methodOwnerForModuleType(contract.view, exact_call.dispatcher_ty) orelse
            boxyPlanInvariant("generated codec dispatch type had no method owner");
        const lookup = self.lookupMethodTarget(contract.view, owner, contract.view, exact_call.method) orelse
            boxyPlanInvariant("checked generated codec method target was absent from the method registry");
        const source = self.workerSourceForMethodTarget(
            lookup,
            dispatch_type,
            exact_call.generated_codec_derivation,
        );
        const source_fn_type = CheckedTypeIdentity{ .module = lookup.view.key, .ty = lookup.target.callable_ty };
        const worker = try self.ensureWorker(source, source_fn_type, null);
        const exact_fn_rep = try self.analyzeType(contract.view, exact_call.callable_ty);
        const function = (self.repQuery().functionChildren(exact_fn_rep)) orelse
            boxyPlanInvariant("generated codec call contract was not a function");
        const children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(function.rep)].children);

        const arg_start: u32 = @intCast(self.plan.generated_codec_call_types.items.len);
        for (children[function.args_start..][0..function.arg_count]) |child| {
            try self.plan.generated_codec_call_types.append(self.allocator, child.source_type);
        }
        const planned = GeneratedCodecCallPlan{
            .caller = caller,
            .dispatch_type = dispatch_type,
            .subject_type = subject_type,
            .method_module = contract.view.key,
            .method = exact_call.method,
            .worker = worker,
            .arg_types = .{ .start = arg_start, .len = function.arg_count },
            .ret_type = self.plan.representations.items[@intFromEnum(function.ret)].source_type,
            .checked_evidence = .{ .start = exact_call.nested.start, .len = exact_call.nested.len },
        };
        try self.plan.generated_codec_calls.append(self.allocator, planned);
        return planned;
    }

    fn ensureGeneratedCodecCallWithCheckedSubject(
        self: *Builder,
        caller: WorkerPlanId,
        dispatch_type: CheckedTypeIdentity,
        method_text: []const u8,
    ) Allocator.Error!GeneratedCodecCallPlan {
        const contract = self.generatedCodecContractForWorker(caller);
        const names = contract.view.canonical_names orelse
            boxyPlanInvariant("generated codec contract module had no checked names");
        var subject_type: ?CheckedTypeIdentity = null;
        for (contract.derivation.callsSlice(contract.view.static_dispatch_plans)) |candidate| {
            if (!std.mem.eql(u8, names.methodNameText(candidate.method), method_text)) continue;
            if (!moduleKeyEqual(dispatch_type.module, contract.view.key) or candidate.dispatcher_ty != dispatch_type.ty) continue;
            const candidate_ty = candidate.subject_ty orelse
                boxyPlanInvariant("generated codec method required a checked subject but the producer recorded none");
            const candidate_type = typeRef(contract.view, candidate_ty);
            if (subject_type) |existing| {
                if (!std.meta.eql(
                    contract.view.checked_types.rootKey(existing.ty),
                    contract.view.checked_types.rootKey(candidate_type.ty),
                )) {
                    boxyPlanInvariant("generated codec method had ambiguous checked subjects");
                }
                continue;
            }
            subject_type = candidate_type;
        }
        return try self.ensureGeneratedCodecCall(
            caller,
            dispatch_type,
            method_text,
            subject_type orelse boxyPlanInvariant("generated codec contract was missing a subject-bearing method call"),
        );
    }

    const GeneratedCodecContractLookup = struct {
        view: ModuleView,
        derivation: static_dispatch.GeneratedCodecDerivation,
    };

    fn generatedCodecContractForConstructor(
        self: *Builder,
        codec: GeneratedCodecSource,
        constructor_type: CheckedTypeIdentity,
        encoding_type: CheckedTypeIdentity,
        runtime_type: CheckedTypeIdentity,
    ) GeneratedCodecContractLookup {
        const expected_kind: static_dispatch.GeneratedCodecDerivationKind = switch (codec.kind) {
            .parser_constructor => .parser,
            .encoder_constructor => .encoder,
            .parser_runtime,
            .encoder_runtime,
            .encoder_record_fields,
            .encoder_dict_fields,
            .encoder_sequence_elements,
            .encoder_tag_field,
            .encoder_tag_payload_thunk,
            .encoder_tag_payload_elements,
            .encoder_value_thunk,
            => boxyPlanInvariant("generated codec constructor contract requested for a runtime worker"),
        };
        const view = self.moduleForId(runtime_type.module);
        if (!moduleKeyEqual(codec.shape.module, view.key) or
            !moduleKeyEqual(constructor_type.module, view.key) or
            !moduleKeyEqual(encoding_type.module, view.key))
        {
            boxyPlanInvariant("generated codec constructor components belonged to different checked modules");
        }
        const runtime_fn = checkedFunctionPayload(view, runtime_type.ty);
        const state_ty = switch (expected_kind) {
            .parser => if (runtime_fn.args.len == 1)
                runtime_fn.args[0]
            else
                boxyPlanInvariant("generated parser constructor runtime had unexpected arity"),
            .encoder => if (runtime_fn.args.len == 2)
                runtime_fn.args[1]
            else
                boxyPlanInvariant("generated encoder constructor runtime had unexpected arity"),
        };
        const constructor_key = view.checked_types.rootKey(constructor_type.ty);
        const shape_key = view.checked_types.rootKey(codec.shape.ty);
        const encoding_key = view.checked_types.rootKey(encoding_type.ty);
        const state_key = view.checked_types.rootKey(state_ty);
        if (codec.contract_derivation) |derivation_id| {
            if (@intFromEnum(derivation_id) >= view.static_dispatch_plans.generated_codec_derivations.len) {
                boxyPlanInvariant("generated codec constructor referenced a missing checked derivation");
            }
            const derivation = view.static_dispatch_plans.generated_codec_derivations[@intFromEnum(derivation_id)];
            if (derivation.kind != expected_kind or
                !std.meta.eql(constructor_key, view.checked_types.rootKey(derivation.constructor_ty)) or
                !std.meta.eql(encoding_key, view.checked_types.rootKey(derivation.encoding_ty)) or
                !std.meta.eql(state_key, view.checked_types.rootKey(derivation.state_ty)) or
                !std.meta.eql(view.checked_types.rootKey(runtime_type.ty), view.checked_types.rootKey(derivation.runtime_ty)))
            {
                boxyPlanInvariant("generated codec constructor disagreed with its checked derivation reference");
            }
            return .{ .view = view, .derivation = derivation };
        }
        var found: ?static_dispatch.GeneratedCodecDerivation = null;
        for (view.static_dispatch_plans.generated_codec_derivations) |derivation| {
            if (derivation.kind != expected_kind or
                !std.meta.eql(constructor_key, view.checked_types.rootKey(derivation.constructor_ty)) or
                !std.meta.eql(shape_key, view.checked_types.rootKey(derivation.shape_ty)) or
                !std.meta.eql(encoding_key, view.checked_types.rootKey(derivation.encoding_ty)) or
                !std.meta.eql(state_key, view.checked_types.rootKey(derivation.state_ty)))
            {
                continue;
            }
            if (found) |existing| {
                if (!generatedCodecDerivationsEql(view.static_dispatch_plans, existing, derivation)) {
                    boxyPlanInvariant("generated codec constructor matched multiple checked contracts");
                }
                continue;
            }
            found = derivation;
        }
        return .{
            .view = view,
            .derivation = found orelse
                boxyPlanInvariant("generated codec constructor had no checked derivation contract"),
        };
    }

    fn generatedCodecContractForWorker(self: *Builder, worker_id: WorkerPlanId) GeneratedCodecContractLookup {
        const worker = self.plan.workers.items[@intFromEnum(worker_id)];
        const codec = switch (worker.source) {
            .generated_codec => |codec| codec,
            .procedure_template,
            .procedure_binding,
            .procedure_use,
            .nested_expr,
            .generated_field_iterator,
            .generated_interpolation_step,
            => boxyPlanInvariant("generated codec call was planned outside a generated worker"),
        };
        const contract_worker = if (codec.contract_worker) |root|
            self.plan.workers.items[@intFromEnum(root)]
        else
            worker;
        const contract_codec = switch (contract_worker.source) {
            .generated_codec => |source| source,
            .procedure_template,
            .procedure_binding,
            .procedure_use,
            .nested_expr,
            .generated_field_iterator,
            .generated_interpolation_step,
            => boxyPlanInvariant("generated codec callback contract did not reference a generated worker"),
        };
        const expected_kind: static_dispatch.GeneratedCodecDerivationKind = switch (codec.kind) {
            .parser_runtime => .parser,
            .encoder_runtime,
            .encoder_record_fields,
            .encoder_dict_fields,
            .encoder_sequence_elements,
            .encoder_tag_field,
            .encoder_tag_payload_thunk,
            .encoder_tag_payload_elements,
            .encoder_value_thunk,
            => .encoder,
            .parser_constructor, .encoder_constructor => boxyPlanInvariant("generated codec call was planned from a constructor worker"),
        };
        const view = if (contract_codec.contract_expr) |expr|
            self.moduleForId(expr.module)
        else
            self.moduleForId(contract_worker.checked_type.module);
        if (!moduleKeyEqual(contract_codec.shape.module, view.key)) {
            boxyPlanInvariant("generated codec shape and runtime contract belonged to different checked modules");
        }
        const capture_type = contract_codec.capture_type orelse
            boxyPlanInvariant("generated codec runtime had no capture type");
        if (!moduleKeyEqual(capture_type.module, view.key)) {
            boxyPlanInvariant("generated codec capture and runtime contract belonged to different checked modules");
        }
        const runtime_type = contract_codec.runtime_type orelse contract_worker.checked_type;
        if (contract_codec.contract_derivation) |derivation_id| {
            if (@intFromEnum(derivation_id) >= view.static_dispatch_plans.generated_codec_derivations.len) {
                boxyPlanInvariant("generated codec worker referenced a missing checked derivation");
            }
            const derivation = view.static_dispatch_plans.generated_codec_derivations[@intFromEnum(derivation_id)];
            if (derivation.kind != expected_kind or
                !std.meta.eql(view.checked_types.rootKey(runtime_type.ty), view.checked_types.rootKey(derivation.runtime_ty)) or
                !std.meta.eql(view.checked_types.rootKey(capture_type.ty), view.checked_types.rootKey(derivation.encoding_ty)))
            {
                boxyPlanInvariant("generated codec worker disagreed with its checked derivation reference");
            }
            return .{ .view = view, .derivation = derivation };
        }
        var found: ?static_dispatch.GeneratedCodecDerivation = null;
        for (view.static_dispatch_plans.generated_codec_derivations) |derivation| {
            const identity_matches = moduleKeyEqual(runtime_type.module, view.key) and
                derivation.runtime_ty == runtime_type.ty and
                derivation.shape_ty == contract_codec.shape.ty;
            if (derivation.kind != expected_kind or !identity_matches or derivation.encoding_ty != capture_type.ty) {
                continue;
            }
            if (found) |existing| {
                if (!generatedCodecDerivationsEql(view.static_dispatch_plans, existing, derivation)) {
                    boxyPlanInvariant("generated codec worker matched multiple checked contracts");
                }
                continue;
            }
            found = derivation;
        }
        return .{
            .view = view,
            .derivation = found orelse
                boxyPlanInvariant("generated codec worker had no checked derivation contract"),
        };
    }

    fn planGeneratedParserShape(
        self: *Builder,
        worker: WorkerPlanId,
        shape: CheckedTypeIdentity,
        encoding_type: CheckedTypeIdentity,
    ) Allocator.Error!void {
        const visit = try self.generated_codec_shapes_seen.getOrPut(.{ .worker = worker, .shape = shape });
        if (visit.found_existing) return;

        const view = self.moduleForId(shape.module);
        switch (view.checked_types.payload(shape.ty)) {
            .pending => boxyPlanInvariant("pending checked type reached generated parser planning"),
            .err => boxyPlanInvariant("checked error type reached generated parser planning"),
            .flex, .rigid, .record_unbound => boxyPlanInvariant("open checked type reached generated parser planning"),
            .function, .empty_tag_union => boxyPlanInvariant("unsupported checked type reached generated parser planning"),
            .alias => |alias| {
                const backing = typeRef(view, alias.backing);
                try self.planGeneratedParserShape(worker, backing, encoding_type);
                try self.propagateGeneratedParserTagCallLink(worker, shape, backing);
                try self.propagateGeneratedParserTryPlan(worker, shape, backing);
            },
            .record => try self.planGeneratedParserRecord(worker, shape, encoding_type),
            .empty_record => {
                _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "parse_record_field", shape);
                _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "skip_record_field", null);
            },
            .tuple => |elems| {
                _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "parse_array_start", null);
                _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "parse_array_next", null);
                _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "parse_array_after_element", null);
                _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "invalid_value", null);
                for (elems) |elem| {
                    try self.planGeneratedParserShape(worker, typeRef(view, elem), encoding_type);
                }
            },
            .tag_union => {
                _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "parse_tag_union", shape);
                try self.appendGeneratedParserTagCallLink(worker, shape, shape);
                try self.planGeneratedParserTagRow(worker, shape, encoding_type, 0);
            },
            .nominal => |nominal| {
                if (nominal.builtin) |builtin| {
                    if (generatedParserScalarMethod(builtin)) |method_text| {
                        _ = try self.ensureGeneratedCodecCall(worker, encoding_type, method_text, shape);
                        return;
                    }
                    switch (builtin) {
                        .box => {
                            if (nominal.args.len != 1) boxyPlanInvariant("Box generated parser type had unexpected arity");
                            try self.planGeneratedParserShape(worker, typeRef(view, nominal.args[0]), encoding_type);
                        },
                        .list => {
                            if (nominal.args.len != 1) boxyPlanInvariant("List generated parser type had unexpected arity");
                            _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "parse_array_start", null);
                            _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "parse_array_next", null);
                            _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "parse_array_after_element", null);
                            try self.planGeneratedParserShape(worker, typeRef(view, nominal.args[0]), encoding_type);
                        },
                        .dict => {
                            if (nominal.args.len != 2) boxyPlanInvariant("Dict generated parser type had unexpected arity");
                            const key_type = typeRef(view, nominal.args[0]);
                            _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "parse_object_next", null);
                            if (generatedParserKeyMethod(view, nominal.args[0])) |method_text| {
                                const key_call = try self.ensureGeneratedCodecCall(worker, encoding_type, method_text, key_type);
                                try self.appendGeneratedParserDictionaryFieldSelection(worker, key_type, .{ .method = .{
                                    .module = key_call.method_module,
                                    .name = key_call.method,
                                } });
                            } else {
                                if (!checkedParserUnitTagKey(view, nominal.args[0])) {
                                    boxyPlanInvariant("generated Dict parser key had no checked parsing strategy");
                                }
                                _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "invalid_value", null);
                                try self.appendGeneratedParserDictionaryFieldSelection(worker, key_type, .unit_tags);
                            }
                            _ = try self.ensureGeneratedCodecCall(worker, shape, "with_capacity", shape);
                            _ = try self.ensureGeneratedCodecCall(worker, shape, "insert", shape);
                            try self.planGeneratedParserShape(worker, typeRef(view, nominal.args[1]), encoding_type);
                        },
                        .set => {
                            if (nominal.args.len != 1) boxyPlanInvariant("Set generated parser type had unexpected arity");
                            _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "parse_array_start", null);
                            _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "parse_array_next", null);
                            _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "parse_array_after_element", null);
                            _ = try self.ensureGeneratedCodecCall(worker, shape, "from_list", shape);
                            try self.planGeneratedParserShape(worker, typeRef(view, nominal.args[0]), encoding_type);
                        },
                        .bool,
                        .try_,
                        .str,
                        .u8,
                        .i8,
                        .u16,
                        .i16,
                        .u32,
                        .i32,
                        .u64,
                        .i64,
                        .u128,
                        .i128,
                        .f32,
                        .f64,
                        .dec,
                        .u8x16,
                        .i8x16,
                        .u16x8,
                        .i16x8,
                        .u32x4,
                        .i32x4,
                        .u64x2,
                        .i64x2,
                        .iter,
                        .parse_tag_union_spec,
                        .fields,
                        .field,
                        .crypto_sha256_digest,
                        .crypto_sha256_hasher,
                        .crypto_blake3_digest,
                        .crypto_blake3_hasher,
                        => boxyPlanInvariant("unsupported builtin reached generated parser planning"),
                    }
                    return;
                }

                if (methodOwnerForModuleType(view, shape.ty)) |owner| {
                    if (view.canonical_names.?.lookupMethodName("parser_for")) |parser_for| {
                        if (self.lookupMethodTarget(view, owner, view, parser_for)) |lookup| {
                            switch (lookup.target.kind) {
                                .procedure, .local_proc => {
                                    _ = try self.ensureGeneratedCodecCall(worker, shape, "parser_for", shape);
                                    return;
                                },
                                .structural => |kind| switch (kind) {
                                    .parser => {},
                                    .encoder => boxyPlanInvariant("parser planning resolved to generated encoder target"),
                                    .equality, .hash, .map, .map_effectful => boxyPlanInvariant("parser planning resolved to a non-parser structural target"),
                                },
                            }
                        }
                    }
                }

                const backing_source = try self.nominalBackingSource(view, nominal);
                if (checkedTryPayloads(backing_source.view, backing_source.ty)) |try_payloads| {
                    const kinds = checkedTryErrorKinds(backing_source.view, try_payloads.err) orelse
                        boxyPlanInvariant("generated Try parser had unsupported error tags");
                    if (kinds.other) {
                        boxyPlanInvariant("generated Try parser had unsupported error tags");
                    }
                    const ok_type = typeRef(backing_source.view, try_payloads.ok);
                    if (kinds.null) {
                        _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "parse_null", null);
                        try self.appendGeneratedParserTryPlan(
                            worker,
                            shape,
                            ok_type,
                            typeRef(backing_source.view, try_payloads.err),
                            kinds,
                        );
                    }
                    try self.planGeneratedParserShape(worker, ok_type, encoding_type);
                    return;
                }
                const backing = typeRef(backing_source.view, backing_source.ty);
                try self.planGeneratedParserShape(worker, backing, encoding_type);
                try self.propagateGeneratedParserTagCallLink(worker, shape, backing);
                try self.propagateGeneratedParserTryPlan(worker, shape, backing);
            },
        }
    }

    fn propagateGeneratedParserTryPlan(
        self: *Builder,
        worker: WorkerPlanId,
        shape_type: CheckedTypeIdentity,
        backing_type: CheckedTypeIdentity,
    ) Allocator.Error!void {
        const backing = self.plan.generatedParserTryPlan(worker, backing_type) orelse return;
        try self.appendGeneratedParserTryPlan(
            worker,
            shape_type,
            backing.ok_type,
            backing.error_type,
            .{ .missing = backing.missing, .null = backing.null },
        );
    }

    fn appendGeneratedParserTryPlan(
        self: *Builder,
        worker: WorkerPlanId,
        try_type: CheckedTypeIdentity,
        ok_type: CheckedTypeIdentity,
        error_type: CheckedTypeIdentity,
        kinds: CheckedTryErrorKinds,
    ) Allocator.Error!void {
        for (self.plan.generated_parser_try_plans.items) |existing| {
            if (existing.worker != worker or !typeRefEql(existing.try_type, try_type)) continue;
            if (!typeRefEql(existing.ok_type, ok_type) or
                !typeRefEql(existing.error_type, error_type) or
                existing.missing != kinds.missing or
                existing.null != kinds.null)
            {
                boxyPlanInvariant("generated Try parser plan had conflicting checked identities");
            }
            return;
        }
        try self.plan.generated_parser_try_plans.append(self.allocator, .{
            .worker = worker,
            .try_type = try_type,
            .ok_type = ok_type,
            .error_type = error_type,
            .missing = kinds.missing,
            .null = kinds.null,
        });
    }

    fn appendGeneratedParserDictionaryFieldSelection(
        self: *Builder,
        worker: WorkerPlanId,
        key_type: CheckedTypeIdentity,
        strategy: GeneratedParserDictionaryFieldStrategy,
    ) Allocator.Error!void {
        for (self.plan.generated_parser_dictionary_field_selections.items) |existing| {
            if (existing.worker != worker or !typeRefEql(existing.key_type, key_type)) continue;
            if (!std.meta.eql(existing.strategy, strategy)) {
                boxyPlanInvariant("generated Dict key parser plan had conflicting checked strategies");
            }
            return;
        }
        try self.plan.generated_parser_dictionary_field_selections.append(self.allocator, .{
            .worker = worker,
            .key_type = key_type,
            .strategy = strategy,
        });
    }

    fn propagateGeneratedParserTagCallLink(
        self: *Builder,
        worker: WorkerPlanId,
        shape_type: CheckedTypeIdentity,
        backing_type: CheckedTypeIdentity,
    ) Allocator.Error!void {
        const call_shape = self.plan.generatedParserTagCallShape(worker, backing_type) orelse return;
        try self.appendGeneratedParserTagCallLink(worker, shape_type, call_shape);
    }

    fn appendGeneratedParserTagCallLink(
        self: *Builder,
        worker: WorkerPlanId,
        shape_type: CheckedTypeIdentity,
        call_shape_type: CheckedTypeIdentity,
    ) Allocator.Error!void {
        for (self.plan.generated_parser_tag_call_links.items) |existing| {
            if (existing.contract_worker != worker or !typeRefEql(existing.shape_type, shape_type)) continue;
            if (!typeRefEql(existing.call_shape_type, call_shape_type)) {
                boxyPlanInvariant("generated parser shape selected conflicting checked tag call subjects");
            }
            return;
        }
        try self.plan.generated_parser_tag_call_links.append(self.allocator, .{
            .contract_worker = worker,
            .shape_type = shape_type,
            .call_shape_type = call_shape_type,
        });
    }

    fn planGeneratedParserTagRow(
        self: *Builder,
        worker: WorkerPlanId,
        row_type: CheckedTypeIdentity,
        encoding_type: CheckedTypeIdentity,
        depth: u16,
    ) Allocator.Error!void {
        if (depth == 1024) boxyPlanInvariant("generated parser tag row exceeded planner limit");
        const view = self.moduleForId(row_type.module);
        switch (view.checked_types.payload(row_type.ty)) {
            .tag_union => |row| {
                for (row.tags) |tag| {
                    const args = tag.argsSlice(view.checked_types);
                    if (args.len > 1) {
                        _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "parse_array_start", null);
                        _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "parse_array_next", null);
                        _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "parse_array_after_element", null);
                        _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "invalid_value", null);
                    }
                    for (args) |arg| {
                        try self.planGeneratedParserShape(worker, typeRef(view, arg), encoding_type);
                    }
                }
                try self.planGeneratedParserTagRow(worker, typeRef(view, row.ext), encoding_type, depth + 1);
            },
            .alias => |alias| try self.planGeneratedParserTagRow(
                worker,
                typeRef(view, alias.backing),
                encoding_type,
                depth + 1,
            ),
            .nominal => |nominal| {
                const backing = try self.nominalBackingSource(view, nominal);
                try self.planGeneratedParserTagRow(
                    worker,
                    typeRef(backing.view, backing.ty),
                    encoding_type,
                    depth + 1,
                );
            },
            .empty_tag_union => {},
            .flex, .rigid => |variable| {
                if (variable.row_default != .empty_tag_union) {
                    boxyPlanInvariant("generated parser tag row had a nonempty open extension");
                }
            },
            .pending => boxyPlanInvariant("pending tag row reached generated parser planning"),
            .err, .record, .record_unbound, .tuple, .function, .empty_record => boxyPlanInvariant("generated parser tag row extension was not a tag row"),
        }
    }

    fn generatedParserRuntimeSchema(
        self: *Builder,
        root_shape: CheckedTypeIdentity,
    ) Allocator.Error!CheckedTypeIdentity {
        var shape = root_shape;
        var depth: u16 = 0;
        while (true) {
            if (depth == 1024) boxyPlanInvariant("generated parser schema wrapper chain exceeded planner limit");
            depth += 1;

            const view = self.moduleForId(shape.module);
            switch (view.checked_types.payload(shape.ty)) {
                .alias => |alias| shape = typeRef(view, alias.backing),
                .nominal => |nominal| {
                    if (nominal.builtin != null) return shape;
                    if (methodOwnerForModuleType(view, shape.ty)) |owner| {
                        if (view.canonical_names.?.lookupMethodName("parser_for")) |parser_for| {
                            if (self.lookupMethodTarget(view, owner, view, parser_for)) |lookup| {
                                switch (lookup.target.kind) {
                                    .procedure, .local_proc => return shape,
                                    .structural => |kind| switch (kind) {
                                        .parser => {},
                                        .encoder => boxyPlanInvariant("parser schema resolved to generated encoder target"),
                                        .equality, .hash, .map, .map_effectful => boxyPlanInvariant("parser schema resolved to a non-parser structural target"),
                                    },
                                }
                            }
                        }
                    }
                    const backing = try self.nominalBackingSource(view, nominal);
                    if (checkedTryPayloads(backing.view, backing.ty) != null) return shape;
                    shape = typeRef(backing.view, backing.ty);
                },
                .pending,
                .err,
                .flex,
                .rigid,
                .record,
                .record_unbound,
                .tuple,
                .function,
                .empty_record,
                .tag_union,
                .empty_tag_union,
                => return shape,
            }
        }
    }

    fn planGeneratedParserRecord(
        self: *Builder,
        worker: WorkerPlanId,
        record_type: CheckedTypeIdentity,
        encoding_type: CheckedTypeIdentity,
    ) Allocator.Error!void {
        const fields = try self.generatedRecordCheckedFields(record_type);
        defer self.allocator.free(fields);
        _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "parse_record_field", record_type);
        _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "skip_record_field", null);
        const rename_call = if (fields.len != 0)
            try self.ensureGeneratedCodecCall(worker, encoding_type, "rename_field", null)
        else
            null;

        var needs_required = false;
        var needs_optional = false;
        for (fields) |planned_field| {
            const field_view = self.moduleForId(planned_field.module);
            const field = planned_field.field;
            const field_type = typeRef(field_view, field.ty);
            const try_payloads = checkedTryPayloads(field_view, field.ty);
            const optional_kinds = if (try_payloads) |payloads|
                checkedTryErrorKinds(field_view, payloads.err) orelse
                    boxyPlanInvariant("generated record parser Try field had unsupported error tags")
            else
                null;
            const optional_missing = if (optional_kinds) |kinds| kinds.missing or kinds.other else false;
            const optional_null = if (optional_kinds) |kinds| kinds.null and !kinds.other else false;
            const parser_wrap_ok = optional_missing and !optional_null;
            const parse_type = if (try_payloads) |payloads|
                if (optional_null) field_type else typeRef(field_view, payloads.ok)
            else
                field_type;
            try self.plan.generated_parser_field_captures.append(self.allocator, .{
                .worker = worker,
                .record_type = record_type,
                .field_module = field_view.key,
                .field_name = field.name,
                .source_type = rename_call.?.ret_type,
                .parse_type = parse_type,
                .parser_wrap_ok = parser_wrap_ok,
                .optional_error_type = if (try_payloads) |payloads| typeRef(field_view, payloads.err) else null,
                .optional_missing = optional_missing,
                .optional_null = optional_null,
            });
            if (optional_kinds != null) {
                if (optional_missing) {
                    needs_optional = true;
                } else {
                    needs_required = true;
                }
                try self.planGeneratedParserShape(worker, parse_type, encoding_type);
            } else {
                needs_required = true;
                try self.planGeneratedParserShape(worker, field_type, encoding_type);
            }
        }
        if (needs_required) _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "missing_record_field", null);
        if (needs_optional) _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "missing_optional_field", null);
    }

    const GeneratedRecordCheckedField = struct {
        module: checked.ModuleId,
        field: checked.CheckedRecordField,
    };

    fn generatedRecordCheckedFields(
        self: *Builder,
        record_type: CheckedTypeIdentity,
    ) Allocator.Error![]GeneratedRecordCheckedField {
        var fields = std.ArrayList(GeneratedRecordCheckedField).empty;
        defer fields.deinit(self.allocator);
        var seen = std.AutoHashMap(CheckedTypeIdentity, void).init(self.allocator);
        defer seen.deinit();

        var current: ?CheckedTypeIdentity = record_type;
        while (current) |row_type| {
            const visit = try seen.getOrPut(row_type);
            if (visit.found_existing) break;

            const view = self.moduleForId(row_type.module);
            switch (view.checked_types.payload(row_type.ty)) {
                .record => |record| {
                    for (record.fields) |field| {
                        try fields.append(self.allocator, .{ .module = view.key, .field = field });
                    }
                    current = typeRef(view, record.ext);
                },
                .record_unbound => |tail_fields| {
                    for (tail_fields) |field| {
                        try fields.append(self.allocator, .{ .module = view.key, .field = field });
                    }
                    current = null;
                },
                .alias => |alias| current = typeRef(view, alias.backing),
                .empty_record => current = null,
                .flex, .rigid => |variable| {
                    if (variable.row_default != .empty_record) {
                        boxyPlanInvariant("generated record codec reached an open checked row");
                    }
                    current = null;
                },
                .pending, .err, .tuple, .nominal, .function, .tag_union, .empty_tag_union => boxyPlanInvariant("generated record codec row extension was not a record row"),
            }
        }
        return try self.allocator.dupe(GeneratedRecordCheckedField, fields.items);
    }

    fn planGeneratedEncoderShape(
        self: *Builder,
        worker: WorkerPlanId,
        contract_worker: WorkerPlanId,
        shape: CheckedTypeIdentity,
        subject_type: CheckedTypeIdentity,
        encoding_type: CheckedTypeIdentity,
    ) Allocator.Error!void {
        const visit = try self.generated_codec_shapes_seen.getOrPut(.{ .worker = worker, .shape = shape });
        if (visit.found_existing) return;

        const view = self.moduleForId(shape.module);
        switch (view.checked_types.payload(shape.ty)) {
            .pending => boxyPlanInvariant("pending checked type reached generated encoder planning"),
            .err => boxyPlanInvariant("checked error type reached generated encoder planning"),
            .flex, .rigid, .record_unbound => boxyPlanInvariant("open checked type reached generated encoder planning"),
            .function, .empty_tag_union => boxyPlanInvariant("unsupported checked type reached generated encoder planning"),
            .alias => {
                const shape_rep = try self.analyzeType(view, shape.ty);
                const backing = requiredSingleChildOf(&self.plan, shape_rep, .alias_backing);
                try self.planGeneratedEncoderShape(
                    worker,
                    contract_worker,
                    backing.source_type,
                    subject_type,
                    encoding_type,
                );
            },
            .record => try self.planGeneratedEncoderRecord(
                worker,
                contract_worker,
                shape,
                subject_type,
                encoding_type,
            ),
            .empty_record => try self.planGeneratedEncoderRecord(
                worker,
                contract_worker,
                shape,
                subject_type,
                encoding_type,
            ),
            .tuple => |elems| try self.planGeneratedEncoderSequence(
                worker,
                contract_worker,
                shape,
                subject_type,
                view,
                elems,
                encoding_type,
                "encode_tuple",
            ),
            .tag_union => try self.planGeneratedEncoderTagUnion(
                worker,
                contract_worker,
                shape,
                subject_type,
                encoding_type,
            ),
            .nominal => |nominal| {
                if (nominal.builtin) |builtin| {
                    if (generatedEncoderScalarMethod(builtin)) |method_text| {
                        _ = try self.ensureGeneratedCodecCall(worker, encoding_type, method_text, subject_type);
                        return;
                    }
                    switch (builtin) {
                        .box => {
                            if (nominal.args.len != 1) boxyPlanInvariant("Box generated encoder type had unexpected arity");
                            try self.planGeneratedEncoderShape(
                                worker,
                                contract_worker,
                                typeRef(view, nominal.args[0]),
                                typeRef(view, nominal.args[0]),
                                encoding_type,
                            );
                        },
                        .list => {
                            if (nominal.args.len != 1) boxyPlanInvariant("List generated encoder type had unexpected arity");
                            try self.planGeneratedEncoderList(
                                worker,
                                contract_worker,
                                shape,
                                subject_type,
                                typeRef(view, nominal.args[0]),
                                encoding_type,
                            );
                        },
                        .set => {
                            if (nominal.args.len != 1) boxyPlanInvariant("Set generated encoder type had unexpected arity");
                            const to_list = try self.ensureGeneratedCodecCall(worker, shape, "to_list", shape);
                            try self.planGeneratedEncoderList(
                                worker,
                                contract_worker,
                                to_list.ret_type,
                                to_list.ret_type,
                                typeRef(view, nominal.args[0]),
                                encoding_type,
                            );
                        },
                        .dict => {
                            if (nominal.args.len != 2) boxyPlanInvariant("Dict generated encoder type had unexpected arity");
                            try self.planGeneratedEncoderDict(
                                worker,
                                contract_worker,
                                shape,
                                typeRef(view, nominal.args[0]),
                                typeRef(view, nominal.args[1]),
                                encoding_type,
                            );
                        },
                        .bool,
                        .try_,
                        .str,
                        .u8,
                        .i8,
                        .u16,
                        .i16,
                        .u32,
                        .i32,
                        .u64,
                        .i64,
                        .u128,
                        .i128,
                        .f32,
                        .f64,
                        .dec,
                        .u8x16,
                        .i8x16,
                        .u16x8,
                        .i16x8,
                        .u32x4,
                        .i32x4,
                        .u64x2,
                        .i64x2,
                        .iter,
                        .parse_tag_union_spec,
                        .fields,
                        .field,
                        .crypto_sha256_digest,
                        .crypto_sha256_hasher,
                        .crypto_blake3_digest,
                        .crypto_blake3_hasher,
                        => boxyPlanInvariant("unsupported builtin reached generated encoder planning"),
                    }
                    return;
                }

                if (methodOwnerForModuleType(view, shape.ty)) |owner| {
                    if (view.canonical_names.?.lookupMethodName("encoder_for")) |encoder_for| {
                        if (self.lookupMethodTarget(view, owner, view, encoder_for)) |lookup| {
                            switch (lookup.target.kind) {
                                .procedure, .local_proc => {
                                    _ = try self.ensureGeneratedCodecCall(worker, shape, "encoder_for", shape);
                                    return;
                                },
                                .structural => |kind| switch (kind) {
                                    .encoder => {},
                                    .parser => boxyPlanInvariant("encoder planning resolved to generated parser target"),
                                    .equality, .hash, .map, .map_effectful => boxyPlanInvariant("encoder planning resolved to a non-encoder structural target"),
                                },
                            }
                        }
                    }
                }

                if (checkedTryPayloads(view, shape.ty)) |try_payloads| {
                    const kinds = checkedTryErrorKinds(view, try_payloads.err) orelse
                        boxyPlanInvariant("generated Try encoder had unsupported error tags");
                    if (kinds.other) {
                        boxyPlanInvariant("generated Try encoder had unsupported error tags");
                    }
                    if (kinds.missing) {
                        boxyPlanInvariant("generated root Try encoder included Missing");
                    }
                    var found = false;
                    for (self.plan.generated_encoder_try_plans.items) |planned| {
                        if (planned.worker == worker and typeRefEql(planned.try_type, shape)) {
                            if (!typeRefEql(planned.ok_type, typeRef(view, try_payloads.ok)) or
                                planned.missing != kinds.missing or planned.null != kinds.null)
                            {
                                boxyPlanInvariant("generated Try encoder shape had conflicting plans");
                            }
                            found = true;
                            break;
                        }
                    }
                    if (!found) try self.plan.generated_encoder_try_plans.append(self.allocator, .{
                        .worker = worker,
                        .try_type = shape,
                        .ok_type = typeRef(view, try_payloads.ok),
                        .missing = kinds.missing,
                        .null = kinds.null,
                    });
                    if (kinds.null) {
                        _ = try self.ensureGeneratedCodecCall(worker, encoding_type, "encode_null", null);
                    }
                    try self.planGeneratedEncoderShape(
                        worker,
                        contract_worker,
                        typeRef(view, try_payloads.ok),
                        typeRef(view, try_payloads.ok),
                        encoding_type,
                    );
                    return;
                }

                const shape_rep = try self.analyzeType(view, shape.ty);
                const backing = requiredSingleChildOf(&self.plan, shape_rep, .nominal_backing);
                try self.planGeneratedEncoderShape(
                    worker,
                    contract_worker,
                    backing.source_type,
                    subject_type,
                    encoding_type,
                );
            },
        }
    }

    fn planGeneratedEncoderSequence(
        self: *Builder,
        worker: WorkerPlanId,
        contract_worker: WorkerPlanId,
        sequence_shape: CheckedTypeIdentity,
        sequence_type: CheckedTypeIdentity,
        item_view: ModuleView,
        item_types: []const checked.CheckedTypeId,
        encoding_type: CheckedTypeIdentity,
        method_text: []const u8,
    ) Allocator.Error!void {
        const encode_call = try self.ensureGeneratedCodecCall(worker, encoding_type, method_text, null);
        const arg_types = self.plan.generatedCodecCallTypeSlice(encode_call.arg_types);
        if (arg_types.len != 3) boxyPlanInvariant("generated sequence encoder call did not have three arguments");
        const body_rep = try self.analyzeType(self.moduleForId(arg_types[2].module), arg_types[2].ty);
        const body_fn = (self.repQuery().functionChildren(body_rep)) orelse
            boxyPlanInvariant("generated sequence encoder body argument was not callable");
        if (body_fn.arg_count != 2) boxyPlanInvariant("generated sequence encoder body had an unexpected arity");
        const body_children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(body_fn.rep)].children);
        const body_args = body_children[body_fn.args_start..][0..body_fn.arg_count];
        const writer_fn = (self.repQuery().functionChildren(body_args[1].rep)) orelse
            boxyPlanInvariant("generated sequence element writer was not callable");
        if (writer_fn.arg_count != 2) boxyPlanInvariant("generated sequence element writer had an unexpected arity");
        const writer_children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(writer_fn.rep)].children);
        const writer_args = writer_children[writer_fn.args_start..][0..writer_fn.arg_count];
        const thunk_type = writer_args[1].source_type;

        _ = try self.ensureWorker(.{ .generated_codec = .{
            .kind = .encoder_sequence_elements,
            .shape = sequence_shape,
            .value_type = sequence_type,
            .capture_type = encoding_type,
            .contract_worker = contract_worker,
            .contract_expr = self.generatedCodecSourceForWorker(contract_worker).contract_expr,
        } }, arg_types[2], null);

        for (item_types) |item_ty| {
            const item_type = typeRef(item_view, item_ty);
            const thunk_worker = try self.ensureWorker(.{ .generated_codec = .{
                .kind = .encoder_value_thunk,
                .shape = item_type,
                .capture_type = encoding_type,
                .contract_worker = contract_worker,
                .contract_expr = self.generatedCodecSourceForWorker(contract_worker).contract_expr,
            } }, thunk_type, null);
            try self.planGeneratedEncoderShape(
                thunk_worker,
                contract_worker,
                item_type,
                item_type,
                encoding_type,
            );
        }
    }

    fn planGeneratedEncoderList(
        self: *Builder,
        worker: WorkerPlanId,
        contract_worker: WorkerPlanId,
        list_shape: CheckedTypeIdentity,
        list_type: CheckedTypeIdentity,
        elem_type: CheckedTypeIdentity,
        encoding_type: CheckedTypeIdentity,
    ) Allocator.Error!void {
        const encode_call = try self.ensureGeneratedCodecCall(worker, encoding_type, "encode_list", null);
        const arg_types = self.plan.generatedCodecCallTypeSlice(encode_call.arg_types);
        if (arg_types.len != 3) boxyPlanInvariant("generated list encoder call did not have three arguments");
        const body_rep = try self.analyzeType(self.moduleForId(arg_types[2].module), arg_types[2].ty);
        const body_fn = (self.repQuery().functionChildren(body_rep)) orelse
            boxyPlanInvariant("generated list encoder body argument was not callable");
        if (body_fn.arg_count != 2) boxyPlanInvariant("generated list encoder body had an unexpected arity");
        const body_children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(body_fn.rep)].children);
        const body_args = body_children[body_fn.args_start..][0..body_fn.arg_count];
        const writer_fn = (self.repQuery().functionChildren(body_args[1].rep)) orelse
            boxyPlanInvariant("generated list element writer was not callable");
        if (writer_fn.arg_count != 2) boxyPlanInvariant("generated list element writer had an unexpected arity");
        const writer_children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(writer_fn.rep)].children);
        const writer_args = writer_children[writer_fn.args_start..][0..writer_fn.arg_count];
        const thunk_type = writer_args[1].source_type;
        const contract_expr = self.generatedCodecSourceForWorker(contract_worker).contract_expr;

        _ = try self.ensureWorker(.{ .generated_codec = .{
            .kind = .encoder_sequence_elements,
            .shape = list_shape,
            .value_type = list_type,
            .capture_type = encoding_type,
            .contract_worker = contract_worker,
            .contract_expr = contract_expr,
        } }, arg_types[2], null);
        const thunk_worker = try self.ensureWorker(.{ .generated_codec = .{
            .kind = .encoder_value_thunk,
            .shape = elem_type,
            .capture_type = encoding_type,
            .contract_worker = contract_worker,
            .contract_expr = contract_expr,
        } }, thunk_type, null);
        try self.planGeneratedEncoderShape(
            thunk_worker,
            contract_worker,
            elem_type,
            elem_type,
            encoding_type,
        );
    }

    fn planGeneratedEncoderDict(
        self: *Builder,
        worker: WorkerPlanId,
        contract_worker: WorkerPlanId,
        dict_shape: CheckedTypeIdentity,
        key_type: CheckedTypeIdentity,
        value_type: CheckedTypeIdentity,
        encoding_type: CheckedTypeIdentity,
    ) Allocator.Error!void {
        const to_list = try self.ensureGeneratedCodecCall(worker, dict_shape, "to_list", dict_shape);
        const encode_record = try self.ensureGeneratedCodecCall(worker, encoding_type, "encode_record", null);
        const arg_types = self.plan.generatedCodecCallTypeSlice(encode_record.arg_types);
        if (arg_types.len != 3) boxyPlanInvariant("generated Dict encode_record call did not have three arguments");
        const body_rep = try self.analyzeType(self.moduleForId(arg_types[2].module), arg_types[2].ty);
        const body_fn = (self.repQuery().functionChildren(body_rep)) orelse
            boxyPlanInvariant("generated Dict encoder body argument was not callable");
        if (body_fn.arg_count != 2) boxyPlanInvariant("generated Dict encoder body had an unexpected arity");
        const body_children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(body_fn.rep)].children);
        const body_args = body_children[body_fn.args_start..][0..body_fn.arg_count];
        const writer_fn = (self.repQuery().functionChildren(body_args[1].rep)) orelse
            boxyPlanInvariant("generated Dict field writer was not callable");
        if (writer_fn.arg_count != 3) boxyPlanInvariant("generated Dict field writer had an unexpected arity");
        const writer_children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(writer_fn.rep)].children);
        const writer_args = writer_children[writer_fn.args_start..][0..writer_fn.arg_count];
        const thunk_type = writer_args[2].source_type;
        const contract_expr = self.generatedCodecSourceForWorker(contract_worker).contract_expr;
        const callback_source = GeneratedCodecSource{
            .kind = .encoder_dict_fields,
            .shape = to_list.ret_type,
            .value_type = to_list.ret_type,
            .capture_type = encoding_type,
            .contract_worker = contract_worker,
            .contract_expr = contract_expr,
        };
        const callback_worker = try self.ensureWorker(
            .{ .generated_codec = callback_source },
            arg_types[2],
            null,
        );
        if (generatedEncoderKeyMethod(self.moduleForId(key_type.module), key_type.ty)) |method_text| {
            _ = try self.ensureGeneratedCodecCall(callback_worker, encoding_type, method_text, key_type);
        }
        const thunk_worker = try self.ensureWorker(.{ .generated_codec = .{
            .kind = .encoder_value_thunk,
            .shape = value_type,
            .capture_type = encoding_type,
            .contract_worker = contract_worker,
            .contract_expr = contract_expr,
        } }, thunk_type, null);
        try self.planGeneratedEncoderShape(
            thunk_worker,
            contract_worker,
            value_type,
            value_type,
            encoding_type,
        );
    }

    fn planGeneratedEncoderTagUnion(
        self: *Builder,
        worker: WorkerPlanId,
        contract_worker: WorkerPlanId,
        tag_shape: CheckedTypeIdentity,
        tag_type: CheckedTypeIdentity,
        encoding_type: CheckedTypeIdentity,
    ) Allocator.Error!void {
        const tag_rep = try self.analyzeType(self.moduleForId(tag_shape.module), tag_shape.ty);
        const row_reps = try self.generatedEncoderTagRowReps(tag_rep);
        defer self.allocator.free(row_reps);

        var has_unit = false;
        var has_payload = false;
        var has_multiple_payloads = false;
        for (row_reps) |row_rep| {
            const row = self.plan.representations.items[@intFromEnum(row_rep)];
            for (self.plan.tagVariantSlice(row.tag_variants)) |variant| {
                const payloads = self.plan.childSlice(variant.payloads);
                if (payloads.len == 0) {
                    has_unit = true;
                } else {
                    has_payload = true;
                    has_multiple_payloads = has_multiple_payloads or payloads.len > 1;
                }
            }
        }
        if (!has_unit and !has_payload) {
            boxyPlanInvariant("generated encoder tag union had no variants");
        }

        if (has_unit) {
            _ = try self.ensureGeneratedCodecCallWithCheckedSubject(worker, encoding_type, "encode_str");
        }
        if (!has_payload) return;
        const encode_record = try self.ensureGeneratedCodecCall(worker, encoding_type, "encode_record", null);
        const record_arg_types = self.plan.generatedCodecCallTypeSlice(encode_record.arg_types);
        if (record_arg_types.len != 3) {
            boxyPlanInvariant("generated tag encoder encode_record call did not have three arguments");
        }
        const record_body_rep = try self.analyzeType(
            self.moduleForId(record_arg_types[2].module),
            record_arg_types[2].ty,
        );
        const record_body_fn = (self.repQuery().functionChildren(record_body_rep)) orelse
            boxyPlanInvariant("generated tag encoder record body was not callable");
        if (record_body_fn.arg_count != 2) {
            boxyPlanInvariant("generated tag encoder record body had an unexpected arity");
        }
        const record_body_children = self.plan.childSlice(
            self.plan.representations.items[@intFromEnum(record_body_fn.rep)].children,
        );
        const record_body_args = record_body_children[record_body_fn.args_start..][0..record_body_fn.arg_count];
        const field_writer_fn = (self.repQuery().functionChildren(record_body_args[1].rep)) orelse
            boxyPlanInvariant("generated tag encoder field writer was not callable");
        if (field_writer_fn.arg_count != 3) {
            boxyPlanInvariant("generated tag encoder field writer had an unexpected arity");
        }
        const field_writer_children = self.plan.childSlice(
            self.plan.representations.items[@intFromEnum(field_writer_fn.rep)].children,
        );
        const field_writer_args = field_writer_children[field_writer_fn.args_start..][0..field_writer_fn.arg_count];
        const payload_thunk_type = field_writer_args[2].source_type;
        const contract_expr = self.generatedCodecSourceForWorker(contract_worker).contract_expr;

        _ = try self.ensureWorker(.{ .generated_codec = .{
            .kind = .encoder_tag_field,
            .shape = tag_shape,
            .value_type = tag_type,
            .capture_type = encoding_type,
            .contract_worker = contract_worker,
            .contract_expr = contract_expr,
        } }, record_arg_types[2], null);
        const payload_thunk_worker = try self.ensureWorker(.{ .generated_codec = .{
            .kind = .encoder_tag_payload_thunk,
            .shape = tag_shape,
            .value_type = tag_type,
            .capture_type = encoding_type,
            .contract_worker = contract_worker,
            .contract_expr = contract_expr,
        } }, payload_thunk_type, null);

        var element_thunk_type: ?CheckedTypeIdentity = null;
        if (has_multiple_payloads) {
            const encode_tuple = try self.ensureGeneratedCodecCall(
                payload_thunk_worker,
                encoding_type,
                "encode_tuple",
                null,
            );
            const tuple_arg_types = self.plan.generatedCodecCallTypeSlice(encode_tuple.arg_types);
            if (tuple_arg_types.len != 3) {
                boxyPlanInvariant("generated tag payload encode_tuple call did not have three arguments");
            }
            const tuple_body_rep = try self.analyzeType(
                self.moduleForId(tuple_arg_types[2].module),
                tuple_arg_types[2].ty,
            );
            const tuple_body_fn = (self.repQuery().functionChildren(tuple_body_rep)) orelse
                boxyPlanInvariant("generated tag payload tuple body was not callable");
            if (tuple_body_fn.arg_count != 2) {
                boxyPlanInvariant("generated tag payload tuple body had an unexpected arity");
            }
            const tuple_body_children = self.plan.childSlice(
                self.plan.representations.items[@intFromEnum(tuple_body_fn.rep)].children,
            );
            const tuple_body_args = tuple_body_children[tuple_body_fn.args_start..][0..tuple_body_fn.arg_count];
            const element_writer_fn = (self.repQuery().functionChildren(tuple_body_args[1].rep)) orelse
                boxyPlanInvariant("generated tag payload element writer was not callable");
            if (element_writer_fn.arg_count != 2) {
                boxyPlanInvariant("generated tag payload element writer had an unexpected arity");
            }
            const element_writer_children = self.plan.childSlice(
                self.plan.representations.items[@intFromEnum(element_writer_fn.rep)].children,
            );
            const element_writer_args = element_writer_children[element_writer_fn.args_start..][0..element_writer_fn.arg_count];
            element_thunk_type = element_writer_args[1].source_type;
            _ = try self.ensureWorker(.{ .generated_codec = .{
                .kind = .encoder_tag_payload_elements,
                .shape = tag_shape,
                .value_type = tag_type,
                .capture_type = encoding_type,
                .contract_worker = contract_worker,
                .contract_expr = contract_expr,
            } }, tuple_arg_types[2], null);
        }

        for (row_reps) |row_rep| {
            const row = self.plan.representations.items[@intFromEnum(row_rep)];
            for (self.plan.tagVariantSlice(row.tag_variants)) |variant| {
                const payloads = self.plan.childSlice(variant.payloads);
                if (payloads.len == 1) {
                    try self.planGeneratedEncoderShape(
                        payload_thunk_worker,
                        contract_worker,
                        payloads[0].source_type,
                        payloads[0].source_type,
                        encoding_type,
                    );
                } else if (payloads.len > 1) {
                    for (payloads) |payload| {
                        const thunk_worker = try self.ensureWorker(.{ .generated_codec = .{
                            .kind = .encoder_value_thunk,
                            .shape = payload.source_type,
                            .capture_type = encoding_type,
                            .contract_worker = contract_worker,
                            .contract_expr = contract_expr,
                        } }, element_thunk_type.?, null);
                        try self.planGeneratedEncoderShape(
                            thunk_worker,
                            contract_worker,
                            payload.source_type,
                            payload.source_type,
                            encoding_type,
                        );
                    }
                }
            }
        }
    }

    fn generatedEncoderTagRowReps(
        self: *Builder,
        source_rep: TypeRepId,
    ) Allocator.Error![]TypeRepId {
        var rows = std.ArrayList(TypeRepId).empty;
        defer rows.deinit(self.allocator);
        var seen = collections.DenseMap(TypeRepId, void).init(self.allocator);
        defer seen.deinit();

        var current = source_rep;
        rows_loop: while (true) {
            const rep = self.plan.representations.items[@intFromEnum(current)];
            switch (rep.kind) {
                .alias => {
                    current = requiredSingleChildOf(&self.plan, current, .alias_backing).rep;
                    continue;
                },
                .nominal => {
                    current = requiredSingleChildOf(&self.plan, current, .nominal_backing).rep;
                    continue;
                },
                .tag_union => {},
                .empty_tag_union => break :rows_loop,
                .dynamic => boxyPlanInvariant("generated tag encoder shape had a dynamic row representation"),
                .bool_tag_union => boxyPlanInvariant("generated tag encoder shape unexpectedly used the Bool representation"),
                .in_progress,
                .primitive,
                .erased_callable,
                .record,
                .record_unbound,
                .tuple,
                .list,
                .box,
                .generated_field,
                .generated_field_names,
                .generated_tag_union_spec,
                .empty_record,
                => boxyPlanInvariant("generated tag encoder shape was not a closed tag union"),
            }
            const visit = try seen.getOrPut(current);
            if (visit.found_existing) boxyPlanInvariant("generated tag encoder row representation was cyclic");
            try rows.append(self.allocator, current);

            var extension: ?TypeRepId = null;
            for (self.plan.childSlice(rep.children)) |child| {
                if (child.role != .tag_ext) continue;
                if (extension != null) boxyPlanInvariant("generated tag encoder row had duplicate extensions");
                extension = child.rep;
            }
            current = extension orelse break;
        }
        return try self.allocator.dupe(TypeRepId, rows.items);
    }

    fn planGeneratedEncoderRecord(
        self: *Builder,
        worker: WorkerPlanId,
        contract_worker: WorkerPlanId,
        record_shape: CheckedTypeIdentity,
        record_type: CheckedTypeIdentity,
        encoding_type: CheckedTypeIdentity,
    ) Allocator.Error!void {
        const fields = try self.generatedRecordCheckedFields(record_shape);
        defer self.allocator.free(fields);
        const encode_call = try self.ensureGeneratedCodecCall(worker, encoding_type, "encode_record", null);
        const arg_types = self.plan.generatedCodecCallTypeSlice(encode_call.arg_types);
        if (arg_types.len != 3) {
            boxyPlanInvariant("generated encode_record call did not have three arguments");
        }
        const body_rep = try self.analyzeType(self.moduleForId(arg_types[2].module), arg_types[2].ty);
        const body_fn = (self.repQuery().functionChildren(body_rep)) orelse
            boxyPlanInvariant("generated encode_record body argument was not callable");
        if (body_fn.arg_count != 2) {
            boxyPlanInvariant("generated encode_record body had an unexpected arity");
        }
        const body_children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(body_fn.rep)].children);
        const body_args = body_children[body_fn.args_start..][0..body_fn.arg_count];
        const writer_rep = body_args[1].rep;
        const writer_fn = (self.repQuery().functionChildren(writer_rep)) orelse
            boxyPlanInvariant("generated encode_record field writer was not callable");
        if (writer_fn.arg_count != 3) {
            boxyPlanInvariant("generated encode_record field writer had an unexpected arity");
        }
        const writer_children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(writer_fn.rep)].children);
        const writer_args = writer_children[writer_fn.args_start..][0..writer_fn.arg_count];
        const thunk_type = writer_args[2].source_type;

        _ = try self.ensureWorker(.{ .generated_codec = .{
            .kind = .encoder_record_fields,
            .shape = record_shape,
            .value_type = record_type,
            .capture_type = encoding_type,
            .contract_worker = contract_worker,
            .contract_expr = self.generatedCodecSourceForWorker(contract_worker).contract_expr,
        } }, arg_types[2], null);

        const rename_call = if (fields.len != 0)
            try self.ensureGeneratedCodecCall(contract_worker, encoding_type, "rename_field", null)
        else
            null;
        for (fields) |planned_field| {
            const field_view = self.moduleForId(planned_field.module);
            const field = planned_field.field;
            const field_type = typeRef(field_view, field.ty);
            const try_payloads = checkedTryPayloads(field_view, field.ty);
            const encode_type = if (try_payloads) |payloads| typeRef(field_view, payloads.ok) else field_type;
            const optional_kinds = if (try_payloads) |payloads|
                checkedTryErrorKinds(field_view, payloads.err) orelse
                    boxyPlanInvariant("generated record encoder Try field had unsupported error tags")
            else
                null;
            if (optional_kinds) |kinds| {
                if (kinds.other) {
                    boxyPlanInvariant("generated record encoder Try field had unsupported error tags");
                }
            }
            var found_capture = false;
            for (self.plan.generated_parser_field_captures.items) |capture| {
                if (capture.worker == contract_worker and
                    typeRefEql(capture.record_type, record_shape) and
                    moduleKeyEqual(capture.field_module, field_view.key) and
                    capture.field_name == field.name)
                {
                    found_capture = true;
                    break;
                }
            }
            if (!found_capture) {
                try self.plan.generated_parser_field_captures.append(self.allocator, .{
                    .worker = contract_worker,
                    .record_type = record_shape,
                    .field_module = field_view.key,
                    .field_name = field.name,
                    .source_type = rename_call.?.ret_type,
                    .parse_type = encode_type,
                    .optional_error_type = if (try_payloads) |payloads| typeRef(field_view, payloads.err) else null,
                    .optional_missing = if (optional_kinds) |kinds| kinds.missing else false,
                    .optional_null = if (optional_kinds) |kinds| kinds.null else false,
                });
            }
            const thunk_worker = try self.ensureWorker(.{ .generated_codec = .{
                .kind = .encoder_value_thunk,
                .shape = encode_type,
                .value_type = if (try_payloads != null) field_type else null,
                .optional_missing = if (optional_kinds) |kinds| kinds.missing else false,
                .optional_null = if (optional_kinds) |kinds| kinds.null else false,
                .capture_type = encoding_type,
                .contract_worker = contract_worker,
                .contract_expr = self.generatedCodecSourceForWorker(contract_worker).contract_expr,
            } }, thunk_type, null);
            if (optional_kinds) |kinds| {
                if (kinds.null) {
                    _ = try self.ensureGeneratedCodecCall(thunk_worker, encoding_type, "encode_null", null);
                }
            }
            try self.planGeneratedEncoderShape(thunk_worker, contract_worker, encode_type, encode_type, encoding_type);
        }
    }

    fn generatedCodecSourceForWorker(self: *const Builder, worker: WorkerPlanId) GeneratedCodecSource {
        return switch (self.plan.workers.items[@intFromEnum(worker)].source) {
            .generated_codec => |source| source,
            .procedure_template,
            .procedure_binding,
            .procedure_use,
            .nested_expr,
            .generated_field_iterator,
            .generated_interpolation_step,
            => boxyPlanInvariant("generated codec contract worker was not generated"),
        };
    }

    fn analyzeType(self: *Builder, view: ModuleView, ty: checked.CheckedTypeId) Allocator.Error!TypeRepId {
        const source_type = typeRef(view, ty);
        const entry = try self.by_type.getOrPut(source_type);
        if (entry.found_existing) return entry.value_ptr.*;

        const rep_id: TypeRepId = @enumFromInt(@as(u32, @intCast(self.plan.representations.items.len)));
        entry.value_ptr.* = rep_id;
        try self.plan.representations.append(self.allocator, .{
            .source_type = source_type,
            .kind = .in_progress,
        });
        try self.plan.type_reps.append(self.allocator, .{
            .source_type = source_type,
            .rep = rep_id,
        });

        const rep = try self.buildRepresentation(view, ty);
        self.plan.representations.items[@intFromEnum(rep_id)] = rep;
        return rep_id;
    }

    fn buildRepresentation(self: *Builder, view: ModuleView, ty: checked.CheckedTypeId) Allocator.Error!TypeRepresentation {
        const source_type = typeRef(view, ty);
        const payload = view.checked_types.payload(ty);
        return switch (payload) {
            .pending => boxyPlanInvariant("checked type payload was pending during boxy planning"),
            .err => boxyPlanInvariant("checked error type reached boxy representation planning"),
            .flex => |flex| try self.dynamicRepresentation(source_type, flex.constraints, .flex),
            .rigid => |rigid| try self.dynamicRepresentation(source_type, rigid.constraints, .rigid),
            .alias => |alias| try self.aliasRepresentation(view, source_type, alias),
            .record => |record| try self.recordRepresentation(view, source_type, .record, record.fields, record.ext),
            .record_unbound => |fields| try self.recordRepresentation(view, source_type, .record_unbound, fields, null),
            .tuple => |elems| try self.tupleRepresentation(view, source_type, elems),
            .nominal => |nominal| try self.nominalRepresentation(view, source_type, nominal),
            .function => |function| try self.functionRepresentation(view, source_type, function),
            .empty_record => .{ .source_type = source_type, .kind = .empty_record },
            .tag_union => |tag_union| try self.tagUnionRepresentation(view, source_type, tag_union),
            .empty_tag_union => .{ .source_type = source_type, .kind = .empty_tag_union },
        };
    }

    fn analyzeStoredType(
        self: *Builder,
        store_view: ModuleView,
        ty: check.ConstStore.ConstTypeId,
        checked_source: CheckedTypeIdentity,
    ) Allocator.Error!TypeRepId {
        const source_type = StoredTypeIdentity{ .module = store_view.key, .ty = ty };
        const entry = try self.by_stored_type.getOrPut(source_type);
        if (entry.found_existing) return entry.value_ptr.*;

        const rep_id: TypeRepId = @enumFromInt(@as(u32, @intCast(self.plan.representations.items.len)));
        entry.value_ptr.* = rep_id;
        try self.plan.representations.append(self.allocator, .{
            .source_type = checked_source,
            .kind = .in_progress,
        });
        try self.plan.stored_type_reps.append(self.allocator, .{
            .source_type = source_type,
            .rep = rep_id,
        });

        const rep = try self.buildStoredRepresentation(store_view, ty, checked_source);
        self.plan.representations.items[@intFromEnum(rep_id)] = rep;
        return rep_id;
    }

    fn buildStoredRepresentation(
        self: *Builder,
        store_view: ModuleView,
        ty: check.ConstStore.ConstTypeId,
        checked_source: CheckedTypeIdentity,
    ) Allocator.Error!TypeRepresentation {
        const store = store_view.const_store orelse
            boxyPlanInvariant("stored representation planning had no ConstStore");
        return switch (store.type_store.get(ty)) {
            .primitive => |primitive| .{
                .source_type = checked_source,
                .kind = .{ .primitive = primitive },
            },
            .zst => .{ .source_type = checked_source, .kind = .empty_record },
            .erased => .{
                .source_type = checked_source,
                .kind = .{ .dynamic = .rigid },
                .contains_dynamic = true,
            },
            .list => |elem| try self.storedUnaryRepresentation(
                store_view,
                checked_source,
                .list,
                .list_elem,
                elem,
            ),
            .box => |elem| try self.storedUnaryRepresentation(
                store_view,
                checked_source,
                .box,
                .box_payload,
                elem,
            ),
            .tuple => |items| try self.storedTupleRepresentation(store_view, checked_source, items),
            .record => |fields| try self.storedRecordRepresentation(store_view, checked_source, fields),
            .tag_union => |tags| try self.storedTagUnionRepresentation(store_view, checked_source, tags),
            .func => |function| try self.storedFunctionRepresentation(store_view, checked_source, function),
            .named => |named| try self.storedNamedRepresentation(store_view, checked_source, named),
        };
    }

    fn storedUnaryRepresentation(
        self: *Builder,
        store_view: ModuleView,
        checked_source: CheckedTypeIdentity,
        kind: RepresentationKind,
        role: ChildRole,
        child_ty: check.ConstStore.ConstTypeId,
    ) Allocator.Error!TypeRepresentation {
        const child_rep = try self.analyzeStoredType(store_view, child_ty, checked_source);
        const start: u32 = @intCast(self.plan.children.items.len);
        try self.plan.children.append(self.allocator, .{
            .role = role,
            .source_type = self.plan.representations.items[@intFromEnum(child_rep)].source_type,
            .rep = child_rep,
        });
        return .{
            .source_type = checked_source,
            .kind = kind,
            .children = .{ .start = start, .len = 1 },
        };
    }

    fn storedTupleRepresentation(
        self: *Builder,
        store_view: ModuleView,
        checked_source: CheckedTypeIdentity,
        items: check.ConstStore.ConstRange,
    ) Allocator.Error!TypeRepresentation {
        const store = store_view.const_store orelse
            boxyPlanInvariant("stored tuple representation had no ConstStore");
        var children = std.ArrayList(RepChild).empty;
        defer children.deinit(self.allocator);
        for (store.type_store.typeSpan(items), 0..) |item, index| {
            const rep = try self.analyzeStoredType(store_view, item, checked_source);
            try children.append(self.allocator, .{
                .role = .{ .tuple_elem = @intCast(index) },
                .source_type = self.plan.representations.items[@intFromEnum(rep)].source_type,
                .rep = rep,
            });
        }
        return .{
            .source_type = checked_source,
            .kind = .tuple,
            .children = try self.commitPendingChildren(children.items),
        };
    }

    fn storedRecordRepresentation(
        self: *Builder,
        store_view: ModuleView,
        checked_source: CheckedTypeIdentity,
        fields: check.ConstStore.ConstRange,
    ) Allocator.Error!TypeRepresentation {
        const store = store_view.const_store orelse
            boxyPlanInvariant("stored record representation had no ConstStore");
        var children = std.ArrayList(RepChild).empty;
        defer children.deinit(self.allocator);
        for (store.type_store.fieldSpan(fields)) |field| {
            const rep = try self.analyzeStoredType(store_view, field.ty, checked_source);
            try children.append(self.allocator, .{
                .role = .{ .record_field = field.name },
                .source_type = self.plan.representations.items[@intFromEnum(rep)].source_type,
                .rep = rep,
                .record_field_kind = if (field.value_ty != null)
                    checked.CheckedFieldKind.optional
                else if (field.default != null)
                    .{ .tag = .defaulted }
                else
                    checked.CheckedFieldKind.required,
            });
        }
        sortRecordFieldChildrenByName(store_view, children.items);
        return .{
            .source_type = checked_source,
            .kind = if (children.items.len == 0) .empty_record else .record,
            .children = try self.commitPendingChildren(children.items),
        };
    }

    fn storedFunctionRepresentation(
        self: *Builder,
        store_view: ModuleView,
        checked_source: CheckedTypeIdentity,
        function: anytype,
    ) Allocator.Error!TypeRepresentation {
        const store = store_view.const_store orelse
            boxyPlanInvariant("stored function representation had no ConstStore");
        var children = std.ArrayList(RepChild).empty;
        defer children.deinit(self.allocator);
        for (store.type_store.typeSpan(function.args), 0..) |arg, index| {
            const rep = try self.analyzeStoredType(store_view, arg, checked_source);
            try children.append(self.allocator, .{
                .role = .{ .function_arg = @intCast(index) },
                .source_type = self.plan.representations.items[@intFromEnum(rep)].source_type,
                .rep = rep,
            });
        }
        const ret_rep = try self.analyzeStoredType(store_view, function.ret, checked_source);
        try children.append(self.allocator, .{
            .role = .function_ret,
            .source_type = self.plan.representations.items[@intFromEnum(ret_rep)].source_type,
            .rep = ret_rep,
        });
        return .{
            .source_type = checked_source,
            .kind = .{ .erased_callable = checked.finalizedFunctionKind(checkedFunctionPayload(
                self.moduleForId(checked_source.module),
                checked_source.ty,
            ).kind) },
            .children = try self.commitPendingChildren(children.items),
        };
    }

    fn storedTagUnionRepresentation(
        self: *Builder,
        store_view: ModuleView,
        checked_source: CheckedTypeIdentity,
        tags: check.ConstStore.ConstRange,
    ) Allocator.Error!TypeRepresentation {
        const store = store_view.const_store orelse
            boxyPlanInvariant("stored tag-union representation had no ConstStore");
        const names = store_view.canonical_names orelse
            boxyPlanInvariant("stored tag-union representation had no resolved tag labels");
        const source_tags = store.type_store.tagSpan(tags);
        const ordered = try self.allocator.dupe(check.ConstStore.TypeTag, source_tags);
        defer self.allocator.free(ordered);
        std.mem.sort(check.ConstStore.TypeTag, ordered, names, struct {
            fn lessThan(name_store: *const checked_names.CanonicalNameStore, lhs: check.ConstStore.TypeTag, rhs: check.ConstStore.TypeTag) bool {
                return name_store.tagLabelTextLessThan(lhs.checked_name, rhs.checked_name);
            }
        }.lessThan);

        var children = std.ArrayList(RepChild).empty;
        defer children.deinit(self.allocator);
        for (ordered) |tag| {
            const payloads = store.type_store.typeSpan(tag.payloads);
            for (payloads, 0..) |payload, index| {
                const rep = try self.analyzeStoredType(store_view, payload, checked_source);
                try children.append(self.allocator, .{
                    .role = .{ .tag_payload = .{ .tag = tag.checked_name, .index = @intCast(index) } },
                    .source_type = self.plan.representations.items[@intFromEnum(rep)].source_type,
                    .rep = rep,
                });
            }
        }
        const child_span = try self.commitPendingChildren(children.items);
        const variant_start: u32 = @intCast(self.plan.tag_variants.items.len);
        var payload_start = child_span.start;
        for (ordered) |tag| {
            const payloads = store.type_store.typeSpan(tag.payloads);
            try self.plan.tag_variants.append(self.allocator, .{
                .name = tag.checked_name,
                .name_module = store_view.key,
                .payloads = .{ .start = payload_start, .len = @intCast(payloads.len) },
            });
            payload_start += @intCast(payloads.len);
        }
        return .{
            .source_type = checked_source,
            .kind = if (ordered.len == 0) .empty_tag_union else .tag_union,
            .children = child_span,
            .tag_variants = .{ .start = variant_start, .len = @intCast(ordered.len) },
        };
    }

    fn storedNamedRepresentation(
        self: *Builder,
        store_view: ModuleView,
        checked_source: CheckedTypeIdentity,
        named: anytype,
    ) Allocator.Error!TypeRepresentation {
        if (named.builtin_owner) |owner| switch (owner) {
            .bool => return .{ .source_type = checked_source, .kind = .bool_tag_union },
            .str => return .{ .source_type = checked_source, .kind = .{ .primitive = .str } },
            .u8 => return .{ .source_type = checked_source, .kind = .{ .primitive = .u8 } },
            .i8 => return .{ .source_type = checked_source, .kind = .{ .primitive = .i8 } },
            .u16 => return .{ .source_type = checked_source, .kind = .{ .primitive = .u16 } },
            .i16 => return .{ .source_type = checked_source, .kind = .{ .primitive = .i16 } },
            .u32 => return .{ .source_type = checked_source, .kind = .{ .primitive = .u32 } },
            .i32 => return .{ .source_type = checked_source, .kind = .{ .primitive = .i32 } },
            .u64 => return .{ .source_type = checked_source, .kind = .{ .primitive = .u64 } },
            .i64 => return .{ .source_type = checked_source, .kind = .{ .primitive = .i64 } },
            .u128 => return .{ .source_type = checked_source, .kind = .{ .primitive = .u128 } },
            .i128 => return .{ .source_type = checked_source, .kind = .{ .primitive = .i128 } },
            .f32 => return .{ .source_type = checked_source, .kind = .{ .primitive = .f32 } },
            .f64 => return .{ .source_type = checked_source, .kind = .{ .primitive = .f64 } },
            .dec => return .{ .source_type = checked_source, .kind = .{ .primitive = .dec } },
            .u8x16 => return .{ .source_type = checked_source, .kind = .{ .primitive = .u8x16 } },
            .i8x16 => return .{ .source_type = checked_source, .kind = .{ .primitive = .i8x16 } },
            .u16x8 => return .{ .source_type = checked_source, .kind = .{ .primitive = .u16x8 } },
            .i16x8 => return .{ .source_type = checked_source, .kind = .{ .primitive = .i16x8 } },
            .u32x4 => return .{ .source_type = checked_source, .kind = .{ .primitive = .u32x4 } },
            .i32x4 => return .{ .source_type = checked_source, .kind = .{ .primitive = .i32x4 } },
            .u64x2 => return .{ .source_type = checked_source, .kind = .{ .primitive = .u64x2 } },
            .i64x2 => return .{ .source_type = checked_source, .kind = .{ .primitive = .i64x2 } },
            .list, .box => {
                const store = store_view.const_store orelse
                    boxyPlanInvariant("stored builtin representation had no ConstStore");
                const args = store.type_store.typeSpan(named.args);
                if (args.len != 1) boxyPlanInvariant("stored unary builtin had an unexpected argument count");
                return try self.storedUnaryRepresentation(
                    store_view,
                    checked_source,
                    if (owner == .list) .list else .box,
                    if (owner == .list) .list_elem else .box_payload,
                    args[0],
                );
            },
            .dict,
            .set,
            .fields,
            .field,
            .parse_tag_union_spec,
            .crypto_sha256_digest,
            .crypto_sha256_hasher,
            .crypto_blake3_digest,
            .crypto_blake3_hasher,
            .iter,
            .stream,
            => {},
        };

        var children = std.ArrayList(RepChild).empty;
        defer children.deinit(self.allocator);
        var backing_rep: ?TypeRepId = null;
        if (named.backing) |backing| {
            const rep = try self.analyzeStoredType(store_view, backing.ty, checked_source);
            backing_rep = rep;
            try children.append(self.allocator, .{
                .role = if (named.kind == .alias) .alias_backing else .nominal_backing,
                .source_type = self.plan.representations.items[@intFromEnum(rep)].source_type,
                .rep = rep,
            });
        }
        const store = store_view.const_store orelse
            boxyPlanInvariant("stored named representation had no ConstStore");
        for (store.type_store.typeSpan(named.args), 0..) |arg, index| {
            const rep = try self.analyzeStoredType(store_view, arg, checked_source);
            try children.append(self.allocator, .{
                .role = if (named.kind == .alias)
                    .{ .alias_arg = @intCast(index) }
                else
                    .{ .nominal_arg = @intCast(index) },
                .source_type = self.plan.representations.items[@intFromEnum(rep)].source_type,
                .rep = rep,
            });
        }
        var declared_fields = Span.empty();
        var record_field_order: RecordFieldOrder = .structural;
        const declared_order = store.type_store.declaredFieldSpan(named.declared_order);
        var has_declared_padding = false;
        for (declared_order) |field| {
            has_declared_padding = has_declared_padding or field == .padding;
        }
        if (declared_order.len != 0) {
            if (named.kind == .alias) boxyPlanInvariant("stored alias carried nominal declared field order");
            const backing = backing_rep orelse
                boxyPlanInvariant("stored nominal declared field order had no backing representation");
            const backing_children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(backing)].children);
            var pending = std.ArrayList(DeclaredField).empty;
            defer pending.deinit(self.allocator);
            var padding_ordinal: u32 = 0;
            var backing_field_count: usize = 0;
            for (backing_children) |child| {
                if (child.role == .record_field) backing_field_count += 1;
            }
            if (backing_field_count > std.math.maxInt(u16)) {
                boxyPlanInvariant("stored nominal backing field count exceeded Boxy layout range");
            }
            for (declared_order) |declared| switch (declared) {
                .named => |name| {
                    var selected: ?DeclaredField = null;
                    var field_index: u16 = 0;
                    for (backing_children) |child| {
                        if (child.role != .record_field) continue;
                        if (child.role.record_field == name) {
                            if (selected != null) boxyPlanInvariant("stored nominal backing had a duplicate declared field");
                            selected = .{
                                .index = field_index,
                                .source_type = child.source_type,
                                .rep = child.rep,
                            };
                        }
                        field_index += 1;
                    }
                    try pending.append(self.allocator, selected orelse
                        boxyPlanInvariant("stored nominal declared field was absent from its backing record"));
                },
                .padding => |padding_type| {
                    const padding_rep = try self.analyzeStoredType(store_view, padding_type, checked_source);
                    try children.append(self.allocator, .{
                        .role = .{ .nominal_padding_field = padding_ordinal },
                        .source_type = self.plan.representations.items[@intFromEnum(padding_rep)].source_type,
                        .rep = padding_rep,
                    });
                    if (padding_ordinal > std.math.maxInt(u16) - backing_field_count) {
                        boxyPlanInvariant("stored nominal declared field count exceeded Boxy layout range");
                    }
                    try pending.append(self.allocator, .{
                        .index = @intCast(backing_field_count + padding_ordinal),
                        .source_type = self.plan.representations.items[@intFromEnum(padding_rep)].source_type,
                        .rep = padding_rep,
                        .is_padding = true,
                    });
                    padding_ordinal += 1;
                },
            };
            const start: u32 = @intCast(self.plan.declared_fields.items.len);
            try self.plan.declared_fields.appendSlice(self.allocator, pending.items);
            declared_fields = .{ .start = start, .len = @intCast(pending.items.len) };
            if (has_declared_padding) record_field_order = .declared;
        }

        const kind: RepresentationKind = if (named.kind == .alias)
            .alias
        else if (named.builtin_owner == .fields)
            .generated_field_names
        else if (named.builtin_owner == .field)
            .generated_field
        else if (named.builtin_owner == .parse_tag_union_spec)
            .generated_tag_union_spec
        else
            .{ .nominal = if (named.backing == null)
                .opaque_nominal
            else if (named.builtin_owner != null)
                .builtin_other
            else
                .transparent };
        return .{
            .source_type = checked_source,
            .kind = kind,
            .children = try self.commitPendingChildren(children.items),
            .declared_fields = declared_fields,
            .record_field_order = record_field_order,
            .inspect_opaque = named.kind == .@"opaque" or
                kind == .generated_field or
                kind == .generated_field_names or
                kind == .generated_tag_union_spec,
        };
    }

    fn dynamicRepresentation(
        self: *Builder,
        source_type: CheckedTypeIdentity,
        constraints: []const checked.CheckedStaticDispatchConstraint,
        kind: DynamicKind,
    ) Allocator.Error!TypeRepresentation {
        const dictionaries = try self.appendDictionaryRequirements(source_type, constraints);
        return .{
            .source_type = source_type,
            .kind = .{ .dynamic = kind },
            .dictionaries = dictionaries,
            .contains_dynamic = true,
        };
    }

    fn aliasRepresentation(
        self: *Builder,
        view: ModuleView,
        source_type: CheckedTypeIdentity,
        alias: checked.CheckedAliasType,
    ) Allocator.Error!TypeRepresentation {
        var children = std.ArrayList(RepChild).empty;
        defer children.deinit(self.allocator);
        try self.appendPendingChild(&children, view, .alias_backing, alias.backing);
        for (alias.args, 0..) |arg, index| {
            try self.appendPendingChild(&children, view, .{ .alias_arg = @intCast(index) }, arg);
        }
        return .{
            .source_type = source_type,
            .kind = .alias,
            .children = try self.commitPendingChildren(children.items),
        };
    }

    fn recordRepresentation(
        self: *Builder,
        view: ModuleView,
        source_type: CheckedTypeIdentity,
        kind: RepresentationKind,
        fields: []const checked.CheckedRecordField,
        ext: ?checked.CheckedTypeId,
    ) Allocator.Error!TypeRepresentation {
        var children = std.ArrayList(RepChild).empty;
        defer children.deinit(self.allocator);
        const closed = try self.appendRecordRowChildren(&children, view, fields, ext);
        // Record-field children are ordered alphabetically by name so every
        // record representation shares one identity field-index space (the same
        // order structural record types already carry). Nominal declaration
        // backings, which the checker keeps in source-declared order, are
        // canonicalized here so a value pairs fields identically on both sides of
        // an erased structural/nominal boundary.
        sortRecordFieldChildrenByName(view, children.items);
        const child_span = try self.commitPendingChildren(children.items);

        if (!closed) {
            return .{
                .source_type = source_type,
                .kind = .{ .dynamic = .flex },
                .children = child_span,
                .contains_dynamic = true,
            };
        }

        return .{
            .source_type = source_type,
            .kind = kind,
            .children = child_span,
        };
    }

    /// Sort a record representation's record-field children alphabetically by
    /// name. All children of a record representation carry the `record_field`
    /// role, so the whole slice is sorted. When the view has no name store, or a
    /// label has no interned text (minimal test fixtures), the checker-supplied
    /// order is left in place.
    fn sortRecordFieldChildrenByName(view: ModuleView, children: []RepChild) void {
        const names = view.canonical_names orelse return;
        for (children) |child| {
            if (child.role != .record_field) return;
            if (!names.recordFieldLabelTextInterned(child.role.record_field)) return;
        }
        const SortContext = struct {
            names: *const checked_names.CanonicalNameStore,
            fn lessThan(ctx: @This(), a: RepChild, b: RepChild) bool {
                if (a.role != .record_field or b.role != .record_field) return false;
                const a_label = a.role.record_field;
                const b_label = b.role.record_field;
                return ctx.names.recordFieldLabelTextLessThan(a_label, b_label);
            }
        };
        std.mem.sort(RepChild, children, SortContext{ .names = names }, SortContext.lessThan);
    }

    fn appendRecordRowChildren(
        self: *Builder,
        children: *std.ArrayList(RepChild),
        view: ModuleView,
        fields: []const checked.CheckedRecordField,
        ext: ?checked.CheckedTypeId,
    ) Allocator.Error!bool {
        for (fields) |field| {
            try self.appendRecordFieldChild(children, view, field);
        }

        var seen = std.AutoHashMap(CheckedTypeIdentity, void).init(self.allocator);
        defer seen.deinit();

        var current = ext;
        while (current) |ext_ty| {
            const source = typeRef(view, ext_ty);
            const entry = try seen.getOrPut(source);
            // A cycle here is only reachable through the structural `.record`/
            // `.alias` links below (`.flex`/`.rigid`/`.record_unbound`/
            // `.empty_record` all return on their first visit). The checker
            // encodes a closed row's empty tail as a zero-field record whose
            // extension is itself, so reaching that fixpoint after collecting at
            // least one field means the whole field set is resolved and the
            // record is a closed concrete record. This is what lets a host-facing
            // entry point whose argument is a concrete record (e.g. `FrameInput`)
            // keep its concrete struct layout across the platform boundary
            // instead of erasing to a dynamic box.
            if (entry.found_existing) return children.items.len > 0;

            switch (view.checked_types.payload(ext_ty)) {
                .empty_record => return true,
                .record => |record| {
                    for (record.fields) |field| {
                        try self.appendRecordFieldChild(children, view, field);
                    }
                    current = record.ext;
                },
                .record_unbound => |tail_fields| {
                    for (tail_fields) |field| {
                        try self.appendRecordFieldChild(children, view, field);
                    }
                    return true;
                },
                .alias => |alias| current = alias.backing,
                .flex, .rigid => |variable| return variable.row_default == .empty_record,
                .pending, .err, .tuple, .nominal, .function, .tag_union, .empty_tag_union => return false,
            }
        }
        return true;
    }

    fn appendRecordFieldChild(
        self: *Builder,
        children: *std.ArrayList(RepChild),
        view: ModuleView,
        field: checked.CheckedRecordField,
    ) Allocator.Error!void {
        const source_type = typeRef(view, field.ty);
        const rep = switch (field.kind.tag) {
            .required, .defaulted => try self.analyzeType(view, field.ty),
            .optional, .undetermined => try self.optionalSlotRepresentation(view, field.ty),
            .err => boxyPlanInvariant("checked-error record field reached boxy representation planning"),
        };
        try children.append(self.allocator, .{
            .role = .{ .record_field = field.name },
            .source_type = source_type,
            .rep = rep,
            .record_field_kind = field.kind,
        });
    }

    fn optionalSlotRepresentation(
        self: *Builder,
        view: ModuleView,
        payload_ty: checked.CheckedTypeId,
    ) Allocator.Error!TypeRepId {
        const source_type = typeRef(view, payload_ty);
        const entry = try self.optional_slots.getOrPut(source_type);
        if (entry.found_existing) return entry.value_ptr.*;

        const rep_id: TypeRepId = @enumFromInt(@as(u32, @intCast(self.plan.representations.items.len)));
        entry.value_ptr.* = rep_id;
        try self.plan.representations.append(self.allocator, .{
            .source_type = source_type,
            .kind = .in_progress,
        });

        const names = view.canonical_names orelse
            boxyPlanInvariant("optional field slot representation ModuleView had no name store");
        const missing = names.lookupTagLabel("#Missing") orelse
            boxyPlanInvariant("optional field slot representation was missing the reserved Missing label");
        const present = names.lookupTagLabel("#Present") orelse
            boxyPlanInvariant("optional field slot representation was missing the reserved Present label");
        const payload_rep = try self.analyzeType(view, payload_ty);

        const child_start: u32 = @intCast(self.plan.children.items.len);
        try self.plan.children.append(self.allocator, .{
            .role = .{ .tag_payload = .{ .tag = present, .index = 0 } },
            .source_type = source_type,
            .rep = payload_rep,
        });
        const variant_start: u32 = @intCast(self.plan.tag_variants.items.len);
        try self.plan.tag_variants.appendSlice(self.allocator, &.{
            .{ .name = missing, .name_module = view.key },
            .{ .name = present, .name_module = view.key, .payloads = .{ .start = child_start, .len = 1 } },
        });

        self.plan.representations.items[@intFromEnum(rep_id)] = .{
            .source_type = source_type,
            .kind = .tag_union,
            .children = .{ .start = child_start, .len = 1 },
            .tag_variants = .{ .start = variant_start, .len = 2 },
            // Slot tags carry their payload descriptor through enclosing
            // records even when this particular payload is concrete.
            .contains_dynamic = true,
            .presence_slot_present_discriminant = 1,
        };
        return rep_id;
    }

    fn tupleRepresentation(
        self: *Builder,
        view: ModuleView,
        source_type: CheckedTypeIdentity,
        elems: []const checked.CheckedTypeId,
    ) Allocator.Error!TypeRepresentation {
        var children = std.ArrayList(RepChild).empty;
        defer children.deinit(self.allocator);
        for (elems, 0..) |elem, index| {
            try self.appendPendingChild(&children, view, .{ .tuple_elem = @intCast(index) }, elem);
        }
        return .{
            .source_type = source_type,
            .kind = .tuple,
            .children = try self.commitPendingChildren(children.items),
        };
    }

    fn nominalRepresentation(
        self: *Builder,
        view: ModuleView,
        source_type: CheckedTypeIdentity,
        nominal: checked.CheckedNominalType,
    ) Allocator.Error!TypeRepresentation {
        if (nominal.builtin) |builtin| {
            switch (checked.builtinRuntimeEncoding(builtin)) {
                .primitive => |primitive| return .{
                    .source_type = source_type,
                    .kind = .{ .primitive = primitive },
                },
                .bool_tag_union => return .{ .source_type = source_type, .kind = .bool_tag_union },
                .try_nominal,
                .iterator,
                => {},
                .list => return try self.builtinUnaryNominalRepresentation(view, source_type, .list, .list_elem, nominal),
                .box => return try self.builtinUnaryNominalRepresentation(view, source_type, .box, .box_payload, nominal),
                .parse_tag_union_spec => return try self.generatedEvidenceRepresentation(
                    view,
                    source_type,
                    .generated_tag_union_spec,
                    nominal,
                ),
                .fields => return try self.generatedEvidenceRepresentation(
                    view,
                    source_type,
                    .generated_field_names,
                    nominal,
                ),
                .field => return try self.generatedEvidenceRepresentation(
                    view,
                    source_type,
                    .generated_field,
                    nominal,
                ),
                .dict,
                .set,
                .crypto_sha256_digest,
                .crypto_sha256_hasher,
                .crypto_blake3_digest,
                .crypto_blake3_hasher,
                => {},
            }
        }

        var children = std.ArrayList(RepChild).empty;
        defer children.deinit(self.allocator);
        if (nominal.representation != .opaque_without_backing) {
            const backing = try self.nominalBackingSource(view, nominal);
            try self.appendPendingChildFromSource(&children, .nominal_backing, backing);
        }
        for (nominal.args, 0..) |arg, index| {
            try self.appendPendingChild(&children, view, .{ .nominal_arg = @intCast(index) }, arg);
        }
        const padding_source = self.nominalPaddingSource(view, nominal);
        if (padding_source) |source| {
            for (source.types, 0..) |padding, index| {
                try self.appendPendingChild(
                    &children,
                    source.view,
                    .{ .nominal_padding_field = @intCast(index) },
                    padding,
                );
            }
        }
        const backing = try self.nominalBackingSource(view, nominal);
        const declared_fields = try self.appendNominalDeclaredFields(view, nominal, backing);
        const backing_arg_substitutions = try self.appendNominalBackingArgSubstitutions(
            view,
            nominal,
            backing,
            children.items,
        );
        return .{
            .source_type = source_type,
            .kind = .{ .nominal = if (nominal.representation == .opaque_without_backing)
                .opaque_nominal
            else if (nominal.builtin != null)
                .builtin_other
            else
                .transparent },
            .children = try self.commitPendingChildren(children.items),
            .declared_fields = declared_fields,
            .record_field_order = if (padding_source != null) .declared else .structural,
            .nominal_backing_arg_substitutions = backing_arg_substitutions,
            .inspect_opaque = nominal.is_opaque,
        };
    }

    fn appendNominalBackingArgSubstitutions(
        self: *Builder,
        view: ModuleView,
        nominal: checked.CheckedNominalType,
        backing: TypeSource,
        children: []const RepChild,
    ) Allocator.Error!Span {
        if (nominal.args.len == 0) return .{};

        const FormalSource = struct {
            view: ModuleView,
            declaration: checked.CheckedNominalDeclaration,
        };
        const formal_source: FormalSource = if (backing.view.checked_types.nominalDeclarationForPayload(nominal)) |declaration|
            .{ .view = backing.view, .declaration = declaration }
        else if (self.nominalDeclarationFor(view, nominal)) |lookup|
            .{ .view = lookup.view, .declaration = lookup.declaration }
        else
            boxyPlanInvariant("checked nominal arguments had no declaration formals");
        const formal_args = formal_source.declaration.formalArgs(formal_source.view.checked_types);
        if (formal_args.len != nominal.args.len) {
            boxyPlanInvariant("checked nominal backing substitution arity disagreed with nominal arguments");
        }

        const start: u32 = @intCast(self.plan.nominal_backing_arg_substitutions.items.len);
        for (formal_args, 0..) |formal_ty, index| {
            const formal_rep = self.plan.repForSourceType(typeRef(formal_source.view, formal_ty)) orelse continue;
            var actual_rep: ?TypeRepId = null;
            for (children) |child| {
                if (child.role == .nominal_arg and child.role.nominal_arg == index) {
                    if (actual_rep != null) {
                        boxyPlanInvariant("checked nominal representation had duplicate argument children");
                    }
                    actual_rep = child.rep;
                }
            }
            try self.plan.nominal_backing_arg_substitutions.append(self.allocator, .{
                .arg_index = @intCast(index),
                .formal_rep = formal_rep,
                .actual_rep = actual_rep orelse
                    boxyPlanInvariant("checked nominal representation was missing an argument child"),
            });
        }
        return .{
            .start = start,
            .len = @intCast(self.plan.nominal_backing_arg_substitutions.items.len - start),
        };
    }

    fn generatedEvidenceRepresentation(
        self: *Builder,
        view: ModuleView,
        source_type: CheckedTypeIdentity,
        kind: RepresentationKind,
        nominal: checked.CheckedNominalType,
    ) Allocator.Error!TypeRepresentation {
        var children = std.ArrayList(RepChild).empty;
        defer children.deinit(self.allocator);
        for (nominal.args, 0..) |arg, index| {
            try self.appendPendingChild(&children, view, .{ .nominal_arg = @intCast(index) }, arg);
        }
        return .{
            .source_type = source_type,
            .kind = kind,
            .children = try self.commitPendingChildren(children.items),
            .inspect_opaque = true,
        };
    }

    const NominalPaddingSource = struct {
        view: ModuleView,
        types: []const checked.CheckedTypeId,
    };

    const TypeSource = struct {
        view: ModuleView,
        ty: checked.CheckedTypeId,
    };

    const NominalDeclaredSource = struct {
        field_view: ModuleView,
        fields: []const checked.CheckedDeclaredField,
        padding_view: ModuleView,
        padding_types: []const checked.CheckedTypeId,
    };

    const NominalDeclarationLookup = struct {
        view: ModuleView,
        declaration: checked.CheckedNominalDeclaration,
        padding_view: ModuleView,
        padding_types: []const checked.CheckedTypeId,
    };

    fn nominalBackingSource(
        self: *Builder,
        view: ModuleView,
        nominal: checked.CheckedNominalType,
    ) Allocator.Error!TypeSource {
        return switch (nominal.representation) {
            .local_box_payload_capability => |capability| .{
                .view = view,
                .ty = view.interface_capabilities.boxPayloadCapability(capability.capability).backing_ty,
            },
            .imported_box_payload_capability => |capability| blk: {
                const source_view = self.moduleForId(checked.importedBoxPayloadCapabilityModuleId(capability));
                const source_capability = source_view.interface_capabilities.boxPayloadCapability(capability.capability);
                break :blk .{ .view = source_view, .ty = source_capability.backing_ty };
            },
            .builtin,
            .local_declaration,
            .imported_declaration,
            => .{ .view = view, .ty = view.checked_types.nominalBackingTemplateForPayload(nominal) orelse
                boxyPlanInvariant("checked nominal representation had no declaration backing") },
            .opaque_without_backing => boxyPlanInvariant("opaque nominal without backing reached Boxy backing planning"),
        };
    }

    fn nominalPaddingSource(self: *Builder, view: ModuleView, nominal: checked.CheckedNominalType) ?NominalPaddingSource {
        if (nominal.padding_field_types.len != 0) return .{
            .view = view,
            .types = nominal.padding_field_types,
        };
        if (nominal.builtin != null) return null;
        const lookup = self.nominalDeclarationFor(view, nominal) orelse return null;
        if (lookup.padding_types.len == 0) return null;
        return .{
            .view = lookup.padding_view,
            .types = lookup.padding_types,
        };
    }

    fn nominalDeclaredSource(self: *Builder, view: ModuleView, nominal: checked.CheckedNominalType) ?NominalDeclaredSource {
        if (nominal.declared_fields.len != 0) {
            return .{
                .field_view = view,
                .fields = nominal.declared_fields,
                .padding_view = view,
                .padding_types = nominal.padding_field_types,
            };
        }
        if (nominal.builtin != null) return null;
        const lookup = self.nominalDeclarationFor(view, nominal) orelse return null;
        const fields = lookup.declaration.declaredFields(lookup.view.checked_types);
        if (fields.len == 0) return null;
        return .{
            .field_view = lookup.view,
            .fields = fields,
            .padding_view = lookup.padding_view,
            .padding_types = lookup.padding_types,
        };
    }

    fn nominalDeclarationFor(self: *Builder, view: ModuleView, nominal: checked.CheckedNominalType) ?NominalDeclarationLookup {
        return switch (nominal.representation) {
            .local_declaration => |id| blk: {
                const declaration = view.checked_types.nominalDeclarationById(id);
                break :blk .{
                    .view = view,
                    .declaration = declaration,
                    .padding_view = view,
                    .padding_types = declaration.paddingFieldTypes(view.checked_types),
                };
            },
            .imported_declaration => |imported| blk: {
                const source_view = self.moduleForId(checked.importedNominalDeclarationModuleId(imported));
                const declaration = source_view.checked_types.nominalDeclarationById(imported.declaration);
                break :blk .{
                    .view = source_view,
                    .declaration = declaration,
                    .padding_view = source_view,
                    .padding_types = declaration.paddingFieldTypes(source_view.checked_types),
                };
            },
            .local_box_payload_capability => |capability_ref| blk: {
                const capability = view.interface_capabilities.boxPayloadCapability(capability_ref.capability);
                const declaration = view.checked_types.nominalDeclaration(capability.nominal) orelse break :blk null;
                break :blk .{
                    .view = view,
                    .declaration = declaration,
                    .padding_view = view,
                    .padding_types = capability.paddingFieldTys(view.interface_capabilities),
                };
            },
            .imported_box_payload_capability => |capability_ref| blk: {
                const source_view = self.moduleForId(checked.importedBoxPayloadCapabilityModuleId(capability_ref));
                const capability = source_view.interface_capabilities.boxPayloadCapability(capability_ref.capability);
                const declaration = source_view.checked_types.nominalDeclaration(capability.nominal) orelse break :blk null;
                break :blk .{
                    .view = source_view,
                    .declaration = declaration,
                    .padding_view = source_view,
                    .padding_types = capability.paddingFieldTys(source_view.interface_capabilities),
                };
            },
            .builtin,
            .opaque_without_backing,
            => null,
        };
    }

    fn appendNominalDeclaredFields(
        self: *Builder,
        view: ModuleView,
        nominal: checked.CheckedNominalType,
        backing: TypeSource,
    ) Allocator.Error!Span {
        const source = self.nominalDeclaredSource(view, nominal) orelse return Span.empty();
        const backing_fields = switch (backing.view.checked_types.payload(backing.ty)) {
            .record => |record| record.fields,
            .pending,
            .err,
            .flex,
            .rigid,
            .alias,
            .record_unbound,
            .tuple,
            .nominal,
            .function,
            .empty_record,
            .tag_union,
            .empty_tag_union,
            => boxyPlanInvariant("checked nominal declared field order had a non-record backing"),
        };

        // Layout field indices are alphabetical-by-name, matching the index
        // space structural records use, so a value that materializes across an
        // erased structural/nominal boundary pairs fields by the same key on
        // both sides. The backing record's stored field order is not relied on.
        const alpha_ranks = try self.nominalBackingFieldAlphabeticalRanks(backing.view, backing_fields);
        defer self.allocator.free(alpha_ranks);

        var pending = std.ArrayList(DeclaredField).empty;
        defer pending.deinit(self.allocator);
        var padding_ordinal: u16 = 0;
        for (source.fields) |declared| {
            switch (declared) {
                .named => |name| {
                    const field = self.nominalBackingField(source.field_view, backing.view, backing_fields, name) orelse
                        boxyPlanInvariant("checked nominal declared named field was missing from backing row");
                    try pending.append(self.allocator, .{
                        .index = alpha_ranks[field.index],
                        .source_type = typeRef(backing.view, field.ty),
                        .rep = try self.analyzeType(backing.view, field.ty),
                    });
                },
                .padding => |index| {
                    const raw_index: usize = @intCast(index);
                    if (raw_index >= source.padding_types.len) {
                        boxyPlanInvariant("checked nominal declared padding field index was out of range");
                    }
                    const padding_ty = source.padding_types[raw_index];
                    try pending.append(self.allocator, .{
                        .index = @intCast(backing_fields.len + padding_ordinal),
                        .source_type = typeRef(source.padding_view, padding_ty),
                        .rep = try self.analyzeType(source.padding_view, padding_ty),
                        .is_padding = true,
                    });
                    padding_ordinal += 1;
                },
            }
        }
        const start: u32 = @intCast(self.plan.declared_fields.items.len);
        try self.plan.declared_fields.appendSlice(self.allocator, pending.items);
        return .{ .start = start, .len = @intCast(pending.items.len) };
    }

    /// For each backing-record field position, its rank when the backing
    /// record's field names are ordered alphabetically. This is the identity
    /// field index space (shared with structural records). When the backing
    /// view has no name store to compare text with, the backing order is taken
    /// as checked_names.
    fn nominalBackingFieldAlphabeticalRanks(
        self: *Builder,
        backing_view: ModuleView,
        backing_fields: []const checked.CheckedRecordField,
    ) Allocator.Error![]u16 {
        const ranks = try self.allocator.alloc(u16, backing_fields.len);
        errdefer self.allocator.free(ranks);
        const names = backing_view.canonical_names orelse {
            for (ranks, 0..) |*rank, index| rank.* = @intCast(index);
            return ranks;
        };
        for (backing_fields) |field| {
            if (!names.recordFieldLabelTextInterned(field.name)) {
                for (ranks, 0..) |*rank, index| rank.* = @intCast(index);
                return ranks;
            }
        }

        const order = try self.allocator.alloc(u16, backing_fields.len);
        defer self.allocator.free(order);
        for (order, 0..) |*slot, index| slot.* = @intCast(index);

        const SortContext = struct {
            names: *const checked_names.CanonicalNameStore,
            fields: []const checked.CheckedRecordField,
            fn lessThan(ctx: @This(), lhs: u16, rhs: u16) bool {
                return ctx.names.recordFieldLabelTextLessThan(ctx.fields[lhs].name, ctx.fields[rhs].name);
            }
        };
        std.mem.sort(u16, order, SortContext{ .names = names, .fields = backing_fields }, SortContext.lessThan);

        for (order, 0..) |backing_pos, rank| ranks[backing_pos] = @intCast(rank);
        return ranks;
    }

    const NominalBackingField = struct {
        index: u16,
        ty: checked.CheckedTypeId,
    };

    fn nominalBackingField(
        _: *Builder,
        field_view: ModuleView,
        backing_view: ModuleView,
        backing_fields: []const checked.CheckedRecordField,
        name: RecordFieldLabelId,
    ) ?NominalBackingField {
        for (backing_fields, 0..) |field, index| {
            if (recordFieldNameMatches(moduleNamesOf(field_view), name, moduleNamesOf(backing_view), field.name)) return .{
                .index = @intCast(index),
                .ty = field.ty,
            };
        }
        return null;
    }

    fn builtinUnaryNominalRepresentation(
        self: *Builder,
        view: ModuleView,
        source_type: CheckedTypeIdentity,
        kind: RepresentationKind,
        role: ChildRole,
        nominal: checked.CheckedNominalType,
    ) Allocator.Error!TypeRepresentation {
        if (nominal.args.len != 1) {
            boxyPlanInvariant("builtin unary nominal had an unexpected checked argument count");
        }
        var children = std.ArrayList(RepChild).empty;
        defer children.deinit(self.allocator);
        try self.appendPendingChild(&children, view, role, nominal.args[0]);
        return .{
            .source_type = source_type,
            .kind = kind,
            .children = try self.commitPendingChildren(children.items),
        };
    }

    fn functionRepresentation(
        self: *Builder,
        view: ModuleView,
        source_type: CheckedTypeIdentity,
        function: checked.CheckedFunctionType,
    ) Allocator.Error!TypeRepresentation {
        var children = std.ArrayList(RepChild).empty;
        defer children.deinit(self.allocator);
        for (function.args, 0..) |arg, index| {
            try self.appendPendingChild(&children, view, .{ .function_arg = @intCast(index) }, arg);
        }
        try self.appendPendingChild(&children, view, .function_ret, function.ret);
        return .{
            .source_type = source_type,
            .kind = .{ .erased_callable = checked.finalizedFunctionKind(function.kind) },
            .children = try self.commitPendingChildren(children.items),
        };
    }

    fn tagUnionRepresentation(
        self: *Builder,
        view: ModuleView,
        source_type: CheckedTypeIdentity,
        tag_union: checked.CheckedTagUnionType,
    ) Allocator.Error!TypeRepresentation {
        const closed = try self.tagUnionExtensionIsExplicitlyClosed(view, tag_union.ext);
        const ordered_tags = try self.layoutOrderedTagUnionTags(view, tag_union.tags);
        defer if (ordered_tags.owned) self.allocator.free(ordered_tags.tags);

        var children = std.ArrayList(RepChild).empty;
        defer children.deinit(self.allocator);
        for (ordered_tags.tags) |tag| {
            for (tag.argsSlice(view.checked_types), 0..) |arg, index| {
                try self.appendPendingChild(&children, view, .{ .tag_payload = .{ .tag = tag.name, .index = @intCast(index) } }, arg);
            }
        }
        if (!try self.rowExtensionIsDefaultClosed(view, tag_union.ext, .empty_tag_union)) {
            try self.appendPendingChild(&children, view, .tag_ext, tag_union.ext);
        }
        const child_span = try self.commitPendingChildren(children.items);

        const variant_start: u32 = @intCast(self.plan.tag_variants.items.len);
        var payload_start = child_span.start;
        for (ordered_tags.tags) |tag| {
            try self.plan.tag_variants.append(self.allocator, .{
                .name = tag.name,
                .name_module = view.key,
                .payloads = .{ .start = payload_start, .len = tag.args_len },
            });
            payload_start += tag.args_len;
        }
        const tag_variants: Span = .{ .start = variant_start, .len = @intCast(ordered_tags.tags.len) };

        if (!closed) {
            return .{
                .source_type = source_type,
                .kind = .{ .dynamic = .flex },
                .children = child_span,
                .tag_variants = tag_variants,
                .contains_dynamic = true,
            };
        }

        return .{
            .source_type = source_type,
            .kind = .tag_union,
            .children = child_span,
            .tag_variants = tag_variants,
        };
    }

    const OrderedTags = struct {
        tags: []const checked.CheckedTag,
        owned: bool,
    };

    fn layoutOrderedTagUnionTags(
        self: *Builder,
        view: ModuleView,
        tags: []const checked.CheckedTag,
    ) Allocator.Error!OrderedTags {
        const names = view.canonical_names orelse return .{
            .tags = tags,
            .owned = false,
        };
        if (tags.len < 2) return .{
            .tags = tags,
            .owned = false,
        };

        const sorted = try self.allocator.dupe(checked.CheckedTag, tags);
        errdefer self.allocator.free(sorted);
        std.mem.sort(checked.CheckedTag, sorted, names, struct {
            fn lessThan(name_store: *const checked_names.CanonicalNameStore, lhs: checked.CheckedTag, rhs: checked.CheckedTag) bool {
                return name_store.tagLabelTextLessThan(lhs.name, rhs.name);
            }
        }.lessThan);

        for (sorted[1..], 1..) |tag, index| {
            if (names.tagLabelTextEql(sorted[index - 1].name, tag.name)) {
                boxyPlanInvariant("boxy tag-union representation encountered duplicate tag names");
            }
        }

        return .{
            .tags = sorted,
            .owned = true,
        };
    }

    fn tagUnionExtensionIsExplicitlyClosed(
        self: *Builder,
        view: ModuleView,
        ext_ty: checked.CheckedTypeId,
    ) Allocator.Error!bool {
        var seen = std.AutoHashMap(CheckedTypeIdentity, void).init(self.allocator);
        defer seen.deinit();
        return try self.tagUnionExtensionIsExplicitlyClosedInner(view, ext_ty, &seen);
    }

    fn tagUnionExtensionIsExplicitlyClosedInner(
        self: *Builder,
        view: ModuleView,
        ext_ty: checked.CheckedTypeId,
        seen: *std.AutoHashMap(CheckedTypeIdentity, void),
    ) Allocator.Error!bool {
        const source = typeRef(view, ext_ty);
        const entry = try seen.getOrPut(source);
        if (entry.found_existing) return false;

        return switch (view.checked_types.payload(ext_ty)) {
            .empty_tag_union => true,
            .alias => |alias| try self.tagUnionExtensionIsExplicitlyClosedInner(view, alias.backing, seen),
            .flex, .rigid => |variable| variable.row_default == .empty_tag_union,
            .pending, .err, .record, .record_unbound, .tuple, .nominal, .function, .empty_record, .tag_union => false,
        };
    }

    fn rowExtensionIsDefaultClosed(
        self: *Builder,
        view: ModuleView,
        ext_ty: checked.CheckedTypeId,
        expected: checked.RowDefault,
    ) Allocator.Error!bool {
        var seen = std.AutoHashMap(CheckedTypeIdentity, void).init(self.allocator);
        defer seen.deinit();
        return try self.rowExtensionIsDefaultClosedInner(view, ext_ty, expected, &seen);
    }

    fn rowExtensionIsDefaultClosedInner(
        self: *Builder,
        view: ModuleView,
        ext_ty: checked.CheckedTypeId,
        expected: checked.RowDefault,
        seen: *std.AutoHashMap(CheckedTypeIdentity, void),
    ) Allocator.Error!bool {
        const source = typeRef(view, ext_ty);
        const entry = try seen.getOrPut(source);
        if (entry.found_existing) return false;

        return switch (view.checked_types.payload(ext_ty)) {
            .alias => |alias| try self.rowExtensionIsDefaultClosedInner(view, alias.backing, expected, seen),
            .flex, .rigid => |variable| variable.row_default == expected,
            .pending, .err, .record, .record_unbound, .tuple, .nominal, .function, .empty_record, .tag_union, .empty_tag_union => false,
        };
    }

    fn appendDictionaryRequirements(
        self: *Builder,
        source_type: CheckedTypeIdentity,
        constraints: []const checked.CheckedStaticDispatchConstraint,
    ) Allocator.Error!Span {
        const start: u32 = @intCast(self.plan.dictionaries.items.len);
        for (constraints, 0..) |constraint, index| {
            if (constraint.origin.literalKind() != null) continue;
            try self.plan.dictionaries.append(self.allocator, .{
                .source_type = source_type,
                .constraint_index = @intCast(index),
                .slot = try self.internDictionaryMethodSlot(source_type.module, constraint.fn_name),
                .fn_name = constraint.fn_name,
                .fn_ty = .{ .module = source_type.module, .ty = constraint.fn_ty },
                .origin = constraint.origin,
                .binop_negated = constraint.binopNegated(),
                .num_literal = constraint.numeralInfo(),
            });
        }
        const view = self.moduleForId(source_type.module);
        for (constraints) |constraint| {
            _ = try self.analyzeType(view, constraint.fn_ty);
        }
        return .{
            .start = start,
            .len = @intCast(self.plan.dictionaries.items.len - start),
        };
    }

    fn internDictionaryMethodSlot(
        self: *Builder,
        module: checked.ModuleId,
        method: MethodNameId,
    ) Allocator.Error!u32 {
        const source_names = self.moduleForId(module).canonical_names;
        for (self.plan.dictionary_method_slots.items, 0..) |existing, slot| {
            const existing_names = self.moduleForId(existing.module).canonical_names;
            const same_method = if (source_names != null and existing_names != null)
                std.mem.eql(
                    u8,
                    source_names.?.methodNameText(method),
                    existing_names.?.methodNameText(existing.method),
                )
            else
                std.meta.eql(module, existing.module) and method == existing.method;
            if (same_method) return @intCast(slot);
        }

        const slot: u32 = @intCast(self.plan.dictionary_method_slots.items.len);
        try self.plan.dictionary_method_slots.append(self.allocator, .{
            .module = module,
            .method = method,
        });
        return slot;
    }

    fn appendPendingChild(
        self: *Builder,
        pending: *std.ArrayList(RepChild),
        view: ModuleView,
        role: ChildRole,
        source_type: checked.CheckedTypeId,
    ) Allocator.Error!void {
        try self.appendPendingChildFromSource(pending, role, .{ .view = view, .ty = source_type });
    }

    fn appendPendingChildFromSource(
        self: *Builder,
        pending: *std.ArrayList(RepChild),
        role: ChildRole,
        source: TypeSource,
    ) Allocator.Error!void {
        try pending.append(self.allocator, .{
            .role = role,
            .source_type = typeRef(source.view, source.ty),
            .rep = try self.analyzeType(source.view, source.ty),
        });
    }

    fn commitPendingChildren(self: *Builder, pending: []const RepChild) Allocator.Error!Span {
        const start: u32 = @intCast(self.plan.children.items.len);
        try self.plan.children.appendSlice(self.allocator, pending);
        return .{ .start = start, .len = @intCast(pending.len) };
    }

    fn materializeGeneratedParserTagUnionPlans(self: *Builder) Allocator.Error!void {
        for (self.plan.generated_codec_calls.items) |call| {
            const method_view = self.moduleForId(call.method_module);
            const method_names = method_view.canonical_names orelse
                boxyPlanInvariant("generated parser tag-union method module had no checked names");
            if (!std.mem.eql(u8, method_names.methodNameText(call.method), "parse_tag_union")) continue;

            const shape_type = call.subject_type orelse
                boxyPlanInvariant("generated parse_tag_union call had no checked subject type");
            var reachable = std.ArrayList(WorkerPlanId).empty;
            defer reachable.deinit(self.allocator);
            try reachable.append(self.allocator, call.worker);

            var index: usize = 0;
            while (index < reachable.items.len) : (index += 1) {
                const worker = reachable.items[index];
                if (self.intrinsicForWorker(worker)) |intrinsic| {
                    if (intrinsic == .parse_tag_union) {
                        const arg_types = self.plan.generatedCodecCallTypeSlice(call.arg_types);
                        if (arg_types.len != 3) {
                            boxyPlanInvariant("generated parse_tag_union call had an unexpected argument count");
                        }
                        try self.appendGeneratedParserTagUnionPlan(
                            worker,
                            call.caller,
                            shape_type,
                            call.dispatch_type,
                            arg_types[2],
                        );
                    }
                }
                for (self.plan.direct_calls.items) |direct| {
                    if (direct.caller != worker) continue;
                    var already_reachable = false;
                    for (reachable.items) |existing| {
                        if (existing == direct.worker) {
                            already_reachable = true;
                            break;
                        }
                    }
                    if (already_reachable) continue;
                    try reachable.append(self.allocator, direct.worker);
                }
            }
        }
    }

    fn appendGeneratedParserTagUnionPlan(
        self: *Builder,
        intrinsic_worker: WorkerPlanId,
        contract_worker: WorkerPlanId,
        shape_type: CheckedTypeIdentity,
        encoding_type: CheckedTypeIdentity,
        state_type: CheckedTypeIdentity,
    ) Allocator.Error!void {
        for (self.plan.generated_parser_tag_union_plans.items) |existing| {
            if (existing.intrinsic_worker != intrinsic_worker) continue;
            if (existing.contract_worker != contract_worker or !typeRefEql(existing.shape_type, shape_type)) continue;
            if (!typeRefEql(existing.encoding_type, encoding_type) or !typeRefEql(existing.state_type, state_type)) {
                boxyPlanInvariant("generated tag-union parser plan had conflicting exact argument types");
            }
            return;
        }

        const start: u32 = @intCast(self.plan.generated_parser_tag_union_record_types.items.len);
        for (self.plan.generated_parser_field_captures.items) |capture| {
            if (capture.worker != contract_worker) continue;
            const current = self.plan.generated_parser_tag_union_record_types.items[start..];
            var found = false;
            for (current) |record_type| {
                if (typeRefEql(record_type, capture.record_type)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                try self.plan.generated_parser_tag_union_record_types.append(self.allocator, capture.record_type);
            }
        }
        const len: u32 = @intCast(self.plan.generated_parser_tag_union_record_types.items.len - start);
        const runtime_id: u32 = @intCast(self.plan.generated_parser_tag_union_plans.items.len);
        try self.plan.generated_parser_tag_union_plans.append(self.allocator, .{
            .runtime_id = runtime_id,
            .intrinsic_worker = intrinsic_worker,
            .contract_worker = contract_worker,
            .shape_type = shape_type,
            .encoding_type = encoding_type,
            .state_type = state_type,
            .record_types = .{ .start = start, .len = len },
        });
    }

    fn intrinsicForWorker(self: *Builder, worker: WorkerPlanId) ?checked.IntrinsicId {
        const source = self.plan.workers.items[@intFromEnum(worker)].source;
        return switch (source) {
            .generated_codec,
            .generated_field_iterator,
            .generated_interpolation_step,
            => null,
            .procedure_template,
            .procedure_binding,
            .procedure_use,
            .nested_expr,
            => switch (self.rootWorkerBody(source)) {
                .intrinsic_wrapper => |intrinsic| intrinsic.wrapper.intrinsic,
                .checked_expr, .hosted_proc, .unimplemented => null,
            },
        };
    }

    fn analyzePlannedEvidenceTypes(self: *Builder) Allocator.Error!void {
        for (self.plan.workers.items) |worker| {
            if (self.workerEvidenceParams(worker.source)) |worker_evidence| {
                for (worker_evidence.params) |param| {
                    _ = try self.analyzeType(worker_evidence.view, param.dispatcher_ty);
                }
            }
        }
        for (self.plan.direct_calls.items) |direct| {
            const call_evidence = self.checkedEvidenceForDirectCall(direct);
            if (call_evidence.entries) |entries| {
                for (entries) |entry| {
                    _ = try self.analyzeType(call_evidence.view, entry.dispatcher_ty);
                }
            }
        }
        for (self.plan.nested_callable_uses.items) |use| {
            const view = self.moduleForId(use.use.module);
            if (view.static_dispatch_plans.siteEvidence(use.use.expr)) |entries| {
                for (entries) |entry| _ = try self.analyzeType(view, entry.dispatcher_ty);
            }
        }
        for (self.plan.callable_uses.items) |use| {
            const view = self.moduleForId(use.use.module);
            if (view.static_dispatch_plans.siteEvidence(use.use.expr)) |entries| {
                for (entries) |entry| _ = try self.analyzeType(view, entry.dispatcher_ty);
            }
        }
    }

    const CheckedCallEvidence = struct {
        view: ModuleView,
        entries: ?[]const static_dispatch.CheckedEvidence,
    };

    fn checkedEvidenceForDirectCall(self: *Builder, direct: DirectCallPlan) CheckedCallEvidence {
        const view = self.moduleForId(direct.call.module);
        const call_expr = view.checked_bodies.expr(direct.call.expr);
        const entries = if (call_expr.data == .call)
            view.static_dispatch_plans.siteEvidence(call_expr.data.call.func)
        else if (call_expr.data == .dispatch_call)
            self.nestedEvidenceForDirectDispatch(view, call_expr.data.dispatch_call)
        else if (call_expr.data == .type_dispatch_call)
            self.nestedEvidenceForDirectDispatch(view, call_expr.data.type_dispatch_call)
        else if (call_expr.data == .method_eq)
            self.nestedEvidenceForDirectDispatch(view, call_expr.data.method_eq)
        else if (call_expr.data == .str_from_quote)
            self.nestedEvidenceForDirectDispatch(view, call_expr.data.str_from_quote.plan)
        else if (call_expr.data == .interpolation)
            self.nestedEvidenceForDirectDispatch(view, call_expr.data.interpolation.plan)
        else if (call_expr.data == .numeral)
            self.nestedEvidenceForDirectDispatch(view, call_expr.data.numeral.plan)
        else
            boxyPlanInvariant("boxy direct call plan referenced a checked expression that is not lowered as a worker call");
        return .{ .view = view, .entries = entries };
    }

    fn propagateDynamicRequirements(self: *Builder) void {
        var changed = true;
        while (changed) {
            changed = false;
            for (self.plan.representations.items) |*rep| {
                if (!rep.contains_dynamic and self.representationContainsDynamic(rep.*)) {
                    rep.contains_dynamic = true;
                    changed = true;
                }
            }
        }
    }

    fn representationContainsDynamic(self: *const Builder, rep: TypeRepresentation) bool {
        switch (rep.kind) {
            .dynamic => return true,
            .in_progress => return false,
            .generated_field,
            .generated_field_names,
            .generated_tag_union_spec,
            => return false,
            .primitive,
            .bool_tag_union,
            .erased_callable,
            .alias,
            .record,
            .record_unbound,
            .tuple,
            .nominal,
            .list,
            .box,
            .empty_record,
            .tag_union,
            .empty_tag_union,
            => {},
        }
        for (self.plan.childSlice(rep.children)) |child| {
            if (!childCarriesRuntimeDescriptor(child.role)) continue;
            if (self.plan.representations.items[@intFromEnum(child.rep)].contains_dynamic) return true;
        }
        return false;
    }

    fn materializeDescriptorRequirements(self: *Builder) Allocator.Error!void {
        for (self.plan.representations.items, 0..) |*rep, index| {
            if (!rep.contains_dynamic) continue;
            if (rep.descriptor != null) continue;
            const reason = descriptorReason(rep.kind) orelse continue;
            const id: DescriptorRequirementId = @enumFromInt(@as(u32, @intCast(self.plan.descriptors.items.len)));
            try self.plan.descriptors.append(self.allocator, .{
                .source_type = rep.source_type,
                .rep = @enumFromInt(@as(u32, @intCast(index))),
                .reason = reason,
            });
            rep.descriptor = id;
        }
    }

    fn materializeWorkerHiddenDescriptorParams(self: *Builder) Allocator.Error!void {
        for (self.plan.workers.items, 0..) |worker, worker_index| {
            if (self.workerResolvesToHosted(worker.source) or
                worker.source == .generated_field_iterator)
            {
                self.plan.workers.items[worker_index].hidden_descs = .{};
                self.plan.workers.items[worker_index].body_hidden_descs = .{};
                self.plan.workers.items[worker_index].evidence_only_descs = .{};
                self.plan.workers.items[worker_index].evidence_descs = .{};
                continue;
            }

            var pending = std.ArrayList(HiddenDescriptorParam).empty;
            defer pending.deinit(self.allocator);
            var seen_reps = collections.DenseMap(TypeRepId, void).init(self.allocator);
            defer seen_reps.deinit();
            var seen_descs = collections.DenseMap(DescriptorRequirementId, void).init(self.allocator);
            defer seen_descs.deinit();

            if (self.repQuery().functionChildren(worker.rep)) |function| {
                const children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(function.rep)].children);
                for (children[function.args_start..][0..function.arg_count]) |child| {
                    try self.collectHiddenDescriptorsForRep(child.rep, &pending, &seen_reps, &seen_descs);
                }
                try self.collectHiddenDescriptorsForRep(function.ret, &pending, &seen_reps, &seen_descs);
            } else {
                try self.collectHiddenDescriptorsForRep(worker.rep, &pending, &seen_reps, &seen_descs);
            }
            const body_start: u32 = @intCast(pending.items.len);
            switch (worker.source) {
                .generated_codec => |codec| switch (codec.kind) {
                    .parser_constructor, .encoder_constructor => {},
                    .parser_runtime, .encoder_runtime => {
                        const capture_type = codec.capture_type orelse
                            boxyPlanInvariant("generated codec runtime had no capture type");
                        const capture_rep = self.plan.repForSourceType(capture_type) orelse
                            boxyPlanInvariant("generated codec runtime capture type was not analyzed");
                        try self.collectHiddenDescriptorsForRep(capture_rep, &pending, &seen_reps, &seen_descs);
                        for (self.plan.generated_parser_field_captures.items) |field_capture| {
                            if (field_capture.worker != worker.id) continue;
                            const field_rep = self.plan.repForSourceType(field_capture.source_type) orelse
                                boxyPlanInvariant("generated codec field capture type was not analyzed");
                            try self.collectHiddenDescriptorsForRep(field_rep, &pending, &seen_reps, &seen_descs);
                        }
                    },
                    .encoder_record_fields,
                    .encoder_dict_fields,
                    .encoder_sequence_elements,
                    .encoder_tag_field,
                    .encoder_tag_payload_thunk,
                    .encoder_tag_payload_elements,
                    .encoder_value_thunk,
                    => {
                        const capture_type = codec.capture_type orelse
                            boxyPlanInvariant("generated encoder callback had no encoding capture type");
                        const capture_rep = self.plan.repForSourceType(capture_type) orelse
                            boxyPlanInvariant("generated encoder callback capture type was not analyzed");
                        try self.collectHiddenDescriptorsForRep(capture_rep, &pending, &seen_reps, &seen_descs);
                        const value_type = codec.value_type orelse codec.shape;
                        const shape_rep = self.plan.repForSourceType(value_type) orelse
                            boxyPlanInvariant("generated encoder callback shape type was not analyzed");
                        try self.collectHiddenDescriptorsForRep(shape_rep, &pending, &seen_reps, &seen_descs);
                        if (!typeRefEql(value_type, codec.shape)) {
                            const schema_rep = self.plan.repForSourceType(codec.shape) orelse
                                boxyPlanInvariant("generated encoder callback schema type was not analyzed");
                            try self.collectHiddenDescriptorsForRep(schema_rep, &pending, &seen_reps, &seen_descs);
                        }
                        const contract_worker = codec.contract_worker orelse
                            boxyPlanInvariant("generated encoder callback had no contract worker");
                        for (self.plan.generated_parser_field_captures.items) |field_capture| {
                            if (field_capture.worker != contract_worker) continue;
                            const field_rep = self.plan.repForSourceType(field_capture.source_type) orelse
                                boxyPlanInvariant("generated encoder field capture type was not analyzed");
                            try self.collectHiddenDescriptorsForRep(field_rep, &pending, &seen_reps, &seen_descs);
                        }
                    },
                },
                .generated_interpolation_step => |step| {
                    if (step.one_payload_type) |payload_type| {
                        const payload_rep = self.plan.repForSourceType(payload_type) orelse
                            boxyPlanInvariant("generated interpolation payload capture type was not analyzed");
                        try self.collectHiddenDescriptorsForRep(payload_rep, &pending, &seen_reps, &seen_descs);
                    }
                },
                .procedure_template,
                .procedure_binding,
                .procedure_use,
                .nested_expr,
                .generated_field_iterator,
                => {},
            }

            const evidence_only_start: u32 = @intCast(pending.items.len);
            const evidence_start: u32 = @intCast(self.plan.worker_evidence_descriptor_params.items.len);
            if (self.workerEvidenceParams(worker.source)) |worker_evidence| {
                for (worker_evidence.params, 0..) |evidence_param, evidence_index| {
                    if (evidence_param.runtime_dictionary) continue;
                    const source_type = typeRef(worker_evidence.view, evidence_param.dispatcher_ty);
                    const rep_id = self.plan.repForSourceType(source_type) orelse
                        boxyPlanInvariant("checked literal evidence dispatcher type was not analyzed for its worker body");
                    const rep = self.plan.representations.items[@intFromEnum(rep_id)];
                    const desc = rep.descriptor orelse continue;
                    try self.collectHiddenDescriptorsForRep(rep_id, &pending, &seen_reps, &seen_descs);

                    var hidden_desc_index: ?u32 = null;
                    for (pending.items, 0..) |candidate, index| {
                        if (candidate.desc == desc and candidate.rep == rep_id) {
                            hidden_desc_index = @intCast(index);
                            break;
                        }
                    }
                    try self.plan.worker_evidence_descriptor_params.append(self.allocator, .{
                        .evidence_index = @intCast(evidence_index),
                        .hidden_desc_index = hidden_desc_index orelse
                            boxyPlanInvariant("checked literal evidence descriptor was not collected into the worker ABI"),
                    });
                }
            }

            const start: u32 = @intCast(self.plan.hidden_descriptor_params.items.len);
            try self.plan.hidden_descriptor_params.appendSlice(self.allocator, pending.items);
            self.plan.workers.items[worker_index].hidden_descs = .{
                .start = start,
                .len = @intCast(pending.items.len),
            };
            self.plan.workers.items[worker_index].body_hidden_descs = .{
                .start = start + body_start,
                .len = @intCast(pending.items.len - body_start),
            };
            self.plan.workers.items[worker_index].evidence_only_descs = .{
                .start = start + evidence_only_start,
                .len = @intCast(pending.items.len - evidence_only_start),
            };
            self.plan.workers.items[worker_index].evidence_descs = .{
                .start = evidence_start,
                .len = @intCast(self.plan.worker_evidence_descriptor_params.items.len - evidence_start),
            };
        }
    }

    const WorkerEvidenceParams = struct {
        view: ModuleView,
        params: []const static_dispatch.EvidenceParamRecord,
    };

    fn workerEvidenceParams(self: *Builder, source: WorkerSource) ?WorkerEvidenceParams {
        return switch (source) {
            .procedure_template => |template| self.templateEvidenceParams(template),
            .procedure_binding => |binding| self.bindingEvidenceParams(self.moduleForId(binding.artifact), binding.binding),
            .procedure_use => |use| switch (use.binding) {
                .top_level => |binding| self.bindingEvidenceParams(self.moduleForId(binding.artifact), binding.binding),
                .platform_required => |required| self.bindingEvidenceParams(
                    self.moduleForId(required.app_value.artifact),
                    required.procedure_binding,
                ),
                .imported => |imported| blk: {
                    const view = self.moduleForId(imported.artifact);
                    const binding = self.importedProcedureBinding(view, imported);
                    break :blk self.bindingBodyEvidenceParams(binding.body);
                },
                .hosted => null,
            },
            .nested_expr => |expr_ref| blk: {
                const view = self.moduleForId(expr_ref.module);
                const site_expr = self.nestedCallableSiteExprForExpr(view, expr_ref.expr) orelse expr_ref.expr;
                const params = self.nestedExprEvidenceParams(view, site_expr) orelse break :blk null;
                break :blk .{ .view = view, .params = params };
            },
            .generated_codec,
            .generated_field_iterator,
            .generated_interpolation_step,
            => null,
        };
    }

    fn templateEvidenceParams(
        self: *Builder,
        template_ref: checked_names.ProcedureTemplateRef,
    ) WorkerEvidenceParams {
        const view = self.moduleForCheckedModuleId(template_ref.artifact);
        const template = &view.checked_procedure_templates.templates[@intFromEnum(template_ref.template)];
        return .{
            .view = view,
            .params = view.checked_procedure_templates.evidenceParams(template),
        };
    }

    fn nestedExprEvidenceParams(
        _: *Builder,
        view: ModuleView,
        expr: checked.CheckedExprId,
    ) ?[]const static_dispatch.EvidenceParamRecord {
        for (view.checked_procedure_templates.dispatch_scopes) |scope| {
            if (scope.checked_expr != expr) continue;
            const start: usize = scope.evidence_params.start;
            const len: usize = scope.evidence_params.len;
            if (start > view.checked_procedure_templates.evidence_params_pool.len or
                len > view.checked_procedure_templates.evidence_params_pool.len - start)
            {
                boxyPlanInvariant("nested procedure evidence span was outside the checked parameter pool");
            }
            return view.checked_procedure_templates.evidence_params_pool[start..][0..len];
        }
        return null;
    }

    fn bindingEvidenceParams(
        self: *Builder,
        view: ModuleView,
        binding_ref: checked.TopLevelProcedureBindingRef,
    ) ?WorkerEvidenceParams {
        return self.bindingBodyEvidenceParams(view.top_level_procedure_bindings.get(binding_ref).body);
    }

    fn bindingBodyEvidenceParams(
        self: *Builder,
        body: anytype,
    ) ?WorkerEvidenceParams {
        return switch (body) {
            .direct_template => |direct| switch (direct.template) {
                .checked => |template| self.templateEvidenceParams(template),
                .lifted, .synthetic => null,
            },
            .callable_eval_template => null,
        };
    }

    fn materializeWorkerHiddenDictionaryParams(self: *Builder) Allocator.Error!void {
        for (self.plan.workers.items, 0..) |worker, worker_index| {
            if (self.workerResolvesToHosted(worker.source) or
                worker.source == .generated_codec or
                worker.source == .generated_field_iterator or
                worker.source == .generated_interpolation_step)
            {
                self.plan.workers.items[worker_index].hidden_dicts = .{};
                continue;
            }

            var pending = std.ArrayList(HiddenDictionaryParam).empty;
            defer pending.deinit(self.allocator);
            var seen_reps = collections.DenseMap(TypeRepId, void).init(self.allocator);
            defer seen_reps.deinit();

            if (self.repQuery().functionChildren(worker.rep)) |function| {
                const children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(function.rep)].children);
                for (children[function.args_start..][0..function.arg_count]) |child| {
                    try self.collectHiddenDictionariesForRep(child.rep, &pending, &seen_reps);
                }
                try self.collectHiddenDictionariesForRep(function.ret, &pending, &seen_reps);
            } else {
                try self.collectHiddenDictionariesForRep(worker.rep, &pending, &seen_reps);
            }

            const body_start: u32 = @intCast(pending.items.len);
            for (self.worker_dictionary_uses.items) |use| {
                if (use.worker != worker.id) continue;
                try self.collectHiddenDictionariesForRep(use.rep, &pending, &seen_reps);
            }

            const start: u32 = @intCast(self.plan.hidden_dictionary_params.items.len);
            try self.plan.hidden_dictionary_params.appendSlice(self.allocator, pending.items);
            self.plan.workers.items[worker_index].hidden_dicts = .{
                .start = start,
                .len = @intCast(pending.items.len),
            };
            self.plan.workers.items[worker_index].body_hidden_dicts = .{
                .start = start + body_start,
                .len = @intCast(pending.items.len - body_start),
            };
        }
    }

    fn materializeWorkerErasedCaptures(self: *Builder) Allocator.Error!void {
        for (self.plan.workers.items, 0..) |worker, worker_index| {
            if (self.workerResolvesToHosted(worker.source)) {
                self.plan.workers.items[worker_index].erased_captures = .{};
                continue;
            }

            var pending = std.ArrayList(ErasedCapture).empty;
            defer pending.deinit(self.allocator);

            switch (worker.source) {
                .nested_expr => |expr_ref| {
                    const view = self.moduleForId(expr_ref.module);
                    const expr = view.checked_bodies.expr(expr_ref.expr);
                    if (expr.data == .closure) {
                        for (expr.data.closure.captures) |capture| {
                            const pattern = view.checked_bodies.pattern(capture.pattern);
                            const rep = self.plan.repForSourceType(typeRef(view, pattern.ty)) orelse
                                boxyPlanInvariant("boxy erased capture pattern type was not analyzed");
                            try pending.append(self.allocator, .{
                                .kind = .captured_value,
                                .source_type = typeRef(view, pattern.ty),
                                .rep = rep,
                                .capture_id = capture.capture_id,
                            });
                        }
                    }
                },
                .procedure_template,
                .procedure_binding,
                .procedure_use,
                => {},
                .generated_codec => |codec| switch (codec.kind) {
                    .parser_constructor,
                    .encoder_constructor,
                    => {},
                    .parser_runtime,
                    .encoder_runtime,
                    => {
                        const capture_type = codec.capture_type orelse
                            boxyPlanInvariant("generated codec runtime had no encoding capture type");
                        const capture_rep = self.plan.repForSourceType(capture_type) orelse
                            boxyPlanInvariant("generated codec encoding capture type was not analyzed");
                        try pending.append(self.allocator, .{
                            .kind = .captured_value,
                            .source_type = capture_type,
                            .rep = capture_rep,
                            .capture_id = checked.CaptureId.generatedCheck(0),
                        });
                        if (codec.kind == .parser_runtime or codec.kind == .encoder_runtime) {
                            var capture_index: u32 = 1;
                            for (self.plan.generated_parser_field_captures.items) |field_capture| {
                                if (field_capture.worker != worker.id) continue;
                                const field_rep = self.plan.repForSourceType(field_capture.source_type) orelse
                                    boxyPlanInvariant("generated parser field capture type was not analyzed");
                                try pending.append(self.allocator, .{
                                    .kind = .captured_value,
                                    .source_type = field_capture.source_type,
                                    .rep = field_rep,
                                    .capture_id = checked.CaptureId.generatedCheck(capture_index),
                                });
                                capture_index += 1;
                            }
                        }
                    },
                    .encoder_record_fields,
                    .encoder_dict_fields,
                    .encoder_sequence_elements,
                    .encoder_tag_field,
                    .encoder_tag_payload_elements,
                    => {
                        const capture_type = codec.capture_type orelse
                            boxyPlanInvariant("generated encoder record callback had no encoding capture type");
                        const capture_rep = self.plan.repForSourceType(capture_type) orelse
                            boxyPlanInvariant("generated encoder record callback encoding type was not analyzed");
                        const value_type = codec.value_type orelse codec.shape;
                        const shape_rep = self.plan.repForSourceType(value_type) orelse
                            boxyPlanInvariant("generated encoder record callback shape type was not analyzed");
                        try pending.append(self.allocator, .{
                            .kind = .captured_value,
                            .source_type = capture_type,
                            .rep = capture_rep,
                            .capture_id = checked.CaptureId.generatedCheck(0),
                        });
                        try pending.append(self.allocator, .{
                            .kind = .captured_value,
                            .source_type = value_type,
                            .rep = shape_rep,
                            .capture_id = checked.CaptureId.generatedCheck(1),
                        });
                        const contract_worker = codec.contract_worker orelse
                            boxyPlanInvariant("generated encoder record callback had no contract worker");
                        var capture_index: u32 = 2;
                        for (self.plan.generated_parser_field_captures.items) |field_capture| {
                            if (field_capture.worker != contract_worker) continue;
                            const field_rep = self.plan.repForSourceType(field_capture.source_type) orelse
                                boxyPlanInvariant("generated encoder field-name capture type was not analyzed");
                            try pending.append(self.allocator, .{
                                .kind = .captured_value,
                                .source_type = field_capture.source_type,
                                .rep = field_rep,
                                .capture_id = checked.CaptureId.generatedCheck(capture_index),
                            });
                            capture_index += 1;
                        }
                    },
                    .encoder_tag_payload_thunk, .encoder_value_thunk => {
                        const capture_type = codec.capture_type orelse
                            boxyPlanInvariant("generated encoder value thunk had no encoding capture type");
                        const capture_rep = self.plan.repForSourceType(capture_type) orelse
                            boxyPlanInvariant("generated encoder value thunk encoding type was not analyzed");
                        const value_type = codec.value_type orelse codec.shape;
                        const shape_rep = self.plan.repForSourceType(value_type) orelse
                            boxyPlanInvariant("generated encoder value thunk shape type was not analyzed");
                        try pending.append(self.allocator, .{
                            .kind = .captured_value,
                            .source_type = capture_type,
                            .rep = capture_rep,
                            .capture_id = checked.CaptureId.generatedCheck(0),
                        });
                        try pending.append(self.allocator, .{
                            .kind = .captured_value,
                            .source_type = value_type,
                            .rep = shape_rep,
                            .capture_id = checked.CaptureId.generatedCheck(1),
                        });
                        const contract_worker = codec.contract_worker orelse
                            boxyPlanInvariant("generated encoder value thunk had no contract worker");
                        var capture_index: u32 = 2;
                        for (self.plan.generated_parser_field_captures.items) |field_capture| {
                            if (field_capture.worker != contract_worker) continue;
                            const field_rep = self.plan.repForSourceType(field_capture.source_type) orelse
                                boxyPlanInvariant("generated encoder value thunk field-name capture type was not analyzed");
                            try pending.append(self.allocator, .{
                                .kind = .captured_value,
                                .source_type = field_capture.source_type,
                                .rep = field_rep,
                                .capture_id = checked.CaptureId.generatedCheck(capture_index),
                            });
                            capture_index += 1;
                        }
                    },
                },
                .generated_field_iterator => |iterator| {
                    const fields_rep = self.plan.repForSourceType(iterator.field_names_type) orelse
                        boxyPlanInvariant("generated FieldNames iterator capture type was not analyzed");
                    try pending.append(self.allocator, .{
                        .kind = .captured_value,
                        .source_type = iterator.field_names_type,
                        .rep = fields_rep,
                        .capture_id = checked.CaptureId.generatedCheck(0),
                    });
                    const index_rep = self.plan.repForSourceType(iterator.index_type) orelse
                        boxyPlanInvariant("generated FieldNames iterator index type was not analyzed");
                    try pending.append(self.allocator, .{
                        .kind = .captured_value,
                        .source_type = iterator.index_type,
                        .rep = index_rep,
                        .capture_id = checked.CaptureId.generatedCheck(1),
                    });
                    try pending.append(self.allocator, .{
                        .kind = .captured_value,
                        .source_type = iterator.index_type,
                        .rep = index_rep,
                        .capture_id = checked.CaptureId.generatedCheck(2),
                    });
                    if (iterator.size_type) |size_type| {
                        const size_rep = self.plan.repForSourceType(size_type) orelse
                            boxyPlanInvariant("generated FieldNames iterator size type was not analyzed");
                        try pending.append(self.allocator, .{
                            .kind = .captured_value,
                            .source_type = size_type,
                            .rep = size_rep,
                            .capture_id = checked.CaptureId.generatedCheck(3),
                        });
                    }
                },
                .generated_interpolation_step => |step| {
                    if (step.one_payload_type) |payload_type| {
                        const payload_rep = self.plan.repForSourceType(payload_type) orelse
                            boxyPlanInvariant("generated interpolation payload capture type was not analyzed");
                        try pending.append(self.allocator, .{
                            .kind = .captured_value,
                            .source_type = payload_type,
                            .rep = payload_rep,
                            .capture_id = checked.CaptureId.generatedCheck(0),
                        });
                    }
                },
            }

            for (self.plan.hiddenDescriptorParamSlice(worker.hidden_descs), 0..) |param, param_index| {
                try pending.append(self.allocator, .{
                    .kind = .hidden_desc,
                    .source_type = param.source_type,
                    .rep = param.rep,
                    .desc = param.desc,
                    .body_descriptor = worker.body_hidden_descs.len != 0 and
                        worker.hidden_descs.start + param_index >= worker.body_hidden_descs.start and
                        worker.hidden_descs.start + param_index < worker.body_hidden_descs.start + worker.body_hidden_descs.len,
                });
            }

            for (self.plan.hiddenDictionaryParamSlice(worker.hidden_dicts), 0..) |param, param_index| {
                try pending.append(self.allocator, .{
                    .kind = .hidden_dict,
                    .source_type = param.source_type,
                    .rep = param.rep,
                    .dictionaries = param.dictionaries,
                    .body_dictionary = worker.body_hidden_dicts.len != 0 and
                        worker.hidden_dicts.start + param_index >= worker.body_hidden_dicts.start and
                        worker.hidden_dicts.start + param_index < worker.body_hidden_dicts.start + worker.body_hidden_dicts.len,
                });
            }

            const start: u32 = @intCast(self.plan.erased_captures.items.len);
            try self.plan.erased_captures.appendSlice(self.allocator, pending.items);
            self.plan.workers.items[worker_index].erased_captures = .{
                .start = start,
                .len = @intCast(pending.items.len),
            };
        }
    }

    fn materializeStoredCallableCaptureSources(self: *Builder) Allocator.Error!void {
        for (self.plan.callable_uses.items) |*use| {
            const stored_fn = use.stored_fn orelse continue;
            use.stored_capture_sources = try self.appendStoredCallableCaptureSources(stored_fn, use.worker);
        }
        for (self.plan.static_fns.items) |*static_fn| {
            static_fn.capture_sources = try self.appendStoredCallableCaptureSources(.{
                .module = static_fn.store_module,
                .fn_id = static_fn.fn_id,
            }, static_fn.worker);
        }
    }

    fn appendStoredCallableCaptureSources(
        self: *Builder,
        stored_fn: StoredFnSource,
        worker_id: WorkerPlanId,
    ) Allocator.Error!Span {
        const store_view = self.moduleForId(stored_fn.module);
        const store = store_view.const_store orelse
            boxyPlanInvariant("stored callable use had no checked ConstStore");
        if (@intFromEnum(stored_fn.fn_id) >= store.fns.items.len) {
            boxyPlanInvariant("stored callable use referenced a missing ConstStore function");
        }
        const fn_value = store.getFn(stored_fn.fn_id);
        const worker = self.plan.workers.items[@intFromEnum(worker_id)];
        const captures = self.plan.erasedCaptureSlice(worker.erased_captures);
        const start: u32 = @intCast(self.plan.stored_callable_capture_sources.items.len);
        var const_capture_count: usize = 0;

        for (captures) |capture| {
            if (capture.kind != .captured_value) continue;
            const capture_id = capture.capture_id orelse
                boxyPlanInvariant("stored callable value capture had no checked capture id");
            var persisted_capture: ?check.ConstStore.ConstCapture = null;
            for (fn_value.captures) |persisted| {
                if (std.meta.eql(persisted.id, capture_id)) {
                    persisted_capture = persisted;
                    break;
                }
            }
            const source: StoredCallableCaptureSource.Source = if (persisted_capture) |persisted| blk: {
                const_capture_count += 1;
                break :blk .{ .const_node = .{
                    .store_module = stored_fn.module,
                    .node = persisted.value,
                    .stored_type = persisted.ty,
                } };
            } else .{ .checked_expr = self.storedGeneratedEncodingCaptureExpr(worker, capture_id) };
            try self.plan.stored_callable_capture_sources.append(self.allocator, .{
                .capture_id = capture_id,
                .source = source,
            });
        }
        if (const_capture_count != fn_value.captures.len) {
            boxyPlanInvariant("stored callable worker did not consume every persisted capture");
        }
        return .{
            .start = start,
            .len = @intCast(self.plan.stored_callable_capture_sources.items.len - start),
        };
    }

    fn storedGeneratedEncodingCaptureExpr(
        self: *Builder,
        worker: WorkerPlan,
        capture_id: checked.CaptureId,
    ) CheckedExprIdentity {
        if (capture_id != checked.CaptureId.generatedCheck(0)) {
            boxyPlanInvariant("stored callable capture was absent from ConstStore without a checked producer expression");
        }
        const codec = switch (worker.source) {
            .generated_codec => |codec| switch (codec.kind) {
                .parser_runtime, .encoder_runtime => codec,
                .parser_constructor,
                .encoder_constructor,
                .encoder_record_fields,
                .encoder_dict_fields,
                .encoder_sequence_elements,
                .encoder_tag_field,
                .encoder_tag_payload_thunk,
                .encoder_tag_payload_elements,
                .encoder_value_thunk,
                => boxyPlanInvariant("non-runtime generated codec had an unpersisted stored capture"),
            },
            .procedure_template,
            .procedure_binding,
            .procedure_use,
            .nested_expr,
            .generated_field_iterator,
            .generated_interpolation_step,
            => boxyPlanInvariant("non-codec callable had an unpersisted stored capture"),
        };
        const contract_expr = codec.contract_expr orelse
            boxyPlanInvariant("stored generated codec had no checked contract expression");
        const view = self.moduleForId(contract_expr.module);
        const dispatch = self.dispatchPlanForGeneratedRuntime(view, contract_expr.expr);
        const operands = dispatch.argsSlice(view.static_dispatch_plans);
        if (operands.len != 1) {
            boxyPlanInvariant("stored generated codec constructor did not have one checked operand");
        }
        return switch (operands[0]) {
            .checked_expr => |expr| .{ .module = view.key, .expr = expr },
            .generated_interpolation_iter,
            .generated_numeral,
            .generated_quote,
            => boxyPlanInvariant("stored generated codec encoding was not a checked expression operand"),
        };
    }

    fn workerResolvesToHosted(self: *Builder, source: WorkerSource) bool {
        if (source == .generated_codec or
            source == .generated_field_iterator or
            source == .generated_interpolation_step)
        {
            return false;
        }
        if (self.root_module == null) return workerSourceIsHosted(source);
        return switch (self.rootWorkerBody(source)) {
            .hosted_proc => true,
            .checked_expr,
            .intrinsic_wrapper,
            .unimplemented,
            => false,
        };
    }

    const CallOperandTypeRep = struct {
        type: CheckedTypeIdentity,
        rep: TypeRepId,
    };

    fn analyzeCallOperandTypeRep(
        self: *Builder,
        call_view: ModuleView,
        operand: CallOperand,
        call_type: CheckedTypeIdentity,
        call_rep: TypeRepId,
        caller: WorkerPlanId,
    ) Allocator.Error!CallOperandTypeRep {
        return switch (operand) {
            .checked_expr => |call_arg| blk: {
                const actual_expr = call_view.checked_bodies.expr(call_arg);
                if (actual_expr.data == .lambda or actual_expr.data == .closure) {
                    try self.recordNestedCallableExprUseForCaller(
                        call_view,
                        call_arg,
                        call_type,
                        caller,
                    );
                }
                break :blk .{
                    .type = typeRef(call_view, actual_expr.ty),
                    .rep = try self.analyzeType(call_view, actual_expr.ty),
                };
            },
            .generated_interpolation_iter,
            .generated_numeral,
            .generated_quote,
            => .{ .type = call_type, .rep = call_rep },
        };
    }

    fn materializeDirectCallTypeSubstitutions(self: *Builder) Allocator.Error!void {
        var direct_index: usize = 0;
        while (direct_index < self.plan.direct_calls.items.len) : (direct_index += 1) {
            const direct = self.plan.direct_calls.items[direct_index];
            if (direct.ret_substitution != null) continue;
            const call_view = self.moduleForId(direct.call.module);
            const call_operands = self.plan.callOperandSlice(direct.operands);

            const source_view = self.moduleForId(direct.source_fn_type.module);
            const source_function = checkedFunctionPayload(source_view, direct.source_fn_type.ty);
            if (source_function.args.len != call_operands.len) {
                boxyPlanInvariant("boxy direct call instantiated function type arity disagreed with call args");
            }

            const worker = self.plan.workers.items[@intFromEnum(direct.worker)];
            const worker_function = (self.repQuery().functionChildren(worker.rep)) orelse
                boxyPlanInvariant("boxy direct call worker substitution target was not a function");
            if (worker_function.arg_count != source_function.args.len) {
                boxyPlanInvariant("boxy direct call worker arity disagreed with its instantiated function type");
            }
            const worker_children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(worker_function.rep)].children);
            const worker_args = worker_children[worker_function.args_start..][0..worker_function.arg_count];

            const start: u32 = @intCast(self.plan.call_type_substitutions.items.len);
            for (source_function.args, worker_args, call_operands) |call_arg_ty, worker_arg, operand| {
                const call_type = CheckedTypeIdentity{ .module = direct.source_fn_type.module, .ty = call_arg_ty };
                const call_rep = try self.analyzeType(source_view, call_arg_ty);
                const operand_info = try self.analyzeCallOperandTypeRep(call_view, operand, call_type, call_rep, direct.caller);
                try self.plan.call_type_substitutions.append(self.allocator, .{
                    .operand_type = operand_info.type,
                    .operand_rep = operand_info.rep,
                    .call_type = call_type,
                    .call_rep = call_rep,
                    .worker_rep = worker_arg.rep,
                });
            }

            const call_ret_type = CheckedTypeIdentity{
                .module = direct.source_fn_type.module,
                .ty = source_function.ret,
            };
            const call_ret_rep = try self.analyzeType(source_view, source_function.ret);
            self.plan.direct_calls.items[direct_index].arg_substitutions = .{
                .start = start,
                .len = @intCast(source_function.args.len),
            };
            self.plan.direct_calls.items[direct_index].ret_substitution = .{
                .operand_type = self.plan.representations.items[@intFromEnum(worker_function.ret)].source_type,
                .operand_rep = worker_function.ret,
                .call_type = call_ret_type,
                .call_rep = call_ret_rep,
                .worker_rep = worker_function.ret,
            };
        }
    }

    fn materializeDictionaryDispatchTypeSubstitutions(self: *Builder) Allocator.Error!void {
        for (self.plan.dictionary_dispatches.items, 0..) |dispatch, dispatch_index| {
            if (dispatch.ret_substitution != null) continue;

            const call_view = self.moduleForId(dispatch.call.module);
            const source_view = self.moduleForId(dispatch.source_fn_type.module);
            const source_function = checkedFunctionPayload(source_view, dispatch.source_fn_type.ty);
            const operands = self.plan.callOperandSlice(dispatch.operands);
            if (source_function.args.len != operands.len) {
                boxyPlanInvariant("boxy dictionary dispatch callable arity disagreed with its operands");
            }

            const start: u32 = @intCast(self.plan.call_type_substitutions.items.len);
            for (source_function.args, operands) |call_arg_ty, operand| {
                const call_type = CheckedTypeIdentity{ .module = dispatch.source_fn_type.module, .ty = call_arg_ty };
                const call_rep = try self.analyzeType(source_view, call_arg_ty);
                const operand_info = try self.analyzeCallOperandTypeRep(call_view, operand, call_type, call_rep, dispatch.caller);
                try self.plan.call_type_substitutions.append(self.allocator, .{
                    .operand_type = operand_info.type,
                    .operand_rep = operand_info.rep,
                    .call_type = call_type,
                    .call_rep = call_rep,
                    .worker_rep = call_rep,
                });
            }

            const result_expr = call_view.checked_bodies.expr(dispatch.call.expr);
            const result_type = typeRef(call_view, result_expr.ty);
            const result_rep = try self.analyzeType(call_view, result_expr.ty);
            const call_ret_type = CheckedTypeIdentity{
                .module = dispatch.source_fn_type.module,
                .ty = source_function.ret,
            };
            const call_ret_rep = try self.analyzeType(source_view, source_function.ret);
            self.plan.dictionary_dispatches.items[dispatch_index].arg_substitutions = .{
                .start = start,
                .len = @intCast(source_function.args.len),
            };
            self.plan.dictionary_dispatches.items[dispatch_index].ret_substitution = .{
                .operand_type = result_type,
                .operand_rep = result_rep,
                .call_type = call_ret_type,
                .call_rep = call_ret_rep,
                .worker_rep = call_ret_rep,
            };
        }
    }

    fn materializeDictionaryCallPlans(self: *Builder) Allocator.Error!void {
        while (true) {
            const worker_count = self.plan.workers.items.len;
            const direct_call_count = self.plan.direct_calls.items.len;
            const dictionary_dispatch_count = self.plan.dictionary_dispatches.items.len;
            const representation_count = self.plan.representations.items.len;
            const dictionary_count = self.plan.dictionaries.items.len;
            const inspect_method_count = self.plan.inspect_methods.items.len;
            const generated_codec_call_count = self.plan.generated_codec_calls.items.len;
            const worker_dictionary_use_count = self.worker_dictionary_uses.items.len;
            const nested_callable_use_count = self.plan.nested_callable_uses.items.len;

            try self.materializeDirectCallTypeSubstitutions();
            try self.materializeDictionaryDispatchTypeSubstitutions();
            // Dictionary-dispatch worker discovery can analyze method bodies
            // that append direct calls. Inspect planning consumes their exact
            // substitutions in this iteration.
            try self.materializeDirectCallTypeSubstitutions();
            try self.materializeInspectMethodPlans();
            // Inspect worker discovery analyzes method bodies and can append
            // direct calls; materialize those substitutions before any hidden
            // argument phase consumes them in this fixed-point iteration.
            try self.materializeDirectCallTypeSubstitutions();
            try self.materializeDictionaryDispatchTypeSubstitutions();
            try self.materializeWorkerHiddenDictionaryParams();
            try self.materializeRootHiddenDictionaryArgs();
            try self.materializeDirectCallHiddenDictionaryArgs();
            try self.materializeConstEvalCallHiddenDictionaryArgs();
            try self.materializeIteratorCallHiddenDictionaryArgs();
            try self.materializeGeneratedCodecCallHiddenDictionaryArgs();
            try self.planNestedCallableUseDictionaries();

            if (worker_count == self.plan.workers.items.len and
                direct_call_count == self.plan.direct_calls.items.len and
                dictionary_dispatch_count == self.plan.dictionary_dispatches.items.len and
                representation_count == self.plan.representations.items.len and
                dictionary_count == self.plan.dictionaries.items.len and
                inspect_method_count == self.plan.inspect_methods.items.len and
                generated_codec_call_count == self.plan.generated_codec_calls.items.len and
                worker_dictionary_use_count == self.worker_dictionary_uses.items.len and
                nested_callable_use_count == self.plan.nested_callable_uses.items.len)
            {
                return;
            }
        }
    }

    fn materializeInspectMethodPlans(self: *Builder) Allocator.Error!void {
        const direct_count = self.plan.direct_calls.items.len;
        var direct_index: usize = 0;
        while (direct_index < direct_count) : (direct_index += 1) {
            const direct = self.plan.direct_calls.items[direct_index];
            if (!self.workerIsStrInspectIntrinsic(direct.worker)) continue;
            const substitutions = self.plan.callTypeSubstitutionSlice(direct.arg_substitutions);
            if (substitutions.len != 1) {
                boxyPlanInvariant("Str.inspect direct call plan had unexpected substitution arity");
            }
            var seen = collections.DenseMap(TypeRepId, void).init(self.allocator);
            defer seen.deinit();
            try self.materializeInspectMethodsForRep(substitutions[0].operand_rep, &seen);
        }

        const nested_callable_count = self.plan.nested_callable_uses.items.len;
        var nested_callable_index: usize = 0;
        while (nested_callable_index < nested_callable_count) : (nested_callable_index += 1) {
            const use = self.plan.nested_callable_uses.items[nested_callable_index];
            if (!self.workerIsStrInspectIntrinsic(use.worker)) continue;
            const callable_rep = self.plan.repForSourceType(use.callable_ty) orelse
                boxyPlanInvariant("Str.inspect function-value use type was not analyzed");
            const function = (self.repQuery().functionChildren(callable_rep)) orelse
                boxyPlanInvariant("Str.inspect function-value use was not callable");
            if (function.arg_count != 1) {
                boxyPlanInvariant("Str.inspect function-value use had unexpected arity");
            }
            const children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(function.rep)].children);
            const arg_rep = children[function.args_start].rep;
            var seen = collections.DenseMap(TypeRepId, void).init(self.allocator);
            defer seen.deinit();
            try self.materializeInspectMethodsForRep(arg_rep, &seen);
        }
    }

    fn workerIsStrInspectIntrinsic(self: *Builder, worker_id: WorkerPlanId) bool {
        const worker = self.plan.workers.items[@intFromEnum(worker_id)];
        if (worker.source == .generated_codec or
            worker.source == .generated_field_iterator or
            worker.source == .generated_interpolation_step)
        {
            return false;
        }
        return switch (self.rootWorkerBody(worker.source)) {
            .intrinsic_wrapper => |intrinsic| intrinsic.wrapper.intrinsic == .str_inspect,
            .checked_expr, .hosted_proc, .unimplemented => false,
        };
    }

    fn materializeInspectMethodsForRep(
        self: *Builder,
        rep_id: TypeRepId,
        seen: *collections.DenseMap(TypeRepId, void),
    ) Allocator.Error!void {
        const entry = try seen.getOrPut(rep_id);
        if (entry.found_existing) return;

        const rep = self.plan.representations.items[@intFromEnum(rep_id)];
        const view = self.moduleForId(rep.source_type.module);
        if (methodOwnerForModuleType(view, rep.source_type.ty)) |owner| {
            if (self.lookupMethodTargetByText(view, owner, "to_inspect")) |lookup| {
                if (self.plan.inspectMethodForRep(rep_id) == null) {
                    const source = self.workerSourceForMethodTarget(lookup, rep.source_type, null);
                    const source_fn_type = CheckedTypeIdentity{ .module = lookup.view.key, .ty = lookup.target.callable_ty };
                    _ = try self.analyzeType(lookup.view, lookup.target.callable_ty);
                    const worker = try self.ensureWorker(source, source_fn_type, null);
                    try self.plan.inspect_methods.append(self.allocator, .{
                        .source_rep = rep_id,
                        .worker = worker,
                        .method_module = lookup.view.key,
                        .method = lookup.method orelse
                            boxyPlanInvariant("planned boxy inspect target had no checked method identity"),
                    });
                }
            }
        }

        for (0..rep.children.len) |child_index| {
            const child = self.plan.children.items[@as(usize, rep.children.start) + child_index];
            try self.materializeInspectMethodsForRep(child.rep, seen);
        }
    }

    fn materializeDirectCallHiddenDescriptorArgs(self: *Builder) Allocator.Error!void {
        var direct_index: usize = 0;
        while (direct_index < self.plan.direct_calls.items.len) : (direct_index += 1) {
            const direct = self.plan.direct_calls.items[direct_index];
            const substitutions = self.plan.callTypeSubstitutionSlice(direct.arg_substitutions);
            if (substitutions.len != direct.operands.len) {
                boxyPlanInvariant("boxy direct call descriptor substitution arity disagreed with call args");
            }
            const operand_types = try self.callSubstitutionTypes(direct.arg_substitutions, .operand);
            defer self.allocator.free(operand_types);
            const call_types = try self.callSubstitutionTypes(direct.arg_substitutions, .call);
            defer self.allocator.free(call_types);
            const evidence = self.checkedEvidenceForDirectCall(direct);
            const hidden_desc_args = try self.materializeWorkerCallHiddenDescriptorArgsWithEvidence(
                direct.worker,
                call_types,
                operand_types,
                direct.ret_substitution.?.call_type,
                evidence.view,
                evidence.entries,
            );
            self.plan.direct_calls.items[direct_index].hidden_desc_args = hidden_desc_args;
        }
    }

    fn materializeCallableUseHiddenDescriptorArgs(self: *Builder) Allocator.Error!void {
        for (self.plan.callable_uses.items) |*use| {
            const worker = self.plan.workers.items[@intFromEnum(use.worker)];
            if (worker.hidden_descs.len == 0) continue;
            const view = self.moduleForId(use.use.module);
            const evidence = view.static_dispatch_plans.siteEvidence(use.use.expr);
            if (evidence == null and self.workerHasPathlessEvidence(worker)) continue;
            use.hidden_desc_args = try self.materializeCallableUseHiddenDescriptorArgsAtType(
                use.worker,
                use.callable_ty,
                view,
                evidence,
            );
        }
        for (self.plan.nested_callable_uses.items) |*use| {
            const worker = self.plan.workers.items[@intFromEnum(use.worker)];
            if (worker.hidden_descs.len == 0) continue;
            const view = self.moduleForId(use.use.module);
            const evidence = view.static_dispatch_plans.siteEvidence(use.use.expr);
            if (evidence == null and self.workerHasPathlessEvidence(worker)) continue;
            use.hidden_desc_args = try self.materializeCallableUseHiddenDescriptorArgsAtType(
                use.worker,
                use.callable_ty,
                view,
                evidence,
            );
        }

        for (self.plan.callable_uses.items) |*use| {
            try self.fillCallableUseHiddenDescriptorArgs(use.worker, use.caller, &use.hidden_desc_args);
        }
        for (self.plan.nested_callable_uses.items) |*use| {
            try self.fillCallableUseHiddenDescriptorArgs(use.worker, use.caller, &use.hidden_desc_args);
        }
    }

    fn materializeCallableUseHiddenDescriptorArgsAtType(
        self: *Builder,
        worker: WorkerPlanId,
        callable_type: CheckedTypeIdentity,
        view: ModuleView,
        evidence: ?[]const static_dispatch.CheckedEvidence,
    ) Allocator.Error!Span {
        const callable_rep = self.plan.repForSourceType(callable_type) orelse
            boxyPlanInvariant("boxy callable use type was not analyzed for descriptor captures");
        const function = (self.repQuery().functionChildren(callable_rep)) orelse
            boxyPlanInvariant("boxy callable use descriptor capture type was not callable");
        const arg_types = try self.allocator.alloc(CheckedTypeIdentity, function.arg_count);
        defer self.allocator.free(arg_types);
        const children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(function.rep)].children);
        for (arg_types, children[function.args_start..][0..function.arg_count]) |*arg_type, child| {
            arg_type.* = self.plan.representations.items[@intFromEnum(child.rep)].source_type;
        }
        const ret_type = self.plan.representations.items[@intFromEnum(function.ret)].source_type;
        return try self.materializeWorkerCallHiddenDescriptorArgsWithEvidence(
            worker,
            arg_types,
            arg_types,
            ret_type,
            view,
            evidence,
        );
    }

    fn workerHasPathlessEvidence(self: *Builder, worker: WorkerPlan) bool {
        const evidence = self.workerEvidenceParams(worker.source) orelse return false;
        for (evidence.params) |param| {
            if (param.runtime_dictionary) continue;
            if (evidence.view.checked_procedure_templates.evidenceParamPath(param).len == 0) return true;
        }
        return false;
    }

    fn fillCallableUseHiddenDescriptorArgs(
        self: *Builder,
        worker_id: WorkerPlanId,
        caller: WorkerPlanId,
        target: *Span,
    ) Allocator.Error!void {
        const worker = self.plan.workers.items[@intFromEnum(worker_id)];
        if (worker.hidden_descs.len == 0 or target.len != 0) return;

        var source: ?Span = null;
        var ambiguous = false;
        for (self.plan.callable_uses.items) |candidate| {
            if (candidate.worker != worker_id or candidate.caller != caller or
                candidate.hidden_desc_args.len != worker.hidden_descs.len) continue;
            self.mergeHiddenDescriptorArgSource(&source, &ambiguous, candidate.hidden_desc_args);
        }
        for (self.plan.nested_callable_uses.items) |candidate| {
            if (candidate.worker != worker_id or candidate.caller != caller or
                candidate.hidden_desc_args.len != worker.hidden_descs.len) continue;
            self.mergeHiddenDescriptorArgSource(&source, &ambiguous, candidate.hidden_desc_args);
        }
        for (self.plan.direct_calls.items) |candidate| {
            if (candidate.worker != worker_id or candidate.caller != caller or
                candidate.hidden_desc_args.len != worker.hidden_descs.len) continue;
            self.mergeHiddenDescriptorArgSource(&source, &ambiguous, candidate.hidden_desc_args);
        }
        if (!ambiguous) {
            if (source) |planned| {
                target.* = planned;
                return;
            }
        }
        boxyPlanInvariant("callable value had no unambiguous checked descriptor capture source");
    }

    fn mergeHiddenDescriptorArgSource(
        self: *Builder,
        source: *?Span,
        ambiguous: *bool,
        candidate: Span,
    ) void {
        if (ambiguous.*) return;
        if (source.*) |existing| {
            if (!std.meta.eql(
                self.plan.directCallHiddenDescriptorArgSlice(existing),
                self.plan.directCallHiddenDescriptorArgSlice(candidate),
            )) {
                source.* = null;
                ambiguous.* = true;
            }
        } else {
            source.* = candidate;
        }
    }

    fn materializeRootHiddenDescriptorArgs(self: *Builder) Allocator.Error!void {
        for (self.plan.roots.items, 0..) |root, root_index| {
            const worker = self.plan.workers.items[@intFromEnum(root.worker)];
            if (worker.hidden_descs.len == 0) {
                self.plan.roots.items[root_index].hidden_desc_args = .{};
                continue;
            }

            const function = (self.repQuery().functionChildren(root.source_rep)) orelse
                boxyPlanInvariant("boxy root with hidden descriptors had no callable source type");
            const children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(function.rep)].children);
            const arg_types = try self.allocator.alloc(CheckedTypeIdentity, function.arg_count);
            defer self.allocator.free(arg_types);
            for (arg_types, children[function.args_start..][0..function.arg_count]) |*arg_type, child| {
                arg_type.* = child.source_type;
            }
            const ret_type = self.plan.representations.items[@intFromEnum(function.ret)].source_type;
            self.plan.roots.items[root_index].hidden_desc_args =
                try self.materializeWorkerCallHiddenDescriptorArgs(root.worker, arg_types, arg_types, ret_type);
        }
    }

    fn materializeConstEvalCallHiddenDescriptorArgs(self: *Builder) Allocator.Error!void {
        var index: usize = 0;
        while (index < self.plan.const_eval_calls.items.len) : (index += 1) {
            const call = self.plan.const_eval_calls.items[index];
            self.plan.const_eval_calls.items[index].hidden_desc_args =
                try self.materializeWorkerCallHiddenDescriptorArgs(call.worker, &.{}, &.{}, call.ret_substitution.call_type);
        }
    }

    fn materializeGeneratedCodecCallHiddenDescriptorArgs(self: *Builder) Allocator.Error!void {
        var index: usize = 0;
        while (index < self.plan.generated_codec_calls.items.len) : (index += 1) {
            const call = self.plan.generated_codec_calls.items[index];
            const arg_types = self.plan.generatedCodecCallTypeSlice(call.arg_types);
            self.plan.generated_codec_calls.items[index].hidden_desc_args =
                try self.materializeWorkerCallHiddenDescriptorArgs(
                    call.worker,
                    arg_types,
                    arg_types,
                    call.ret_type,
                );
        }
    }

    fn materializeConstEvalCallHiddenDictionaryArgs(self: *Builder) Allocator.Error!void {
        var index: usize = 0;
        while (index < self.plan.const_eval_calls.items.len) : (index += 1) {
            const call = self.plan.const_eval_calls.items[index];
            self.plan.const_eval_calls.items[index].hidden_dict_args =
                try self.materializeWorkerCallHiddenDictionaryArgs(call.worker, null, &.{}, call.ret_type);
        }
    }

    fn materializeRootHiddenDictionaryArgs(self: *Builder) Allocator.Error!void {
        for (self.plan.roots.items, 0..) |root, root_index| {
            const worker = self.plan.workers.items[@intFromEnum(root.worker)];
            if (worker.hidden_dicts.len == 0) {
                self.plan.roots.items[root_index].hidden_dict_args = .{};
                continue;
            }

            const function = (self.repQuery().functionChildren(root.source_rep)) orelse
                boxyPlanInvariant("boxy root with hidden dictionaries had no callable source type");
            const children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(function.rep)].children);
            const arg_types = try self.allocator.alloc(CheckedTypeIdentity, function.arg_count);
            defer self.allocator.free(arg_types);
            for (arg_types, children[function.args_start..][0..function.arg_count]) |*arg_type, child| {
                arg_type.* = child.source_type;
            }
            const ret_type = self.plan.representations.items[@intFromEnum(function.ret)].source_type;
            self.plan.roots.items[root_index].hidden_dict_args =
                try self.materializeWorkerCallHiddenDictionaryArgs(root.worker, null, arg_types, ret_type);
        }
    }

    fn materializeIteratorCallHiddenDescriptorArgs(self: *Builder) Allocator.Error!void {
        for (self.plan.iterator_calls.items, 0..) |call, call_index| {
            const operand_types = try self.callSubstitutionTypes(call.arg_substitutions, .operand);
            defer self.allocator.free(operand_types);
            const call_types = try self.callSubstitutionTypes(call.arg_substitutions, .call);
            defer self.allocator.free(call_types);
            self.plan.iterator_calls.items[call_index].hidden_desc_args =
                try self.materializeWorkerCallHiddenDescriptorArgs(call.worker, call_types, operand_types, call.ret_substitution.call_type);
        }
    }

    fn materializeDirectCallHiddenDictionaryArgs(self: *Builder) Allocator.Error!void {
        const call_count = self.plan.direct_calls.items.len;
        var direct_index: usize = 0;
        while (direct_index < call_count) : (direct_index += 1) {
            const direct = self.plan.direct_calls.items[direct_index];
            const call_evidence = self.checkedEvidenceForDirectCall(direct);
            const call_view = call_evidence.view;
            const checked_evidence = call_evidence.entries;

            const substitutions = self.plan.callTypeSubstitutionSlice(direct.arg_substitutions);
            if (substitutions.len != direct.operands.len) {
                boxyPlanInvariant("boxy direct call dictionary substitution arity disagreed with call args");
            }
            const arg_types = try self.callSubstitutionTypes(direct.arg_substitutions, .operand);
            defer self.allocator.free(arg_types);
            const hidden_dict_args = try self.materializeWorkerCallHiddenDictionaryArgsWithEvidence(
                direct.worker,
                direct.caller,
                arg_types,
                direct.ret_substitution.?.call_type,
                call_view,
                checked_evidence,
            );
            self.plan.direct_calls.items[direct_index].hidden_dict_args = hidden_dict_args;
        }
    }

    fn nestedEvidenceForDirectDispatch(
        _: *Builder,
        view: ModuleView,
        maybe_plan: ?static_dispatch.StaticDispatchPlanId,
    ) ?[]const static_dispatch.CheckedEvidence {
        const plan_id = maybe_plan orelse
            boxyPlanInvariant("direct dispatch call had no checked dispatch plan");
        const raw = @intFromEnum(plan_id);
        if (raw >= view.static_dispatch_plans.plans.len) {
            boxyPlanInvariant("direct dispatch call referenced a missing checked dispatch plan");
        }
        return switch (view.static_dispatch_plans.plans[raw].resolution) {
            .direct_closed, .direct_parametric => |direct| view.static_dispatch_plans.nestedEvidence(view.static_dispatch_plans.evidenceNode(direct.evidence)),
            .direct_pending => boxyPlanInvariant("unfinalized direct call reached Boxy planning"),
            .evidence_dependent,
            .structural,
            .checked_error,
            .@"unreachable",
            => null,
        };
    }

    fn materializeIteratorCallHiddenDictionaryArgs(self: *Builder) Allocator.Error!void {
        for (self.plan.iterator_calls.items, 0..) |call, call_index| {
            const arg_types = try self.callSubstitutionTypes(call.arg_substitutions, .operand);
            defer self.allocator.free(arg_types);
            self.plan.iterator_calls.items[call_index].hidden_dict_args =
                try self.materializeWorkerCallHiddenDictionaryArgs(call.worker, call.caller, arg_types, call.ret_type);
        }
    }

    fn materializeGeneratedCodecCallHiddenDictionaryArgs(self: *Builder) Allocator.Error!void {
        var index: usize = 0;
        while (index < self.plan.generated_codec_calls.items.len) : (index += 1) {
            const call = self.plan.generated_codec_calls.items[index];
            const arg_types = self.plan.generatedCodecCallTypeSlice(call.arg_types);
            const call_view = self.moduleForId(call.method_module);
            const checked_evidence = call_view.static_dispatch_plans.evidence_refs[call.checked_evidence.start .. call.checked_evidence.start + call.checked_evidence.len];
            self.plan.generated_codec_calls.items[index].hidden_dict_args =
                try self.materializeWorkerCallHiddenDictionaryArgsWithEvidence(
                    call.worker,
                    call.caller,
                    arg_types,
                    call.ret_type,
                    call_view,
                    checked_evidence,
                );
        }
    }

    const SubstitutionTypeKind = enum { operand, call };

    fn callSubstitutionTypes(
        self: *Builder,
        span: Span,
        kind: SubstitutionTypeKind,
    ) Allocator.Error![]CheckedTypeIdentity {
        const substitutions = self.plan.callTypeSubstitutionSlice(span);
        const types = try self.allocator.alloc(CheckedTypeIdentity, substitutions.len);
        errdefer self.allocator.free(types);
        for (substitutions, types) |substitution, *ty| {
            ty.* = switch (kind) {
                .operand => substitution.operand_type,
                .call => substitution.call_type,
            };
        }
        return types;
    }

    fn materializeWorkerCallHiddenDescriptorArgs(
        self: *Builder,
        worker_id: WorkerPlanId,
        call_arg_types: []const CheckedTypeIdentity,
        operand_arg_types: []const CheckedTypeIdentity,
        ret_type: CheckedTypeIdentity,
    ) Allocator.Error!Span {
        return try self.materializeWorkerCallHiddenDescriptorArgsWithEvidence(
            worker_id,
            call_arg_types,
            operand_arg_types,
            ret_type,
            null,
            null,
        );
    }

    fn materializeWorkerCallHiddenDescriptorArgsWithEvidence(
        self: *Builder,
        worker_id: WorkerPlanId,
        call_arg_types: []const CheckedTypeIdentity,
        operand_arg_types: []const CheckedTypeIdentity,
        ret_type: CheckedTypeIdentity,
        evidence_view: ?ModuleView,
        evidence: ?[]const static_dispatch.CheckedEvidence,
    ) Allocator.Error!Span {
        if (call_arg_types.len != operand_arg_types.len) {
            boxyPlanInvariant("boxy worker call hidden descriptor mapping saw mismatched function arity");
        }

        const call_arg_reps = try self.allocator.alloc(TypeRepId, call_arg_types.len);
        defer self.allocator.free(call_arg_reps);
        const operand_arg_reps = try self.allocator.alloc(TypeRepId, operand_arg_types.len);
        defer self.allocator.free(operand_arg_reps);
        for (call_arg_types, operand_arg_types, call_arg_reps, operand_arg_reps) |call_arg_type, operand_arg_type, *call_arg_rep, *operand_arg_rep| {
            call_arg_rep.* = self.plan.repForSourceType(call_arg_type) orelse
                boxyPlanInvariant("boxy worker call argument type was not analyzed");
            operand_arg_rep.* = self.plan.repForSourceType(operand_arg_type) orelse
                boxyPlanInvariant("boxy worker call operand type was not analyzed");
        }
        const ret_rep = self.plan.repForSourceType(ret_type) orelse
            boxyPlanInvariant("boxy worker call result type was not analyzed");
        return try self.materializeWorkerCallHiddenDescriptorArgsForRepsWithEvidence(
            worker_id,
            call_arg_reps,
            operand_arg_reps,
            ret_rep,
            call_arg_types,
            ret_type,
            evidence_view,
            evidence,
        );
    }

    fn materializeWorkerCallHiddenDescriptorArgsForRepsWithEvidence(
        self: *Builder,
        worker_id: WorkerPlanId,
        call_arg_reps: []const TypeRepId,
        operand_arg_reps: []const TypeRepId,
        ret_rep: TypeRepId,
        call_arg_types: []const CheckedTypeIdentity,
        ret_type: CheckedTypeIdentity,
        evidence_view: ?ModuleView,
        evidence: ?[]const static_dispatch.CheckedEvidence,
    ) Allocator.Error!Span {
        if (call_arg_reps.len != operand_arg_reps.len) {
            boxyPlanInvariant("boxy worker call hidden descriptor mapping saw mismatched operand arity");
        }
        const worker = self.plan.workers.items[@intFromEnum(worker_id)];
        const params = self.plan.hiddenDescriptorParamSlice(worker.hidden_descs);
        if (params.len == 0) return .{};

        const worker_function = (self.repQuery().functionChildren(worker.rep)) orelse
            boxyPlanInvariant("boxy worker call target with hidden descriptors was not a function worker");
        if (worker_function.arg_count != call_arg_reps.len) {
            boxyPlanInvariant("boxy worker call hidden descriptor mapping saw mismatched function arity");
        }

        var pending = std.ArrayList(DirectCallHiddenDescriptorArg).empty;
        defer pending.deinit(self.allocator);
        var seen_reps = collections.DenseMap(TypeRepId, void).init(self.allocator);
        defer seen_reps.deinit();
        var seen_descriptor_reps = collections.DenseMap(TypeRepId, void).init(self.allocator);
        defer seen_descriptor_reps.deinit();
        var substitutions = CallDescriptorRepSubstitutionMap{};
        defer substitutions.deinit(self.allocator);
        const evidence_only_start: usize = if (worker.evidence_only_descs.len == 0)
            params.len
        else
            worker.evidence_only_descs.start - worker.hidden_descs.start;
        if (evidence_only_start > params.len or
            worker.evidence_only_descs.start + worker.evidence_only_descs.len != worker.hidden_descs.start + worker.hidden_descs.len)
        {
            boxyPlanInvariant("boxy evidence-only worker descriptors were not a suffix of hidden descriptors");
        }
        const ordinary_params = params[0..evidence_only_start];
        var next_param: usize = 0;

        const worker_children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(worker_function.rep)].children);
        for (
            worker_children[worker_function.args_start..][0..worker_function.arg_count],
            call_arg_reps,
            operand_arg_reps,
            0..,
        ) |worker_child, call_arg_rep, operand_arg_rep, arg_index| {
            try self.collectCallHiddenDescriptorArgs(
                worker_child.rep,
                call_arg_rep,
                call_arg_rep,
                operand_arg_rep,
                @intCast(arg_index),
                ordinary_params,
                &next_param,
                &pending,
                &seen_reps,
                &seen_descriptor_reps,
                &substitutions,
                false,
            );
        }
        try self.collectCallHiddenDescriptorArgs(worker_function.ret, ret_rep, ret_rep, ret_rep, null, ordinary_params, &next_param, &pending, &seen_reps, &seen_descriptor_reps, &substitutions, false);

        if (next_param != ordinary_params.len or pending.items.len != ordinary_params.len) {
            boxyPlanInvariant("boxy worker call hidden descriptor mapping did not cover every ordinary worker descriptor param");
        }

        try self.appendEvidenceOnlyCallHiddenDescriptorArgs(
            worker,
            params,
            call_arg_types,
            call_arg_reps,
            ret_type,
            evidence_view,
            evidence,
            &pending,
        );
        if (pending.items.len != params.len) {
            boxyPlanInvariant("boxy worker call evidence mapping did not cover every worker descriptor param");
        }

        const start: u32 = @intCast(self.plan.direct_call_hidden_desc_args.items.len);
        try self.plan.direct_call_hidden_desc_args.appendSlice(self.allocator, pending.items);
        return .{
            .start = start,
            .len = @intCast(pending.items.len),
        };
    }

    fn appendEvidenceOnlyCallHiddenDescriptorArgs(
        self: *Builder,
        worker: WorkerPlan,
        params: []const HiddenDescriptorParam,
        call_arg_types: []const CheckedTypeIdentity,
        call_arg_reps: []const TypeRepId,
        ret_type: CheckedTypeIdentity,
        maybe_view: ?ModuleView,
        maybe_evidence: ?[]const static_dispatch.CheckedEvidence,
        pending: *std.ArrayList(DirectCallHiddenDescriptorArg),
    ) Allocator.Error!void {
        const mappings = self.plan.workerEvidenceDescriptorParamSlice(worker.evidence_descs);
        if (mappings.len == 0) return;
        const evidence_only_start: usize = @intCast(worker.evidence_only_descs.start - worker.hidden_descs.start);

        // Evidence that aliases an ordinary signature descriptor must select
        // the same concrete call representation already chosen by the normal
        // function-boundary substitution.
        for (mappings) |mapping| {
            if (mapping.hidden_desc_index >= params.len) {
                boxyPlanInvariant("worker literal-evidence descriptor mapping exceeded its checked vectors");
            }
            const source = try self.workerEvidenceDescriptorCallSource(
                worker,
                mapping.evidence_index,
                call_arg_types,
                call_arg_reps,
                ret_type,
                maybe_view,
                maybe_evidence,
            );
            if (mapping.hidden_desc_index < evidence_only_start and
                !self.callDescriptorRepsAgreeAcrossPresenceSlot(
                    pending.items[mapping.hidden_desc_index].rep,
                    source.rep,
                ))
            {
                boxyPlanInvariant("literal evidence disagreed with the concrete signature descriptor substitution");
            }
        }

        var hidden_index = evidence_only_start;
        while (hidden_index < params.len) : (hidden_index += 1) {
            var source_type: ?CheckedTypeIdentity = null;
            var source_rep: ?TypeRepId = null;
            var source_arg_index: ?u32 = null;
            var source_value_rep: ?TypeRepId = null;
            for (mappings) |mapping| {
                if (mapping.hidden_desc_index != hidden_index) continue;
                const source = try self.workerEvidenceDescriptorCallSource(
                    worker,
                    mapping.evidence_index,
                    call_arg_types,
                    call_arg_reps,
                    ret_type,
                    maybe_view,
                    maybe_evidence,
                );
                if (source_rep) |existing| {
                    if (existing != source.rep) {
                        boxyPlanInvariant("one worker descriptor parameter received conflicting literal evidence types");
                    }
                } else {
                    source_type = source.source_type;
                    source_rep = source.rep;
                    source_arg_index = source.source_arg_index;
                    source_value_rep = source.source_value_rep;
                }
            }
            const param = params[hidden_index];
            try pending.append(self.allocator, .{
                .worker_desc = param.desc,
                .worker_rep = param.rep,
                .source_type = source_type orelse
                    boxyPlanInvariant("evidence-only worker descriptor had no checked evidence mapping"),
                .rep = source_rep.?,
                .source_arg_index = source_arg_index,
                .source_value_rep = source_value_rep,
            });
        }
    }

    const EvidenceDescriptorCallSource = struct {
        source_type: CheckedTypeIdentity,
        rep: TypeRepId,
        source_arg_index: ?u32,
        source_value_rep: ?TypeRepId,
    };

    fn workerEvidenceDescriptorCallSource(
        self: *Builder,
        worker: WorkerPlan,
        evidence_index: u32,
        call_arg_types: []const CheckedTypeIdentity,
        call_arg_reps: []const TypeRepId,
        ret_type: CheckedTypeIdentity,
        maybe_view: ?ModuleView,
        maybe_evidence: ?[]const static_dispatch.CheckedEvidence,
    ) Allocator.Error!EvidenceDescriptorCallSource {
        if (call_arg_types.len != call_arg_reps.len) {
            boxyPlanInvariant("worker evidence descriptor call source saw mismatched argument metadata");
        }
        const worker_evidence = self.workerEvidenceParams(worker.source) orelse
            boxyPlanInvariant("worker evidence descriptor had no checked parameters");
        if (evidence_index >= worker_evidence.params.len) {
            boxyPlanInvariant("worker evidence descriptor index exceeded its checked parameter vector");
        }
        const param = worker_evidence.params[evidence_index];
        if (param.runtime_dictionary) {
            boxyPlanInvariant("worker literal-evidence descriptor mapped to a runtime dictionary entry");
        }
        const path_view = worker_evidence.view;
        const path = path_view.checked_procedure_templates.evidenceParamPath(param);
        const source_arg_index = evidencePathSourceArgIndex(path, call_arg_types.len);

        const source_type = if (maybe_evidence) |evidence| blk: {
            const view = maybe_view orelse
                boxyPlanInvariant("worker literal evidence had no checked call-site module");
            if (evidence_index >= evidence.len) {
                boxyPlanInvariant("worker literal evidence index exceeded the checked call-site vector");
            }
            const entry = evidence[evidence_index];
            if (entry.runtime_dictionary) {
                boxyPlanInvariant("worker literal-evidence descriptor selected a runtime dictionary entry");
            }
            break :blk typeRef(view, entry.dispatcher_ty);
        } else try self.checkedTypeAtEvidenceCallPath(
            worker_evidence.view,
            path,
            call_arg_types,
            ret_type,
        );
        const rep = try self.evidenceCallRepAtPath(path_view, path, source_type, call_arg_reps, ret_type);
        return .{
            .source_type = source_type,
            .rep = rep,
            .source_arg_index = source_arg_index,
            .source_value_rep = if (source_arg_index) |index| call_arg_reps[index] else null,
        };
    }

    fn evidenceCallRepAtPath(
        self: *Builder,
        path_view: ModuleView,
        path: []const static_dispatch.EvidencePathStep,
        pathless_type: CheckedTypeIdentity,
        call_arg_reps: []const TypeRepId,
        ret_type: CheckedTypeIdentity,
    ) Allocator.Error!TypeRepId {
        if (path.len == 0) {
            return self.plan.repForSourceType(pathless_type) orelse
                boxyPlanInvariant("pathless worker evidence type was not analyzed");
        }
        const start = switch (path[0].stepKind()) {
            .fn_arg => blk: {
                if (path[0].data >= call_arg_reps.len) {
                    boxyPlanInvariant("worker evidence representation path function argument exceeded arity");
                }
                break :blk call_arg_reps[path[0].data];
            },
            .fn_ret => self.plan.repForSourceType(ret_type) orelse
                boxyPlanInvariant("worker evidence representation path result type was not analyzed"),
            .alias_arg,
            .alias_backing,
            .nominal_arg,
            .nominal_backing,
            .tuple_elem,
            .record_field,
            .tag_payload_tag,
            .tag_payload_index,
            => boxyPlanInvariant("worker evidence representation path did not begin at its callable boundary"),
        };
        return try self.walkEvidenceRepPath(path_view, start, path[1..]);
    }

    fn walkEvidenceRepPath(
        self: *Builder,
        path_view: ModuleView,
        start: TypeRepId,
        path: []const static_dispatch.EvidencePathStep,
    ) Allocator.Error!TypeRepId {
        var substitutions = CallDescriptorRepSubstitutionMap{};
        defer substitutions.deinit(self.allocator);

        var current = start;
        var path_index: usize = 0;
        while (path_index < path.len) {
            const path_step = path[path_index];
            const current_rep = self.plan.representations.items[@intFromEnum(current)];
            const children = self.plan.childSlice(current_rep.children);
            if (current_rep.kind == .nominal) {
                for (self.plan.nominalBackingArgSubstitutionSlice(current_rep.nominal_backing_arg_substitutions)) |substitution| {
                    if (substitution.formal_rep == substitution.actual_rep) continue;
                    try substitutions.put(self.allocator, substitution.formal_rep, substitution.actual_rep);
                }
            }
            const selected = switch (path_step.stepKind()) {
                .fn_arg => for (children) |child| {
                    if (child.role == .function_arg and child.role.function_arg == path_step.data) break child.rep;
                } else boxyPlanInvariant("worker evidence representation path expected a function argument"),
                .fn_ret => for (children) |child| {
                    if (child.role == .function_ret) break child.rep;
                } else boxyPlanInvariant("worker evidence representation path expected a function return"),
                .alias_arg => for (children) |child| {
                    if (child.role == .alias_arg and child.role.alias_arg == path_step.data) break child.rep;
                } else boxyPlanInvariant("worker evidence representation path expected an alias argument"),
                .alias_backing => for (children) |child| {
                    if (child.role == .alias_backing) break child.rep;
                } else boxyPlanInvariant("worker evidence representation path expected an alias backing"),
                .nominal_arg => self.nominalBackingArgActualRep(current, path_step.data) orelse switch (current_rep.kind) {
                    .list => if (path_step.data == 0)
                        requiredSingleChildOf(&self.plan, current, .list_elem).rep
                    else
                        boxyPlanInvariant("worker evidence representation path list argument exceeded arity"),
                    .box => if (path_step.data == 0)
                        requiredSingleChildOf(&self.plan, current, .box_payload).rep
                    else
                        boxyPlanInvariant("worker evidence representation path box argument exceeded arity"),
                    .in_progress,
                    .dynamic,
                    .primitive,
                    .bool_tag_union,
                    .erased_callable,
                    .alias,
                    .record,
                    .record_unbound,
                    .tuple,
                    .nominal,
                    .generated_field,
                    .generated_field_names,
                    .generated_tag_union_spec,
                    .empty_record,
                    .tag_union,
                    .empty_tag_union,
                    => for (children) |child| {
                        if (child.role == .nominal_arg and child.role.nominal_arg == path_step.data) break child.rep;
                    } else boxyPlanInvariant("worker evidence representation path expected a nominal argument"),
                },
                .nominal_backing => for (children) |child| {
                    if (child.role == .nominal_backing) break child.rep;
                } else boxyPlanInvariant("worker evidence representation path expected a nominal backing"),
                .tuple_elem => for (children) |child| {
                    if (child.role == .tuple_elem and child.role.tuple_elem == path_step.data) break child.rep;
                } else boxyPlanInvariant("worker evidence representation path expected a tuple element"),
                .record_field => blk: {
                    const path_name: RecordFieldLabelId = @enumFromInt(path_step.data);
                    for (children) |child| {
                        if (child.role != .record_field) continue;
                        const child_view = self.moduleForId(child.source_type.module);
                        if (recordFieldNameMatches(moduleNamesOf(path_view), path_name, moduleNamesOf(child_view), child.role.record_field)) {
                            break :blk child.rep;
                        }
                    }
                    boxyPlanInvariant("worker evidence representation path field was absent from the checked record");
                },
                .tag_payload_tag => blk: {
                    if (path_index + 1 >= path.len or path[path_index + 1].stepKind() != .tag_payload_index) {
                        boxyPlanInvariant("worker evidence representation tag path had no payload index");
                    }
                    const path_tag: TagLabelId = @enumFromInt(path_step.data);
                    const payload_index = path[path_index + 1].data;
                    for (children) |child| {
                        if (child.role != .tag_payload) continue;
                        const payload = child.role.tag_payload;
                        const child_view = self.moduleForId(child.source_type.module);
                        if (payload.index == payload_index and
                            tagLabelNameMatches(moduleNamesOf(path_view), path_tag, moduleNamesOf(child_view), payload.tag))
                        {
                            path_index += 1;
                            break :blk child.rep;
                        }
                    }
                    boxyPlanInvariant("worker evidence representation path tag payload was absent from the checked union");
                },
                .tag_payload_index => boxyPlanInvariant("worker evidence representation payload index had no preceding tag"),
            };
            current = selected;
            var substitution_depth: u16 = 0;
            while (substitutions.get(current)) |substituted| {
                if (substitution_depth == 1024) {
                    boxyPlanInvariant("worker evidence representation substitution chain exceeded its limit");
                }
                substitution_depth += 1;
                current = substituted;
            }
            path_index += 1;
        }
        return current;
    }

    fn evidencePathSourceArgIndex(path: []const static_dispatch.EvidencePathStep, arg_count: usize) ?u32 {
        for (path) |path_step| switch (path_step.stepKind()) {
            .fn_arg => {
                if (path_step.data >= arg_count) {
                    boxyPlanInvariant("worker evidence path function argument exceeded call arity");
                }
                return path_step.data;
            },
            .fn_ret => return null,
            .alias_arg,
            .alias_backing,
            .nominal_arg,
            .nominal_backing,
            .tuple_elem,
            .record_field,
            .tag_payload_tag,
            .tag_payload_index,
            => {},
        };
        return null;
    }

    fn checkedTypeAtEvidenceCallPath(
        self: *Builder,
        path_view: ModuleView,
        path: []const static_dispatch.EvidencePathStep,
        call_arg_types: []const CheckedTypeIdentity,
        ret_type: CheckedTypeIdentity,
    ) Allocator.Error!CheckedTypeIdentity {
        if (path.len == 0) {
            boxyPlanInvariant("compiler-generated call could not resolve a pathless worker evidence parameter");
        }
        const start = switch (path[0].stepKind()) {
            .fn_arg => blk: {
                if (path[0].data >= call_arg_types.len) {
                    boxyPlanInvariant("worker evidence path function argument exceeded call arity");
                }
                break :blk call_arg_types[path[0].data];
            },
            .fn_ret => ret_type,
            .alias_arg,
            .alias_backing,
            .nominal_arg,
            .nominal_backing,
            .tuple_elem,
            .record_field,
            .tag_payload_tag,
            .tag_payload_index,
            => boxyPlanInvariant("worker evidence path did not begin at its callable boundary"),
        };
        return try self.walkCheckedEvidencePath(path_view, start, path[1..]);
    }

    fn walkCheckedEvidencePath(
        self: *Builder,
        path_view: ModuleView,
        start: CheckedTypeIdentity,
        path: []const static_dispatch.EvidencePathStep,
    ) Allocator.Error!CheckedTypeIdentity {
        var current = start;
        var path_index: usize = 0;
        while (path_index < path.len) {
            const path_step = path[path_index];
            const view = self.moduleForId(current.module);
            current = switch (path_step.stepKind()) {
                .fn_arg => blk: {
                    const payload = view.checked_types.payload(current.ty);
                    if (payload != .function) boxyPlanInvariant("worker evidence path expected a function argument");
                    if (path_step.data >= payload.function.args.len) {
                        boxyPlanInvariant("worker evidence path nested function argument exceeded arity");
                    }
                    break :blk typeRef(view, payload.function.args[path_step.data]);
                },
                .fn_ret => blk: {
                    const payload = view.checked_types.payload(current.ty);
                    if (payload != .function) boxyPlanInvariant("worker evidence path expected a function return");
                    break :blk typeRef(view, payload.function.ret);
                },
                .alias_arg => blk: {
                    const payload = view.checked_types.payload(current.ty);
                    if (payload != .alias) boxyPlanInvariant("worker evidence path expected an alias argument");
                    if (path_step.data >= payload.alias.args.len) {
                        boxyPlanInvariant("worker evidence path alias argument exceeded arity");
                    }
                    break :blk typeRef(view, payload.alias.args[path_step.data]);
                },
                .alias_backing => blk: {
                    const payload = view.checked_types.payload(current.ty);
                    if (payload != .alias) boxyPlanInvariant("worker evidence path expected an alias backing");
                    break :blk typeRef(view, payload.alias.backing);
                },
                .nominal_arg => blk: {
                    const payload = view.checked_types.payload(current.ty);
                    if (payload != .nominal) boxyPlanInvariant("worker evidence path expected a nominal argument");
                    if (path_step.data >= payload.nominal.args.len) {
                        boxyPlanInvariant("worker evidence path nominal argument exceeded arity");
                    }
                    break :blk typeRef(view, payload.nominal.args[path_step.data]);
                },
                .nominal_backing => blk: {
                    const payload = view.checked_types.payload(current.ty);
                    if (payload != .nominal) boxyPlanInvariant("worker evidence path expected a nominal backing");
                    const backing = try self.nominalBackingSource(view, payload.nominal);
                    break :blk typeRef(backing.view, backing.ty);
                },
                .tuple_elem => blk: {
                    const payload = view.checked_types.payload(current.ty);
                    if (payload != .tuple) boxyPlanInvariant("worker evidence path expected a tuple element");
                    if (path_step.data >= payload.tuple.len) {
                        boxyPlanInvariant("worker evidence path tuple element exceeded arity");
                    }
                    break :blk typeRef(view, payload.tuple[path_step.data]);
                },
                .record_field => blk: {
                    const payload = view.checked_types.payload(current.ty);
                    if (payload == .record) {
                        break :blk self.checkedRecordFieldAtEvidencePath(path_view, @enumFromInt(path_step.data), view, payload.record.fields);
                    }
                    if (payload == .record_unbound) {
                        break :blk self.checkedRecordFieldAtEvidencePath(path_view, @enumFromInt(path_step.data), view, payload.record_unbound);
                    }
                    boxyPlanInvariant("worker evidence path expected a record field");
                },
                .tag_payload_tag => blk: {
                    if (path_index + 1 >= path.len or path[path_index + 1].stepKind() != .tag_payload_index) {
                        boxyPlanInvariant("worker evidence tag path had no payload index");
                    }
                    const payload_index = path[path_index + 1].data;
                    const payload = view.checked_types.payload(current.ty);
                    if (payload != .tag_union) boxyPlanInvariant("worker evidence path expected a tag union payload");
                    const tag_union = payload.tag_union;
                    const path_tag: TagLabelId = @enumFromInt(path_step.data);
                    for (tag_union.tags) |tag| {
                        if (!tagLabelNameMatches(moduleNamesOf(path_view), path_tag, moduleNamesOf(view), tag.name)) continue;
                        const args = tag.argsSlice(view.checked_types);
                        if (payload_index >= args.len) {
                            boxyPlanInvariant("worker evidence path tag payload exceeded arity");
                        }
                        path_index += 1;
                        break :blk typeRef(view, args[payload_index]);
                    }
                    boxyPlanInvariant("worker evidence path tag was absent from the checked union");
                },
                .tag_payload_index => boxyPlanInvariant("worker evidence payload index had no preceding tag"),
            };
            path_index += 1;
        }
        return current;
    }

    fn checkedRecordFieldAtEvidencePath(
        _: *Builder,
        path_view: ModuleView,
        path_name: RecordFieldLabelId,
        record_view: ModuleView,
        fields: []const checked.CheckedRecordField,
    ) CheckedTypeIdentity {
        for (fields) |field| {
            if (recordFieldNameMatches(moduleNamesOf(path_view), path_name, moduleNamesOf(record_view), field.name)) {
                return typeRef(record_view, field.ty);
            }
        }
        boxyPlanInvariant("worker evidence path field was absent from the checked record");
    }

    fn materializeWorkerCallHiddenDictionaryArgs(
        self: *Builder,
        worker_id: WorkerPlanId,
        caller_id: ?WorkerPlanId,
        arg_types: []const CheckedTypeIdentity,
        ret_type: CheckedTypeIdentity,
    ) Allocator.Error!Span {
        return try self.materializeWorkerCallHiddenDictionaryArgsWithEvidence(
            worker_id,
            caller_id,
            arg_types,
            ret_type,
            null,
            null,
        );
    }

    fn materializeWorkerCallHiddenDictionaryArgsWithEvidence(
        self: *Builder,
        worker_id: WorkerPlanId,
        caller_id: ?WorkerPlanId,
        arg_types: []const CheckedTypeIdentity,
        ret_type: CheckedTypeIdentity,
        evidence_view: ?ModuleView,
        evidence: ?[]const static_dispatch.CheckedEvidence,
    ) Allocator.Error!Span {
        const worker = self.plan.workers.items[@intFromEnum(worker_id)];
        const params = self.plan.hiddenDictionaryParamSlice(worker.hidden_dicts);
        if (params.len == 0) return .{};

        const worker_function = (self.repQuery().functionChildren(worker.rep)) orelse
            boxyPlanInvariant("boxy worker call target with hidden dictionaries was not a function worker");
        if (worker_function.arg_count != arg_types.len) {
            boxyPlanInvariant("boxy worker call hidden dictionary mapping saw mismatched function arity");
        }

        var substitutions = CallDictionaryRepSubstitutionMap{};
        defer substitutions.deinit(self.allocator);
        var seen_substitutions = std.AutoHashMap(u64, void).init(self.allocator);
        defer seen_substitutions.deinit();

        const definition_type = self.workerCheckedTypeForSource(worker.source, worker.checked_type);
        if (!typeRefEql(definition_type, worker.checked_type)) {
            const definition_rep = self.plan.repForSourceType(definition_type) orelse
                boxyPlanInvariant("boxy specialized worker definition type was not analyzed");
            const definition_function = (self.repQuery().functionChildren(definition_rep)) orelse
                boxyPlanInvariant("boxy specialized worker definition was not callable");
            if (definition_function.arg_count != worker_function.arg_count) {
                boxyPlanInvariant("boxy specialized worker definition arity disagreed with worker boundary");
            }
            const definition_children = self.plan.childSlice(
                self.plan.representations.items[@intFromEnum(definition_function.rep)].children,
            );
            const worker_children = self.plan.childSlice(
                self.plan.representations.items[@intFromEnum(worker_function.rep)].children,
            );
            for (
                definition_children[definition_function.args_start..][0..definition_function.arg_count],
                worker_children[worker_function.args_start..][0..worker_function.arg_count],
            ) |definition_arg, worker_arg| {
                try self.collectCallDictionaryRepSubstitutions(
                    definition_arg.rep,
                    worker_arg.rep,
                    &substitutions,
                    &seen_substitutions,
                );
            }
            try self.collectCallDictionaryRepSubstitutions(
                definition_function.ret,
                worker_function.ret,
                &substitutions,
                &seen_substitutions,
            );
        }

        const fn_children_span = self.plan.representations.items[@intFromEnum(worker_function.rep)].children;
        var arg_index: usize = 0;
        while (arg_index < worker_function.arg_count) : (arg_index += 1) {
            const worker_child = self.plan.children.items[fn_children_span.start + worker_function.args_start + arg_index];
            const arg_rep = self.plan.repForSourceType(arg_types[arg_index]) orelse
                boxyPlanInvariant("boxy worker call argument type was not analyzed");
            try self.collectCallDictionaryRepSubstitutions(worker_child.rep, arg_rep, &substitutions, &seen_substitutions);
        }
        const ret_rep = self.plan.repForSourceType(ret_type) orelse
            boxyPlanInvariant("boxy worker call result type was not analyzed");
        try self.collectCallDictionaryRepSubstitutions(worker_function.ret, ret_rep, &substitutions, &seen_substitutions);

        const body_param_start: usize = @intCast(worker.body_hidden_dicts.start - worker.hidden_dicts.start);
        if (body_param_start > params.len) {
            boxyPlanInvariant("boxy worker body dictionary span was outside its hidden dictionary span");
        }

        var pending = std.ArrayList(DirectCallHiddenDictionaryArg).empty;
        defer pending.deinit(self.allocator);
        var next_evidence: usize = 0;
        for (params, 0..) |param, param_index| {
            const evidence_source = try self.evidenceDictionarySource(
                evidence_view,
                evidence,
                &next_evidence,
                param.dictionaries,
            );
            const substituted_rep = substitutions.get(param.rep);
            if (param_index < body_param_start and substituted_rep == null and evidence_source.rep == null) {
                boxyPlanInvariant("boxy callable dictionary parameter had no checked call substitution or dispatch evidence");
            }
            const source_rep = self.repQuery().dictionaryArgumentIdentityRep(evidence_source.rep orelse substituted_rep orelse param.rep);
            const source_rep_dictionaries = self.plan.representations.items[@intFromEnum(source_rep)].dictionaries;
            const bound_dictionaries = if (substituted_rep == null and evidence_source.rep == null)
                param.dictionaries
            else
                source_rep_dictionaries;
            const source_is_bound = evidence_source.rep == null and caller_id != null and
                self.workerBindsDictionarySpan(caller_id.?, bound_dictionaries);
            const source_is_defaulted = evidence_source.rep == null and substituted_rep == null and
                self.defaultedDictionaryOwner(source_rep) != null;
            if (!source_is_bound and !source_is_defaulted and substituted_rep == null and evidence_source.rep == null) {
                boxyPlanInvariant("boxy callable body dictionary had neither a bound caller dictionary nor concrete checked use-site evidence");
            }
            var planned_method_evidence = evidence_source.method_evidence;
            if (!source_is_bound) {
                planned_method_evidence = try self.ensureStaticDictionaryWorkers(
                    source_rep,
                    param.dictionaries,
                    evidence_source.method_evidence,
                );
            }
            try pending.append(self.allocator, .{
                .worker_dictionaries = param.dictionaries,
                .source_type = self.plan.representations.items[@intFromEnum(source_rep)].source_type,
                .rep = source_rep,
                .method_evidence = planned_method_evidence,
                .source = if (source_is_bound)
                    .{ .bound_dictionaries = bound_dictionaries }
                else
                    .{ .static_rep = source_rep },
            });
        }

        if (pending.items.len != params.len) {
            boxyPlanInvariant("boxy worker call hidden dictionary mapping did not cover every worker dictionary param");
        }
        if (evidence) |entries| {
            if (next_evidence > entries.len) {
                boxyPlanInvariant("boxy callable use exhausted its checked dictionary evidence");
            }
        }

        const start: u32 = @intCast(self.plan.direct_call_hidden_dict_args.items.len);
        try self.plan.direct_call_hidden_dict_args.appendSlice(self.allocator, pending.items);
        return .{
            .start = start,
            .len = @intCast(pending.items.len),
        };
    }

    fn collectHiddenDescriptorsForRep(
        self: *Builder,
        rep_id: TypeRepId,
        pending: *std.ArrayList(HiddenDescriptorParam),
        seen_reps: *collections.DenseMap(TypeRepId, void),
        seen_descs: *collections.DenseMap(DescriptorRequirementId, void),
    ) Allocator.Error!void {
        const rep_entry = try seen_reps.getOrPut(rep_id);
        if (rep_entry.found_existing) return;

        const rep = self.plan.representations.items[@intFromEnum(rep_id)];
        if (rep.descriptor) |desc| {
            const identity_rep = self.repQuery().descriptorArgumentIdentityRep(rep_id);
            const identity_desc = self.plan.representations.items[@intFromEnum(identity_rep)].descriptor orelse desc;
            const desc_entry = try seen_descs.getOrPut(identity_desc);
            if (!desc_entry.found_existing) {
                try pending.append(self.allocator, .{
                    .source_type = rep.source_type,
                    .rep = rep_id,
                    .desc = desc,
                });
            }
        }

        for (self.plan.childSlice(rep.children)) |child| {
            try self.collectHiddenDescriptorsForRep(child.rep, pending, seen_reps, seen_descs);
        }
    }

    fn collectRuntimeHiddenDescriptorsForRep(
        self: *Builder,
        rep_id: TypeRepId,
        pending: *std.ArrayList(HiddenDescriptorParam),
        seen_reps: *collections.DenseMap(TypeRepId, void),
        seen_descs: *collections.DenseMap(DescriptorRequirementId, void),
    ) Allocator.Error!void {
        const rep_entry = try seen_reps.getOrPut(rep_id);
        if (rep_entry.found_existing) return;

        const rep = self.plan.representations.items[@intFromEnum(rep_id)];
        if (rep.descriptor) |desc| {
            const identity_rep = self.repQuery().descriptorArgumentIdentityRep(rep_id);
            const identity_desc = self.plan.representations.items[@intFromEnum(identity_rep)].descriptor orelse desc;
            const desc_entry = try seen_descs.getOrPut(identity_desc);
            if (!desc_entry.found_existing) {
                try pending.append(self.allocator, .{
                    .source_type = rep.source_type,
                    .rep = rep_id,
                    .desc = desc,
                });
            }
        }

        if (rep.kind == .erased_callable) return;
        for (self.plan.childSlice(rep.children)) |child| {
            if (!childCarriesRuntimeDescriptor(child.role)) continue;
            try self.collectRuntimeHiddenDescriptorsForRep(child.rep, pending, seen_reps, seen_descs);
        }
    }

    fn collectHiddenDictionariesForRep(
        self: *Builder,
        rep_id: TypeRepId,
        pending: *std.ArrayList(HiddenDictionaryParam),
        seen_reps: *collections.DenseMap(TypeRepId, void),
    ) Allocator.Error!void {
        const rep_entry = try seen_reps.getOrPut(rep_id);
        if (rep_entry.found_existing) return;

        const rep = self.plan.representations.items[@intFromEnum(rep_id)];
        if (rep.dictionaries.len != 0) {
            try pending.append(self.allocator, .{
                .source_type = rep.source_type,
                .rep = rep_id,
                .dictionaries = rep.dictionaries,
            });
        }

        var child_index: usize = 0;
        while (self.plan.dictionaryChildAt(rep_id, child_index)) |child| : (child_index += 1) {
            try self.collectHiddenDictionariesForRep(child.rep, pending, seen_reps);
        }
    }

    fn collectCallHiddenDescriptorArgs(
        self: *Builder,
        worker_rep_id: TypeRepId,
        call_rep_id: TypeRepId,
        call_value_rep: TypeRepId,
        source_value_rep: TypeRepId,
        source_arg_index: ?u32,
        params: []const HiddenDescriptorParam,
        next_param: *usize,
        pending: *std.ArrayList(DirectCallHiddenDescriptorArg),
        seen_reps: *collections.DenseMap(TypeRepId, void),
        seen_descriptor_reps: *collections.DenseMap(TypeRepId, void),
        substitutions: *CallDescriptorRepSubstitutionMap,
        runtime_value_only: bool,
    ) Allocator.Error!void {
        const rep_entry = try seen_reps.getOrPut(worker_rep_id);
        if (rep_entry.found_existing) return;

        const substitution_scope = substitutions.entries.items.len;
        defer substitutions.entries.shrinkRetainingCapacity(substitution_scope);
        const inherited_call_rep_id = substitutions.get(worker_rep_id) orelse call_rep_id;
        try self.recordCallDescriptorWrapperSubstitutions(worker_rep_id, inherited_call_rep_id, substitutions);
        const aligned_call_rep_id = substitutions.get(worker_rep_id) orelse inherited_call_rep_id;

        const worker_rep = self.plan.representations.items[@intFromEnum(worker_rep_id)];
        const call_rep = self.plan.representations.items[@intFromEnum(aligned_call_rep_id)];

        if (worker_rep.descriptor) |worker_desc| {
            const worker_identity = self.repQuery().descriptorArgumentIdentityRep(worker_rep_id);
            const identity_entry = try seen_descriptor_reps.getOrPut(worker_identity);
            if (!identity_entry.found_existing) {
                if (next_param.* >= params.len or params[next_param.*].desc != worker_desc) {
                    boxyPlanInvariant("boxy direct call hidden descriptor order disagreed with worker descriptor params");
                }
                next_param.* += 1;
                const operand_nominal_actual = (try self.nominalBackingActualForCallRep(
                    call_value_rep,
                    source_value_rep,
                    aligned_call_rep_id,
                )) orelse try self.nominalBackingActualForFormal(source_value_rep, worker_rep_id);
                const desc_arg_rep_id = self.repQuery().descriptorArgumentIdentityRep(
                    operand_nominal_actual orelse aligned_call_rep_id,
                );
                const desc_arg_rep = self.plan.representations.items[@intFromEnum(desc_arg_rep_id)];
                try pending.append(self.allocator, .{
                    .worker_desc = worker_desc,
                    .worker_rep = worker_rep_id,
                    .source_type = desc_arg_rep.source_type,
                    .rep = desc_arg_rep_id,
                    .source_arg_index = source_arg_index,
                    .source_value_rep = source_value_rep,
                });
            }
        }

        if (runtime_value_only and worker_rep.kind == .erased_callable) return;

        if (worker_rep.children.len == 0) return;

        // The recursion can analyze new types, growing the children pool and
        // invalidating any held slice; children are re-read by index on every
        // iteration.
        if (call_rep.kind == .empty_tag_union) {
            var child_index: usize = 0;
            while (child_index < worker_rep.children.len) : (child_index += 1) {
                const worker_child = self.plan.children.items[worker_rep.children.start + child_index];
                if (runtime_value_only and !childCarriesRuntimeDescriptor(worker_child.role)) continue;
                if (!try self.repQuery().repSubtreeHasDescriptor(worker_child.rep)) continue;
                try self.collectCallHiddenDescriptorArgs(worker_child.rep, aligned_call_rep_id, call_value_rep, source_value_rep, source_arg_index, params, next_param, pending, seen_reps, seen_descriptor_reps, substitutions, runtime_value_only);
            }
            return;
        }

        var child_index: usize = 0;
        while (child_index < worker_rep.children.len) : (child_index += 1) {
            const worker_child = self.plan.children.items[worker_rep.children.start + child_index];
            const call_children = self.plan.childSlice(call_rep.children);
            if (runtime_value_only and !childCarriesRuntimeDescriptor(worker_child.role)) continue;
            if (!try self.repQuery().repSubtreeHasDescriptor(worker_child.rep)) continue;
            // A generic argument reachable through an unwrapped sibling (e.g. an
            // alias's arg that also appears inside its backing) contributes its
            // descriptor once, via that sibling; skip the duplicate here to
            // mirror the worker param collection's per-rep dedup.
            if (seen_reps.contains(worker_child.rep)) continue;
            if (self.rowInstantiationTarget(worker_rep_id, aligned_call_rep_id, worker_child)) |row_target| {
                try self.collectCallHiddenDescriptorArgs(worker_child.rep, row_target, call_value_rep, source_value_rep, source_arg_index, params, next_param, pending, seen_reps, seen_descriptor_reps, substitutions, runtime_value_only);
                continue;
            }
            if (self.namedQuery().findMatchingChildByRole(call_children, worker_child)) |call_child| {
                try self.collectCallHiddenDescriptorArgs(worker_child.rep, call_child.rep, call_value_rep, source_value_rep, source_arg_index, params, next_param, pending, seen_reps, seen_descriptor_reps, substitutions, runtime_value_only);
                continue;
            }
            if (self.repQuery().structuralWrapperBackingRep(aligned_call_rep_id)) |call_backing| {
                const backing_children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(call_backing)].children);
                if (self.namedQuery().findMatchingChildByRole(backing_children, worker_child)) |call_child| {
                    try self.collectCallHiddenDescriptorArgs(worker_child.rep, call_child.rep, call_value_rep, source_value_rep, source_arg_index, params, next_param, pending, seen_reps, seen_descriptor_reps, substitutions, runtime_value_only);
                    continue;
                }
            }
            if (try self.namedQuery().findMatchingTagPayloadInRowExtension(call_children, worker_child)) |call_child| {
                try self.collectCallHiddenDescriptorArgs(worker_child.rep, call_child.rep, call_value_rep, source_value_rep, source_arg_index, params, next_param, pending, seen_reps, seen_descriptor_reps, substitutions, runtime_value_only);
                continue;
            }
            if (try self.repQuery().findMatchingChildBySourceType(call_children, worker_child)) |call_child| {
                try self.collectCallHiddenDescriptorArgs(worker_child.rep, call_child.rep, call_value_rep, source_value_rep, source_arg_index, params, next_param, pending, seen_reps, seen_descriptor_reps, substitutions, runtime_value_only);
                continue;
            }
            if (self.workerPresenceSlotPayloadMatchesUnwrappedCallRep(worker_rep_id, aligned_call_rep_id, worker_child)) {
                try self.collectCallHiddenDescriptorArgs(worker_child.rep, aligned_call_rep_id, call_value_rep, source_value_rep, source_arg_index, params, next_param, pending, seen_reps, seen_descriptor_reps, substitutions, runtime_value_only);
                continue;
            }
            if (try self.repQuery().workerChildCanMatchUnwrappedCallRep(worker_rep_id, worker_child)) {
                try self.collectCallHiddenDescriptorArgs(worker_child.rep, aligned_call_rep_id, call_value_rep, source_value_rep, source_arg_index, params, next_param, pending, seen_reps, seen_descriptor_reps, substitutions, runtime_value_only);
                continue;
            }
            if (worker_child.role == .tag_ext and call_children.len == 0 and call_rep.descriptor != null) {
                try self.collectCallHiddenDescriptorArgs(worker_child.rep, aligned_call_rep_id, call_value_rep, source_value_rep, source_arg_index, params, next_param, pending, seen_reps, seen_descriptor_reps, substitutions, runtime_value_only);
                continue;
            }
            if (call_rep.kind == .dynamic and call_rep.children.len == 0 and call_rep.descriptor != null) {
                try self.collectCallHiddenDescriptorArgs(worker_child.rep, aligned_call_rep_id, call_value_rep, source_value_rep, source_arg_index, params, next_param, pending, seen_reps, seen_descriptor_reps, substitutions, runtime_value_only);
                continue;
            }
            boxyPlanInvariant("boxy direct call hidden descriptor mapping saw mismatched child roles");
        }
    }

    const CallDescriptorRepSubstitution = struct {
        worker_rep: TypeRepId,
        call_rep: TypeRepId,
    };

    const CallDescriptorRepSubstitutionMap = struct {
        entries: std.ArrayList(CallDescriptorRepSubstitution) = .empty,

        fn deinit(self: *CallDescriptorRepSubstitutionMap, allocator: Allocator) void {
            self.entries.deinit(allocator);
        }

        fn get(self: *const CallDescriptorRepSubstitutionMap, worker_rep: TypeRepId) ?TypeRepId {
            for (self.entries.items) |entry| {
                if (entry.worker_rep == worker_rep) return entry.call_rep;
            }
            return null;
        }

        fn put(
            self: *CallDescriptorRepSubstitutionMap,
            allocator: Allocator,
            worker_rep: TypeRepId,
            call_rep: TypeRepId,
        ) Allocator.Error!void {
            for (self.entries.items) |entry| {
                if (entry.worker_rep != worker_rep) continue;
                if (entry.call_rep != call_rep) {
                    boxyPlanInvariant("one worker descriptor representation mapped to two call representations");
                }
                return;
            }
            try self.entries.append(allocator, .{ .worker_rep = worker_rep, .call_rep = call_rep });
        }
    };

    fn nominalBackingArgActualRep(
        self: *const Builder,
        nominal_rep_id: TypeRepId,
        arg_index: u32,
    ) ?TypeRepId {
        const nominal_rep = self.plan.representations.items[@intFromEnum(nominal_rep_id)];
        var found: ?TypeRepId = null;
        for (self.plan.nominalBackingArgSubstitutionSlice(nominal_rep.nominal_backing_arg_substitutions)) |substitution| {
            if (substitution.arg_index != arg_index) continue;
            if (found != null) {
                boxyPlanInvariant("checked nominal representation had duplicate backing argument substitutions");
            }
            found = substitution.actual_rep;
        }
        return found;
    }

    fn nominalBackingActualForCallRep(
        self: *Builder,
        call_root_rep: TypeRepId,
        operand_root_rep: TypeRepId,
        target_call_rep: TypeRepId,
    ) Allocator.Error!?TypeRepId {
        var seen_pairs = std.AutoHashMap(u64, void).init(self.allocator);
        defer seen_pairs.deinit();
        var found: ?TypeRepId = null;
        try self.collectNominalBackingActualForCallRep(
            call_root_rep,
            operand_root_rep,
            target_call_rep,
            &found,
            &seen_pairs,
        );
        return found;
    }

    fn collectNominalBackingActualForCallRep(
        self: *Builder,
        call_rep_id: TypeRepId,
        operand_rep_id: TypeRepId,
        target_call_rep: TypeRepId,
        found: *?TypeRepId,
        seen_pairs: *std.AutoHashMap(u64, void),
    ) Allocator.Error!void {
        const pair_key = (@as(u64, @intFromEnum(call_rep_id)) << 32) |
            @as(u64, @intFromEnum(operand_rep_id));
        const pair_entry = try seen_pairs.getOrPut(pair_key);
        if (pair_entry.found_existing) return;

        if (call_rep_id == target_call_rep) {
            if (found.*) |existing| {
                if (existing != operand_rep_id) {
                    boxyPlanInvariant("one call representation corresponded to two operand representations");
                }
            } else {
                found.* = operand_rep_id;
            }
            return;
        }

        const call_rep = self.plan.representations.items[@intFromEnum(call_rep_id)];
        const operand_rep = self.plan.representations.items[@intFromEnum(operand_rep_id)];
        if (call_rep.kind == .nominal and operand_rep.kind == .nominal) {
            for (self.plan.nominalBackingArgSubstitutionSlice(call_rep.nominal_backing_arg_substitutions)) |call_substitution| {
                const operand_actual = self.nominalBackingArgActualRep(operand_rep_id, call_substitution.arg_index) orelse
                    boxyPlanInvariant("call operand nominal was missing a checked backing argument substitution");
                try self.collectNominalBackingActualForCallRep(
                    call_substitution.actual_rep,
                    operand_actual,
                    target_call_rep,
                    found,
                    seen_pairs,
                );
            }
        }

        const operand_children = self.plan.childSlice(operand_rep.children);
        for (self.plan.childSlice(call_rep.children)) |call_child| {
            const operand_child = self.namedQuery().findMatchingChildByRole(operand_children, call_child) orelse continue;
            try self.collectNominalBackingActualForCallRep(
                call_child.rep,
                operand_child.rep,
                target_call_rep,
                found,
                seen_pairs,
            );
        }
    }

    fn nominalBackingActualForFormal(
        self: *Builder,
        root_rep: TypeRepId,
        formal_rep: TypeRepId,
    ) Allocator.Error!?TypeRepId {
        var seen = collections.DenseMap(TypeRepId, void).init(self.allocator);
        defer seen.deinit();
        var found: ?TypeRepId = null;
        try self.collectNominalBackingActualForFormal(root_rep, formal_rep, &found, &seen);
        return found;
    }

    fn collectNominalBackingActualForFormal(
        self: *Builder,
        rep_id: TypeRepId,
        formal_rep: TypeRepId,
        found: *?TypeRepId,
        seen: *collections.DenseMap(TypeRepId, void),
    ) Allocator.Error!void {
        const entry = try seen.getOrPut(rep_id);
        if (entry.found_existing) return;

        const rep = self.plan.representations.items[@intFromEnum(rep_id)];
        for (self.plan.nominalBackingArgSubstitutionSlice(rep.nominal_backing_arg_substitutions)) |substitution| {
            if (substitution.formal_rep == formal_rep and substitution.actual_rep != formal_rep) {
                if (found.*) |existing| {
                    if (existing != substitution.actual_rep) {
                        boxyPlanInvariant("one call operand assigned a nominal backing formal to two exact reps");
                    }
                } else {
                    found.* = substitution.actual_rep;
                }
            }
            try self.collectNominalBackingActualForFormal(substitution.actual_rep, formal_rep, found, seen);
        }
        for (self.plan.childSlice(rep.children)) |child| {
            try self.collectNominalBackingActualForFormal(child.rep, formal_rep, found, seen);
        }
        for (self.plan.tagVariantSlice(rep.tag_variants)) |variant| {
            for (self.plan.childSlice(variant.payloads)) |payload| {
                try self.collectNominalBackingActualForFormal(payload.rep, formal_rep, found, seen);
            }
        }
    }

    fn recordCallDescriptorWrapperSubstitutions(
        self: *Builder,
        worker_rep_id: TypeRepId,
        call_rep_id: TypeRepId,
        substitutions: *CallDescriptorRepSubstitutionMap,
    ) Allocator.Error!void {
        const worker_rep = self.plan.representations.items[@intFromEnum(worker_rep_id)];
        const call_rep = self.plan.representations.items[@intFromEnum(call_rep_id)];
        const roles_match = (worker_rep.kind == .alias and call_rep.kind == .alias) or
            (worker_rep.kind == .nominal and call_rep.kind == .nominal);
        if (!roles_match) return;

        if (worker_rep.kind == .nominal) {
            for (self.plan.nominalBackingArgSubstitutionSlice(worker_rep.nominal_backing_arg_substitutions)) |backing_substitution| {
                const exact_call_arg_rep = self.nominalBackingArgActualRep(call_rep_id, backing_substitution.arg_index) orelse
                    boxyPlanInvariant("checked nominal call was missing a backing argument substitution");
                if (backing_substitution.formal_rep != exact_call_arg_rep) {
                    try substitutions.put(self.allocator, backing_substitution.formal_rep, exact_call_arg_rep);
                }
            }
        }
        const call_children = self.plan.childSlice(call_rep.children);
        for (self.plan.childSlice(worker_rep.children)) |worker_child| {
            if (worker_child.role != .alias_arg and worker_child.role != .nominal_arg) continue;
            const exact_call_arg_rep = if (worker_child.role == .nominal_arg)
                self.nominalBackingArgActualRep(call_rep_id, worker_child.role.nominal_arg) orelse blk: {
                    const call_child = self.namedQuery().findMatchingChildByRole(call_children, worker_child) orelse
                        boxyPlanInvariant("checked wrapper call was missing a type argument substitution");
                    break :blk call_child.rep;
                }
            else blk: {
                const call_child = self.namedQuery().findMatchingChildByRole(call_children, worker_child) orelse
                    boxyPlanInvariant("checked wrapper call was missing a type argument substitution");
                break :blk call_child.rep;
            };
            if (worker_child.rep == exact_call_arg_rep) continue;
            try substitutions.put(self.allocator, worker_child.rep, exact_call_arg_rep);
        }
    }

    const CallDictionaryContext = struct {
        caller_id: ?WorkerPlanId,
        params: []const HiddenDictionaryParam,
        next_param: *usize,
        evidence_view: ?ModuleView,
        evidence: ?[]const static_dispatch.CheckedEvidence,
        next_evidence: *usize,
        pending: *std.ArrayList(DirectCallHiddenDictionaryArg),
        seen_reps: *collections.DenseMap(TypeRepId, void),
    };

    const CallDictionaryRepSubstitution = struct {
        worker_rep: TypeRepId,
        call_rep: TypeRepId,
    };

    const CallDictionaryRepSubstitutionMap = struct {
        entries: std.ArrayList(CallDictionaryRepSubstitution) = .empty,

        fn deinit(self: *CallDictionaryRepSubstitutionMap, allocator: Allocator) void {
            self.entries.deinit(allocator);
        }

        fn get(self: *const CallDictionaryRepSubstitutionMap, worker_rep: TypeRepId) ?TypeRepId {
            for (self.entries.items) |entry| {
                if (entry.worker_rep == worker_rep) return entry.call_rep;
            }
            return null;
        }

        fn put(
            self: *CallDictionaryRepSubstitutionMap,
            allocator: Allocator,
            worker_rep: TypeRepId,
            call_rep: TypeRepId,
        ) Allocator.Error!void {
            for (self.entries.items) |entry| {
                if (entry.worker_rep != worker_rep) continue;
                if (entry.call_rep != call_rep) {
                    boxyPlanInvariant("one worker dictionary representation mapped to two call representations");
                }
                return;
            }
            try self.entries.append(allocator, .{ .worker_rep = worker_rep, .call_rep = call_rep });
        }
    };

    fn collectCallHiddenDictionaryArgs(
        self: *Builder,
        worker_rep_id: TypeRepId,
        call_rep_id: TypeRepId,
        context: *CallDictionaryContext,
    ) Allocator.Error!void {
        const rep_entry = try context.seen_reps.getOrPut(worker_rep_id);
        if (rep_entry.found_existing) return;

        const worker_rep = self.plan.representations.items[@intFromEnum(worker_rep_id)];
        const call_rep = self.plan.representations.items[@intFromEnum(call_rep_id)];

        if (worker_rep.dictionaries.len != 0) {
            if (context.next_param.* >= context.params.len or !std.meta.eql(context.params[context.next_param.*].dictionaries, worker_rep.dictionaries)) {
                boxyPlanInvariant("boxy direct call hidden dictionary order disagreed with worker dictionary params");
            }
            context.next_param.* += 1;
            var dict_arg_rep_id = self.repQuery().dictionaryArgumentIdentityRep(call_rep_id);
            const evidence_source = try self.callableEvidenceSource(context, worker_rep.dictionaries);
            if (evidence_source.rep) |evidence_rep| {
                dict_arg_rep_id = self.repQuery().dictionaryArgumentIdentityRep(evidence_rep);
            }
            const dict_arg_rep = self.plan.representations.items[@intFromEnum(dict_arg_rep_id)];
            const source_is_bound = context.caller_id != null and self.workerBindsDictionarySpan(context.caller_id.?, dict_arg_rep.dictionaries);
            var planned_method_evidence = evidence_source.method_evidence;
            if (!source_is_bound) {
                planned_method_evidence = try self.ensureStaticDictionaryWorkers(
                    dict_arg_rep_id,
                    worker_rep.dictionaries,
                    evidence_source.method_evidence,
                );
            }
            try context.pending.append(self.allocator, .{
                .worker_dictionaries = worker_rep.dictionaries,
                .source_type = dict_arg_rep.source_type,
                .rep = dict_arg_rep_id,
                .method_evidence = planned_method_evidence,
                .source = if (source_is_bound)
                    .{ .bound_dictionaries = dict_arg_rep.dictionaries }
                else
                    .{ .static_rep = dict_arg_rep_id },
            });
        }

        if (worker_rep.children.len == 0) return;

        // The recursion can analyze new types, growing the children pool and
        // invalidating any held slice; children are re-read by index on every
        // iteration.
        if (call_rep.kind == .empty_tag_union) {
            var child_index: usize = 0;
            while (self.plan.dictionaryChildAt(worker_rep_id, child_index)) |worker_child| : (child_index += 1) {
                if (!try self.repQuery().repSubtreeHasDictionary(worker_child.rep)) continue;
                try self.collectCallHiddenDictionaryArgs(worker_child.rep, call_rep_id, context);
            }
            return;
        }

        var child_index: usize = 0;
        while (self.plan.dictionaryChildAt(worker_rep_id, child_index)) |worker_child| : (child_index += 1) {
            const call_children = self.plan.childSlice(call_rep.children);
            if (!try self.repQuery().repSubtreeHasDictionary(worker_child.rep)) continue;
            // A generic argument reachable through an unwrapped sibling (e.g. an
            // alias's arg that also appears inside its backing) contributes its
            // dictionary once, via that sibling; skip the duplicate here to
            // mirror the worker param collection's per-rep dedup.
            if (context.seen_reps.contains(worker_child.rep)) continue;
            if (self.rowInstantiationTarget(worker_rep_id, call_rep_id, worker_child)) |row_target| {
                try self.collectCallHiddenDictionaryArgs(worker_child.rep, row_target, context);
                continue;
            }
            if (self.namedQuery().findMatchingChildByRole(call_children, worker_child)) |call_child| {
                try self.collectCallHiddenDictionaryArgs(worker_child.rep, call_child.rep, context);
                continue;
            }
            if (self.repQuery().structuralWrapperBackingRep(call_rep_id)) |call_backing| {
                const backing_children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(call_backing)].children);
                if (self.namedQuery().findMatchingChildByRole(backing_children, worker_child)) |call_child| {
                    try self.collectCallHiddenDictionaryArgs(worker_child.rep, call_child.rep, context);
                    continue;
                }
            }
            if (try self.namedQuery().findMatchingTagPayloadInRowExtension(call_children, worker_child)) |call_child| {
                try self.collectCallHiddenDictionaryArgs(worker_child.rep, call_child.rep, context);
                continue;
            }
            if (try self.repQuery().findMatchingDictionaryChildBySourceType(call_children, worker_child)) |call_child| {
                try self.collectCallHiddenDictionaryArgs(worker_child.rep, call_child.rep, context);
                continue;
            }
            if (self.workerPresenceSlotPayloadMatchesUnwrappedCallRep(worker_rep_id, call_rep_id, worker_child)) {
                try self.collectCallHiddenDictionaryArgs(worker_child.rep, call_rep_id, context);
                continue;
            }
            if (try self.repQuery().workerChildCanMatchUnwrappedCallRepForDictionaries(worker_rep_id, worker_child)) {
                try self.collectCallHiddenDictionaryArgs(worker_child.rep, call_rep_id, context);
                continue;
            }
            if (worker_child.role == .tag_ext and call_children.len == 0 and call_rep.dictionaries.len != 0) {
                try self.collectCallHiddenDictionaryArgs(worker_child.rep, call_rep_id, context);
                continue;
            }
            if (call_rep.kind == .dynamic and call_rep.children.len == 0 and call_rep.dictionaries.len != 0) {
                try self.collectCallHiddenDictionaryArgs(worker_child.rep, call_rep_id, context);
                continue;
            }
            boxyPlanInvariant("boxy direct call hidden dictionary mapping saw mismatched child roles");
        }
    }

    fn collectCallDictionaryRepSubstitutions(
        self: *Builder,
        worker_rep_id: TypeRepId,
        call_rep_id: TypeRepId,
        substitutions: *CallDictionaryRepSubstitutionMap,
        seen: *std.AutoHashMap(u64, void),
    ) Allocator.Error!void {
        const seen_key = (@as(u64, @intFromEnum(worker_rep_id)) << 32) | @as(u64, @intFromEnum(call_rep_id));
        const seen_entry = try seen.getOrPut(seen_key);
        if (seen_entry.found_existing) return;

        const worker_rep = self.plan.representations.items[@intFromEnum(worker_rep_id)];
        const call_rep = self.plan.representations.items[@intFromEnum(call_rep_id)];
        if (worker_rep.dictionaries.len != 0) {
            try substitutions.put(self.allocator, worker_rep_id, call_rep_id);
        }
        if (worker_rep.kind == .nominal and call_rep.kind == .nominal) {
            for (self.plan.nominalBackingArgSubstitutionSlice(worker_rep.nominal_backing_arg_substitutions)) |backing_substitution| {
                const exact_call_arg_rep = self.nominalBackingArgActualRep(call_rep_id, backing_substitution.arg_index) orelse
                    boxyPlanInvariant("checked nominal call was missing a backing dictionary argument substitution");
                try self.collectCallDictionaryRepSubstitutions(
                    backing_substitution.formal_rep,
                    exact_call_arg_rep,
                    substitutions,
                    seen,
                );
            }
        }
        if (worker_rep.children.len == 0) return;

        if (call_rep.kind == .empty_tag_union) {
            for (self.plan.childSlice(worker_rep.children)) |worker_child| {
                if (!try self.repQuery().repSubtreeHasDictionary(worker_child.rep)) continue;
                try self.collectCallDictionaryRepSubstitutions(worker_child.rep, call_rep_id, substitutions, seen);
            }
            return;
        }

        const worker_children = self.plan.childSlice(worker_rep.children);
        const call_children = self.plan.childSlice(call_rep.children);
        for (worker_children) |worker_child| {
            if (!try self.repQuery().repSubtreeHasDictionary(worker_child.rep)) continue;
            if (substitutions.get(worker_child.rep) != null) continue;
            if (self.rowInstantiationTarget(worker_rep_id, call_rep_id, worker_child)) |row_target| {
                try self.collectCallDictionaryRepSubstitutions(worker_child.rep, row_target, substitutions, seen);
                continue;
            }
            if (worker_child.role == .nominal_arg) {
                const arg_index = worker_child.role.nominal_arg;
                if (self.nominalBackingArgActualRep(call_rep_id, arg_index)) |exact_call_arg_rep| {
                    try self.collectCallDictionaryRepSubstitutions(worker_child.rep, exact_call_arg_rep, substitutions, seen);
                    continue;
                }
            }
            if (self.namedQuery().findMatchingChildByRole(call_children, worker_child)) |call_child| {
                try self.collectCallDictionaryRepSubstitutions(worker_child.rep, call_child.rep, substitutions, seen);
                continue;
            }
            if (self.repQuery().structuralWrapperBackingRep(call_rep_id)) |call_backing| {
                const backing_children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(call_backing)].children);
                if (self.namedQuery().findMatchingChildByRole(backing_children, worker_child)) |call_child| {
                    try self.collectCallDictionaryRepSubstitutions(worker_child.rep, call_child.rep, substitutions, seen);
                    continue;
                }
            }
            if (try self.namedQuery().findMatchingTagPayloadInRowExtension(call_children, worker_child)) |call_child| {
                try self.collectCallDictionaryRepSubstitutions(worker_child.rep, call_child.rep, substitutions, seen);
                continue;
            }
            if (try self.repQuery().findMatchingDictionaryChildBySourceType(call_children, worker_child)) |call_child| {
                try self.collectCallDictionaryRepSubstitutions(worker_child.rep, call_child.rep, substitutions, seen);
                continue;
            }
            if (self.workerPresenceSlotPayloadMatchesUnwrappedCallRep(worker_rep_id, call_rep_id, worker_child)) {
                try self.collectCallDictionaryRepSubstitutions(worker_child.rep, call_rep_id, substitutions, seen);
                continue;
            }
            if (try self.repQuery().workerChildCanMatchUnwrappedCallRepForDictionaries(worker_rep_id, worker_child)) {
                try self.collectCallDictionaryRepSubstitutions(worker_child.rep, call_rep_id, substitutions, seen);
                continue;
            }
            if (worker_child.role == .tag_ext and call_children.len == 0 and call_rep.dictionaries.len != 0) {
                try self.collectCallDictionaryRepSubstitutions(worker_child.rep, call_rep_id, substitutions, seen);
                continue;
            }
            if (call_rep.kind == .dynamic and call_rep.children.len == 0 and call_rep.dictionaries.len != 0) {
                try self.collectCallDictionaryRepSubstitutions(worker_child.rep, call_rep_id, substitutions, seen);
            }
        }
    }

    const CallableEvidenceSource = struct {
        rep: ?TypeRepId = null,
        method_evidence: Span = .{},
    };

    fn callableEvidenceSource(
        self: *Builder,
        context: *CallDictionaryContext,
        dictionaries: Span,
    ) Allocator.Error!CallableEvidenceSource {
        return try self.evidenceDictionarySource(
            context.evidence_view,
            context.evidence,
            context.next_evidence,
            dictionaries,
        );
    }

    fn evidenceDictionarySource(
        self: *Builder,
        maybe_view: ?ModuleView,
        maybe_entries: ?[]const static_dispatch.CheckedEvidence,
        next_evidence: *usize,
        dictionaries: Span,
    ) Allocator.Error!CallableEvidenceSource {
        const entries = maybe_entries orelse return .{};
        const view = maybe_view orelse
            boxyPlanInvariant("boxy callable evidence had no checked module view");
        if (next_evidence.* + dictionaries.len > entries.len) return .{};

        var selected = std.ArrayList(static_dispatch.CheckedEvidence).empty;
        defer selected.deinit(self.allocator);
        try selected.ensureTotalCapacity(self.allocator, dictionaries.len);
        while (selected.items.len < dictionaries.len and next_evidence.* < entries.len) {
            const entry = entries[next_evidence.*];
            next_evidence.* += 1;
            if (!entry.runtime_dictionary) continue;
            selected.appendAssumeCapacity(entry);
        }
        if (selected.items.len != dictionaries.len) return .{};
        var found: ?TypeRepId = null;
        var methods = std.ArrayList(DictionaryMethodEvidence).empty;
        defer methods.deinit(self.allocator);
        try methods.ensureTotalCapacity(self.allocator, selected.items.len);
        for (selected.items, self.plan.dictionarySlice(dictionaries)) |entry, requirement| {
            const planned: DictionaryMethodEvidence = switch (entry.resolution) {
                .direct => |node_id| blk: {
                    const node = view.static_dispatch_plans.evidenceNode(node_id);
                    const dispatcher = node.dispatcher_ty orelse
                        boxyPlanInvariant("direct callable dictionary evidence had no concrete dispatcher type");
                    const dispatcher_type = typeRef(view, dispatcher);
                    const rep = try self.analyzeType(view, dispatcher);
                    if (found) |existing| {
                        if (existing != rep) {
                            boxyPlanInvariant("one callable dictionary captured evidence for two dispatcher types");
                        }
                    } else {
                        found = rep;
                    }
                    const target_view = switch (node.target.kind) {
                        .procedure => |procedure| self.moduleForCheckedModuleId(procedure.template.artifact),
                        .local_proc, .structural => view,
                    };
                    const callable_type = switch (node.instantiation) {
                        .callable => |callable_ty| typeRef(view, callable_ty),
                        .monomorphic => typeRef(target_view, node.target.callable_ty),
                    };
                    const source = self.workerSourceForMethodTarget(.{
                        .view = target_view,
                        .target = node.target,
                    }, dispatcher_type, node.generated_codec_derivation);
                    _ = try self.analyzeType(target_view, node.target.callable_ty);
                    _ = try self.analyzeType(self.moduleForId(callable_type.module), callable_type.ty);
                    const worker = try self.ensureWorker(
                        source,
                        self.workerCheckedTypeForSource(source, callable_type),
                        null,
                    );
                    break :blk .{
                        .requirement_type = requirement.fn_ty,
                        .callable_type = callable_type,
                        .resolution = .{ .worker = worker },
                        .nested_dict_args = try self.materializeDictionaryMethodHiddenArgs(
                            worker,
                            callable_type,
                            view,
                            view.static_dispatch_plans.nestedEvidence(node),
                        ),
                    };
                },
                .structural => |structural| blk: {
                    const rep = try self.analyzeType(view, structural.dispatcher_ty);
                    if (found) |existing| {
                        if (existing != rep) {
                            boxyPlanInvariant("one callable dictionary captured evidence for two dispatcher types");
                        }
                    } else {
                        found = rep;
                    }
                    const callable_type = typeRef(view, structural.callable_ty);
                    _ = try self.analyzeType(view, structural.callable_ty);
                    const structural_kind = structural.derivation.kind();
                    const resolution: DictionaryMethodEvidence.Resolution = switch (structural_kind) {
                        .equality, .hash => .{ .structural = structural_kind },
                        .parser, .encoder => blk_worker: {
                            const derivation_id = structural.generated_codec_derivation orelse
                                boxyPlanInvariant("structural codec evidence had no checked derivation reference");
                            if (@intFromEnum(derivation_id) >= view.static_dispatch_plans.generated_codec_derivations.len) {
                                boxyPlanInvariant("structural codec evidence referenced a missing checked derivation");
                            }
                            break :blk_worker .{ .worker = try self.ensureWorker(
                                .{ .generated_codec = .{
                                    .kind = if (structural_kind == .parser) .parser_constructor else .encoder_constructor,
                                    .shape = typeRef(view, structural.dispatcher_ty),
                                    .contract_derivation = derivation_id,
                                } },
                                callable_type,
                                null,
                            ) };
                        },
                        .map, .map_effectful => boxyPlanInvariant("derived map evidence reached runtime dictionary planning"),
                    };
                    break :blk .{
                        .requirement_type = requirement.fn_ty,
                        .callable_type = callable_type,
                        .resolution = resolution,
                    };
                },
                .constraint, .from_callable => .{
                    .requirement_type = requirement.fn_ty,
                    .callable_type = requirement.fn_ty,
                    .resolution = .constraint,
                },
                .checked_error => .{
                    .requirement_type = requirement.fn_ty,
                    .callable_type = requirement.fn_ty,
                    .resolution = .checked_error,
                },
                .unreachable_value => .{
                    .requirement_type = requirement.fn_ty,
                    .callable_type = requirement.fn_ty,
                    .resolution = .unreachable_value,
                },
            };
            methods.appendAssumeCapacity(planned);
        }
        const method_start: u32 = @intCast(self.plan.dictionary_method_evidence.items.len);
        try self.plan.dictionary_method_evidence.appendSlice(self.allocator, methods.items);
        return .{
            .rep = found,
            .method_evidence = .{ .start = method_start, .len = @intCast(methods.items.len) },
        };
    }

    fn materializeDictionaryMethodHiddenArgs(
        self: *Builder,
        worker_id: WorkerPlanId,
        callable_type: CheckedTypeIdentity,
        evidence_view: ModuleView,
        evidence: []const static_dispatch.CheckedEvidence,
    ) Allocator.Error!Span {
        const worker = self.plan.workers.items[@intFromEnum(worker_id)];
        if (worker.hidden_dicts.len == 0) return .{};

        const callable_rep = self.plan.repForSourceType(callable_type) orelse
            boxyPlanInvariant("dictionary method callable type was not analyzed");
        const function = (self.repQuery().functionChildren(callable_rep)) orelse
            boxyPlanInvariant("dictionary method checked target was not callable");
        const arg_types = try self.allocator.alloc(CheckedTypeIdentity, function.arg_count);
        defer self.allocator.free(arg_types);
        const children_span = self.plan.representations.items[@intFromEnum(function.rep)].children;
        for (arg_types, 0..) |*arg_type, index| {
            const child = self.plan.children.items[children_span.start + function.args_start + index];
            arg_type.* = self.plan.representations.items[@intFromEnum(child.rep)].source_type;
        }
        const ret_type = self.plan.representations.items[@intFromEnum(function.ret)].source_type;
        return try self.materializeWorkerCallHiddenDictionaryArgsWithEvidence(
            worker_id,
            null,
            arg_types,
            ret_type,
            evidence_view,
            evidence,
        );
    }

    const DictionaryMethodRequirementDescriptorPlan = struct {
        args: Span = .{},
        sources: Span = .{},
    };

    fn dictionaryMethodRequirementDescriptorSources(
        self: *Builder,
        requirement_type: CheckedTypeIdentity,
        callable_type: CheckedTypeIdentity,
    ) Allocator.Error!DictionaryMethodRequirementDescriptorPlan {
        const requirement_rep = self.plan.repForSourceType(requirement_type) orelse
            boxyPlanInvariant("dictionary method requirement type was not analyzed");
        const callable_rep = self.plan.repForSourceType(callable_type) orelse
            boxyPlanInvariant("dictionary method callable type was not analyzed");
        const requirement_function = (self.repQuery().functionChildren(requirement_rep)) orelse
            boxyPlanInvariant("dictionary method requirement was not callable");
        const callable_function = (self.repQuery().functionChildren(callable_rep)) orelse
            boxyPlanInvariant("dictionary method evidence callable was not callable");
        if (requirement_function.arg_count != callable_function.arg_count) {
            boxyPlanInvariant("dictionary method requirement and evidence callable arity differed");
        }

        var params = std.ArrayList(HiddenDescriptorParam).empty;
        defer params.deinit(self.allocator);
        var param_seen_reps = collections.DenseMap(TypeRepId, void).init(self.allocator);
        defer param_seen_reps.deinit();
        var param_seen_descs = collections.DenseMap(DescriptorRequirementId, void).init(self.allocator);
        defer param_seen_descs.deinit();
        const requirement_children = self.plan.childSlice(
            self.plan.representations.items[@intFromEnum(requirement_function.rep)].children,
        );
        for (requirement_children[requirement_function.args_start..][0..requirement_function.arg_count]) |arg| {
            try self.collectRuntimeHiddenDescriptorsForRep(arg.rep, &params, &param_seen_reps, &param_seen_descs);
        }
        try self.collectRuntimeHiddenDescriptorsForRep(
            requirement_function.ret,
            &params,
            &param_seen_reps,
            &param_seen_descs,
        );
        if (params.items.len == 0) return .{};

        var pending = std.ArrayList(DirectCallHiddenDescriptorArg).empty;
        defer pending.deinit(self.allocator);
        var seen_reps = collections.DenseMap(TypeRepId, void).init(self.allocator);
        defer seen_reps.deinit();
        var seen_descriptor_reps = collections.DenseMap(TypeRepId, void).init(self.allocator);
        defer seen_descriptor_reps.deinit();
        var substitutions = CallDescriptorRepSubstitutionMap{};
        defer substitutions.deinit(self.allocator);
        var next_param: usize = 0;
        const callable_children = self.plan.childSlice(
            self.plan.representations.items[@intFromEnum(callable_function.rep)].children,
        );
        const requirement_args = requirement_children[requirement_function.args_start..][0..requirement_function.arg_count];
        const callable_args = callable_children[callable_function.args_start..][0..callable_function.arg_count];
        for (requirement_args, callable_args, 0..) |requirement_arg, callable_arg, arg_index| {
            try self.collectCallHiddenDescriptorArgs(
                requirement_arg.rep,
                callable_arg.rep,
                callable_arg.rep,
                callable_arg.rep,
                @intCast(arg_index),
                params.items,
                &next_param,
                &pending,
                &seen_reps,
                &seen_descriptor_reps,
                &substitutions,
                true,
            );
        }
        try self.collectCallHiddenDescriptorArgs(
            requirement_function.ret,
            callable_function.ret,
            callable_function.ret,
            callable_function.ret,
            null,
            params.items,
            &next_param,
            &pending,
            &seen_reps,
            &seen_descriptor_reps,
            &substitutions,
            true,
        );
        if (next_param != params.items.len or pending.items.len != params.items.len) {
            boxyPlanInvariant("dictionary method descriptor source mapping did not cover every requirement");
        }

        const args_start: u32 = @intCast(self.plan.direct_call_hidden_desc_args.items.len);
        try self.plan.direct_call_hidden_desc_args.appendSlice(self.allocator, pending.items);
        const sources_start: u32 = @intCast(self.plan.dictionary_method_desc_sources.items.len);
        for (pending.items, 0..) |source, source_index| {
            try self.plan.dictionary_method_desc_sources.append(self.allocator, .{
                .rep = source.rep,
                .source = if (source.source_arg_index) |arg_index|
                    .{ .argument = arg_index }
                else if (try self.repQuery().repSubtreeHasDescriptor(source.rep))
                    .{ .call = @intCast(source_index) }
                else
                    .static_rep,
            });
        }
        return .{
            .args = .{ .start = args_start, .len = @intCast(pending.items.len) },
            .sources = .{ .start = sources_start, .len = @intCast(pending.items.len) },
        };
    }

    fn dictionaryMethodWorkerDescriptorArgs(
        self: *Builder,
        worker_id: WorkerPlanId,
        boundary_type: CheckedTypeIdentity,
    ) Allocator.Error!Span {
        const boundary_rep = self.plan.repForSourceType(boundary_type) orelse
            boxyPlanInvariant("dictionary method descriptor boundary type was not analyzed");
        const boundary_function = (self.repQuery().functionChildren(boundary_rep)) orelse
            boxyPlanInvariant("dictionary method descriptor boundary was not callable");
        const boundary_children = self.plan.childSlice(
            self.plan.representations.items[@intFromEnum(boundary_function.rep)].children,
        );
        const arg_types = try self.allocator.alloc(CheckedTypeIdentity, boundary_function.arg_count);
        defer self.allocator.free(arg_types);
        for (arg_types, boundary_children[boundary_function.args_start..][0..boundary_function.arg_count]) |*arg_type, arg| {
            arg_type.* = arg.source_type;
        }
        const ret_type = self.plan.representations.items[@intFromEnum(boundary_function.ret)].source_type;
        return try self.materializeWorkerCallHiddenDescriptorArgs(
            worker_id,
            arg_types,
            arg_types,
            ret_type,
        );
    }

    fn dictionaryMethodHiddenDescriptorSources(
        self: *Builder,
        worker_id: WorkerPlanId,
        worker_desc_args: Span,
        requirement_desc_args: Span,
    ) Allocator.Error!Span {
        const worker = self.plan.workers.items[@intFromEnum(worker_id)];
        const params = self.plan.hiddenDescriptorParamSlice(worker.hidden_descs);
        const worker_args = self.plan.directCallHiddenDescriptorArgSlice(worker_desc_args);
        const requirement_args = self.plan.directCallHiddenDescriptorArgSlice(requirement_desc_args);
        if (params.len != worker_args.len) {
            boxyPlanInvariant("dictionary method worker descriptor source count disagreed with worker parameters");
        }
        if (params.len == 0) return .{};

        const start: u32 = @intCast(self.plan.dictionary_method_hidden_desc_sources.items.len);
        var next_slot: u32 = 0;
        for (params, worker_args) |param, worker_arg| {
            if (param.desc != worker_arg.worker_desc or param.rep != worker_arg.worker_rep) {
                boxyPlanInvariant("dictionary method worker descriptor source order disagreed with worker parameters");
            }

            const argument_source: ?u32 = switch (worker.source) {
                .generated_codec => |codec| switch (codec.kind) {
                    .parser_constructor, .encoder_constructor => worker_arg.source_arg_index,
                    .parser_runtime,
                    .encoder_runtime,
                    .encoder_record_fields,
                    .encoder_dict_fields,
                    .encoder_sequence_elements,
                    .encoder_tag_field,
                    .encoder_tag_payload_thunk,
                    .encoder_tag_payload_elements,
                    .encoder_value_thunk,
                    => null,
                },
                .procedure_template,
                .procedure_binding,
                .procedure_use,
                .nested_expr,
                .generated_field_iterator,
                .generated_interpolation_step,
                => null,
            };
            var call_source: ?u32 = null;
            if (argument_source == null) {
                const requirement_source_identity = self.repQuery().descriptorArgumentIdentityRep(worker_arg.rep);
                for (requirement_args, 0..) |requirement_arg, call_index| {
                    const requirement_call_identity = self.repQuery().descriptorArgumentIdentityRep(requirement_arg.rep);
                    if (requirement_call_identity != requirement_source_identity) continue;
                    if (call_source != null) {
                        boxyPlanInvariant("dictionary method worker descriptor mapped to multiple call descriptors");
                    }
                    call_source = @intCast(call_index);
                }
            }
            const source: DictionaryMethodHiddenDescriptorSource = if (argument_source) |arg_index|
                .{ .argument = arg_index }
            else if (call_source) |call_index|
                .{ .call = call_index }
            else blk: {
                const slot = next_slot;
                next_slot += 1;
                break :blk .{ .slot = slot };
            };
            try self.plan.dictionary_method_hidden_desc_sources.append(self.allocator, source);
        }
        return .{ .start = start, .len = @intCast(params.len) };
    }

    fn materializeDictionaryMethodDescriptorSources(self: *Builder) Allocator.Error!void {
        for (self.plan.dictionary_method_evidence.items) |*method| {
            switch (method.resolution) {
                .worker => |worker| {
                    method.worker_desc_args = try self.dictionaryMethodWorkerDescriptorArgs(worker, method.callable_type);
                    const requirement_plan = try self.dictionaryMethodRequirementDescriptorSources(
                        method.requirement_type,
                        method.callable_type,
                    );
                    method.requirement_desc_args = requirement_plan.args;
                    method.requirement_desc_sources = requirement_plan.sources;
                    method.hidden_desc_sources = try self.dictionaryMethodHiddenDescriptorSources(
                        worker,
                        method.worker_desc_args,
                        method.requirement_desc_args,
                    );
                },
                .structural,
                .constraint,
                .checked_error,
                .unreachable_value,
                => {},
            }
        }
    }

    fn workerBindsDictionarySpan(self: *const Builder, worker_id: WorkerPlanId, dictionaries: Span) bool {
        if (dictionaries.len == 0) return false;
        const worker = self.plan.workers.items[@intFromEnum(worker_id)];
        for (self.plan.hiddenDictionaryParamSlice(worker.hidden_dicts)) |param| {
            if (std.meta.eql(param.dictionaries, dictionaries)) return true;
        }
        return false;
    }

    fn ensureStaticDictionaryWorkers(
        self: *Builder,
        source_rep_id: TypeRepId,
        worker_dictionaries: Span,
        method_evidence: Span,
    ) Allocator.Error!Span {
        if (method_evidence.len != 0) {
            if (method_evidence.len != worker_dictionaries.len) {
                boxyPlanInvariant("static dictionary checked evidence did not cover every method requirement");
            }
            const start: u32 = @intCast(self.plan.dictionary_method_evidence.items.len);
            var method_index: usize = 0;
            while (method_index < method_evidence.len) : (method_index += 1) {
                const method = self.plan.dictionary_method_evidence.items[method_evidence.start + method_index];
                const planned = switch (method.resolution) {
                    .worker => method,
                    .structural => |kind| switch (kind) {
                        .equality, .hash => method,
                        .parser, .encoder => boxyPlanInvariant("structural codec dictionary evidence had no generated worker"),
                        .map, .map_effectful => boxyPlanInvariant("derived map evidence reached static dictionary worker planning"),
                    },
                    .constraint => try self.staticDictionaryMethodEvidence(
                        source_rep_id,
                        self.plan.dictionaries.items[worker_dictionaries.start + method_index],
                    ),
                    .checked_error => boxyPlanInvariant("checked-error dictionary evidence reached Boxy worker planning"),
                    .unreachable_value => method,
                };
                try self.plan.dictionary_method_evidence.append(self.allocator, planned);
            }
            return .{ .start = start, .len = method_evidence.len };
        }

        const method_start: u32 = @intCast(self.plan.dictionary_method_evidence.items.len);

        // analyzeType/ensureWorker can append dictionary requirements, growing
        // the pool and invalidating any held slice; requirements are re-read
        // by index on every iteration.
        var requirement_index: usize = 0;
        while (requirement_index < worker_dictionaries.len) : (requirement_index += 1) {
            const requirement = self.plan.dictionaries.items[worker_dictionaries.start + requirement_index];
            try self.plan.dictionary_method_evidence.append(
                self.allocator,
                try self.staticDictionaryMethodEvidence(source_rep_id, requirement),
            );
        }
        return .{ .start = method_start, .len = worker_dictionaries.len };
    }

    fn staticDictionaryMethodEvidence(
        self: *Builder,
        source_rep_id: TypeRepId,
        requirement: DictionaryRequirement,
    ) Allocator.Error!DictionaryMethodEvidence {
        const source_rep = self.plan.representations.items[@intFromEnum(source_rep_id)];
        const source_view = self.moduleForId(source_rep.source_type.module);
        const requirement_view = self.moduleForId(requirement.source_type.module);
        const owner = methodOwnerForModuleType(source_view, source_rep.source_type.ty) orelse
            self.defaultedDictionaryOwner(source_rep_id);
        if (owner) |method_owner| {
            if (self.lookupMethodTarget(source_view, method_owner, requirement_view, requirement.fn_name)) |lookup| {
                const source = self.workerSourceForMethodTarget(lookup, source_rep.source_type, null);
                const callable_type = CheckedTypeIdentity{ .module = lookup.view.key, .ty = lookup.target.callable_ty };
                _ = try self.analyzeType(lookup.view, lookup.target.callable_ty);
                return .{
                    .requirement_type = requirement.fn_ty,
                    .callable_type = callable_type,
                    .resolution = .{ .worker = try self.ensureWorker(source, callable_type, null) },
                };
            }
        }
        return try self.structuralDictionaryMethodEvidence(source_rep, requirement, requirement_view);
    }

    fn defaultedDictionaryOwner(self: *Builder, rep_id: TypeRepId) ?static_dispatch.MethodOwner {
        const rep = self.plan.representations.items[@intFromEnum(rep_id)];
        const view = self.moduleForId(rep.source_type.module);
        const payload = view.checked_types.payload(rep.source_type.ty);
        const variable = if (payload == .flex)
            payload.flex
        else if (payload == .rigid)
            payload.rigid
        else
            return null;
        const builtin: static_dispatch.BuiltinOwner = switch (variable.numeric_default_phase orelse return null) {
            .mono_specialization => .dec,
            .mono_specialization_str => .str,
            .checking_finalized => return null,
        };
        return .{ .builtin = builtin };
    }

    fn structuralDictionaryMethodEvidence(
        self: *Builder,
        source_rep: TypeRepresentation,
        requirement: DictionaryRequirement,
        requirement_view: ModuleView,
    ) Allocator.Error!DictionaryMethodEvidence {
        const kind = self.structuralKindForRequirement(requirement, requirement_view) orelse
            boxyPlanInvariant("static boxy dictionary could not resolve a checked method target or structural method");
        _ = try self.analyzeType(requirement_view, requirement.fn_ty.ty);
        return switch (kind) {
            .equality, .hash => .{
                .requirement_type = requirement.fn_ty,
                .callable_type = requirement.fn_ty,
                .resolution = .{ .structural = kind },
            },
            .parser, .encoder => try self.generatedCodecStaticDictionaryMethodEvidence(
                source_rep,
                requirement,
                requirement_view,
                kind,
            ),
            .map, .map_effectful => boxyPlanInvariant("derived map evidence reached Boxy runtime dictionary planning"),
        };
    }

    fn generatedCodecStaticDictionaryMethodEvidence(
        self: *Builder,
        source_rep: TypeRepresentation,
        requirement: DictionaryRequirement,
        requirement_view: ModuleView,
        kind: static_dispatch.StructuralKind,
    ) Allocator.Error!DictionaryMethodEvidence {
        const expected_kind: static_dispatch.GeneratedCodecDerivationKind = switch (kind) {
            .parser => .parser,
            .encoder => .encoder,
            .equality, .hash, .map, .map_effectful => boxyPlanInvariant("non-codec dictionary requested a generated codec worker"),
        };
        const source_view = self.moduleForId(source_rep.source_type.module);
        if (!moduleKeyEqual(source_view.key, requirement_view.key) or
            !moduleKeyEqual(requirement.fn_ty.module, requirement_view.key))
        {
            boxyPlanInvariant("generated codec dictionary types belonged to different checked modules");
        }

        const constructor = checkedFunctionPayload(requirement_view, requirement.fn_ty.ty);
        if (constructor.args.len != 1) {
            boxyPlanInvariant("generated codec dictionary constructor had unexpected arity");
        }
        const runtime = checkedFunctionPayload(requirement_view, constructor.ret);
        const state_type = switch (expected_kind) {
            .parser => if (runtime.args.len == 1)
                runtime.args[0]
            else
                boxyPlanInvariant("generated parser dictionary runtime had unexpected arity"),
            .encoder => if (runtime.args.len == 2)
                runtime.args[1]
            else
                boxyPlanInvariant("generated encoder dictionary runtime had unexpected arity"),
        };
        const try_payloads = checkedTryPayloads(requirement_view, runtime.ret) orelse
            boxyPlanInvariant("generated codec dictionary runtime did not return Try");
        const shape_key = source_view.checked_types.rootKey(source_rep.source_type.ty);
        const encoding_key = requirement_view.checked_types.rootKey(constructor.args[0]);
        const state_key = requirement_view.checked_types.rootKey(state_type);
        const error_key = requirement_view.checked_types.rootKey(try_payloads.err);
        var found: ?struct {
            id: static_dispatch.GeneratedCodecDerivationId,
            derivation: static_dispatch.GeneratedCodecDerivation,
        } = null;
        for (source_view.static_dispatch_plans.generated_codec_derivations, 0..) |derivation, index| {
            if (derivation.kind != expected_kind or
                !std.meta.eql(shape_key, source_view.checked_types.rootKey(derivation.shape_ty)) or
                !std.meta.eql(encoding_key, source_view.checked_types.rootKey(derivation.encoding_ty)) or
                !std.meta.eql(state_key, source_view.checked_types.rootKey(derivation.state_ty)) or
                !std.meta.eql(error_key, source_view.checked_types.rootKey(derivation.error_ty)))
            {
                continue;
            }
            if (found) |existing| {
                if (!generatedCodecDerivationsEql(source_view.static_dispatch_plans, existing.derivation, derivation)) {
                    boxyPlanInvariant("generated codec dictionary matched multiple checked derivation contracts");
                }
            } else {
                found = .{
                    .id = @enumFromInt(@as(u32, @intCast(index))),
                    .derivation = derivation,
                };
            }
        }

        const selected = found orelse
            boxyPlanInvariant("generated codec dictionary had no matching checked derivation contract");
        const callable_type = typeRef(source_view, selected.derivation.constructor_ty);
        return .{
            .requirement_type = requirement.fn_ty,
            .callable_type = callable_type,
            .resolution = .{ .worker = try self.ensureWorker(
                .{ .generated_codec = .{
                    .kind = if (expected_kind == .parser) .parser_constructor else .encoder_constructor,
                    .shape = source_rep.source_type,
                    .contract_derivation = selected.id,
                } },
                callable_type,
                null,
            ) },
        };
    }

    fn structuralKindForRequirement(
        _: *Builder,
        requirement: DictionaryRequirement,
        requirement_view: ModuleView,
    ) ?static_dispatch.StructuralKind {
        const canonical_names = requirement_view.canonical_names orelse return null;
        if (canonical_names.lookupMethodName("is_eq")) |method| {
            if (method == requirement.fn_name) return .equality;
        }
        if (canonical_names.lookupMethodName("to_hash")) |method| {
            if (method == requirement.fn_name) return .hash;
        }
        if (canonical_names.lookupMethodName("parser_for")) |method| {
            if (method == requirement.fn_name) return .parser;
        }
        if (canonical_names.lookupMethodName("encoder_for")) |method| {
            if (method == requirement.fn_name) return .encoder;
        }
        return null;
    }

    const MethodTargetLookup = struct {
        view: ModuleView,
        method: ?MethodNameId = null,
        target: static_dispatch.MethodTarget,
    };

    fn lookupMethodTarget(
        self: *Builder,
        owner_view: ModuleView,
        owner: static_dispatch.MethodOwner,
        method_view: ModuleView,
        method: MethodNameId,
    ) ?MethodTargetLookup {
        const method_text = method_view.canonical_names.?.methodNameText(method);
        return self.lookupMethodTargetByText(owner_view, owner, method_text);
    }

    fn lookupMethodTargetByText(
        self: *Builder,
        owner_view: ModuleView,
        owner: static_dispatch.MethodOwner,
        method_text: []const u8,
    ) ?MethodTargetLookup {
        if (self.lookupMethodTargetInView(owner_view, owner_view, owner, method_text)) |target| return target;
        for (self.imports) |imported| {
            const view = moduleViewFromImported(imported);
            if (moduleKeyEqual(view.key, owner_view.key)) continue;
            if (self.lookupMethodTargetInView(view, owner_view, owner, method_text)) |target| return target;
        }
        for (self.relation_modules) |relation| {
            const view = moduleViewFromImported(relation);
            if (moduleKeyEqual(view.key, owner_view.key)) continue;
            if (self.lookupMethodTargetInView(view, owner_view, owner, method_text)) |target| return target;
        }
        return null;
    }

    fn lookupMethodTargetInView(
        _: *Builder,
        candidate: ModuleView,
        owner_view: ModuleView,
        owner: static_dispatch.MethodOwner,
        method_text: []const u8,
    ) ?MethodTargetLookup {
        const owner_names = owner_view.canonical_names orelse return null;
        const candidate_names = candidate.canonical_names orelse return null;
        const candidate_owner = methodOwnerInNames(owner_names, candidate_names, owner) orelse return null;
        const candidate_method = candidate_names.lookupMethodName(method_text) orelse return null;
        const found = candidate.method_registry.lookup(.{ .owner = candidate_owner, .method = candidate_method }) orelse return null;
        return .{ .view = candidate, .method = candidate_method, .target = found.requireTarget("boxy planning") };
    }

    fn workerSourceForMethodTarget(
        self: *Builder,
        lookup: MethodTargetLookup,
        shape: CheckedTypeIdentity,
        contract_derivation: ?static_dispatch.GeneratedCodecDerivationId,
    ) WorkerSource {
        return switch (lookup.target.kind) {
            .procedure => |procedure| .{ .procedure_template = procedure.template },
            .local_proc => |local| if (self.topLevelProcedureBindingForExpr(lookup.view, local.expr)) |binding|
                .{ .procedure_binding = binding }
            else
                .{ .nested_expr = .{ .module = lookup.view.key, .expr = self.nestedCallableSiteExprForExpr(lookup.view, local.expr) orelse local.expr } },
            .structural => |kind| switch (kind) {
                .parser => .{ .generated_codec = .{
                    .kind = .parser_constructor,
                    .shape = shape,
                    .contract_derivation = contract_derivation,
                } },
                .encoder => .{ .generated_codec = .{
                    .kind = .encoder_constructor,
                    .shape = shape,
                    .contract_derivation = contract_derivation,
                } },
                .equality, .hash, .map, .map_effectful => boxyPlanInvariant("non-codec structural target reached Boxy method worker selection"),
            },
        };
    }

    /// Returns the exact call-side row that instantiates a worker child when
    /// open tag rows expose different tags on each side of a checked function
    /// boundary. Unmatched worker payloads live in the call extension; when
    /// the call contributes unmatched payloads, the worker extension denotes
    /// the complete call row rather than only its residual extension.
    fn rowInstantiationTarget(
        self: *Builder,
        worker_rep_id: TypeRepId,
        call_rep_id: TypeRepId,
        worker_child: RepChild,
    ) ?TypeRepId {
        const worker_rep = self.plan.representations.items[@intFromEnum(worker_rep_id)];
        const call_rep = self.plan.representations.items[@intFromEnum(call_rep_id)];
        if (worker_rep.kind != .tag_union or call_rep.kind != .tag_union) return null;

        const worker_children = self.plan.childSlice(worker_rep.children);
        const call_children = self.plan.childSlice(call_rep.children);
        switch (worker_child.role) {
            .tag_payload => {
                if (self.namedQuery().findMatchingChildByRole(call_children, worker_child) != null) return null;
                var extension: ?TypeRepId = null;
                for (call_children) |call_child| {
                    if (call_child.role != .tag_ext) continue;
                    if (extension != null) {
                        boxyPlanInvariant("boxy tag union representation had multiple row extensions");
                    }
                    extension = call_child.rep;
                }
                return extension;
            },
            .tag_ext => {
                for (call_children) |call_child| {
                    if (call_child.role != .tag_payload) continue;
                    if (self.namedQuery().findMatchingChildByRole(worker_children, call_child) == null) {
                        return call_rep_id;
                    }
                }
                return null;
            },
            .alias_backing,
            .alias_arg,
            .nominal_backing,
            .nominal_arg,
            .nominal_padding_field,
            .record_field,
            .record_ext,
            .tuple_elem,
            .function_arg,
            .function_ret,
            .list_elem,
            .box_payload,
            => return null,
        }
    }

    fn workerPresenceSlotPayloadMatchesUnwrappedCallRep(
        self: *const Builder,
        worker_rep_id: TypeRepId,
        call_rep_id: TypeRepId,
        worker_child: RepChild,
    ) bool {
        const worker_rep = self.plan.representations.items[@intFromEnum(worker_rep_id)];
        const present_discriminant = worker_rep.presence_slot_present_discriminant orelse return false;
        const call_rep = self.plan.representations.items[@intFromEnum(call_rep_id)];
        if (call_rep.presence_slot_present_discriminant != null) return false;

        const variants = self.plan.tagVariantSlice(worker_rep.tag_variants);
        const present_index: usize = present_discriminant;
        if (present_index >= variants.len) {
            boxyPlanInvariant("presence-slot Present discriminant exceeded its planned variants");
        }
        for (self.plan.childSlice(variants[present_index].payloads)) |payload| {
            if (payload.rep == worker_child.rep and std.meta.eql(payload.role, worker_child.role)) return true;
        }
        return false;
    }

    fn callDescriptorRepsAgreeAcrossPresenceSlot(
        self: *const Builder,
        first: TypeRepId,
        second: TypeRepId,
    ) bool {
        if (first == second) return true;
        if (self.presenceSlotPayloadRep(first)) |payload| {
            if (payload == second) return true;
        }
        if (self.presenceSlotPayloadRep(second)) |payload| {
            if (payload == first) return true;
        }
        return false;
    }

    fn presenceSlotPayloadRep(self: *const Builder, rep_id: TypeRepId) ?TypeRepId {
        const rep = self.plan.representations.items[@intFromEnum(rep_id)];
        const present_discriminant = rep.presence_slot_present_discriminant orelse return null;
        const variants = self.plan.tagVariantSlice(rep.tag_variants);
        const present_index: usize = present_discriminant;
        if (present_index >= variants.len) {
            boxyPlanInvariant("presence-slot Present discriminant exceeded its planned variants");
        }
        const payloads = self.plan.childSlice(variants[present_index].payloads);
        if (payloads.len != 1) {
            boxyPlanInvariant("presence-slot Present variant did not have exactly one planned payload");
        }
        return payloads[0].rep;
    }

    fn analyzeWorkerBodyTypes(self: *Builder, body: WorkerBody) Allocator.Error!void {
        switch (body) {
            .checked_expr => |checked_body| {
                if (checked_body.stored_fn) |stored_fn| try self.analyzeStoredFnCaptureTypes(stored_fn);
                try self.analyzeWorkerRootExprTypes(checked_body.view, checked_body.root_expr);
            },
            .intrinsic_wrapper => |intrinsic| try self.analyzeIntrinsicWrapperTypes(intrinsic.view, intrinsic.wrapper),
            .hosted_proc => |hosted| try self.analyzeHostedProcTypes(hosted.view, hosted.proc),
            // A crash body references no types beyond the declared signature,
            // which the worker's own representation already covers.
            .unimplemented => {},
        }
    }

    fn analyzeWorkerRootExprTypes(
        self: *Builder,
        view: ModuleView,
        root_expr: checked.CheckedExprId,
    ) Allocator.Error!void {
        const expr = view.checked_bodies.expr(root_expr);
        if (expr.data != .lambda) return try self.analyzeExprTypes(view, root_expr);
        for (expr.data.lambda.args) |arg| try self.analyzePatternTypes(view, arg);
        try self.analyzeExprTypes(view, expr.data.lambda.body);
    }

    const WorkerBody = union(enum) {
        checked_expr: struct {
            view: ModuleView,
            root_expr: checked.CheckedExprId,
            stored_fn: ?StoredFnSource = null,
        },
        intrinsic_wrapper: struct {
            view: ModuleView,
            wrapper: checked.IntrinsicWrapper,
        },
        hosted_proc: struct {
            view: ModuleView,
            proc: checked.HostedProc,
        },
        /// The declaration behind this worker has a type annotation and no
        /// implementation, so reaching the worker crashes.
        unimplemented,
    };

    fn rootWorkerBody(self: *Builder, source: WorkerSource) WorkerBody {
        return switch (source) {
            .procedure_template => |template| self.rootProcedureTemplateBody(template),
            .procedure_binding => |binding| self.rootProcedureBindingBody(self.moduleForId(binding.artifact), binding.binding),
            .procedure_use => |use| switch (use.binding) {
                .top_level => |top_level| blk: {
                    const view = self.moduleForId(top_level.artifact);
                    break :blk self.rootProcedureBindingBody(view, top_level.binding);
                },
                .platform_required => |required| blk: {
                    const view = self.moduleForId(required.app_value.artifact);
                    break :blk self.rootProcedureBindingBody(view, required.procedure_binding);
                },
                .imported => |imported| self.importedProcedureBindingBody(imported),
                .hosted => |hosted| self.hostedProcedureBody(hosted),
            },
            .nested_expr => |expr_ref| self.nestedExprWorkerBody(expr_ref),
            .generated_codec => boxyPlanInvariant("generated codec worker has no checked procedure body"),
            .generated_field_iterator => boxyPlanInvariant("generated FieldNames iterator worker has no checked procedure body"),
            .generated_interpolation_step => boxyPlanInvariant("generated interpolation step worker has no checked procedure body"),
        };
    }

    fn nestedExprWorkerBody(self: *Builder, expr_ref: CheckedExprIdentity) WorkerBody {
        const view = self.moduleForId(expr_ref.module);
        const expr = view.checked_bodies.expr(expr_ref.expr);
        const root_expr = if (expr.data == .lambda)
            expr_ref.expr
        else if (expr.data == .closure)
            expr.data.closure.lambda
        else
            boxyPlanInvariant("nested callable worker source did not point at a lambda or closure");
        return .{ .checked_expr = .{
            .view = view,
            .root_expr = root_expr,
        } };
    }

    /// Whether this binding's body is a callable-eval whose compile-time root
    /// is still pending (an unfinalized binding the running roots never
    /// reference; `roc test` finalizes only what its expects reach). Such a
    /// body cannot be planned; callers skip eager work for it and the panic is
    /// deferred to an actual attempt to lower it.
    fn procedureBindingBodyIsPendingEval(self: *Builder, view: ModuleView, binding_ref: checked.TopLevelProcedureBindingRef) bool {
        const binding = view.top_level_procedure_bindings.get(binding_ref);
        return switch (binding.body) {
            .direct_template => false,
            .callable_eval_template => |template_id| blk: {
                const template = self.callableEvalTemplate(view, template_id);
                const root = view.compile_time_roots.root(template.root);
                break :blk root.payload == .pending;
            },
        };
    }

    fn pendingCallableEvalExprForSource(
        self: *Builder,
        source: WorkerSource,
    ) ?CheckedExprIdentity {
        return switch (source) {
            .procedure_binding => |binding_ref| blk: {
                const view = self.moduleForId(binding_ref.artifact);
                const binding = view.top_level_procedure_bindings.get(binding_ref.binding);
                break :blk self.pendingCallableEvalExprForBody(view, binding.body);
            },
            .procedure_use => |procedure| switch (procedure.binding) {
                .top_level => |binding_ref| blk: {
                    const view = self.moduleForId(binding_ref.artifact);
                    const binding = view.top_level_procedure_bindings.get(binding_ref.binding);
                    break :blk self.pendingCallableEvalExprForBody(view, binding.body);
                },
                .platform_required => |required| blk: {
                    const view = self.moduleForId(required.app_value.artifact);
                    const binding = view.top_level_procedure_bindings.get(required.procedure_binding);
                    break :blk self.pendingCallableEvalExprForBody(view, binding.body);
                },
                .imported => |imported| blk: {
                    const view = self.moduleForId(imported.artifact);
                    break :blk self.pendingCallableEvalExprForBody(
                        view,
                        self.importedProcedureBinding(view, imported).body,
                    );
                },
                .hosted => null,
            },
            .procedure_template,
            .nested_expr,
            .generated_codec,
            .generated_field_iterator,
            .generated_interpolation_step,
            => null,
        };
    }

    fn pendingCallableEvalExprForBody(
        self: *Builder,
        view: ModuleView,
        body: anytype,
    ) ?CheckedExprIdentity {
        const template_id = switch (body) {
            .direct_template => return null,
            .callable_eval_template => |template| template,
        };
        const template = self.callableEvalTemplate(view, template_id);
        const root = view.compile_time_roots.root(template.root);
        return switch (root.payload) {
            .pending => .{ .module = view.key, .expr = root.expr },
            .fn_value, .const_node, .discarded, .expect => null,
        };
    }

    fn rootProcedureBindingBody(self: *Builder, view: ModuleView, binding_ref: checked.TopLevelProcedureBindingRef) WorkerBody {
        const binding = view.top_level_procedure_bindings.get(binding_ref);
        return switch (binding.body) {
            .direct_template => |direct| switch (direct.template) {
                .checked => |template| self.rootProcedureTemplateBody(template),
                .lifted,
                .synthetic,
                => boxyPlanInvariant("non-checked procedure template reached boxy body type planning"),
            },
            .callable_eval_template => |template| self.callableEvalTemplateBody(view, template),
        };
    }

    fn importedProcedureBindingBody(self: *Builder, binding_ref: checked.ImportedProcedureBindingRef) WorkerBody {
        const view = self.moduleForId(binding_ref.artifact);
        const binding = self.importedProcedureBinding(view, binding_ref);
        return switch (binding.body) {
            .direct_template => |direct| switch (direct.template) {
                .checked => |template| self.rootProcedureTemplateBody(template),
                .lifted,
                .synthetic,
                => boxyPlanInvariant("non-checked imported procedure template reached boxy body type planning"),
            },
            .callable_eval_template => |template| self.callableEvalTemplateBody(view, template),
        };
    }

    fn hostedProcedureBody(self: *Builder, hosted_ref: checked.HostedProcRef) WorkerBody {
        const view = self.moduleForId(checked.hostedProcedureTemplateModuleId(hosted_ref));
        return .{ .hosted_proc = .{
            .view = view,
            .proc = hostedProcForTemplate(view, hosted_ref.template),
        } };
    }

    fn importedProcedureBinding(
        _: *Builder,
        view: ModuleView,
        binding_ref: checked.ImportedProcedureBindingRef,
    ) checked.ImportedProcedureBindingView {
        for (view.exported_procedure_bindings.bindings) |binding| {
            if (moduleKeyEqual(binding.binding.artifact, binding_ref.artifact) and
                binding.binding.def == binding_ref.def and
                binding.binding.pattern == binding_ref.pattern)
            {
                return binding;
            }
        }
        boxyPlanInvariant("imported procedure binding was not exported by its checked module");
    }

    fn rootProcedureTemplateBody(self: *Builder, template_ref: checked_names.ProcedureTemplateRef) WorkerBody {
        const view = self.moduleForCheckedModuleId(template_ref.artifact);
        const template = view.checked_procedure_templates.get(template_ref.template);
        if (template.target == .hosted) {
            return .{ .hosted_proc = .{
                .view = view,
                .proc = hostedProcForTemplate(view, template_ref),
            } };
        }
        return switch (template.body) {
            .checked_body => |body| .{ .checked_expr = .{
                .view = view,
                .root_expr = view.checked_bodies.body(body).root_expr,
            } },
            .intrinsic_wrapper => |wrapper_id| .{ .intrinsic_wrapper = .{
                .view = view,
                .wrapper = view.intrinsic_wrappers.get(wrapper_id),
            } },
            .entry_wrapper => |wrapper_id| .{ .checked_expr = .{
                .view = view,
                .root_expr = view.entry_wrappers.get(wrapper_id).body_expr,
            } },
            .unimplemented => .unimplemented,
        };
    }

    fn callableEvalTemplateBody(
        self: *Builder,
        view: ModuleView,
        template_id: checked.CallableEvalTemplateId,
    ) WorkerBody {
        const template = self.callableEvalTemplate(view, template_id);
        const root = view.compile_time_roots.root(template.root);
        return switch (root.payload) {
            .fn_value => |fn_id| self.constFnValueBody(view, fn_id),
            .pending => boxyPlanInvariant("pending callable eval root reached runtime boxy body type planning before compile-time finalization"),
            .const_node,
            .discarded,
            .expect,
            => boxyPlanInvariant("callable eval binding root did not output a callable value"),
        };
    }

    fn constFnValueBody(
        self: *Builder,
        store_view: ModuleView,
        fn_id: checked.ConstFnId,
    ) WorkerBody {
        const store = store_view.const_store orelse
            boxyPlanInvariant("callable eval function value had no checked ConstStore");
        const raw = @intFromEnum(fn_id);
        if (raw >= store.fns.items.len) {
            boxyPlanInvariant("callable eval function value referenced a missing ConstStore function");
        }
        const fn_value = store.getFn(fn_id);
        var body = switch (fn_value.fn_def) {
            .local_template,
            .imported_template,
            .checked_generated,
            => |template| self.rootProcedureTemplateBody(template),
            .nested => |nested| self.nestedConstFnBody(nested),
            .local_hosted,
            .imported_hosted,
            => boxyPlanInvariant("hosted stored function reached runtime boxy body type planning before hosted wrapper planning"),
            .parser_runtime,
            .encoder_for_runtime,
            => boxyPlanInvariant("generated parser/encoder stored function reached runtime boxy body type planning before generated runtime support"),
        };
        if (fn_value.captures.len != 0) {
            switch (body) {
                .checked_expr => |*checked_body| checked_body.stored_fn = .{
                    .module = store_view.key,
                    .fn_id = fn_id,
                },
                .intrinsic_wrapper,
                .hosted_proc,
                .unimplemented,
                => boxyPlanInvariant("capturing stored function did not resolve to a checked function body"),
            }
        }
        return body;
    }

    fn analyzeStoredFnCaptureTypes(self: *Builder, stored_fn: StoredFnSource) Allocator.Error!void {
        const store_view = self.moduleForId(stored_fn.module);
        const store = store_view.const_store orelse
            boxyPlanInvariant("stored function capture plan had no checked ConstStore");
        const fn_value = store.getFn(stored_fn.fn_id);
        const fn_view = switch (fn_value.fn_def) {
            .nested => |nested| self.moduleForId(.{ .bytes = checked_names.procTemplateModuleDigest(nested.owner).bytes }),
            .local_template,
            .imported_template,
            .local_hosted,
            .imported_hosted,
            .checked_generated,
            .parser_runtime,
            .encoder_for_runtime,
            => boxyPlanInvariant("capturing stored function did not reference a checked nested function"),
        };
        for (fn_value.captures) |capture| {
            if (!capture.id.isCanonical()) continue;
            _ = try self.analyzeType(fn_view, self.checkedBinderType(fn_view, capture.id.binder()));
        }
    }

    fn nestedConstFnBody(self: *Builder, nested: anytype) WorkerBody {
        const view = self.moduleForId(.{ .bytes = checked_names.procTemplateModuleDigest(nested.owner).bytes });
        const expr_id = self.checkedLambdaExprForNestedFn(view, nested);
        return .{ .checked_expr = .{
            .view = view,
            .root_expr = expr_id,
        } };
    }

    fn analyzeIntrinsicWrapperTypes(
        self: *Builder,
        view: ModuleView,
        wrapper: checked.IntrinsicWrapper,
    ) Allocator.Error!void {
        const function = checkedFunctionPayload(view, wrapper.checked_fn_root);
        return switch (wrapper.intrinsic) {
            .str_inspect => {
                if (function.args.len != 1) {
                    boxyPlanInvariant("Str.inspect intrinsic wrapper had an unexpected arity");
                }
                _ = try self.analyzeType(view, function.args[0]);
                _ = try self.analyzeType(view, function.ret);
            },
            .structural_eq => boxyPlanInvariant("structural equality intrinsic wrapper must lower through checked dispatch plans"),
            .field_names_iter => {
                for (function.args) |arg| _ = try self.analyzeType(view, arg);
                _ = try self.analyzeType(view, function.ret);
                try self.planGeneratedFieldIterator(.all);
            },
            .field_names_for_size => {
                for (function.args) |arg| _ = try self.analyzeType(view, arg);
                _ = try self.analyzeType(view, function.ret);
                try self.planGeneratedFieldIterator(.for_size);
            },
            .parse_tag_union,
            .field_names_rename_fields,
            .field_names_shortest_name,
            .field_names_longest_name,
            .field_name,
            => {
                for (function.args) |arg| _ = try self.analyzeType(view, arg);
                _ = try self.analyzeType(view, function.ret);
            },
        };
    }

    fn planGeneratedFieldIterator(
        self: *Builder,
        mode: GeneratedFieldIteratorMode,
    ) Allocator.Error!void {
        const intrinsic_worker = self.active_worker orelse
            boxyPlanInvariant("FieldNames iterator intrinsic was analyzed outside a worker");
        const intrinsic = self.plan.workers.items[@intFromEnum(intrinsic_worker)];
        const function = (self.repQuery().functionChildren(intrinsic.rep)) orelse
            boxyPlanInvariant("FieldNames iterator intrinsic was not callable");
        const expected_arity: u32 = switch (mode) {
            .all => 1,
            .for_size => 2,
        };
        if (function.arg_count != expected_arity) {
            boxyPlanInvariant("FieldNames iterator intrinsic had an unexpected arity");
        }

        const function_children = self.plan.childSlice(
            self.plan.representations.items[@intFromEnum(function.rep)].children,
        );
        const field_names_type = function_children[function.args_start].source_type;
        const iter_type = self.plan.representations.items[@intFromEnum(function.ret)].source_type;
        const size_type: ?CheckedTypeIdentity = switch (mode) {
            .all => null,
            .for_size => function_children[function.args_start + 1].source_type,
        };
        const len_if_known = try self.generatedRecordFieldForRep(function.ret, "len_if_known");
        const index_type = (try self.generatedTagPayloadForRep(len_if_known.rep, "Known")).source_type;
        const step = try self.generatedRecordFieldForRep(function.ret, "step");
        const step_function = (self.repQuery().functionChildren(step.rep)) orelse
            boxyPlanInvariant("FieldNames iterator step field was not callable");
        if (step_function.arg_count != 0) {
            boxyPlanInvariant("FieldNames iterator step worker was not zero-argument");
        }

        const first_step = try self.ensureWorker(.{ .generated_field_iterator = .{
            .mode = mode,
            .field_names_type = field_names_type,
            .iter_type = iter_type,
            .index_type = index_type,
            .size_type = size_type,
        } }, step.source_type, null);
        try self.plan.generated_field_iterator_links.append(self.allocator, .{
            .intrinsic = intrinsic_worker,
            .first_step = first_step,
        });
    }

    fn generatedTagPayloadForRep(
        self: *Builder,
        rep_id: TypeRepId,
        tag_text: []const u8,
    ) Allocator.Error!RepChild {
        var current = try self.tagIdentityRep(rep_id);
        var seen = collections.DenseMap(TypeRepId, void).init(self.allocator);
        defer seen.deinit();
        while (true) {
            const entry = try seen.getOrPut(current);
            if (entry.found_existing) boxyPlanInvariant("generated tag payload lookup encountered a cyclic extension");
            const rep = self.plan.representations.items[@intFromEnum(current)];
            if (rep.kind != .tag_union and rep.kind != .dynamic) {
                boxyPlanInvariant("generated tag payload lookup reached a non-tag representation");
            }
            for (self.plan.tagVariantSlice(rep.tag_variants)) |variant| {
                const module = self.moduleForId(variant.name_module);
                const names = module.canonical_names orelse
                    boxyPlanInvariant("generated tag payload lookup had no checked name store");
                if (!std.mem.eql(u8, names.tagLabelText(variant.name), tag_text)) continue;
                const payloads = self.plan.childSlice(variant.payloads);
                if (payloads.len != 1) boxyPlanInvariant("generated tag did not have one payload");
                return payloads[0];
            }
            var extension: ?TypeRepId = null;
            for (self.plan.childSlice(rep.children)) |child| {
                if (child.role != .tag_ext) continue;
                if (extension != null) boxyPlanInvariant("generated tag payload lookup found duplicate row extensions");
                extension = try self.tagIdentityRep(child.rep);
            }
            current = extension orelse
                boxyPlanInvariant("generated tag union was missing a required tag");
        }
    }

    fn tagIdentityRep(self: *Builder, rep_id: TypeRepId) Allocator.Error!TypeRepId {
        var current = rep_id;
        var seen = collections.DenseMap(TypeRepId, void).init(self.allocator);
        defer seen.deinit();
        while (true) {
            const entry = try seen.getOrPut(current);
            if (entry.found_existing) boxyPlanInvariant("tag representation wrapper chain was cyclic");
            const rep = self.plan.representations.items[@intFromEnum(current)];
            if (rep.kind == .alias) {
                current = requiredSingleChildOf(&self.plan, current, .alias_backing).rep;
            } else if (rep.kind == .nominal) {
                switch (rep.kind.nominal) {
                    .transparent, .builtin_other => current = requiredSingleChildOf(&self.plan, current, .nominal_backing).rep,
                    .opaque_nominal => return current,
                }
            } else {
                return current;
            }
        }
    }

    fn generatedRecordFieldForRep(
        self: *Builder,
        rep_id: TypeRepId,
        field_text: []const u8,
    ) Allocator.Error!RepChild {
        var current = try self.recordIdentityRep(rep_id);
        var seen = collections.DenseMap(TypeRepId, void).init(self.allocator);
        defer seen.deinit();

        while (true) {
            const entry = try seen.getOrPut(current);
            if (entry.found_existing) boxyPlanInvariant("generated record field lookup encountered a cyclic extension");
            const rep = self.plan.representations.items[@intFromEnum(current)];
            if (rep.kind != .record) boxyPlanInvariant("generated record field lookup reached a non-record");
            const view = self.moduleForId(rep.source_type.module);
            var extension: ?TypeRepId = null;
            for (self.plan.childSlice(rep.children)) |child| {
                switch (child.role) {
                    .record_field => |name| {
                        const names = view.canonical_names orelse
                            boxyPlanInvariant("generated record field lookup had no checked name store");
                        if (std.mem.eql(u8, names.recordFieldLabelText(name), field_text)) return child;
                    },
                    .record_ext => extension = try self.recordIdentityRep(child.rep),
                    .alias_backing,
                    .alias_arg,
                    .nominal_backing,
                    .nominal_arg,
                    .nominal_padding_field,
                    .tuple_elem,
                    .function_arg,
                    .function_ret,
                    .tag_payload,
                    .tag_ext,
                    .list_elem,
                    .box_payload,
                    => boxyPlanInvariant("generated record field lookup found a non-record child"),
                }
            }
            current = extension orelse
                boxyPlanInvariant("generated record type was missing a required field");
        }
    }

    fn recordIdentityRep(self: *Builder, rep_id: TypeRepId) Allocator.Error!TypeRepId {
        var current = rep_id;
        var seen = collections.DenseMap(TypeRepId, void).init(self.allocator);
        defer seen.deinit();
        while (true) {
            const entry = try seen.getOrPut(current);
            if (entry.found_existing) boxyPlanInvariant("record representation wrapper chain was cyclic");
            const rep = self.plan.representations.items[@intFromEnum(current)];
            if (rep.kind == .alias) {
                current = requiredSingleChildOf(&self.plan, current, .alias_backing).rep;
            } else if (rep.kind == .nominal) {
                switch (rep.kind.nominal) {
                    .transparent, .builtin_other => current = requiredSingleChildOf(&self.plan, current, .nominal_backing).rep,
                    .opaque_nominal => return current,
                }
            } else {
                return current;
            }
        }
    }

    fn analyzeHostedProcTypes(
        self: *Builder,
        view: ModuleView,
        hosted: checked.HostedProc,
    ) Allocator.Error!void {
        const template = view.checked_procedure_templates.get(hosted.template.template);
        const worker_function = checkedFunctionPayload(view, template.checked_fn_root);
        for (worker_function.args) |arg| {
            _ = try self.analyzeType(view, arg);
        }
        _ = try self.analyzeType(view, worker_function.ret);

        const host_capability = hostedRepresentationForTemplate(view, hosted.template);
        const host_function = checkedFunctionPayload(view, host_capability.host_checked_fn_root);
        if (host_function.args.len != worker_function.args.len) {
            boxyPlanInvariant("hosted host signature arity disagreed with worker signature");
        }
        _ = try self.analyzeType(view, host_capability.host_checked_fn_root);
        for (host_function.args) |arg| {
            _ = try self.analyzeType(view, arg);
        }
        _ = try self.analyzeType(view, host_function.ret);
    }

    fn checkedLambdaExprForNestedFn(
        _: *Builder,
        view: ModuleView,
        nested: anytype,
    ) checked.CheckedExprId {
        for (view.nested_proc_sites.sites) |site| {
            if (site.site != nested.site) continue;
            if (!checked_names.procedureTemplateRefEql(site.owner_template, nested.owner)) continue;
            const expr_id = site.checked_expr orelse
                boxyPlanInvariant("stored nested function had no checked expression site");
            const expr = view.checked_bodies.expr(expr_id);
            if (expr.data == .lambda) return expr_id;
            if (expr.data == .closure) return expr.data.closure.lambda;
            boxyPlanInvariant("stored nested function site did not point at a lambda or closure");
        }
        boxyPlanInvariant("stored nested function referenced a missing checked nested site");
    }

    fn checkedBinderType(_: *Builder, view: ModuleView, binder: checked.PatternBinderId) checked.CheckedTypeId {
        const raw = @intFromEnum(binder);
        if (raw >= view.checked_bodies.patternBinderCount()) {
            boxyPlanInvariant("stored function capture binder was outside the checked body store");
        }
        const pattern = view.checked_bodies.patternBinder(@enumFromInt(raw)).pattern;
        if (@intFromEnum(pattern) >= view.checked_bodies.patternCount()) {
            boxyPlanInvariant("stored function capture pattern was outside the checked body store");
        }
        return view.checked_bodies.pattern(pattern).ty;
    }

    fn callableEvalTemplate(
        _: *Builder,
        view: ModuleView,
        template_id: checked.CallableEvalTemplateId,
    ) checked.CallableEvalTemplate {
        const raw = @intFromEnum(template_id);
        if (raw >= view.callable_eval_templates.templates.len) {
            boxyPlanInvariant("callable eval binding referenced a missing checked template");
        }
        return view.callable_eval_templates.templates[raw];
    }

    fn analyzeExprTypes(self: *Builder, view: ModuleView, expr_id: checked.CheckedExprId) Allocator.Error!void {
        const worker = self.active_worker orelse
            boxyPlanInvariant("checked expression was analyzed outside a worker body");
        const entry = try self.body_exprs_seen.getOrPut(.{
            .expr = .{ .module = view.key, .expr = expr_id },
            .worker = worker,
        });
        if (entry.found_existing) return;

        const bodies = view.checked_bodies;
        const expr = bodies.expr(expr_id);
        _ = try self.analyzeType(view, expr.ty);

        switch (expr.data) {
            .pending => boxyPlanInvariant("pending checked expression reached boxy body type planning"),
            .numeral => |numeral| try self.analyzeNumeralConversionTypes(view, expr_id, numeral.plan),
            .str_segment,
            .bytes_literal,
            .empty_list,
            .empty_record,
            .zero_argument_tag,
            .runtime_error,
            .crash,
            .ellipsis,
            .anno_only,
            .break_,
            => {},
            .str_from_quote => |quote| try self.analyzeQuoteConversionTypes(view, expr_id, quote.plan),
            .lookup_local,
            .lookup_external,
            .lookup_required,
            => try self.analyzeCallableLookupWorker(view, expr_id),
            .str,
            .list,
            .tuple,
            => |items| try self.analyzeExprSliceTypes(view, items),
            .match_ => |match| {
                try self.analyzeExprTypes(view, match.cond);
                for (match.branches) |branch| {
                    for (branch.patternsSlice(bodies)) |branch_pattern| {
                        try self.analyzePatternTypes(view, branch_pattern.pattern);
                    }
                    if (branch.guard) |guard| try self.analyzeExprTypes(view, guard);
                    try self.analyzeExprTypes(view, branch.value);
                }
            },
            .if_ => |if_| {
                for (if_.branches) |branch| {
                    try self.analyzeExprTypes(view, branch.cond);
                    try self.analyzeExprTypes(view, branch.body);
                }
                try self.analyzeExprTypes(view, if_.final_else);
            },
            .call => |call| {
                const local_proc_direct_target = if (call.direct_target) |target|
                    self.directTargetIsLocalProc(view, target)
                else
                    false;
                const local_proc_worker_call = local_proc_direct_target and self.directTargetHasNoCaptures(view, call.direct_target.?);
                if (call.direct_target == null or (local_proc_direct_target and !local_proc_worker_call)) {
                    try self.analyzeExprTypes(view, call.func);
                } else {
                    const func = view.checked_bodies.expr(call.func);
                    _ = try self.analyzeType(view, func.ty);
                }
                try self.analyzeExprSliceTypes(view, call.args);
                _ = try self.analyzeType(view, call.source_fn_ty_payload);
                if (local_proc_worker_call) {
                    try self.analyzeDirectCallTarget(view, expr_id, call);
                } else if (local_proc_direct_target) {
                    try self.recordNestedCallableUse(view, call.direct_target.?, call.func);
                } else {
                    try self.analyzeDirectCallTarget(view, expr_id, call);
                }
            },
            .record => |record| {
                if (record.ext) |ext| try self.analyzeExprTypes(view, ext);
                for (record.fields) |field| try self.analyzeExprTypes(view, field.value);
            },
            .block => |block| {
                for (block.statements) |statement| try self.analyzeStatementTypes(view, statement);
                try self.analyzeExprTypes(view, block.final_expr);
            },
            .tag => |tag| try self.analyzeExprSliceTypes(view, tag.args),
            .nominal => |nominal| try self.analyzeExprTypes(view, nominal.backing_expr),
            .closure => |closure| {
                try self.recordNestedCallableExprUse(view, expr_id);
                for (closure.captures) |capture| try self.analyzePatternTypes(view, capture.pattern);
            },
            .lambda => |lambda| {
                try self.recordNestedCallableExprUse(view, expr_id);
                for (lambda.args) |arg| try self.analyzePatternTypes(view, arg);
            },
            .binop => |binop| {
                try self.analyzeExprTypes(view, binop.lhs);
                try self.analyzeExprTypes(view, binop.rhs);
            },
            .unary_minus,
            .unary_not,
            .dbg,
            .expect,
            => |child| try self.analyzeExprTypes(view, child),
            .field_access => |access| try self.analyzeExprTypes(view, access.receiver),
            .interpolation => |interpolation| {
                try self.analyzeExprTypes(view, interpolation.first);
                for (interpolation.parts) |part| {
                    try self.analyzeExprTypes(view, part.value);
                    try self.analyzeExprTypes(view, part.following_segment);
                }
                _ = try self.analyzeType(view, interpolation.step_fn_ty);
                try self.analyzeDispatchCallTarget(view, expr_id, interpolation.plan);
            },
            .structural_eq => |eq| {
                try self.analyzeExprTypes(view, eq.lhs);
                try self.analyzeExprTypes(view, eq.rhs);
            },
            .structural_hash => |hash| {
                try self.analyzeExprTypes(view, hash.value);
                try self.analyzeExprTypes(view, hash.hasher);
            },
            .dispatch_call => |plan| try self.analyzeDispatchCallTarget(view, expr_id, plan),
            .type_dispatch_call => |plan| try self.analyzeDispatchCallTarget(view, expr_id, plan),
            .method_eq => |plan| try self.analyzeDispatchCallTarget(view, expr_id, plan),
            .tuple_access => |access| try self.analyzeExprTypes(view, access.tuple),
            .expect_err => |expect_err| {
                try self.analyzeExprTypes(view, expect_err.expr);
                const child_expr = view.checked_bodies.expr(expect_err.expr);
                _ = try self.analyzeType(view, child_expr.ty);
            },
            .return_ => |ret| try self.analyzeExprTypes(view, ret.expr),
            .for_ => |for_| {
                try self.analyzeIteratorForPlan(view, for_.plan);
                try self.analyzePatternTypes(view, for_.pattern);
                try self.analyzeExprTypes(view, for_.expr);
                try self.analyzeExprTypes(view, for_.body);
            },
            .hosted_lambda => |hosted| for (hosted.args) |arg| try self.analyzePatternTypes(view, arg),
            .run_low_level => |run| try self.analyzeExprSliceTypes(view, run.args),
        }
    }

    fn analyzeQuoteConversionTypes(
        self: *Builder,
        view: ModuleView,
        expr_id: checked.CheckedExprId,
        maybe_plan: ?static_dispatch.StaticDispatchPlanId,
    ) Allocator.Error!void {
        const root = view.compile_time_roots.lookupNumeralRootByExpr(expr_id) orelse
            boxyPlanInvariant("checked from_quote expression had no compile-time conversion root");
        switch (root.payload) {
            .const_node => |node| {
                const store = view.const_store orelse
                    boxyPlanInvariant("finalized from_quote conversion module had no ConstStore");
                if (store.get(node) == .crash) return;
                const expr = view.checked_bodies.expr(expr_id);
                const rep = try self.analyzeType(view, expr.ty);
                var visited = std.AutoHashMap(StaticConstVisit, void).init(self.allocator);
                defer visited.deinit();
                try self.analyzeStaticConstNode(view, node, rep, null, &visited);
            },
            .pending => try self.analyzeDispatchCallTarget(view, expr_id, maybe_plan),
            .fn_value,
            .discarded,
            .expect,
            => boxyPlanInvariant("from_quote conversion root had a non-data payload"),
        }
    }

    fn analyzeNumeralConversionTypes(
        self: *Builder,
        view: ModuleView,
        expr_id: checked.CheckedExprId,
        maybe_plan: ?static_dispatch.StaticDispatchPlanId,
    ) Allocator.Error!void {
        const root = view.compile_time_roots.lookupNumeralRootByExpr(expr_id) orelse return;
        switch (root.payload) {
            .const_node => |node| {
                const store = view.const_store orelse
                    boxyPlanInvariant("finalized from_numeral conversion module had no ConstStore");
                if (store.get(node) == .crash) return;
                const expr = view.checked_bodies.expr(expr_id);
                const rep = try self.analyzeType(view, expr.ty);
                var visited = std.AutoHashMap(StaticConstVisit, void).init(self.allocator);
                defer visited.deinit();
                try self.analyzeStaticConstNode(view, node, rep, null, &visited);
            },
            .pending => try self.analyzeDispatchCallTarget(view, expr_id, maybe_plan),
            .fn_value,
            .discarded,
            .expect,
            => boxyPlanInvariant("from_numeral conversion root had a non-data payload"),
        }
    }

    fn analyzeCallableLookupWorker(
        self: *Builder,
        view: ModuleView,
        expr_id: checked.CheckedExprId,
    ) Allocator.Error!void {
        const expr = view.checked_bodies.expr(expr_id);
        const maybe_ref: ?checked.ResolvedValueRefId = if (expr.data == .lookup_local)
            expr.data.lookup_local.resolved
        else if (expr.data == .lookup_external)
            expr.data.lookup_external
        else if (expr.data == .lookup_required)
            expr.data.lookup_required
        else
            boxyPlanInvariant("non-lookup expression reached callable lookup worker planning");
        const ref_id = maybe_ref orelse return;
        try self.analyzeConstDefinitionTypes(view, ref_id);
        const stored_fn = self.storedFnSourceForProcedureValueRef(view, ref_id);
        const source = if (stored_fn) |stored|
            try self.workerSourceForStoredFnAtType(stored, typeRef(view, expr.ty))
        else
            self.workerSourceForProcedureValueRef(view, ref_id) orelse return;
        if (self.pendingCallableEvalExprForSource(source)) |runtime_source| {
            const source_view = self.moduleForId(runtime_source.module);
            try self.analyzeExprTypes(source_view, runtime_source.expr);
            const use = CheckedExprIdentity{ .module = view.key, .expr = expr_id };
            const caller = self.active_worker orelse
                boxyPlanInvariant("boxy runtime callable evaluation was analyzed outside a worker body");
            if (self.plan.runtimeCallableEvalUsePlan(use, caller) == null) {
                try self.plan.runtime_callable_eval_uses.append(self.allocator, .{
                    .use = use,
                    .caller = caller,
                    .source = runtime_source,
                    .callable_ty = typeRef(view, expr.ty),
                });
            }
            return;
        }
        const checked_type = if (source == .nested_expr)
            self.workerCheckedTypeForSource(source, typeRef(view, expr.ty))
        else
            typeRef(view, expr.ty);
        const worker = try self.ensureWorker(source, checked_type, null);
        if (stored_fn) |stored| try self.analyzeStoredFnCaptureNodes(stored, worker);
        const use = CheckedExprIdentity{ .module = view.key, .expr = expr_id };
        const caller = self.active_worker orelse
            boxyPlanInvariant("boxy callable lookup was analyzed outside a worker body");
        if (self.plan.callableUsePlan(use, caller) == null) {
            try self.plan.callable_uses.append(self.allocator, .{
                .use = use,
                .caller = caller,
                .worker = worker,
                .callable_ty = typeRef(view, expr.ty),
                .stored_fn = stored_fn,
            });
        }
    }

    fn workerSourceForStoredFnAtType(
        self: *Builder,
        stored_fn: StoredFnSource,
        requested_type: CheckedTypeIdentity,
    ) Allocator.Error!WorkerSource {
        const store_view = self.moduleForId(stored_fn.module);
        const store = store_view.const_store orelse
            boxyPlanInvariant("stored callable worker selection had no ConstStore");
        if (@intFromEnum(stored_fn.fn_id) >= store.fns.items.len) {
            boxyPlanInvariant("stored callable worker selection referenced a missing function");
        }
        const fn_value = store.getFn(stored_fn.fn_id);
        const generated: ?struct {
            owner: checked_names.ProcedureTemplateRef,
            expr: checked.CheckedExprId,
            kind: GeneratedCodecKind,
            derivation_kind: static_dispatch.GeneratedCodecDerivationKind,
        } = switch (fn_value.fn_def) {
            .parser_runtime => |runtime| .{
                .owner = runtime.owner,
                .expr = runtime.expr,
                .kind = .parser_runtime,
                .derivation_kind = .parser,
            },
            .encoder_for_runtime => |runtime| .{
                .owner = runtime.owner,
                .expr = runtime.expr,
                .kind = .encoder_runtime,
                .derivation_kind = .encoder,
            },
            .local_template,
            .imported_template,
            .nested,
            .local_hosted,
            .imported_hosted,
            .checked_generated,
            => null,
        };
        const codec = generated orelse return self.workerSourceForConstFnValue(fn_value, requested_type);
        var stored_encoding_type: ?check.ConstStore.ConstTypeId = null;
        for (fn_value.captures) |capture| {
            if (capture.id != checked.CaptureId.generatedCheck(0)) continue;
            if (stored_encoding_type != null) {
                boxyPlanInvariant("stored generated codec had duplicate encoding captures");
            }
            stored_encoding_type = capture.ty;
        }
        const view = self.moduleForId(.{ .bytes = checked_names.procTemplateModuleDigest(codec.owner).bytes });
        const source = if (stored_encoding_type) |encoding_type|
            try self.storedGeneratedCodecSourceAtEncodingType(
                store_view,
                encoding_type,
                view,
                codec.expr,
                requested_type.ty,
                codec.kind,
                codec.derivation_kind,
            )
        else
            self.storedGeneratedCodecSource(
                view,
                codec.expr,
                requested_type.ty,
                codec.kind,
                codec.derivation_kind,
            );
        return .{ .generated_codec = source };
    }

    fn storedGeneratedCodecSourceAtEncodingType(
        self: *Builder,
        store_view: ModuleView,
        stored_encoding_type: check.ConstStore.ConstTypeId,
        view: ModuleView,
        expr_id: checked.CheckedExprId,
        requested_runtime_type: checked.CheckedTypeId,
        kind: GeneratedCodecKind,
        derivation_kind: static_dispatch.GeneratedCodecDerivationKind,
    ) Allocator.Error!GeneratedCodecSource {
        var found: ?static_dispatch.GeneratedCodecDerivation = null;
        for (view.static_dispatch_plans.generated_codec_derivations) |derivation| {
            if (derivation.kind != derivation_kind or derivation.source_runtime_ty != requested_runtime_type) continue;
            if (!try self.storedTypeMatchesCheckedType(store_view, stored_encoding_type, view, derivation.encoding_ty)) continue;
            if (found) |existing| {
                if (!generatedCodecDerivationsEql(view.static_dispatch_plans, existing, derivation)) {
                    boxyPlanInvariant("stored generated codec encoding matched multiple checked derivations");
                }
                continue;
            }
            found = derivation;
        }
        const derivation = found orelse
            boxyPlanInvariant("stored generated codec encoding had no checked derivation");
        return .{
            .kind = kind,
            .shape = typeRef(view, derivation.shape_ty),
            .runtime_type = typeRef(view, derivation.runtime_ty),
            .capture_type = typeRef(view, derivation.encoding_ty),
            .contract_expr = .{ .module = view.key, .expr = expr_id },
        };
    }

    fn analyzeStoredFnCaptureNodes(
        self: *Builder,
        stored_fn: StoredFnSource,
        worker: WorkerPlanId,
    ) Allocator.Error!void {
        const store_view = self.moduleForId(stored_fn.module);
        const store = store_view.const_store orelse
            boxyPlanInvariant("stored callable capture planning had no ConstStore");
        if (@intFromEnum(stored_fn.fn_id) >= store.fns.items.len) {
            boxyPlanInvariant("stored callable capture planning referenced a missing function");
        }
        const fn_value = store.getFn(stored_fn.fn_id);
        var visited = std.AutoHashMap(StaticConstVisit, void).init(self.allocator);
        defer visited.deinit();
        try self.analyzeConstFnCaptures(store_view, fn_value, worker, &visited);
    }

    /// Analyze the exact producer representation through which lowering restores
    /// this constant. The checked root supplies source identity while
    /// `StoredConstTemplate.root_type` proves that it is the producer-owned
    /// monomorphic representation stored beside the value.
    fn analyzeConstDefinitionTypes(
        self: *Builder,
        view: ModuleView,
        ref_id: checked.ResolvedValueRefId,
    ) Allocator.Error!void {
        const record = self.resolvedValueRecord(view, ref_id);
        const const_use = if (record.ref == .selected_hoisted_const)
            record.ref.selected_hoisted_const.const_use
        else if (record.ref == .top_level_const)
            record.ref.top_level_const
        else if (record.ref == .imported_const)
            record.ref.imported_const
        else if (record.ref == .platform_required_const)
            record.ref.platform_required_const.const_use
        else
            return;
        // The lowerer restores the const through its use-site requested type
        // (`restoreConstUseInto`), so that type's representation must be
        // planned. The enclosing expression's type is not always the same
        // node as the requested payload type in module graphs whose root is
        // not the app itself.
        if (const_use.requested_source_ty_payload) |requested_ty| {
            _ = try self.analyzeType(view, requested_ty);
        }
        const store_view = self.moduleForId(checked.constModuleId(const_use.const_ref));
        // A constant whose value is computed by evaluating a body (rather than
        // a compile-time-stored value) is produced at runtime by calling the
        // constant's entry-wrapper thunk. Plan that thunk as a worker so the
        // lowerer can emit the call.
        var stored_template: ?checked.StoredConstTemplate = null;
        if (store_view.const_templates) |const_templates| {
            switch (const_templates.get(const_use.const_ref).state) {
                .eval_template => |eval| {
                    const worker = try self.ensureWorker(
                        .{ .procedure_template = eval.entry_template },
                        self.checkedTypeForTemplate(eval.entry_template),
                        null,
                    );
                    if (const_use.requested_source_ty_payload) |requested_ty| {
                        const ret_type = typeRef(view, requested_ty);
                        if (self.plan.constEvalCallFor(worker, ret_type) == null) {
                            const worker_plan = self.plan.workers.items[@intFromEnum(worker)];
                            const worker_function = (self.repQuery().functionChildren(worker_plan.rep)) orelse
                                boxyPlanInvariant("boxy const-eval worker was not a function");
                            if (worker_function.arg_count != 0) {
                                boxyPlanInvariant("boxy const-eval worker had explicit arguments");
                            }
                            const call_rep = try self.analyzeType(view, requested_ty);
                            try self.plan.const_eval_calls.append(self.allocator, .{
                                .worker = worker,
                                .ret_type = ret_type,
                                .ret_substitution = .{
                                    .operand_type = self.plan.representations.items[@intFromEnum(worker_function.ret)].source_type,
                                    .operand_rep = worker_function.ret,
                                    .call_type = ret_type,
                                    .call_rep = call_rep,
                                    .worker_rep = worker_function.ret,
                                },
                            });
                        }
                    }
                },
                .stored_const => |stored| stored_template = stored,
                // Neither a reserved template nor a declaration without an
                // implementation contributes a value to plan for.
                .reserved, .unimplemented => {},
            }
        }
        if (stored_template) |stored| {
            const producer_ty = self.constProducerCheckedType(store_view, const_use.const_ref);
            const producer_rep = try self.analyzeStoredType(
                store_view,
                stored.root_type,
                typeRef(store_view, producer_ty),
            );
            var visited = std.AutoHashMap(StaticConstVisit, void).init(self.allocator);
            defer visited.deinit();
            try self.analyzeStaticConstNode(store_view, stored.node, producer_rep, stored.root_type, &visited);
        }
    }

    fn constProducerCheckedType(
        _: *Builder,
        store_view: ModuleView,
        const_ref: checked.ConstRef,
    ) checked.CheckedTypeId {
        const root = switch (const_ref.owner) {
            .top_level_binding => |owner| blk: {
                const root_id = store_view.compile_time_roots.lookupIdByPattern(owner.pattern) orelse
                    boxyPlanInvariant("stored top-level constant had no compile-time root");
                const root = store_view.compile_time_roots.root(root_id);
                if (root.kind != .constant or root.module_idx != owner.module_idx or root.pattern != owner.pattern) {
                    boxyPlanInvariant("stored top-level constant owner disagreed with its compile-time root");
                }
                break :blk root;
            },
            .hoisted_expr => |owner| blk: {
                const root = store_view.compile_time_roots.lookupHoistedRootByExpr(owner.expr) orelse
                    boxyPlanInvariant("stored hoisted constant had no compile-time root");
                if (root.module_idx != owner.module_idx) {
                    boxyPlanInvariant("stored hoisted constant owner disagreed with its compile-time root");
                }
                break :blk root;
            },
        };
        return root.checked_type;
    }

    fn ensureNestedCallableWorker(
        self: *Builder,
        view: ModuleView,
        expr_id: checked.CheckedExprId,
    ) Allocator.Error!WorkerPlanId {
        const source = WorkerSource{ .nested_expr = .{ .module = view.key, .expr = expr_id } };
        return try self.ensureWorker(source, self.workerCheckedTypeForSource(source, typeRef(view, view.checked_bodies.expr(expr_id).ty)), null);
    }

    fn recordNestedCallableExprUse(
        self: *Builder,
        view: ModuleView,
        expr_id: checked.CheckedExprId,
    ) Allocator.Error!void {
        const caller = self.active_worker orelse
            boxyPlanInvariant("boxy nested callable value was analyzed outside a worker body");
        return try self.recordNestedCallableExprUseForCaller(
            view,
            expr_id,
            typeRef(view, view.checked_bodies.expr(expr_id).ty),
            caller,
        );
    }

    fn recordNestedCallableExprUseForCaller(
        self: *Builder,
        view: ModuleView,
        expr_id: checked.CheckedExprId,
        callable_ty: CheckedTypeIdentity,
        caller: WorkerPlanId,
    ) Allocator.Error!void {
        const use = CheckedExprIdentity{ .module = view.key, .expr = expr_id };
        for (self.plan.nested_callable_uses.items) |planned| {
            if (planned.caller == caller and
                exprRefEql(planned.use, use) and
                typeRefEql(planned.callable_ty, callable_ty))
            {
                return;
            }
        }
        const worker = try self.ensureNestedCallableWorker(view, expr_id);
        try self.plan.nested_callable_uses.append(self.allocator, .{
            .use = use,
            .caller = caller,
            .worker = worker,
            .callable_ty = callable_ty,
        });
    }

    fn recordNestedCallableUse(
        self: *Builder,
        view: ModuleView,
        target: checked.ResolvedValueId,
        func: checked.CheckedExprId,
    ) Allocator.Error!void {
        const source = self.workerSourceForDirectTarget(view, target);
        if (source != .nested_expr) return;
        const callable_ty = typeRef(view, view.checked_bodies.expr(func).ty);
        const worker = try self.ensureWorker(source, self.workerCheckedTypeForSource(source, callable_ty), null);
        const caller = self.active_worker orelse
            boxyPlanInvariant("boxy nested callable use was analyzed outside a worker body");
        try self.plan.nested_callable_uses.append(self.allocator, .{
            .use = .{ .module = view.key, .expr = func },
            .caller = caller,
            .worker = worker,
            .callable_ty = callable_ty,
        });
    }

    fn planNestedCallableUseDictionaries(self: *Builder) Allocator.Error!void {
        var callable_index: usize = 0;
        while (callable_index < self.plan.callable_uses.items.len) : (callable_index += 1) {
            const use = self.plan.callable_uses.items[callable_index];
            const worker = self.plan.workers.items[@intFromEnum(use.worker)];
            const callable_rep = self.plan.repForSourceType(use.callable_ty) orelse
                boxyPlanInvariant("boxy callable lookup type was not analyzed");
            const fn_children = (self.repQuery().functionChildren(callable_rep)) orelse
                boxyPlanInvariant("boxy callable lookup type was not callable");

            const arg_types = try self.allocator.alloc(CheckedTypeIdentity, fn_children.arg_count);
            defer self.allocator.free(arg_types);
            const children_span = self.plan.representations.items[@intFromEnum(fn_children.rep)].children;
            var arg_index: usize = 0;
            while (arg_index < fn_children.arg_count) : (arg_index += 1) {
                const child = self.plan.children.items[children_span.start + fn_children.args_start + arg_index];
                arg_types[arg_index] = self.plan.representations.items[@intFromEnum(child.rep)].source_type;
            }
            const ret_type = self.plan.representations.items[@intFromEnum(fn_children.ret)].source_type;
            const view = self.moduleForId(use.use.module);
            const evidence = view.static_dispatch_plans.siteEvidence(use.use.expr);
            if (worker.hidden_dicts.len == 0) {
                self.plan.callable_uses.items[callable_index].hidden_dict_args = .{};
                continue;
            }
            self.plan.callable_uses.items[callable_index].hidden_dict_args =
                try self.materializeWorkerCallHiddenDictionaryArgsWithEvidence(
                    use.worker,
                    use.caller,
                    arg_types,
                    ret_type,
                    view,
                    evidence,
                );
        }

        // Materialize checked instantiation edges first. A local callable's
        // declaration expression itself is not an instantiation and therefore
        // has no site evidence; it is handled in the second pass below.
        var index: usize = 0;
        while (index < self.plan.nested_callable_uses.items.len) : (index += 1) {
            const use = self.plan.nested_callable_uses.items[index];
            const worker = self.plan.workers.items[@intFromEnum(use.worker)];
            if (worker.hidden_dicts.len == 0) {
                self.plan.nested_callable_uses.items[index].hidden_dict_args = .{};
                continue;
            }
            const view = self.moduleForId(use.use.module);
            const evidence = view.static_dispatch_plans.siteEvidence(use.use.expr);
            if (evidence == null and typeRefEql(use.callable_ty, worker.checked_type)) continue;
            self.plan.nested_callable_uses.items[index].hidden_dict_args =
                try self.materializeNestedCallableUseDictionaries(use, view, evidence);
        }

        const dictionary_use_count = self.worker_dictionary_uses.items.len;
        index = 0;
        while (index < self.plan.nested_callable_uses.items.len) : (index += 1) {
            const use = self.plan.nested_callable_uses.items[index];
            const worker = self.plan.workers.items[@intFromEnum(use.worker)];
            if (worker.hidden_dicts.len == 0 or use.hidden_dict_args.len != 0) continue;

            var source: ?Span = null;
            var source_is_ambiguous = false;
            for (self.plan.nested_callable_uses.items) |candidate| {
                if (candidate.worker != use.worker or candidate.caller != use.caller) continue;
                if (candidate.hidden_dict_args.len != worker.hidden_dicts.len) continue;
                if (source) |existing| {
                    const existing_args = self.plan.directCallHiddenDictionaryArgSlice(existing);
                    const candidate_args = self.plan.directCallHiddenDictionaryArgSlice(candidate.hidden_dict_args);
                    if (!std.meta.eql(existing_args, candidate_args)) {
                        source = null;
                        source_is_ambiguous = true;
                        break;
                    }
                } else {
                    source = candidate.hidden_dict_args;
                }
            }
            if (!source_is_ambiguous) {
                for (self.plan.direct_calls.items) |candidate| {
                    if (candidate.worker != use.worker or candidate.caller != use.caller) continue;
                    if (candidate.hidden_dict_args.len != worker.hidden_dicts.len) continue;
                    if (source) |existing| {
                        const existing_args = self.plan.directCallHiddenDictionaryArgSlice(existing);
                        const candidate_args = self.plan.directCallHiddenDictionaryArgSlice(candidate.hidden_dict_args);
                        if (!std.meta.eql(existing_args, candidate_args)) {
                            source = null;
                            source_is_ambiguous = true;
                            break;
                        }
                    } else {
                        source = candidate.hidden_dict_args;
                    }
                }
            }
            if (source) |planned| {
                self.plan.nested_callable_uses.items[index].hidden_dict_args = planned;
                continue;
            }

            if (self.workerBindsAllDictionaryParams(use.caller, worker.hidden_dicts)) {
                const view = self.moduleForId(use.use.module);
                self.plan.nested_callable_uses.items[index].hidden_dict_args =
                    try self.materializeNestedCallableUseDictionaries(use, view, null);
                continue;
            }

            for (self.plan.hiddenDictionaryParamSlice(worker.hidden_dicts)) |param| {
                if (self.workerBindsDictionarySpan(use.caller, param.dictionaries)) continue;
                try self.recordWorkerDictionaryUse(use.caller, param.rep);
            }
        }
        if (self.worker_dictionary_uses.items.len != dictionary_use_count) return;

        for (self.plan.nested_callable_uses.items) |use| {
            const worker = self.plan.workers.items[@intFromEnum(use.worker)];
            if (worker.hidden_dicts.len != 0 and use.hidden_dict_args.len == 0) {
                boxyPlanInvariant("nested callable value had no checked dictionary capture source");
            }
        }
    }

    fn materializeNestedCallableUseDictionaries(
        self: *Builder,
        use: NestedCallableUsePlan,
        view: ModuleView,
        evidence: ?[]const static_dispatch.CheckedEvidence,
    ) Allocator.Error!Span {
        const callable_rep = self.plan.repForSourceType(use.callable_ty) orelse
            boxyPlanInvariant("boxy nested callable use type was not analyzed");
        const function = (self.repQuery().functionChildren(callable_rep)) orelse
            boxyPlanInvariant("boxy nested callable use type was not callable");
        const arg_types = try self.allocator.alloc(CheckedTypeIdentity, function.arg_count);
        defer self.allocator.free(arg_types);
        const children_span = self.plan.representations.items[@intFromEnum(function.rep)].children;
        for (arg_types, 0..) |*arg_type, arg_index| {
            const child = self.plan.children.items[children_span.start + function.args_start + arg_index];
            arg_type.* = self.plan.representations.items[@intFromEnum(child.rep)].source_type;
        }
        const ret_type = self.plan.representations.items[@intFromEnum(function.ret)].source_type;
        return try self.materializeWorkerCallHiddenDictionaryArgsWithEvidence(
            use.worker,
            use.caller,
            arg_types,
            ret_type,
            view,
            evidence,
        );
    }

    fn workerBindsAllDictionaryParams(self: *const Builder, worker_id: WorkerPlanId, params: Span) bool {
        for (self.plan.hiddenDictionaryParamSlice(params)) |param| {
            if (!self.workerBindsDictionarySpan(worker_id, param.dictionaries)) return false;
        }
        return true;
    }

    fn analyzeExprSliceTypes(self: *Builder, view: ModuleView, exprs: []const checked.CheckedExprId) Allocator.Error!void {
        for (exprs) |expr| try self.analyzeExprTypes(view, expr);
    }

    fn analyzeDispatchPlanTypes(
        self: *Builder,
        view: ModuleView,
        maybe_plan: ?static_dispatch.StaticDispatchPlanId,
    ) Allocator.Error!void {
        const plan_id = maybe_plan orelse
            boxyPlanInvariant("checked dispatch expression reached boxy planning without a dispatch plan");
        const raw = @intFromEnum(plan_id);
        if (raw >= view.static_dispatch_plans.plans.len) {
            boxyPlanInvariant("checked dispatch expression referenced a missing dispatch plan");
        }
        const plan = view.static_dispatch_plans.plans[raw];
        _ = try self.analyzeType(view, plan.dispatcher_ty);
        _ = try self.analyzeType(view, plan.callable_ty);
        const operands = plan.argsSlice(view.static_dispatch_plans);
        const callable = checkedFunctionPayload(view, plan.callable_ty);
        if (operands.len != callable.args.len) {
            boxyPlanInvariant("checked dispatch operand arity disagreed with its callable type");
        }
        for (operands, callable.args) |operand, formal_ty| {
            switch (operand) {
                .checked_expr => |operand_expr| try self.analyzeExprTypes(view, operand_expr),
                .generated_interpolation_iter => |operand_expr| {
                    try self.analyzeExprTypes(view, operand_expr);
                    try self.planGeneratedInterpolation(view, operand_expr, formal_ty);
                },
                .generated_numeral,
                .generated_quote,
                => {},
            }
        }
    }

    fn planGeneratedInterpolation(
        self: *Builder,
        view: ModuleView,
        expr_id: checked.CheckedExprId,
        iter_ty: checked.CheckedTypeId,
    ) Allocator.Error!void {
        const caller = self.active_worker orelse
            boxyPlanInvariant("generated interpolation was planned outside a worker body");
        const identity = CheckedExprIdentity{ .module = view.key, .expr = expr_id };
        if (self.plan.generatedInterpolationPlan(identity, caller) != null) return;

        const expr = view.checked_bodies.expr(expr_id);
        if (expr.data != .interpolation) {
            boxyPlanInvariant("generated interpolation operand pointed at a non-interpolation expression");
        }
        const interpolation = expr.data.interpolation;
        const iter_rep = try self.analyzeType(view, iter_ty);
        const step = try self.generatedRecordFieldForRep(iter_rep, "step");
        const step_function = (self.repQuery().functionChildren(step.rep)) orelse
            boxyPlanInvariant("generated interpolation iterator step field was not callable");
        if (step_function.arg_count != 0) {
            boxyPlanInvariant("generated interpolation iterator step worker was not zero-argument");
        }
        const source_step_type = typeRef(view, interpolation.step_fn_ty);
        const source_step_rep = try self.analyzeType(view, interpolation.step_fn_ty);
        const source_step_function = (self.repQuery().functionChildren(source_step_rep)) orelse
            boxyPlanInvariant("checked generated interpolation step type was not callable");
        if (source_step_function.arg_count != 0) {
            boxyPlanInvariant("checked generated interpolation step worker was not zero-argument");
        }
        const one_payload = try self.generatedTagPayloadForRep(source_step_function.ret, "One");
        const one_step = try self.ensureWorker(.{ .generated_interpolation_step = .{
            .step_type = source_step_type,
            .one_payload_type = one_payload.source_type,
        } }, source_step_type, null);
        const done_step = try self.ensureWorker(.{ .generated_interpolation_step = .{
            .step_type = source_step_type,
        } }, source_step_type, null);
        try self.plan.generated_interpolations.append(self.allocator, .{
            .interpolation = identity,
            .caller = caller,
            .iter_rep = iter_rep,
            .one_step = one_step,
            .done_step = done_step,
        });
    }

    fn appendCheckedCallOperands(
        self: *Builder,
        args: []const checked.CheckedExprId,
    ) Allocator.Error!Span {
        const start: u32 = @intCast(self.plan.call_operands.items.len);
        for (args) |arg| try self.plan.call_operands.append(self.allocator, .{ .checked_expr = arg });
        return .{ .start = start, .len = @intCast(args.len) };
    }

    fn appendDispatchCallOperands(
        self: *Builder,
        dispatch: static_dispatch.StaticDispatchCallPlan,
        table: *const static_dispatch.StaticDispatchPlanTable,
    ) Allocator.Error!Span {
        const operands = dispatch.argsSlice(table);
        const start: u32 = @intCast(self.plan.call_operands.items.len);
        for (operands) |operand| {
            try self.plan.call_operands.append(self.allocator, switch (operand) {
                .checked_expr => |expr| .{ .checked_expr = expr },
                .generated_interpolation_iter => |expr| .{ .generated_interpolation_iter = expr },
                .generated_numeral => |literal| .{ .generated_numeral = literal },
                .generated_quote => |literal| .{ .generated_quote = literal },
            });
        }
        return .{ .start = start, .len = @intCast(operands.len) };
    }

    fn analyzeDispatchCallTarget(
        self: *Builder,
        view: ModuleView,
        call_expr: checked.CheckedExprId,
        maybe_plan: ?static_dispatch.StaticDispatchPlanId,
    ) Allocator.Error!void {
        try self.analyzeDispatchPlanTypes(view, maybe_plan);

        const plan_id = maybe_plan orelse
            boxyPlanInvariant("checked dispatch expression reached boxy planning without a dispatch plan");
        const raw = @intFromEnum(plan_id);
        if (raw >= view.static_dispatch_plans.plans.len) {
            boxyPlanInvariant("checked dispatch expression referenced a missing dispatch plan");
        }
        const dispatch = view.static_dispatch_plans.plans[raw];
        const dispatcher_rep = try self.analyzeType(view, dispatch.dispatcher_ty);
        const target = directDispatchTarget(view.static_dispatch_plans, dispatch.resolution);
        if (target == null) {
            try self.recordActiveWorkerDictionaryUse(dispatcher_rep);
            if (self.plan.representations.items[@intFromEnum(dispatcher_rep)].dictionaries.len != 0) {
                const call_ref = CheckedExprIdentity{ .module = view.key, .expr = call_expr };
                const caller = self.active_worker orelse
                    boxyPlanInvariant("boxy dictionary dispatch was analyzed outside a worker body");
                if (self.plan.dictionaryDispatchPlanForCall(call_ref, caller) == null) {
                    try self.plan.dictionary_dispatches.append(self.allocator, .{
                        .call = call_ref,
                        .caller = caller,
                        .dispatcher_rep = dispatcher_rep,
                        .method = dispatch.method,
                        .source_fn_type = typeRef(view, dispatch.callable_ty),
                        .operands = try self.appendDispatchCallOperands(dispatch, view.static_dispatch_plans),
                    });
                }
            }
        }
        const direct_target = target orelse return;
        const lookup = self.dispatchMethodTargetLookup(
            view,
            direct_target,
            typeRef(view, dispatch.dispatcher_ty),
        );
        const source_fn_type = CheckedTypeIdentity{ .module = view.key, .ty = dispatch.callable_ty };
        const worker = try self.ensureWorker(lookup.source, self.workerCheckedTypeForSource(lookup.source, source_fn_type), null);
        const call_ref = CheckedExprIdentity{ .module = view.key, .expr = call_expr };
        const caller = self.active_worker orelse
            boxyPlanInvariant("boxy dispatch call was analyzed outside a worker body");
        if (self.plan.directWorkerForCall(call_ref, caller)) |existing| {
            if (existing != worker) {
                boxyPlanInvariant("boxy dispatch call plan tried to bind a checked call to two workers");
            }
            return;
        }
        try self.plan.direct_calls.append(self.allocator, .{
            .call = call_ref,
            .caller = caller,
            .worker = worker,
            .source_fn_type = source_fn_type,
            .operands = try self.appendDispatchCallOperands(dispatch, view.static_dispatch_plans),
        });
    }

    fn recordActiveWorkerDictionaryUse(self: *Builder, rep: TypeRepId) Allocator.Error!void {
        const worker = self.active_worker orelse
            boxyPlanInvariant("unresolved dictionary dispatch was analyzed outside a worker body");
        return try self.recordWorkerDictionaryUse(worker, rep);
    }

    fn recordWorkerDictionaryUse(
        self: *Builder,
        worker: WorkerPlanId,
        rep: TypeRepId,
    ) Allocator.Error!void {
        for (self.worker_dictionary_uses.items) |use| {
            if (use.worker == worker and use.rep == rep) return;
        }
        try self.worker_dictionary_uses.append(self.allocator, .{ .worker = worker, .rep = rep });
    }

    fn analyzeIteratorForPlan(
        self: *Builder,
        view: ModuleView,
        maybe_plan: ?static_dispatch.IteratorForPlanId,
    ) Allocator.Error!void {
        const plan_id = maybe_plan orelse
            boxyPlanInvariant("checked iterator for reached boxy planning without an iterator dispatch plan");
        const raw = @intFromEnum(plan_id);
        if (raw >= view.static_dispatch_plans.iterator_for_plans.len) {
            boxyPlanInvariant("checked iterator for referenced a missing iterator dispatch plan");
        }
        const plan = view.static_dispatch_plans.iterator_for_plans[raw];

        if (iteratorPlanDoesNotExecute(plan)) return;

        _ = try self.analyzeType(view, plan.item_ty);
        _ = try self.analyzeType(view, plan.iterator_ty);
        _ = try self.analyzeType(view, plan.step_ty);
        try self.analyzeIteratorDispatchCallTarget(view, plan_id, plan, .iter, plan.iter, typeRef(view, plan.iterator_ty));
        try self.analyzeIteratorDispatchCallTarget(view, plan_id, plan, .next, plan.next, typeRef(view, plan.step_ty));
    }

    fn analyzeIteratorDispatchCallTarget(
        self: *Builder,
        view: ModuleView,
        plan_id: static_dispatch.IteratorForPlanId,
        plan: static_dispatch.IteratorForPlan,
        kind: IteratorCallKind,
        call: static_dispatch.IteratorDispatchCall,
        ret_type: CheckedTypeIdentity,
    ) Allocator.Error!void {
        const dispatcher_rep = try self.analyzeType(view, call.dispatcher_ty);
        _ = try self.analyzeType(view, call.callable_ty);
        _ = try self.analyzeType(self.moduleForId(ret_type.module), ret_type.ty);
        for (call.argsSlice(view.static_dispatch_plans)) |operand| {
            switch (operand) {
                .checked_expr => |expr| try self.analyzeExprTypes(view, expr),
                .loop_iterator_state => _ = try self.analyzeType(view, plan.iterator_ty),
            }
        }

        if (directDispatchTarget(view.static_dispatch_plans, call.resolution) == null) {
            try self.recordActiveWorkerDictionaryUse(dispatcher_rep);
        }
        const target = directDispatchTarget(view.static_dispatch_plans, call.resolution) orelse return;
        const lookup = self.dispatchMethodTargetLookup(
            view,
            target,
            typeRef(view, call.dispatcher_ty),
        );
        const source_fn_type = CheckedTypeIdentity{ .module = view.key, .ty = call.callable_ty };
        const worker = try self.ensureWorker(lookup.source, self.workerCheckedTypeForSource(lookup.source, source_fn_type), null);

        const caller = self.active_worker orelse
            boxyPlanInvariant("boxy iterator call was analyzed outside a worker body");
        if (self.plan.iteratorCallPlanFor(view.key, plan_id, kind, caller)) |existing| {
            if (existing.worker != worker or !typeRefEql(existing.source_fn_type, source_fn_type)) {
                boxyPlanInvariant("boxy iterator dispatch plan tried to bind a checked iterator call to two workers");
            }
            return;
        }

        const source_fn = checkedFunctionPayload(view, call.callable_ty);
        const operands = call.argsSlice(view.static_dispatch_plans);
        if (source_fn.args.len != operands.len) {
            boxyPlanInvariant("boxy iterator dispatch source function type arity disagreed with operands");
        }
        const worker_plan = self.plan.workers.items[@intFromEnum(worker)];
        const worker_function = (self.repQuery().functionChildren(worker_plan.rep)) orelse
            boxyPlanInvariant("boxy iterator dispatch worker was not a function");
        if (worker_function.arg_count != operands.len) {
            boxyPlanInvariant("boxy iterator dispatch worker arity disagreed with operands");
        }
        const worker_children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(worker_function.rep)].children);
        const worker_args = worker_children[worker_function.args_start..][0..worker_function.arg_count];

        const arg_start: u32 = @intCast(self.plan.call_type_substitutions.items.len);
        for (operands, source_fn.args, worker_args) |operand, source_arg_ty, worker_arg| {
            const operand_type = switch (operand) {
                .checked_expr => |expr_id| typeRef(view, view.checked_bodies.expr(expr_id).ty),
                .loop_iterator_state => typeRef(view, plan.iterator_ty),
            };
            const call_type = typeRef(view, source_arg_ty);
            try self.plan.call_type_substitutions.append(self.allocator, .{
                .operand_type = operand_type,
                .operand_rep = try self.analyzeType(self.moduleForId(operand_type.module), operand_type.ty),
                .call_type = call_type,
                .call_rep = try self.analyzeType(view, source_arg_ty),
                .worker_rep = worker_arg.rep,
            });
        }
        const call_ret_rep = self.plan.repForSourceType(ret_type) orelse
            boxyPlanInvariant("boxy iterator dispatch return type was not analyzed");
        try self.plan.iterator_calls.append(self.allocator, .{
            .module = view.key,
            .for_plan = plan_id,
            .kind = kind,
            .caller = caller,
            .worker = worker,
            .source_fn_type = source_fn_type,
            .arg_substitutions = .{
                .start = arg_start,
                .len = @intCast(call.argsSlice(view.static_dispatch_plans).len),
            },
            .ret_type = ret_type,
            .ret_substitution = .{
                .operand_type = self.plan.representations.items[@intFromEnum(worker_function.ret)].source_type,
                .operand_rep = worker_function.ret,
                .call_type = ret_type,
                .call_rep = call_ret_rep,
                .worker_rep = worker_function.ret,
            },
        });
    }

    const DispatchWorkerLookup = struct {
        view: ModuleView,
        source: WorkerSource,
    };

    fn dispatchMethodTargetLookup(
        self: *Builder,
        dispatch_view: ModuleView,
        target: static_dispatch.MethodTarget,
        shape: CheckedTypeIdentity,
    ) DispatchWorkerLookup {
        return switch (target.kind) {
            .procedure => |procedure| .{
                .view = self.moduleForCheckedModuleId(procedure.template.artifact),
                .source = .{ .procedure_template = procedure.template },
            },
            .local_proc => boxyPlanInvariant("local procedure dispatch target reached boxy planning before nested procedure worker planning"),
            .structural => |kind| .{
                .view = dispatch_view,
                .source = .{ .generated_codec = .{
                    .kind = switch (kind) {
                        .parser => .parser_constructor,
                        .encoder => .encoder_constructor,
                        .equality, .hash, .map, .map_effectful => boxyPlanInvariant("non-codec structural target reached dispatch worker planning"),
                    },
                    .shape = shape,
                } },
            },
        };
    }

    fn analyzeStatementTypes(self: *Builder, view: ModuleView, statement_id: checked.CheckedStatementId) Allocator.Error!void {
        const worker = self.active_worker orelse
            boxyPlanInvariant("checked statement was analyzed outside a worker body");
        const entry = try self.body_statements_seen.getOrPut(.{
            .statement = .{ .module = view.key, .statement = statement_id },
            .worker = worker,
        });
        if (entry.found_existing) return;

        const statement = view.checked_bodies.statement(statement_id);
        switch (statement.data) {
            .pending => boxyPlanInvariant("pending checked statement reached boxy body type planning"),
            .decl => |decl| {
                try self.analyzePatternTypes(view, decl.pattern);
                try self.analyzeExprTypes(view, decl.expr);
            },
            .var_ => |decl| {
                try self.analyzePatternTypes(view, decl.pattern);
                try self.analyzeExprTypes(view, decl.expr);
            },
            .var_uninitialized => |decl| try self.analyzePatternTypes(view, decl.pattern),
            .reassign => |reassign| {
                try self.analyzePatternTypes(view, reassign.pattern);
                try self.analyzeExprTypes(view, reassign.expr);
            },
            .crash,
            .break_,
            .import_,
            .alias_decl,
            .nominal_decl,
            .type_anno,
            .type_var_alias,
            .where_alias_decl,
            .runtime_error,
            => {},
            .dbg,
            .expr,
            .expect,
            => |expr| try self.analyzeExprTypes(view, expr),
            .for_ => |for_| {
                try self.analyzeIteratorForPlan(view, for_.plan);
                try self.analyzePatternTypes(view, for_.pattern);
                try self.analyzeExprTypes(view, for_.expr);
                try self.analyzeExprTypes(view, for_.body);
            },
            .while_ => |loop| {
                try self.analyzeExprTypes(view, loop.cond);
                try self.analyzeExprTypes(view, loop.body);
            },
            .infinite_loop => |loop| {
                try self.analyzeExprTypes(view, loop.cond);
                try self.analyzeExprTypes(view, loop.body);
            },
            .breakable_loop => |loop| {
                try self.analyzeExprTypes(view, loop.cond);
                try self.analyzeExprTypes(view, loop.body);
            },
            .return_ => |ret| try self.analyzeExprTypes(view, ret.expr),
        }
    }

    fn analyzePatternTypes(self: *Builder, view: ModuleView, pattern_id: checked.CheckedPatternId) Allocator.Error!void {
        const worker = self.active_worker orelse
            boxyPlanInvariant("checked pattern was analyzed outside a worker body");
        const entry = try self.body_patterns_seen.getOrPut(.{
            .pattern = .{ .module = view.key, .pattern = pattern_id },
            .worker = worker,
        });
        if (entry.found_existing) return;

        const pattern = view.checked_bodies.pattern(pattern_id);
        _ = try self.analyzeType(view, pattern.ty);
        switch (pattern.data) {
            .pending => boxyPlanInvariant("pending checked pattern reached boxy body type planning"),
            .assign,
            .underscore,
            .runtime_error,
            => {},
            .as => |as| try self.analyzePatternTypes(view, as.pattern),
            .applied_tag => |tag| for (tag.args) |arg| try self.analyzePatternTypes(view, arg),
            .nominal => |nominal| try self.analyzePatternTypes(view, nominal.backing_pattern),
            .record_destructure => |fields| for (fields) |field| {
                switch (field.kind) {
                    .required,
                    .sub_pattern,
                    .rest,
                    => |child| try self.analyzePatternTypes(view, child),
                }
            },
            .list => |list| {
                for (list.patterns) |child| try self.analyzePatternTypes(view, child);
                if (list.rest) |rest| if (rest.pattern) |child| try self.analyzePatternTypes(view, child);
            },
            .tuple => |items| for (items) |child| try self.analyzePatternTypes(view, child),
            .numeral_literal => |literal| if (literal.conversion) |conversion| try self.analyzeExprTypes(view, conversion),
            .str_literal => |literal| if (literal.conversion) |conversion| try self.analyzeExprTypes(view, conversion),
            .str_interpolation => |interpolation| {
                for (interpolation.steps) |step| {
                    if (step.capture) |capture| try self.analyzePatternTypes(view, capture);
                }
            },
        }
    }

    fn analyzeDirectCallTarget(
        self: *Builder,
        view: ModuleView,
        call_expr: checked.CheckedExprId,
        call: anytype,
    ) Allocator.Error!void {
        const target = call.direct_target orelse return;
        const source = self.workerSourceForDirectTarget(view, target);
        const checked_type = self.workerCheckedTypeForSource(source, typeRef(view, call.source_fn_ty_payload));
        const source_fn_type = self.directCallInstantiationSourceFnType(view, target, call.source_fn_ty_payload);
        _ = try self.analyzeType(self.moduleForId(source_fn_type.module), source_fn_type.ty);
        const worker = try self.ensureWorker(source, checked_type, null);
        const call_ref = CheckedExprIdentity{ .module = view.key, .expr = call_expr };
        const caller = self.active_worker orelse
            boxyPlanInvariant("boxy direct call was analyzed outside a worker body");
        if (self.plan.directWorkerForCall(call_ref, caller)) |existing| {
            if (existing != worker) {
                boxyPlanInvariant("boxy direct call plan tried to bind a checked call to two workers");
            }
            return;
        }
        try self.plan.direct_calls.append(self.allocator, .{
            .call = call_ref,
            .caller = caller,
            .worker = worker,
            .source_fn_type = source_fn_type,
            .operands = try self.appendCheckedCallOperands(call.args),
        });
    }

    fn directTargetIsLocalProc(
        self: *Builder,
        view: ModuleView,
        target: checked.ResolvedValueId,
    ) bool {
        return self.resolvedValueRecord(view, target).ref == .local_proc;
    }

    fn directTargetHasNoCaptures(
        self: *Builder,
        view: ModuleView,
        target: checked.ResolvedValueId,
    ) bool {
        const source = self.workerSourceForDirectTarget(view, target);
        if (source != .nested_expr) return false;
        return self.nestedCallableHasNoCaptures(source.nested_expr);
    }

    fn nestedCallableHasNoCaptures(self: *Builder, expr_ref: CheckedExprIdentity) bool {
        const view = self.moduleForId(expr_ref.module);
        const expr = view.checked_bodies.expr(expr_ref.expr);
        if (expr.data == .lambda) return true;
        if (expr.data == .closure) return expr.data.closure.captures.len == 0;
        boxyPlanInvariant("nested callable capture lookup did not reference a lambda or closure");
    }

    fn directCallInstantiationSourceFnType(
        self: *Builder,
        view: ModuleView,
        target: checked.ResolvedValueId,
        call_site_type: checked.CheckedTypeId,
    ) CheckedTypeIdentity {
        const record = self.resolvedValueRecord(view, target);
        if (record.ref == .platform_required_proc) {
            return .{
                .module = view.key,
                .ty = record.ref.platform_required_proc.procedure.source_fn_ty_payload orelse
                    boxyPlanInvariant("platform-required procedure call missing relation-owned source function type"),
            };
        }
        return typeRef(view, call_site_type);
    }

    fn workerSourceForDirectTarget(self: *Builder, view: ModuleView, target: checked.ResolvedValueId) WorkerSource {
        const record = self.resolvedValueRecord(view, target);
        return switch (record.ref) {
            .top_level_proc,
            .promoted_top_level_proc,
            => |procedure| self.workerSourceForProcedureUse(procedure),
            .platform_required_proc => |required| self.workerSourceForProcedureUse(required.procedure),
            .local_proc => |local| if (self.topLevelProcedureBindingForExpr(view, local.expr)) |binding|
                .{ .procedure_binding = binding }
            else
                .{ .nested_expr = .{ .module = view.key, .expr = self.nestedCallableSiteExprForExpr(view, local.expr) orelse local.expr } },
            .imported_proc => |procedure| self.workerSourceForProcedureUse(procedure),
            .hosted_proc => |procedure| self.workerSourceForProcedureUse(procedure),
            .local_param,
            .local_value,
            .local_mutable_version,
            .pattern_binder,
            .selected_hoisted_const,
            .top_level_const,
            .imported_const,
            .platform_required_declaration,
            .platform_required_checked_error,
            .platform_required_const,
            => boxyPlanInvariant("checked direct call target did not reference a procedure"),
        };
    }

    fn workerSourceForProcedureValueRef(
        self: *Builder,
        view: ModuleView,
        ref_id: checked.ResolvedValueRefId,
    ) ?WorkerSource {
        const record = self.resolvedValueRecord(view, ref_id);
        return switch (record.ref) {
            .local_proc => |local| if (self.topLevelProcedureBindingForExpr(view, local.expr)) |binding|
                .{ .procedure_binding = binding }
            else
                .{ .nested_expr = .{ .module = view.key, .expr = self.nestedCallableSiteExprForExpr(view, local.expr) orelse local.expr } },
            .top_level_proc,
            .promoted_top_level_proc,
            => |procedure| self.workerSourceForProcedureUse(procedure),
            .platform_required_proc => |required| self.workerSourceForProcedureUse(required.procedure),
            .imported_proc => |procedure| self.workerSourceForProcedureUse(procedure),
            .hosted_proc => |procedure| self.workerSourceForProcedureUse(procedure),
            .local_param,
            .local_value,
            .local_mutable_version,
            .pattern_binder,
            .selected_hoisted_const,
            .top_level_const,
            .imported_const,
            .platform_required_declaration,
            .platform_required_checked_error,
            .platform_required_const,
            => null,
        };
    }

    fn storedFnSourceForProcedureValueRef(
        self: *Builder,
        view: ModuleView,
        ref_id: checked.ResolvedValueRefId,
    ) ?StoredFnSource {
        const record = self.resolvedValueRecord(view, ref_id);
        return switch (record.ref) {
            .top_level_proc,
            .promoted_top_level_proc,
            => |procedure| self.storedFnSourceForProcedureUse(procedure),
            .platform_required_proc => |required| self.storedFnSourceForProcedureUse(required.procedure),
            .imported_proc => |procedure| self.storedFnSourceForProcedureUse(procedure),
            .local_proc => |local| if (self.topLevelProcedureBindingForExpr(view, local.expr)) |binding|
                self.storedFnSourceForTopLevelBinding(binding)
            else
                null,
            .local_param,
            .local_value,
            .local_mutable_version,
            .pattern_binder,
            .selected_hoisted_const,
            .top_level_const,
            .imported_const,
            .hosted_proc,
            .platform_required_declaration,
            .platform_required_checked_error,
            .platform_required_const,
            => null,
        };
    }

    fn storedFnSourceForProcedureUse(
        self: *Builder,
        procedure: checked.ProcedureUseTemplate,
    ) ?StoredFnSource {
        return switch (procedure.binding) {
            .top_level => |binding| self.storedFnSourceForTopLevelBinding(binding),
            .platform_required => |required| self.storedFnSourceForTopLevelBinding(.{
                .artifact = required.app_value.artifact,
                .binding = required.procedure_binding,
            }),
            .imported => |imported| blk: {
                const view = self.moduleForId(imported.artifact);
                const binding = self.importedProcedureBinding(view, imported);
                break :blk switch (binding.body) {
                    .callable_eval_template => |template| self.storedFnSourceForCallableEvalTemplate(view, template),
                    .direct_template => null,
                };
            },
            .hosted => null,
        };
    }

    fn storedFnSourceForTopLevelBinding(
        self: *Builder,
        binding_ref: checked.ArtifactTopLevelProcedureBindingRef,
    ) ?StoredFnSource {
        const view = self.moduleForId(binding_ref.artifact);
        const binding = view.top_level_procedure_bindings.get(binding_ref.binding);
        return switch (binding.body) {
            .callable_eval_template => |template| self.storedFnSourceForCallableEvalTemplate(view, template),
            .direct_template => null,
        };
    }

    fn storedFnSourceForCallableEvalTemplate(
        self: *Builder,
        view: ModuleView,
        template_id: checked.CallableEvalTemplateId,
    ) ?StoredFnSource {
        const template = self.callableEvalTemplate(view, template_id);
        const root = view.compile_time_roots.root(template.root);
        return switch (root.payload) {
            .fn_value => |fn_id| .{ .module = view.key, .fn_id = fn_id },
            .pending, .const_node, .discarded, .expect => null,
        };
    }

    fn workerSourceForCallableRootExpr(
        self: *Builder,
        view: ModuleView,
        expr_id: checked.CheckedExprId,
    ) ?WorkerSource {
        const expr = view.checked_bodies.expr(expr_id);
        if (expr.data == .lookup_local) {
            return if (expr.data.lookup_local.resolved) |ref_id|
                self.workerSourceForProcedureValueRef(view, ref_id)
            else
                null;
        }
        if (expr.data == .lookup_external) {
            return if (expr.data.lookup_external) |ref_id|
                self.workerSourceForProcedureValueRef(view, ref_id)
            else
                null;
        }
        if (expr.data == .lookup_required) {
            return if (expr.data.lookup_required) |ref_id|
                self.workerSourceForProcedureValueRef(view, ref_id)
            else
                null;
        }
        if (expr.data == .lambda or expr.data == .closure) {
            return .{ .nested_expr = .{ .module = view.key, .expr = expr_id } };
        }
        return null;
    }

    fn nestedCallableSiteExprForExpr(
        _: *Builder,
        view: ModuleView,
        expr: checked.CheckedExprId,
    ) ?checked.CheckedExprId {
        for (view.nested_proc_sites.sites) |site| {
            const site_expr_id = site.checked_expr orelse continue;
            if (site_expr_id == expr) return expr;
            const site_expr = view.checked_bodies.expr(site_expr_id);
            if (site_expr.data == .closure and site_expr.data.closure.lambda == expr) return site_expr_id;
        }
        return null;
    }

    fn topLevelProcedureBindingForExpr(
        _: *Builder,
        view: ModuleView,
        expr: checked.CheckedExprId,
    ) ?checked.ArtifactTopLevelProcedureBindingRef {
        for (view.top_level_procedure_bindings.bindings, 0..) |binding, index| {
            const template_ref = switch (binding.body) {
                .direct_template => |direct| switch (direct.template) {
                    .checked => |template| template,
                    .lifted,
                    .synthetic,
                    => continue,
                },
                .callable_eval_template => continue,
            };
            const template = view.checked_procedure_templates.get(template_ref.template);
            const body_id = switch (template.body) {
                .checked_body => |body| body,
                .intrinsic_wrapper,
                .entry_wrapper,
                .unimplemented,
                => continue,
            };
            if (view.checked_bodies.body(body_id).root_expr == expr) {
                return .{
                    .artifact = view.key,
                    .binding = @enumFromInt(@as(u32, @intCast(index))),
                };
            }
        }
        return null;
    }

    fn workerCheckedTypeForSource(self: *Builder, source: WorkerSource, requested_type: CheckedTypeIdentity) CheckedTypeIdentity {
        return switch (source) {
            .procedure_template => |template| self.checkedTypeForTemplate(template),
            .procedure_binding => |binding| self.checkedTypeForTopLevelBinding(binding),
            .procedure_use => |use| switch (use.binding) {
                .top_level => |binding| self.checkedTypeForTopLevelBinding(binding),
                .platform_required => |required| self.checkedTypeForTopLevelBinding(.{
                    .artifact = required.app_value.artifact,
                    .binding = required.procedure_binding,
                }),
                .imported => |imported| self.checkedTypeForImportedBinding(imported),
                .hosted => requested_type,
            },
            .nested_expr => |expr_ref| self.nestedExprDefinitionType(expr_ref),
            .generated_codec => requested_type,
            .generated_field_iterator => requested_type,
            .generated_interpolation_step => requested_type,
        };
    }

    fn nestedExprDefinitionType(self: *Builder, expr_ref: CheckedExprIdentity) CheckedTypeIdentity {
        const view = self.moduleForId(expr_ref.module);
        return typeRef(view, view.checked_bodies.expr(expr_ref.expr).ty);
    }

    fn checkedTypeForTopLevelBinding(
        self: *Builder,
        binding_ref: checked.ArtifactTopLevelProcedureBindingRef,
    ) CheckedTypeIdentity {
        const view = self.moduleForId(binding_ref.artifact);
        const binding = view.top_level_procedure_bindings.get(binding_ref.binding);
        return switch (binding.body) {
            .direct_template => |direct| switch (direct.template) {
                .checked => |template| self.checkedTypeForTemplate(template),
                .lifted,
                .synthetic,
                => boxyPlanInvariant("non-checked procedure template reached boxy worker type planning"),
            },
            .callable_eval_template => |template| typeRef(view, self.callableEvalTemplate(view, template).checked_fn_root),
        };
    }

    fn checkedTypeForImportedBinding(
        self: *Builder,
        binding_ref: checked.ImportedProcedureBindingRef,
    ) CheckedTypeIdentity {
        const view = self.moduleForId(binding_ref.artifact);
        const binding = self.importedProcedureBinding(view, binding_ref);
        return switch (binding.body) {
            .direct_template => |direct| switch (direct.template) {
                .checked => |template| self.checkedTypeForTemplate(template),
                .lifted,
                .synthetic,
                => boxyPlanInvariant("non-checked imported procedure template reached boxy worker type planning"),
            },
            .callable_eval_template => |template| typeRef(view, self.callableEvalTemplate(view, template).checked_fn_root),
        };
    }

    fn checkedTypeForTemplate(self: *Builder, template_ref: checked_names.ProcedureTemplateRef) CheckedTypeIdentity {
        const view = self.moduleForCheckedModuleId(template_ref.artifact);
        const template = view.checked_procedure_templates.get(template_ref.template);
        return typeRef(view, template.checked_fn_root);
    }

    fn workerSourceForProcedureUse(self: *Builder, procedure: checked.ProcedureUseTemplate) WorkerSource {
        return switch (procedure.binding) {
            .top_level => |top_level| blk: {
                const view = self.moduleForId(top_level.artifact);
                const binding = view.top_level_procedure_bindings.get(top_level.binding);
                switch (binding.body) {
                    .callable_eval_template => |template| if (self.workerSourceForCallableEvalTemplate(view, template)) |source| {
                        break :blk source;
                    },
                    .direct_template => {},
                }
                if (!self.procedureBindingBodyIsPendingEval(view, top_level.binding)) {
                    _ = self.rootProcedureBindingBody(view, top_level.binding);
                }
                break :blk .{ .procedure_binding = top_level };
            },
            .platform_required => |required| blk: {
                const view = self.moduleForId(required.app_value.artifact);
                const binding_ref: checked.ArtifactTopLevelProcedureBindingRef = .{
                    .artifact = required.app_value.artifact,
                    .binding = required.procedure_binding,
                };
                const binding = view.top_level_procedure_bindings.get(required.procedure_binding);
                switch (binding.body) {
                    .callable_eval_template => |template| if (self.workerSourceForCallableEvalTemplate(view, template)) |source| {
                        break :blk source;
                    },
                    .direct_template => {},
                }
                if (!self.procedureBindingBodyIsPendingEval(view, required.procedure_binding)) {
                    _ = self.rootProcedureBindingBody(view, required.procedure_binding);
                }
                break :blk .{ .procedure_binding = binding_ref };
            },
            .imported => |imported| blk: {
                const view = self.moduleForId(imported.artifact);
                const binding = self.importedProcedureBinding(view, imported);
                switch (binding.body) {
                    .callable_eval_template => |template| if (self.workerSourceForCallableEvalTemplate(view, template)) |source| {
                        break :blk source;
                    },
                    .direct_template => {},
                }
                break :blk .{ .procedure_use = procedure };
            },
            .hosted => .{ .procedure_use = procedure },
        };
    }

    fn workerSourceForCallableEvalSource(self: *Builder, source: WorkerSource) WorkerSource {
        return switch (source) {
            .procedure_binding => |binding_ref| blk: {
                const view = self.moduleForId(binding_ref.artifact);
                const binding = view.top_level_procedure_bindings.get(binding_ref.binding);
                break :blk switch (binding.body) {
                    .callable_eval_template => |template| self.workerSourceForCallableEvalTemplate(view, template) orelse source,
                    .direct_template => source,
                };
            },
            .procedure_use => |procedure| self.workerSourceForProcedureUse(procedure),
            .procedure_template,
            .nested_expr,
            .generated_codec,
            .generated_field_iterator,
            .generated_interpolation_step,
            => source,
        };
    }

    fn workerSourceForCallableEvalTemplate(
        self: *Builder,
        view: ModuleView,
        template_id: checked.CallableEvalTemplateId,
    ) ?WorkerSource {
        const template = self.callableEvalTemplate(view, template_id);
        const root = view.compile_time_roots.root(template.root);
        return switch (root.payload) {
            .fn_value => |fn_id| blk: {
                const store = view.const_store orelse
                    boxyPlanInvariant("finalized callable eval root had no ConstStore");
                break :blk self.workerSourceForConstFnValue(
                    store.getFn(fn_id),
                    typeRef(view, template.checked_fn_root),
                );
            },
            .pending => self.workerSourceForCallableRootExpr(view, root.expr),
            .const_node, .discarded, .expect => null,
        };
    }

    fn resolvedValueRecord(_: *Builder, view: ModuleView, target: checked.ResolvedValueId) checked.ResolvedValueRefRecord {
        const raw = @intFromEnum(target);
        if (raw >= view.resolved_value_refs.records.len) {
            boxyPlanInvariant("checked direct call target referenced a missing resolved value");
        }
        return view.resolved_value_refs.records[raw];
    }
};

fn rootRequiresHostWrapper(root: checked.RootRequest) bool {
    return root.abi != .roc or root.exposure != .private;
}

fn workerSourceForRoot(root: checked.RootRequest, root_key: checked.CheckedModuleArtifactKey) ?WorkerSource {
    if (root.procedure_binding) |binding| return .{ .procedure_binding = .{ .artifact = root_key, .binding = binding } };
    if (root.procedure_use) |procedure| return .{ .procedure_use = procedure };
    if (root.procedure_template) |template| return .{ .procedure_template = template };
    return null;
}

fn workerSourceIsHosted(source: WorkerSource) bool {
    return switch (source) {
        .procedure_use => |use| switch (use.binding) {
            .hosted => true,
            .top_level,
            .platform_required,
            .imported,
            => false,
        },
        .procedure_template,
        .procedure_binding,
        .nested_expr,
        .generated_codec,
        .generated_field_iterator,
        .generated_interpolation_step,
        => false,
    };
}

fn workerSourceEql(a: WorkerSource, b: WorkerSource) bool {
    return std.meta.eql(a, b);
}

/// One module's identity plus its name store: the only module data the
/// label-comparing representation queries need. Callers build this from
/// whatever module view they already hold.
pub const ModuleNames = struct {
    key: checked.ModuleId = .{},
    canonical_names: ?*const checked_names.CanonicalNameStore = null,
};

/// The erased-callable children of a function representation.
pub const FunctionChildren = struct {
    rep: TypeRepId,
    args_start: u32,
    arg_count: u32,
    ret: TypeRepId,
};

/// Read-only queries over a finished representation plan.
///
/// This is the single source of truth for every "what does this representation
/// contain" question. Boxy planning decides an unwrap, a descriptor argument,
/// or a dictionary argument by asking these; Boxy lowering emits that decision
/// by asking the same ones. A second copy would let the two disagree about the
/// program they are both describing, so no consumer may re-derive these.
pub const RepQuery = struct {
    plan: *const ProgramPlan,
    allocator: Allocator,

    fn rep(self: RepQuery, rep_id: TypeRepId) TypeRepresentation {
        return self.plan.representations.items[@intFromEnum(rep_id)];
    }

    /// The one child of `rep_id` with `role`; an invariant failure when the
    /// representation has none or more than one.
    pub fn requiredSingleChild(self: RepQuery, rep_id: TypeRepId, role: ChildRole) RepChild {
        return requiredSingleChildOf(self.plan, rep_id, role);
    }

    /// True when `rep_id` or anything reachable below it carries a descriptor.
    pub fn repSubtreeHasDescriptor(self: RepQuery, rep_id: TypeRepId) Allocator.Error!bool {
        var seen = collections.DenseMap(TypeRepId, void).init(self.allocator);
        defer seen.deinit();
        return try self.repSubtreeHasDescriptorInner(rep_id, &seen);
    }

    fn repSubtreeHasDescriptorInner(
        self: RepQuery,
        rep_id: TypeRepId,
        seen: *collections.DenseMap(TypeRepId, void),
    ) Allocator.Error!bool {
        const entry = try seen.getOrPut(rep_id);
        if (entry.found_existing) return false;

        const current = self.rep(rep_id);
        if (current.descriptor != null) return true;

        for (self.plan.childSlice(current.children)) |child| {
            if (try self.repSubtreeHasDescriptorInner(child.rep, seen)) return true;
        }
        return false;
    }

    /// True when a child of `rep_id` other than `selected_child` carries a
    /// descriptor that the selected child's subtree does not already cover.
    pub fn repSubtreeHasDescriptorInOtherChildren(
        self: RepQuery,
        rep_id: TypeRepId,
        selected_child: RepChild,
    ) Allocator.Error!bool {
        for (self.plan.childSlice(self.rep(rep_id).children)) |child| {
            if (child.rep == selected_child.rep and std.meta.eql(child.role, selected_child.role)) continue;
            // A sibling reachable through the selected child's subtree carries the
            // same descriptor the backing already covers; it does not block the
            // unwrap because its descriptor is collected once via the backing.
            if (try self.repSubtreeContainsRep(selected_child.rep, child.rep)) continue;
            if (try self.repSubtreeHasDescriptor(child.rep)) return true;
        }
        return false;
    }

    /// True when `rep_id` or anything reachable below it carries a dictionary.
    pub fn repSubtreeHasDictionary(self: RepQuery, rep_id: TypeRepId) Allocator.Error!bool {
        var seen = collections.DenseMap(TypeRepId, void).init(self.allocator);
        defer seen.deinit();
        return try self.repSubtreeHasDictionaryInner(rep_id, &seen);
    }

    fn repSubtreeHasDictionaryInner(
        self: RepQuery,
        rep_id: TypeRepId,
        seen: *collections.DenseMap(TypeRepId, void),
    ) Allocator.Error!bool {
        const entry = try seen.getOrPut(rep_id);
        if (entry.found_existing) return false;

        const current = self.rep(rep_id);
        if (current.dictionaries.len != 0) return true;

        for (self.plan.childSlice(current.children)) |child| {
            if (try self.repSubtreeHasDictionaryInner(child.rep, seen)) return true;
        }
        return false;
    }

    /// True when a child of `rep_id` other than `selected_child` carries a
    /// dictionary that the selected child's subtree does not already cover.
    pub fn repSubtreeHasDictionaryInOtherChildren(
        self: RepQuery,
        rep_id: TypeRepId,
        selected_child: RepChild,
    ) Allocator.Error!bool {
        for (self.plan.childSlice(self.rep(rep_id).children)) |child| {
            if (child.rep == selected_child.rep and std.meta.eql(child.role, selected_child.role)) continue;
            // A sibling reachable through the selected child's subtree carries the
            // same dictionary the backing already covers; it does not block the
            // unwrap because its dictionary is collected once via the backing.
            if (try self.repSubtreeContainsRep(selected_child.rep, child.rep)) continue;
            if (try self.repSubtreeHasDictionary(child.rep)) return true;
        }
        return false;
    }

    /// True when `target` is `root` or is reachable below it.
    pub fn repSubtreeContainsRep(self: RepQuery, root: TypeRepId, target: TypeRepId) Allocator.Error!bool {
        var seen = collections.DenseMap(TypeRepId, void).init(self.allocator);
        defer seen.deinit();
        return try self.repSubtreeContainsRepInner(root, target, &seen);
    }

    fn repSubtreeContainsRepInner(
        self: RepQuery,
        root: TypeRepId,
        target: TypeRepId,
        seen: *collections.DenseMap(TypeRepId, void),
    ) Allocator.Error!bool {
        if (root == target) return true;
        const entry = try seen.getOrPut(root);
        if (entry.found_existing) return false;
        for (self.plan.childSlice(self.rep(root).children)) |child| {
            if (try self.repSubtreeContainsRepInner(child.rep, target, seen)) return true;
        }
        return false;
    }

    /// The backing representation an alias or field-less transparent nominal
    /// wraps, or null when `rep_id` is not such a wrapper.
    pub fn structuralWrapperBackingRep(self: RepQuery, rep_id: TypeRepId) ?TypeRepId {
        const current = self.rep(rep_id);
        if (current.kind == .alias) return self.requiredSingleChild(rep_id, .alias_backing).rep;
        if (current.kind == .nominal) {
            return switch (current.kind.nominal) {
                .transparent => if (current.declared_fields.len == 0)
                    self.requiredSingleChild(rep_id, .nominal_backing).rep
                else
                    null,
                .opaque_nominal, .builtin_other => null,
            };
        }
        return null;
    }

    /// The representation a descriptor argument is keyed by: wrappers unwrap
    /// until an inspect method, a nominal-backing substitution, or a
    /// non-wrapper is reached.
    pub fn descriptorArgumentIdentityRep(self: RepQuery, rep_id: TypeRepId) TypeRepId {
        var current = rep_id;
        var depth: u16 = 0;
        while (true) {
            if (depth == 1024) boxyPlanInvariant("descriptor argument wrapper chain exceeded boxy planner limit");
            depth += 1;
            if (self.plan.inspectMethodForRep(current) != null) return current;

            if (self.rep(current).nominal_backing_arg_substitutions.len != 0) return current;

            current = self.structuralWrapperBackingRep(current) orelse return current;
        }
    }

    /// The representation a dictionary argument is keyed by. Aliases are pure
    /// transparency, but a transparent nominal owns the method namespace its
    /// dictionary slots dispatch through, so only aliases are unwrapped here;
    /// the nominal identity is preserved.
    pub fn dictionaryArgumentIdentityRep(self: RepQuery, rep_id: TypeRepId) TypeRepId {
        var current = rep_id;
        var depth: u16 = 0;
        while (true) {
            if (depth == 1024) boxyPlanInvariant("dictionary argument wrapper chain exceeded boxy planner limit");
            depth += 1;

            if (self.rep(current).kind != .alias) return current;
            current = self.requiredSingleChild(current, .alias_backing).rep;
        }
    }

    /// True when a worker child is exactly the unwrapped backing of its
    /// worker representation and no sibling descriptor blocks the unwrap.
    pub fn workerChildCanMatchUnwrappedCallRep(
        self: RepQuery,
        worker_rep_id: TypeRepId,
        worker_child: RepChild,
    ) Allocator.Error!bool {
        const worker_backing = self.structuralWrapperBackingRep(worker_rep_id) orelse return false;
        return worker_child.rep == worker_backing and
            !try self.repSubtreeHasDescriptorInOtherChildren(worker_rep_id, worker_child);
    }

    /// The dictionary counterpart of `workerChildCanMatchUnwrappedCallRep`.
    pub fn workerChildCanMatchUnwrappedCallRepForDictionaries(
        self: RepQuery,
        worker_rep_id: TypeRepId,
        worker_child: RepChild,
    ) Allocator.Error!bool {
        const worker_backing = self.structuralWrapperBackingRep(worker_rep_id) orelse return false;
        return worker_child.rep == worker_backing and
            !try self.repSubtreeHasDictionaryInOtherChildren(worker_rep_id, worker_child);
    }

    /// The representation that carries a function's identity, unwrapping alias
    /// and transparent-nominal layers.
    pub fn functionIdentityRep(self: RepQuery, rep_id: TypeRepId) TypeRepId {
        var current = rep_id;
        var depth: u16 = 0;
        while (true) {
            if (depth == 1024) boxyPlanInvariant("function root alias chain exceeded boxy planner limit");
            depth += 1;

            const current_rep = self.rep(current);
            if (current_rep.kind == .alias) {
                current = self.requiredSingleChild(current, .alias_backing).rep;
            } else if (current_rep.kind == .nominal) {
                switch (current_rep.kind.nominal) {
                    .transparent => current = self.requiredSingleChild(current, .nominal_backing).rep,
                    .opaque_nominal, .builtin_other => return current,
                }
            } else {
                return current;
            }
        }
    }

    /// The argument and return children of a function representation, or null
    /// when `rep_id` does not resolve to an erased callable.
    pub fn functionChildren(self: RepQuery, rep_id: TypeRepId) ?FunctionChildren {
        const identity_rep = self.functionIdentityRep(rep_id);
        const identity = self.rep(identity_rep);
        if (identity.kind != .erased_callable) return null;

        var args_start: ?u32 = null;
        var arg_count: u32 = 0;
        var ret: ?TypeRepId = null;
        for (self.plan.childSlice(identity.children), 0..) |child, i| {
            if (child.role == .function_arg) {
                if (args_start == null) args_start = @intCast(i);
                arg_count += 1;
            } else if (child.role == .function_ret) {
                ret = child.rep;
            }
        }
        return .{
            .rep = identity_rep,
            .args_start = args_start orelse 0,
            .arg_count = arg_count,
            .ret = ret orelse boxyPlanInvariant("function representation had no return child"),
        };
    }

    /// The single child of `children` whose checked source type matches
    /// `target` and whose subtree carries a descriptor.
    pub fn findMatchingChildBySourceType(
        self: RepQuery,
        children: []const RepChild,
        target: RepChild,
    ) Allocator.Error!?RepChild {
        var found: ?RepChild = null;
        for (children) |child| {
            if (!typeRefEql(child.source_type, target.source_type)) continue;
            if (!try self.repSubtreeHasDescriptor(child.rep)) continue;
            if (found != null) boxyPlanInvariant("boxy direct call descriptor mapping found ambiguous checked-type children");
            found = child;
        }
        return found;
    }

    /// The dictionary counterpart of `findMatchingChildBySourceType`.
    pub fn findMatchingDictionaryChildBySourceType(
        self: RepQuery,
        children: []const RepChild,
        target: RepChild,
    ) Allocator.Error!?RepChild {
        var found: ?RepChild = null;
        for (children) |child| {
            if (!typeRefEql(child.source_type, target.source_type)) continue;
            if (!try self.repSubtreeHasDictionary(child.rep)) continue;
            if (found != null) boxyPlanInvariant("boxy direct call dictionary mapping found ambiguous checked-type children");
            found = child;
        }
        return found;
    }
};

/// True when two child roles are the same payload-free role.
///
/// Every role that carries a payload (an index, a label) answers false: this
/// asks only "is this the same kind of position", not "is this the same
/// position". Callers that need the exact position compare roles directly, and
/// callers that need cross-module label equality use
/// `NamedRepQuery.childRolesMatch`.
pub fn sameChildRoleKind(a: ChildRole, b: ChildRole) bool {
    return switch (a) {
        .alias_backing => b == .alias_backing,
        .nominal_backing => b == .nominal_backing,
        .record_ext => b == .record_ext,
        .tag_ext => b == .tag_ext,
        .list_elem => b == .list_elem,
        .box_payload => b == .box_payload,
        .alias_arg,
        .nominal_arg,
        .nominal_padding_field,
        .record_field,
        .tuple_elem,
        .function_arg,
        .function_ret,
        .tag_payload,
        => false,
    };
}

/// Representation queries that compare source-level labels, and therefore need
/// to resolve a module id to its name store.
///
/// `Modules` is any type exposing `moduleNames(checked.ModuleId) ModuleNames`.
/// Boxy planning and Boxy lowering hold different module-view types; this is
/// the seam that lets both ask one implementation the same question.
pub fn NamedRepQuery(comptime Modules: type) type {
    return struct {
        const Self = @This();

        query: RepQuery,
        modules: Modules,

        /// True when two children fill the same role, comparing record field
        /// and tag payload labels by text across modules.
        pub fn childRolesMatch(self: Self, target: RepChild, candidate: RepChild) bool {
            return switch (target.role) {
                .record_field => |target_label| switch (candidate.role) {
                    .record_field => |candidate_label| recordFieldNameMatches(
                        self.modules.moduleNames(target.source_type.module),
                        target_label,
                        self.modules.moduleNames(candidate.source_type.module),
                        candidate_label,
                    ),
                    .alias_backing,
                    .alias_arg,
                    .nominal_backing,
                    .nominal_arg,
                    .nominal_padding_field,
                    .record_ext,
                    .tuple_elem,
                    .function_arg,
                    .function_ret,
                    .tag_payload,
                    .tag_ext,
                    .list_elem,
                    .box_payload,
                    => false,
                },
                .tag_payload => |target_payload| switch (candidate.role) {
                    .tag_payload => |candidate_payload| target_payload.index == candidate_payload.index and
                        tagLabelNameMatches(
                            self.modules.moduleNames(target.source_type.module),
                            target_payload.tag,
                            self.modules.moduleNames(candidate.source_type.module),
                            candidate_payload.tag,
                        ),
                    .alias_backing,
                    .alias_arg,
                    .nominal_backing,
                    .nominal_arg,
                    .nominal_padding_field,
                    .record_field,
                    .record_ext,
                    .tuple_elem,
                    .function_arg,
                    .function_ret,
                    .tag_ext,
                    .list_elem,
                    .box_payload,
                    => false,
                },
                .alias_backing,
                .alias_arg,
                .nominal_backing,
                .nominal_arg,
                .nominal_padding_field,
                .record_ext,
                .tuple_elem,
                .function_arg,
                .function_ret,
                .tag_ext,
                .list_elem,
                .box_payload,
                => std.meta.eql(target.role, candidate.role),
            };
        }

        /// The first child of `children` filling the same role as `target`.
        pub fn findMatchingChildByRole(self: Self, children: []const RepChild, target: RepChild) ?RepChild {
            for (children) |child| {
                if (self.childRolesMatch(target, child)) return child;
            }
            return null;
        }

        /// The tag payload matching `target` reachable through the tag-row
        /// extensions of `children`.
        pub fn findMatchingTagPayloadInRowExtension(
            self: Self,
            children: []const RepChild,
            target: RepChild,
        ) Allocator.Error!?RepChild {
            if (target.role != .tag_payload) return null;

            var seen = collections.DenseMap(TypeRepId, void).init(self.query.allocator);
            defer seen.deinit();
            return try self.findMatchingTagPayloadInRowExtensionInner(children, target, &seen);
        }

        fn findMatchingTagPayloadInRowExtensionInner(
            self: Self,
            children: []const RepChild,
            target: RepChild,
            seen: *collections.DenseMap(TypeRepId, void),
        ) Allocator.Error!?RepChild {
            for (children) |child| {
                if (child.role != .tag_ext) continue;
                if (try self.findMatchingTagPayloadInRep(child.rep, target, seen)) |match| return match;
            }
            return null;
        }

        /// The tag payload matching `target` inside `rep_id`, following
        /// structural wrappers and tag-row extensions.
        pub fn findMatchingTagPayloadInRep(
            self: Self,
            rep_id: TypeRepId,
            target: RepChild,
            seen: *collections.DenseMap(TypeRepId, void),
        ) Allocator.Error!?RepChild {
            const entry = try seen.getOrPut(rep_id);
            if (entry.found_existing) return null;

            const children = self.query.plan.childSlice(self.query.rep(rep_id).children);
            if (self.findMatchingChildByRole(children, target)) |match| return match;

            if (self.query.structuralWrapperBackingRep(rep_id)) |backing_rep| {
                const backing_children = self.query.plan.childSlice(self.query.rep(backing_rep).children);
                if (self.findMatchingChildByRole(backing_children, target)) |match| return match;
                if (try self.findMatchingTagPayloadInRowExtensionInner(backing_children, target, seen)) |match| return match;
            }

            return try self.findMatchingTagPayloadInRowExtensionInner(children, target, seen);
        }
    };
}

/// The label-comparison view of a module view.
pub fn moduleNamesOf(view: ModuleView) ModuleNames {
    return .{ .key = view.key, .canonical_names = view.canonical_names };
}

/// True when two record field labels name the same field, comparing by text
/// when the labels come from different modules.
pub fn recordFieldNameMatches(
    source: ModuleNames,
    source_name: RecordFieldLabelId,
    target: ModuleNames,
    target_name: RecordFieldLabelId,
) bool {
    if (moduleKeyEqual(source.key, target.key)) return source_name == target_name;
    const source_names = source.canonical_names orelse return source_name == target_name;
    const target_names = target.canonical_names orelse return source_name == target_name;
    return std.mem.eql(
        u8,
        source_names.recordFieldLabelText(source_name),
        target_names.recordFieldLabelText(target_name),
    );
}

/// True when two tag labels name the same tag, comparing by text when the
/// labels come from different modules.
pub fn tagLabelNameMatches(
    source: ModuleNames,
    source_name: TagLabelId,
    target: ModuleNames,
    target_name: TagLabelId,
) bool {
    if (moduleKeyEqual(source.key, target.key)) return source_name == target_name;
    const source_names = source.canonical_names orelse return source_name == target_name;
    const target_names = target.canonical_names orelse return source_name == target_name;
    return checked.tagLabelsMatch(source_names, source_name, target_names, target_name);
}

/// The one child of `rep_id` with `role`, resolved directly from a plan.
fn requiredSingleChildOf(plan: *const ProgramPlan, rep_id: TypeRepId, role: ChildRole) RepChild {
    var found: ?RepChild = null;
    const rep = plan.representations.items[@intFromEnum(rep_id)];
    for (plan.childSlice(rep.children)) |child| {
        if (std.meta.eql(child.role, role)) {
            if (found != null) boxyPlanInvariant("boxy representation had duplicate child role");
            found = child;
        }
    }
    return found orelse boxyPlanInvariant("boxy representation was missing required child role");
}

fn checkedFunctionPayload(view: ModuleView, checked_ty: checked.CheckedTypeId) checked.CheckedFunctionType {
    var current = checked_ty;
    var depth: u16 = 0;
    while (true) {
        if (depth == 1024) boxyPlanInvariant("checked function alias chain exceeded boxy planner limit");
        depth += 1;

        switch (view.checked_types.payload(current)) {
            .pending => boxyPlanInvariant("pending checked function type reached boxy planning"),
            .alias => |alias| {
                current = alias.backing;
                continue;
            },
            .function => |function| return function,
            .err,
            .flex,
            .rigid,
            .record,
            .record_unbound,
            .tuple,
            .nominal,
            .empty_record,
            .tag_union,
            .empty_tag_union,
            => boxyPlanInvariant("checked intrinsic wrapper did not have a function type"),
        }
    }
}

fn generatedParserScalarMethod(builtin: checked.CheckedBuiltinNominal) ?[]const u8 {
    return switch (builtin) {
        .bool => "parse_bool",
        .str => "parse_str",
        .u8 => "parse_u8",
        .i8 => "parse_i8",
        .u16 => "parse_u16",
        .i16 => "parse_i16",
        .u32 => "parse_u32",
        .i32 => "parse_i32",
        .u64 => "parse_u64",
        .i64 => "parse_i64",
        .u128 => "parse_u128",
        .i128 => "parse_i128",
        .dec => "parse_dec",
        .f32 => "parse_f32",
        .f64 => "parse_f64",
        .try_,
        .u8x16,
        .i8x16,
        .u16x8,
        .i16x8,
        .u32x4,
        .i32x4,
        .u64x2,
        .i64x2,
        .list,
        .box,
        .dict,
        .set,
        .iter,
        .parse_tag_union_spec,
        .fields,
        .field,
        .crypto_sha256_digest,
        .crypto_sha256_hasher,
        .crypto_blake3_digest,
        .crypto_blake3_hasher,
        => null,
    };
}

fn generatedParserKeyMethod(view: ModuleView, ty: checked.CheckedTypeId) ?[]const u8 {
    return switch (view.checked_types.payload(ty)) {
        .alias => |alias| generatedParserKeyMethod(view, alias.backing),
        .nominal => |nominal| switch (nominal.builtin orelse return null) {
            .bool => "parse_key_bool",
            .str => "parse_key_str",
            .u8 => "parse_key_u8",
            .i8 => "parse_key_i8",
            .u16 => "parse_key_u16",
            .i16 => "parse_key_i16",
            .u32 => "parse_key_u32",
            .i32 => "parse_key_i32",
            .u64 => "parse_key_u64",
            .i64 => "parse_key_i64",
            .u128 => "parse_key_u128",
            .i128 => "parse_key_i128",
            .dec => "parse_key_dec",
            .f32 => "parse_key_f32",
            .f64 => "parse_key_f64",
            .try_,
            .u8x16,
            .i8x16,
            .u16x8,
            .i16x8,
            .u32x4,
            .i32x4,
            .u64x2,
            .i64x2,
            .list,
            .box,
            .dict,
            .set,
            .iter,
            .parse_tag_union_spec,
            .fields,
            .field,
            .crypto_sha256_digest,
            .crypto_sha256_hasher,
            .crypto_blake3_digest,
            .crypto_blake3_hasher,
            => null,
        },
        .pending,
        .err,
        .flex,
        .rigid,
        .record,
        .record_unbound,
        .tuple,
        .function,
        .empty_record,
        .tag_union,
        .empty_tag_union,
        => null,
    };
}

fn checkedParserUnitTagKey(view: ModuleView, ty: checked.CheckedTypeId) bool {
    var current = ty;
    var saw_tag = false;
    var remaining = view.checked_types.payloadCount();
    while (remaining > 0) : (remaining -= 1) {
        switch (view.checked_types.payload(current)) {
            .alias => |alias| current = alias.backing,
            .nominal => |nominal| {
                if (nominal.builtin != null) return false;
                current = view.checked_types.nominalBackingTemplateForPayload(nominal) orelse return false;
            },
            .tag_union => |row| {
                for (row.tags) |tag| {
                    if (tag.argsSlice(view.checked_types).len != 0) return false;
                    saw_tag = true;
                }
                current = row.ext;
            },
            .empty_tag_union => return saw_tag,
            .flex, .rigid => |variable| return saw_tag and variable.row_default == .empty_tag_union,
            .pending, .err, .record, .record_unbound, .tuple, .function, .empty_record => return false,
        }
    }
    boxyPlanInvariant("checked Dict key tag row was cyclic");
}

fn generatedEncoderScalarMethod(builtin: checked.CheckedBuiltinNominal) ?[]const u8 {
    return switch (builtin) {
        .bool => "encode_bool",
        .str => "encode_str",
        .u8 => "encode_u8",
        .i8 => "encode_i8",
        .u16 => "encode_u16",
        .i16 => "encode_i16",
        .u32 => "encode_u32",
        .i32 => "encode_i32",
        .u64 => "encode_u64",
        .i64 => "encode_i64",
        .u128 => "encode_u128",
        .i128 => "encode_i128",
        .dec => "encode_dec",
        .f32 => "encode_f32",
        .f64 => "encode_f64",
        .try_,
        .u8x16,
        .i8x16,
        .u16x8,
        .i16x8,
        .u32x4,
        .i32x4,
        .u64x2,
        .i64x2,
        .list,
        .box,
        .dict,
        .set,
        .iter,
        .parse_tag_union_spec,
        .fields,
        .field,
        .crypto_sha256_digest,
        .crypto_sha256_hasher,
        .crypto_blake3_digest,
        .crypto_blake3_hasher,
        => null,
    };
}

fn generatedEncoderKeyMethod(view: ModuleView, ty: checked.CheckedTypeId) ?[]const u8 {
    return switch (view.checked_types.payload(ty)) {
        .alias => |alias| generatedEncoderKeyMethod(view, alias.backing),
        .nominal => |nominal| switch (nominal.builtin orelse return null) {
            .bool => "encode_key_bool",
            .str => "encode_key_str",
            .u8 => "encode_key_u8",
            .i8 => "encode_key_i8",
            .u16 => "encode_key_u16",
            .i16 => "encode_key_i16",
            .u32 => "encode_key_u32",
            .i32 => "encode_key_i32",
            .u64 => "encode_key_u64",
            .i64 => "encode_key_i64",
            .u128 => "encode_key_u128",
            .i128 => "encode_key_i128",
            .dec => "encode_key_dec",
            .f32 => "encode_key_f32",
            .f64 => "encode_key_f64",
            .try_,
            .u8x16,
            .i8x16,
            .u16x8,
            .i16x8,
            .u32x4,
            .i32x4,
            .u64x2,
            .i64x2,
            .list,
            .box,
            .dict,
            .set,
            .iter,
            .parse_tag_union_spec,
            .fields,
            .field,
            .crypto_sha256_digest,
            .crypto_sha256_hasher,
            .crypto_blake3_digest,
            .crypto_blake3_hasher,
            => null,
        },
        .pending,
        .err,
        .flex,
        .rigid,
        .record,
        .record_unbound,
        .tuple,
        .function,
        .empty_record,
        .tag_union,
        .empty_tag_union,
        => null,
    };
}

fn storedPrimitiveMatchesBuiltin(
    primitive: check.ConstStore.Primitive,
    builtin: checked.CheckedBuiltinNominal,
) bool {
    return switch (primitive) {
        .bool => builtin == .bool,
        .str => builtin == .str,
        .u8 => builtin == .u8,
        .i8 => builtin == .i8,
        .u16 => builtin == .u16,
        .i16 => builtin == .i16,
        .u32 => builtin == .u32,
        .i32 => builtin == .i32,
        .u64 => builtin == .u64,
        .i64 => builtin == .i64,
        .u128 => builtin == .u128,
        .i128 => builtin == .i128,
        .f32 => builtin == .f32,
        .f64 => builtin == .f64,
        .dec => builtin == .dec,
        .u8x16 => builtin == .u8x16,
        .i8x16 => builtin == .i8x16,
        .u16x8 => builtin == .u16x8,
        .i16x8 => builtin == .i16x8,
        .u32x4 => builtin == .u32x4,
        .i32x4 => builtin == .i32x4,
        .u64x2 => builtin == .u64x2,
        .i64x2 => builtin == .i64x2,
    };
}

const CheckedTryPayloads = struct {
    ok: checked.CheckedTypeId,
    err: checked.CheckedTypeId,
};

const CheckedTryErrorKinds = struct {
    missing: bool = false,
    null: bool = false,
    other: bool = false,
};

fn checkedTryErrorKinds(view: ModuleView, checked_ty: checked.CheckedTypeId) ?CheckedTryErrorKinds {
    const names = view.canonical_names orelse return null;
    var result = CheckedTryErrorKinds{};
    var has_tag = false;
    var current = checked_ty;
    var remaining = view.checked_types.payloadCount();
    while (remaining > 0) : (remaining -= 1) {
        switch (view.checked_types.payload(current)) {
            .alias => |alias| current = alias.backing,
            .nominal => |nominal| current = view.checked_types.nominalBackingTemplateForPayload(nominal) orelse return null,
            .tag_union => |tag_union| {
                for (tag_union.tags) |tag| {
                    if (tag.argsSlice(view.checked_types).len != 0) return null;
                    has_tag = true;
                    const text = names.tagLabelText(tag.name);
                    if (std.mem.eql(u8, text, "Missing")) {
                        if (result.missing) return null;
                        result.missing = true;
                    } else if (std.mem.eql(u8, text, "Null")) {
                        if (result.null) return null;
                        result.null = true;
                    } else {
                        result.other = true;
                    }
                }
                current = tag_union.ext;
            },
            .empty_tag_union => return if (has_tag) result else null,
            .flex, .rigid => |variable| return if (variable.row_default == .empty_tag_union and
                has_tag) result else null,
            .pending, .err, .record, .record_unbound, .tuple, .function, .empty_record => return null,
        }
    }
    boxyPlanInvariant("checked Try error row was cyclic");
}

fn checkedTryPayloads(view: ModuleView, checked_ty: checked.CheckedTypeId) ?CheckedTryPayloads {
    const names = view.canonical_names orelse return null;
    var current = checked_ty;
    var ok: ?checked.CheckedTypeId = null;
    var err: ?checked.CheckedTypeId = null;
    var remaining = view.checked_types.payloadCount();
    while (remaining > 0) : (remaining -= 1) {
        switch (view.checked_types.payload(current)) {
            .alias => |alias| current = alias.backing,
            .nominal => |nominal| current = view.checked_types.nominalBackingTemplateForPayload(nominal) orelse return null,
            .tag_union => |tag_union| {
                for (tag_union.tags) |tag| {
                    const args = tag.argsSlice(view.checked_types);
                    if (args.len != 1) return null;
                    const text = names.tagLabelText(tag.name);
                    if (std.mem.eql(u8, text, "Ok")) {
                        if (ok != null) return null;
                        ok = args[0];
                    } else if (std.mem.eql(u8, text, "Err")) {
                        if (err != null) return null;
                        err = args[0];
                    } else {
                        return null;
                    }
                }
                current = tag_union.ext;
            },
            .empty_tag_union => return if (ok != null and err != null) .{ .ok = ok.?, .err = err.? } else null,
            .flex, .rigid => |variable| return if (variable.row_default == .empty_tag_union and
                ok != null and err != null) .{ .ok = ok.?, .err = err.? } else null,
            .pending, .err, .record, .record_unbound, .tuple, .function, .empty_record => return null,
        }
    }
    boxyPlanInvariant("checked Try alias chain was cyclic");
}

fn hostedProcForTemplate(view: ModuleView, template_ref: checked_names.ProcedureTemplateRef) checked.HostedProc {
    for (view.hosted_procs.procs) |hosted| {
        if (checked_names.procedureTemplateRefEql(hosted.template, template_ref)) {
            return hosted;
        }
    }
    boxyPlanInvariant("hosted procedure template was missing from the checked hosted proc table");
}

fn hostedRepresentationForTemplate(
    view: ModuleView,
    template_ref: checked_names.ProcedureTemplateRef,
) checked.HostedRepresentationCapability {
    for (view.interface_capabilities.hosted_representations) |hosted| {
        if (checked_names.procedureTemplateRefEql(hosted.template, template_ref)) {
            return hosted;
        }
    }
    boxyPlanInvariant("hosted procedure template was missing from the interface hosted representation table");
}

fn moduleViewFromImported(imported: checked.ImportedModuleView) ModuleView {
    return .{
        .key = imported.key,
        .canonical_names = imported.canonical_names,
        .checked_types = imported.checked_types,
        .checked_bodies = imported.checked_bodies,
        .compile_time_roots = imported.compile_time_roots,
        .entry_wrappers = imported.entry_wrappers,
        .intrinsic_wrappers = imported.intrinsic_wrappers,
        .hosted_procs = imported.hosted_procs,
        .resolved_value_refs = imported.resolved_value_refs,
        .static_dispatch_plans = imported.static_dispatch_plans,
        .method_registry = imported.method_registry,
        .checked_procedure_templates = imported.checked_procedure_templates,
        .nested_proc_sites = imported.nested_proc_sites,
        .top_level_procedure_bindings = imported.top_level_procedure_bindings,
        .callable_eval_templates = imported.callable_eval_templates,
        .exported_procedure_bindings = imported.exported_procedure_bindings,
        .interface_capabilities = imported.interface_capabilities,
        .const_store = imported.const_store,
        .const_templates = imported.const_templates,
    };
}

fn moduleViewFromCheckedModule(checked_module: *const checked.CheckedModuleArtifact) ModuleView {
    return .{
        .key = checked_module.key,
        .canonical_names = &checked_module.canonical_names,
        .checked_types = checked_module.checked_types.view(),
        .checked_bodies = checked_module.checked_bodies.view(),
        .compile_time_roots = &checked_module.compile_time_roots,
        .entry_wrappers = &checked_module.entry_wrappers,
        .intrinsic_wrappers = &checked_module.intrinsic_wrappers,
        .hosted_procs = &checked_module.hosted_procs,
        .resolved_value_refs = &checked_module.resolved_value_refs,
        .static_dispatch_plans = &checked_module.static_dispatch_plans,
        .method_registry = &checked_module.method_registry,
        .checked_procedure_templates = &checked_module.checked_procedure_templates,
        .nested_proc_sites = &checked_module.nested_proc_sites,
        .top_level_procedure_bindings = &checked_module.top_level_procedure_bindings,
        .callable_eval_templates = checked_module.callable_eval_templates.view(),
        .exported_procedure_bindings = checked_module.exported_procedure_bindings.view(),
        .interface_capabilities = &checked_module.interface_capabilities,
        .const_store = &checked_module.const_store,
        .const_templates = &checked_module.const_templates,
    };
}

fn typeRef(view: ModuleView, ty: checked.CheckedTypeId) CheckedTypeIdentity {
    return .{ .module = view.key, .ty = ty };
}

fn typeRefEql(a: CheckedTypeIdentity, b: CheckedTypeIdentity) bool {
    return a.ty == b.ty and moduleKeyEqual(a.module, b.module);
}

fn optionalTypeRefEql(a: ?CheckedTypeIdentity, b: ?CheckedTypeIdentity) bool {
    if (a) |a_value| return if (b) |b_value| typeRefEql(a_value, b_value) else false;
    return b == null;
}

fn generatedCodecDerivationsEql(
    table: *const static_dispatch.StaticDispatchPlanTable,
    a: static_dispatch.GeneratedCodecDerivation,
    b: static_dispatch.GeneratedCodecDerivation,
) bool {
    if (a.kind != b.kind or
        a.constructor_ty != b.constructor_ty or
        a.runtime_ty != b.runtime_ty or
        a.shape_ty != b.shape_ty or
        a.encoding_ty != b.encoding_ty or
        a.state_ty != b.state_ty or
        a.error_ty != b.error_ty)
    {
        return false;
    }
    const a_calls = a.callsSlice(table);
    const b_calls = b.callsSlice(table);
    if (a_calls.len != b_calls.len) return false;
    for (a_calls, b_calls) |a_call, b_call| {
        if (!std.meta.eql(a_call, b_call)) return false;
    }
    return true;
}

fn exprRefEql(a: CheckedExprIdentity, b: CheckedExprIdentity) bool {
    return a.expr == b.expr and moduleKeyEqual(a.module, b.module);
}

fn moduleKeyEqual(a: checked.ModuleId, b: checked.ModuleId) bool {
    return std.mem.eql(u8, a.bytes[0..], b.bytes[0..]);
}

fn methodOwnerForModuleType(view: ModuleView, ty: checked.CheckedTypeId) ?static_dispatch.MethodOwner {
    var current = ty;
    var remaining = view.checked_types.payloadCount();
    while (true) {
        if (remaining == 0) boxyPlanInvariant("checked type alias chain was cyclic during boxy method owner lookup");
        remaining -= 1;
        const payload = view.checked_types.payload(current);
        if (payload != .alias) return methodOwnerForCheckedPayload(payload);
        current = payload.alias.backing;
    }
}

fn methodOwnerForCheckedPayload(payload: checked.CheckedTypePayload) ?static_dispatch.MethodOwner {
    if (payload != .nominal) return null;
    const nominal = payload.nominal;
    const nominal_owner: static_dispatch.MethodOwner = .{ .nominal = .{
        .module = nominal.origin_module,
        .type_name = nominal.name,
        .source_decl = nominal.source_decl,
    } };
    const builtin = nominal.builtin orelse return nominal_owner;
    if (builtin == .try_) return nominal_owner;
    return .{ .builtin = static_dispatch.builtinOwnerForCheckedBuiltin(builtin) };
}

fn methodOwnerInNames(
    source_names: *const checked_names.CanonicalNameStore,
    target_names: *const checked_names.CanonicalNameStore,
    owner: static_dispatch.MethodOwner,
) ?static_dispatch.MethodOwner {
    return switch (owner) {
        .builtin => |builtin| .{ .builtin = builtin },
        .nominal => |nominal| .{ .nominal = .{
            .module = target_names.lookupModuleIdentity(source_names.moduleIdentityBytes(nominal.module)) orelse return null,
            .type_name = target_names.lookupTypeName(source_names.typeNameText(nominal.type_name)) orelse return null,
            .source_decl = nominal.source_decl,
        } },
    };
}

fn directDispatchTarget(
    plans: *const static_dispatch.StaticDispatchPlanTable,
    resolution: static_dispatch.CheckedCallResolution,
) ?static_dispatch.MethodTarget {
    return switch (resolution) {
        .direct_closed, .direct_parametric => |direct| plans.evidenceNode(direct.evidence).target,
        .direct_pending => boxyPlanInvariant("unfinalized direct call reached Boxy planning"),
        .evidence_dependent,
        .structural,
        .checked_error,
        .@"unreachable",
        => null,
    };
}

fn iteratorPlanDoesNotExecute(plan: static_dispatch.IteratorForPlan) bool {
    inline for (.{ plan.iter.resolution, plan.next.resolution }) |resolution| {
        switch (resolution) {
            .checked_error, .@"unreachable" => return true,
            .direct_pending => boxyPlanInvariant("unfinalized iterator call reached Boxy planning"),
            .direct_closed, .direct_parametric, .evidence_dependent => {},
            .structural => boxyPlanInvariant("structural iterator dispatch reached Boxy planning"),
        }
    }
    return false;
}

fn descriptorReason(kind: RepresentationKind) ?DescriptorReason {
    return switch (kind) {
        .dynamic => .dynamic_payload,
        .record,
        .record_unbound,
        .tuple,
        .nominal,
        .tag_union,
        => .aggregate_contains_dynamic,
        .list => .list_element_dynamic,
        .box => .box_payload_dynamic,
        .in_progress,
        .primitive,
        .bool_tag_union,
        .erased_callable,
        .alias,
        .generated_field,
        .generated_field_names,
        .generated_tag_union_spec,
        .empty_record,
        .empty_tag_union,
        => null,
    };
}

fn boxyPlanInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("boxy plan invariant violated: {s}", .{message});
    }
    unreachable;
}

test "boxy planner records root wrapper plans from checked root metadata" {
    const gpa = std.testing.allocator;

    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.u64, @enumFromInt(fixtureTableIndex(0)), .{}) },
        .{ .nominal = builtinNominal(.u8, @enumFromInt(1), .{}) },
    };
    const view = checked.CheckedTypeStoreView{ .stored_payloads = &payloads };
    const roots = [_]checked.RootRequest{
        .{
            .order = 3,
            .module_idx = 0,
            .kind = .provided_export,
            .source = .{ .def = @enumFromInt(4) },
            .checked_type = @enumFromInt(fixtureTableIndex(0)),
            .abi = .roc,
            .exposure = .exported,
            .procedure_template = dummyProcedureTemplate(),
        },
    };
    const template_ref = dummyProcedureTemplate();
    var templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(template_ref, @enumFromInt(1), @enumFromInt(fixtureTableIndex(0)), .roc),
    };
    var template_table = checked.CheckedProcedureTemplateTable{ .templates = &templates };
    const root_view = ModuleView{
        .checked_types = view,
        .checked_procedure_templates = &template_table,
    };

    var plan = try analyzeProgram(gpa, .{ .root_view = root_view, .roots = &roots }, .{});
    defer plan.deinit();

    try std.testing.expectEqual(@as(usize, 1), plan.roots.items.len);
    try std.testing.expectEqual(@as(usize, 1), plan.workers.items.len);
    try std.testing.expectEqual(WorkerSource{ .procedure_template = dummyProcedureTemplate() }, plan.workers.items[0].source);
    try std.testing.expectEqual(plan.workers.items[0].id, plan.roots.items[0].worker);
    try std.testing.expectEqual(RootWrapperKind.host_shaped_wrapper, plan.roots.items[0].wrapper_kind);
    try std.testing.expectEqual(@as(u32, 3), plan.roots.items[0].request.order);
    try std.testing.expectEqual(plan.roots.items[0].host_rep, plan.roots.items[0].worker_rep);
    try expectTypeRef(root_view.key, @enumFromInt(fixtureTableIndex(0)), plan.roots.items[0].host_type);
    try expectTypeRef(root_view.key, @enumFromInt(1), plan.roots.items[0].source_type);
    try std.testing.expect(plan.roots.items[0].host_rep != plan.roots.items[0].source_rep);
    try std.testing.expectEqual(@as(usize, 1), plan.root_reps.items.len);
}

test "boxy planner walks callable eval finalized const function bodies" {
    const gpa = std.testing.allocator;

    const root_key = moduleKey(1);
    const template_ref = procedureTemplateRef(root_key, 0);
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .empty_record = {} },
        .{ .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(fixtureTableIndex(0)),
        } },
        .{ .nominal = builtinNominal(.u64, @enumFromInt(fixtureTableIndex(0)), .{}) },
    };
    const exprs = [_]checked.StoredCheckedExpr{
        .{
            .id = @enumFromInt(fixtureTableIndex(0)),
            .ty = @enumFromInt(1),
            .source_region = .zero(),
            .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
        },
        .{
            .id = @enumFromInt(1),
            .ty = @enumFromInt(fixtureTableIndex(0)),
            .source_region = .zero(),
            .data = .empty_record,
        },
    };
    const callable_templates = [_]checked.CallableEvalTemplate{
        .{
            .id = @enumFromInt(fixtureTableIndex(0)),
            .module_idx = 0,
            .pattern = @enumFromInt(fixtureTableIndex(0)),
            .root = @enumFromInt(fixtureTableIndex(0)),
            .source_scheme = .{},
            .checked_fn_root = @enumFromInt(1),
        },
    };
    const patterns = [_]checked.StoredCheckedPattern{.{
        .id = @enumFromInt(fixtureTableIndex(0)),
        .ty = @enumFromInt(2),
        .source_region = .zero(),
        .data = .{ .assign = @enumFromInt(fixtureTableIndex(0)) },
    }};
    const pattern_binders = [_]checked.CheckedPatternBinder{.{
        .id = @enumFromInt(fixtureTableIndex(0)),
        .pattern = @enumFromInt(fixtureTableIndex(0)),
        .reassignable = false,
    }};
    var const_store = check.ConstStore.ConstStore.init(gpa);
    defer const_store.deinit();
    const capture_value = try const_store.append(.{ .scalar = .{ .u64 = 42 } });
    const captures = [_]check.ConstStore.ConstCapture{.{
        .id = checked.CaptureId.fromBinder(@enumFromInt(fixtureTableIndex(0))),
        .ty = @enumFromInt(fixtureTableIndex(0)),
        .value = capture_value,
    }};
    const evidence_frames = [_]check.ConstStore.ConstFnEvidenceFrame{
        check.ConstStore.ConstFnEvidenceFrame.init(.root, null, 0, 0),
    };
    const fn_id = try const_store.appendFn(.{
        .fn_def = .{ .nested = .{
            .owner = template_ref,
            .site = @enumFromInt(fixtureTableIndex(0)),
            .context_fn_key = typeKey(1),
        } },
        .source_fn_ty = @enumFromInt(1),
        .source_fn_key = typeKey(1),
        .captures = &captures,
        .evidence_frames = &evidence_frames,
        .evidence_frame_head = 0,
    });
    var compile_time_roots = [_]checked.CompileTimeRoot{
        .{
            .id = @enumFromInt(fixtureTableIndex(0)),
            .module_idx = 0,
            .kind = .callable_binding,
            .source = .{ .def = @enumFromInt(fixtureTableIndex(0)) },
            .pattern = @enumFromInt(fixtureTableIndex(0)),
            .expr = @enumFromInt(fixtureTableIndex(0)),
            .checked_type = @enumFromInt(1),
            .request_eligibility = .eligible,
            .payload = .{ .fn_value = fn_id },
        },
    };
    var compile_time_root_table = checked.CompileTimeRootTable{ .roots = &compile_time_roots };
    var nested_sites = [_]checked.NestedProcSite{
        .{
            .site = @enumFromInt(fixtureTableIndex(0)),
            .owner_template = template_ref,
            .lexical_scope = .root,
            .evidence_source = .inherited,
            .evidence = .{},
            .path_start = 0,
            .path_len = 0,
            .kind = .local_function,
            .checked_expr = @enumFromInt(fixtureTableIndex(0)),
            .checked_pattern = null,
        },
    };
    var nested_proc_site_table = checked.NestedProcSiteTable{ .sites = &nested_sites };
    var bindings = [_]checked.TopLevelProcedureBinding{
        .{
            .source_scheme = .{},
            .body = .{ .callable_eval_template = @enumFromInt(fixtureTableIndex(0)) },
        },
    };
    var binding_table = checked.TopLevelProcedureBindingTable{ .bindings = &bindings };
    const roots = [_]checked.RootRequest{
        .{
            .order = 0,
            .module_idx = 0,
            .kind = .runtime_entrypoint,
            .source = .{ .def = @enumFromInt(fixtureTableIndex(0)) },
            .checked_type = @enumFromInt(1),
            .abi = .roc,
            .exposure = .private,
            .procedure_binding = @enumFromInt(fixtureTableIndex(0)),
        },
    };
    const root_view = ModuleView{
        .key = root_key,
        .checked_types = .{ .stored_payloads = &payloads },
        .checked_bodies = .{
            .stored_exprs = &exprs,
            .stored_patterns = &patterns,
            .pattern_binders = &pattern_binders,
        },
        .compile_time_roots = &compile_time_root_table,
        .nested_proc_sites = &nested_proc_site_table,
        .top_level_procedure_bindings = &binding_table,
        .callable_eval_templates = .{ .templates = &callable_templates },
        .const_store = &const_store,
    };

    var body_builder = Builder.init(gpa, .{ .root_view = root_view });
    defer body_builder.deinit();
    const body = body_builder.callableEvalTemplateBody(root_view, @enumFromInt(fixtureTableIndex(0)));
    const stored_fn = switch (body) {
        .checked_expr => |checked_body| checked_body.stored_fn orelse return error.TestUnexpectedResult,
        .intrinsic_wrapper, .hosted_proc, .unimplemented => return error.TestUnexpectedResult,
    };
    try std.testing.expectEqual(root_key, stored_fn.module);
    try std.testing.expectEqual(fn_id, stored_fn.fn_id);
    const worker_id = try body_builder.ensureWorker(
        .{ .procedure_binding = .{
            .artifact = root_key,
            .binding = @enumFromInt(fixtureTableIndex(0)),
        } },
        typeRef(root_view, @enumFromInt(1)),
        null,
    );
    const previous_worker = body_builder.active_worker;
    body_builder.active_worker = worker_id;
    defer body_builder.active_worker = previous_worker;
    try body_builder.analyzeWorkerBodyTypes(body);
    try std.testing.expect(body_builder.plan.repForSourceType(.{ .module = root_key, .ty = @enumFromInt(2) }) != null);

    var plan = try analyzeProgram(gpa, .{
        .root_view = root_view,
        .roots = &roots,
    }, .{});
    defer plan.deinit();

    try std.testing.expectEqual(@as(usize, 1), plan.workers.items.len);
    try std.testing.expectEqual(WorkerSource{ .nested_expr = .{ .module = root_key, .expr = @enumFromInt(fixtureTableIndex(0)) } }, plan.workers.items[0].source);
    try expectTypeRef(root_key, @enumFromInt(1), plan.workers.items[0].checked_type);
    try std.testing.expect(plan.repForSourceType(.{ .module = root_key, .ty = @enumFromInt(fixtureTableIndex(0)) }) != null);
}

test "boxy planner does not add hidden descriptor params to imported hosted workers" {
    const gpa = std.testing.allocator;

    var root_checked_module = minimalCheckedArtifact(gpa);
    defer root_checked_module.canonical_names.deinit();
    defer root_checked_module.checked_types.deinit(gpa);
    defer root_checked_module.checked_bodies.deinit(gpa);

    var import_checked_module = minimalCheckedArtifact(gpa);
    import_checked_module.key = moduleKey(2);
    defer import_checked_module.canonical_names.deinit();
    defer import_checked_module.checked_types.deinit(gpa);

    try import_checked_module.checked_types.type_id_pool.append(gpa, @as(checked.CheckedTypeId, @enumFromInt(fixtureTableIndex(0))));
    try import_checked_module.checked_types.payloads.append(gpa, .{ .flex = .{ .constraints = .{} } });
    try import_checked_module.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{ .start = 0, .len = 1 },
            .ret = @enumFromInt(fixtureTableIndex(0)),
        },
    });

    const import_template = procedureTemplateRef(import_checked_module.key, 0);
    var import_templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(import_template, @enumFromInt(1), @enumFromInt(fixtureTableIndex(0)), .hosted),
    };
    import_checked_module.checked_procedure_templates = .{ .templates = &import_templates };

    const hosted_order_key = "Import.dynamic_hosted";
    var hosted_procs = [_]checked.HostedProc{
        .{
            .module_idx = 0,
            .def_idx = @enumFromInt(fixtureTableIndex(0)),
            .expr_idx = @enumFromInt(fixtureTableIndex(0)),
            .external_symbol_name = @enumFromInt(fixtureTableIndex(0)),
            .deterministic_index = 0,
            .order_key_start = 0,
            .order_key_len = hosted_order_key.len,
            .proc = procedureValueRef(import_template),
            .template = import_template,
        },
    };
    import_checked_module.hosted_procs = .{
        .procs = &hosted_procs,
        .order_key_bytes = hosted_order_key,
    };
    var hosted_representations = [_]checked.HostedRepresentationCapability{
        .{
            .id = @enumFromInt(fixtureTableIndex(0)),
            .external_symbol_name = @enumFromInt(fixtureTableIndex(0)),
            .proc = procedureValueRef(import_template),
            .template = import_template,
            .host_checked_fn_root = @enumFromInt(1),
        },
    };
    import_checked_module.interface_capabilities.hosted_representations = &hosted_representations;

    const imported_binding = checked.ImportedProcedureBindingRef{
        .artifact = import_checked_module.key,
        .def = @enumFromInt(fixtureTableIndex(0)),
        .pattern = @enumFromInt(fixtureTableIndex(0)),
    };
    var exported_bindings = [_]checked.ImportedProcedureBindingView{
        .{
            .binding = imported_binding,
            .source_scheme = typeSchemeKey(2),
            .body = .{ .direct_template = .{
                .proc_value = procedureValueRef(import_template),
                .template = .{ .checked = import_template },
            } },
            .runtime_result_provenance = null,
            .template_closure = .{},
        },
    };
    import_checked_module.exported_procedure_bindings = .{ .bindings = &exported_bindings };

    try root_checked_module.checked_types.type_id_pool.append(gpa, @as(checked.CheckedTypeId, @enumFromInt(fixtureTableIndex(0))));
    try root_checked_module.checked_types.type_id_pool.append(gpa, @as(checked.CheckedTypeId, @enumFromInt(fixtureTableIndex(0))));
    try root_checked_module.checked_types.payloads.append(gpa, .{ .flex = .{ .constraints = .{} } });
    try root_checked_module.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{ .start = 0, .len = 1 },
            .ret = @enumFromInt(fixtureTableIndex(0)),
        },
    });
    try root_checked_module.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{ .start = 1, .len = 1 },
            .ret = @enumFromInt(fixtureTableIndex(0)),
        },
    });

    try root_checked_module.checked_bodies.pattern_id_pool.append(gpa, @as(checked.CheckedPatternId, @enumFromInt(fixtureTableIndex(0))));
    try root_checked_module.checked_bodies.expr_id_pool.append(gpa, @as(checked.CheckedExprId, @enumFromInt(3)));
    try root_checked_module.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(fixtureTableIndex(0)),
        .ty = @enumFromInt(fixtureTableIndex(0)),
        .source_region = .zero(),
        .data = .{ .assign = @enumFromInt(fixtureTableIndex(0)) },
    });
    try root_checked_module.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(fixtureTableIndex(0)),
        .ty = @enumFromInt(1),
        .source_region = .zero(),
        .data = .{ .lambda = .{
            .args = .{ .start = 0, .len = 1 },
            .body = @enumFromInt(1),
        } },
    });
    try root_checked_module.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(fixtureTableIndex(0)),
        .source_region = .zero(),
        .data = .{ .call = .{
            .func = @enumFromInt(2),
            .args = .{ .start = 0, .len = 1 },
            .called_via = .apply,
            .source_fn_ty_payload = @enumFromInt(2),
            .direct_target = @enumFromInt(fixtureTableIndex(0)),
        } },
    });
    try root_checked_module.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(2),
        .source_region = .zero(),
        .data = .{ .lookup_external = @enumFromInt(fixtureTableIndex(0)) },
    });
    try root_checked_module.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(fixtureTableIndex(0)),
        .source_region = .zero(),
        .data = .{ .lookup_local = .{ .pattern = @enumFromInt(fixtureTableIndex(0)), .resolved = null } },
    });

    const root_template = procedureTemplateRef(root_checked_module.key, 0);
    try root_checked_module.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(fixtureTableIndex(0)),
        .root_expr = @enumFromInt(fixtureTableIndex(0)),
        .owner_template = root_template,
    });
    var root_templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(root_template, @enumFromInt(1), @enumFromInt(fixtureTableIndex(0)), .roc),
    };
    root_checked_module.checked_procedure_templates = .{ .templates = &root_templates };

    const imported_use = checked.ProcedureUseTemplate{
        .binding = .{ .imported = imported_binding },
        .source_fn_ty_template = typeKey(2),
        .source_fn_ty_payload = @enumFromInt(2),
        .runtime_result_provenance = null,
    };
    var resolved_records = [_]checked.ResolvedValueRefRecord{
        .{
            .expr = @enumFromInt(2),
            .ref = .{ .imported_proc = imported_use },
            .checked_ty = @enumFromInt(2),
            .scope_depth = 0,
        },
    };
    var refs_by_expr = [_]?checked.ResolvedValueRefId{
        null,
        null,
        @as(checked.ResolvedValueRefId, @enumFromInt(fixtureTableIndex(0))),
        null,
    };
    root_checked_module.resolved_value_refs = .{
        .records = &resolved_records,
        .by_checked_expr = &refs_by_expr,
    };

    const imports = [_]checked.ImportedModuleView{checked.importedView(&import_checked_module)};

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(fixtureTableIndex(0)) },
        .checked_type = @enumFromInt(1),
        .abi = .roc,
        .exposure = .private,
        .procedure_template = root_template,
    };
    var plan = try analyzeProgram(gpa, .{
        .root_module = .{ .module = &root_checked_module, .roots = undefined },
        .imports = &imports,
        .roots = &.{root},
    }, .{});
    defer plan.deinit();

    try std.testing.expect(plan.descriptors.items.len != 0);
    const call_ref = CheckedExprIdentity{ .module = root_checked_module.key, .expr = @enumFromInt(1) };
    const direct = plan.directCallPlanForCall(call_ref, plan.roots.items[0].worker) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(plan.roots.items[0].worker, direct.caller);
    const callee_worker = plan.workers.items[@intFromEnum(direct.worker)];
    try std.testing.expectEqual(WorkerSource{ .procedure_use = imported_use }, callee_worker.source);
    try std.testing.expectEqual(@as(usize, 0), plan.hiddenDescriptorParamSlice(callee_worker.hidden_descs).len);
    try std.testing.expectEqual(@as(usize, 0), plan.directCallHiddenDescriptorArgSlice(direct.hidden_desc_args).len);
    const substitutions = plan.callTypeSubstitutionSlice(direct.arg_substitutions);
    try std.testing.expectEqual(@as(usize, 1), substitutions.len);
    try expectTypeRef(root_checked_module.key, @enumFromInt(fixtureTableIndex(0)), substitutions[0].operand_type);
    try std.testing.expectEqual(plan.repForSourceType(substitutions[0].operand_type).?, substitutions[0].operand_rep);
    try expectTypeRef(root_checked_module.key, @enumFromInt(fixtureTableIndex(0)), substitutions[0].call_type);
    try std.testing.expectEqual(plan.repForSourceType(substitutions[0].call_type).?, substitutions[0].call_rep);
    try expectTypeRef(import_checked_module.key, @enumFromInt(fixtureTableIndex(0)), plan.representations.items[@intFromEnum(substitutions[0].worker_rep)].source_type);
    const ret_substitution = direct.ret_substitution orelse return error.TestUnexpectedResult;
    try expectTypeRef(root_checked_module.key, @enumFromInt(fixtureTableIndex(0)), ret_substitution.call_type);
    try expectTypeRef(import_checked_module.key, @enumFromInt(fixtureTableIndex(0)), plan.representations.items[@intFromEnum(ret_substitution.worker_rep)].source_type);
}

test "boxy planner records relation-owned source type for platform-required direct calls" {
    const gpa = std.testing.allocator;

    var platform_checked_module = minimalCheckedArtifact(gpa);
    defer platform_checked_module.canonical_names.deinit();
    defer platform_checked_module.checked_types.deinit(gpa);
    defer platform_checked_module.checked_bodies.deinit(gpa);

    var app_checked_module = minimalCheckedArtifact(gpa);
    app_checked_module.key = moduleKey(3);
    defer app_checked_module.canonical_names.deinit();
    defer app_checked_module.checked_types.deinit(gpa);
    defer app_checked_module.checked_bodies.deinit(gpa);

    try app_checked_module.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(fixtureTableIndex(0)), .{}),
    });
    try app_checked_module.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(fixtureTableIndex(0)),
        },
    });
    const app_template = procedureTemplateRef(app_checked_module.key, 0);
    try app_checked_module.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(fixtureTableIndex(0)),
        .ty = @enumFromInt(1),
        .source_region = .zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try app_checked_module.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(fixtureTableIndex(0)),
        .source_region = .zero(),
        .data = .{ .numeral = .{ .literal = try testIntNumeral(1), .plan = null } },
    });
    try app_checked_module.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(fixtureTableIndex(0)),
        .root_expr = @enumFromInt(fixtureTableIndex(0)),
        .owner_template = app_template,
    });
    var app_templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(app_template, @enumFromInt(1), @enumFromInt(fixtureTableIndex(0)), .roc),
    };
    app_checked_module.checked_procedure_templates = .{ .templates = &app_templates };
    var app_bindings = [_]checked.TopLevelProcedureBinding{
        .{
            .source_scheme = typeSchemeKey(4),
            .body = .{ .direct_template = .{
                .proc_value = procedureValueRef(app_template),
                .template = .{ .checked = app_template },
            } },
        },
    };
    app_checked_module.top_level_procedure_bindings = .{ .bindings = &app_bindings };

    try platform_checked_module.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(fixtureTableIndex(0)), .{}),
    });
    try platform_checked_module.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(fixtureTableIndex(0)),
        },
    });
    try platform_checked_module.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(fixtureTableIndex(0)),
        },
    });

    const platform_template = procedureTemplateRef(platform_checked_module.key, 0);
    try platform_checked_module.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(fixtureTableIndex(0)),
        .ty = @enumFromInt(1),
        .source_region = .zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try platform_checked_module.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(fixtureTableIndex(0)),
        .source_region = .zero(),
        .data = .{ .call = .{
            .func = @enumFromInt(2),
            .args = .{},
            .called_via = .apply,
            .source_fn_ty_payload = @enumFromInt(1),
            .direct_target = @enumFromInt(fixtureTableIndex(0)),
        } },
    });
    try platform_checked_module.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(1),
        .source_region = .zero(),
        .data = .{ .lookup_required = @as(?checked.ResolvedValueRefId, @enumFromInt(fixtureTableIndex(0))) },
    });
    try platform_checked_module.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(fixtureTableIndex(0)),
        .root_expr = @enumFromInt(fixtureTableIndex(0)),
        .owner_template = platform_template,
    });
    var platform_templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(platform_template, @enumFromInt(1), @enumFromInt(fixtureTableIndex(0)), .roc),
    };
    platform_checked_module.checked_procedure_templates = .{ .templates = &platform_templates };

    const required = checked.RequiredAppProcedureRef{
        .artifact = app_checked_module.key,
        .app_value = .{
            .artifact = app_checked_module.key,
            .pattern = @enumFromInt(fixtureTableIndex(0)),
        },
        .procedure_binding = @enumFromInt(fixtureTableIndex(0)),
    };
    const required_use = checked.ProcedureUseTemplate{
        .binding = .{ .platform_required = required },
        .source_fn_ty_template = typeKey(5),
        .source_fn_ty_payload = @enumFromInt(2),
        .runtime_result_provenance = null,
    };
    var resolved_records = [_]checked.ResolvedValueRefRecord{
        .{
            .expr = @enumFromInt(2),
            .ref = .{ .platform_required_proc = .{
                .binding = @enumFromInt(fixtureTableIndex(0)),
                .procedure = required_use,
            } },
            .checked_ty = @enumFromInt(1),
            .scope_depth = 0,
        },
    };
    var refs_by_expr = [_]?checked.ResolvedValueRefId{
        null,
        null,
        @as(checked.ResolvedValueRefId, @enumFromInt(fixtureTableIndex(0))),
    };
    platform_checked_module.resolved_value_refs = .{
        .records = &resolved_records,
        .by_checked_expr = &refs_by_expr,
    };

    const imports = [_]checked.ImportedModuleView{checked.importedView(&app_checked_module)};

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(fixtureTableIndex(0)) },
        .checked_type = @enumFromInt(1),
        .abi = .roc,
        .exposure = .private,
        .procedure_template = platform_template,
    };
    var plan = try analyzeProgram(gpa, .{
        .root_module = .{ .module = &platform_checked_module, .roots = undefined },
        .imports = &imports,
        .roots = &.{root},
    }, .{});
    defer plan.deinit();

    const direct = plan.directCallPlanForCall(.{
        .module = platform_checked_module.key,
        .expr = @enumFromInt(1),
    }, plan.roots.items[0].worker) orelse return error.TestUnexpectedResult;
    try expectTypeRef(platform_checked_module.key, @enumFromInt(2), direct.source_fn_type);
    try std.testing.expect(plan.repForSourceType(.{ .module = platform_checked_module.key, .ty = @enumFromInt(2) }) != null);
    try std.testing.expectEqual(@as(usize, 0), plan.callTypeSubstitutionSlice(direct.arg_substitutions).len);
    const ret_substitution = direct.ret_substitution orelse return error.TestUnexpectedResult;
    try expectTypeRef(platform_checked_module.key, @enumFromInt(fixtureTableIndex(0)), ret_substitution.call_type);
    try std.testing.expectEqual(plan.repForSourceType(ret_substitution.call_type).?, ret_substitution.call_rep);
    try expectTypeRef(app_checked_module.key, @enumFromInt(fixtureTableIndex(0)), plan.representations.items[@intFromEnum(ret_substitution.worker_rep)].source_type);
}

test "boxy planner records explicit source type representation bindings" {
    const gpa = std.testing.allocator;

    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.u64, @enumFromInt(fixtureTableIndex(0)), .{}) },
        .{ .nominal = builtinNominal(.u8, @enumFromInt(1), .{}) },
        .{ .function = .{
            .kind = .pure,
            .args = .{ .start = 0, .len = 1 },
            .ret = @enumFromInt(1),
        } },
    };
    const type_pool = [_]checked.CheckedTypeId{@enumFromInt(fixtureTableIndex(0))};
    const view = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .type_id_pool = &type_pool,
    };

    var plan = try analyzeCheckedTypes(gpa, view, &.{@as(checked.CheckedTypeId, @enumFromInt(2))}, .{});
    defer plan.deinit();

    try std.testing.expectEqual(@as(usize, 3), plan.type_reps.items.len);
    try std.testing.expectEqual(plan.root_reps.items[0], plan.repForSourceType(rootTypeRef(@enumFromInt(2))).?);
    try std.testing.expect(plan.repForSourceType(rootTypeRef(@enumFromInt(fixtureTableIndex(0)))) != null);
    try std.testing.expect(plan.repForSourceType(rootTypeRef(@enumFromInt(1))) != null);
    try std.testing.expect(plan.repForSourceType(rootTypeRef(@enumFromInt(99))) == null);
}

test "boxy planner classifies constrained variables as dynamic with descriptor and dictionary requirements" {
    const gpa = std.testing.allocator;

    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(2),
        } },
        .{ .flex = .{ .constraints = .{ .start = 0, .len = 1 } } },
        .{ .nominal = builtinNominal(.u64, @enumFromInt(2), .{}) },
    };
    const constraints = [_]checked.CheckedStaticDispatchConstraint{
        .{
            .fn_name = @enumFromInt(9),
            .fn_ty = @enumFromInt(fixtureTableIndex(0)),
            .origin = .method_call,
        },
    };
    const view = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .constraint_pool = &constraints,
    };

    var plan = try analyzeCheckedTypes(gpa, view, &.{@as(checked.CheckedTypeId, @enumFromInt(1))}, .{});
    defer plan.deinit();

    try std.testing.expectEqual(@as(usize, 1), plan.root_reps.items.len);
    const rep = plan.representations.items[@intFromEnum(plan.root_reps.items[0])];
    try std.testing.expectEqual(RepresentationKind{ .dynamic = .flex }, rep.kind);
    try std.testing.expect(rep.contains_dynamic);
    try std.testing.expect(rep.descriptor != null);
    try std.testing.expectEqual(@as(usize, 1), plan.dictionarySlice(rep.dictionaries).len);
    try std.testing.expectEqual(@as(usize, 1), plan.descriptors.items.len);
    try std.testing.expectEqual(DescriptorReason.dynamic_payload, plan.descriptors.items[0].reason);
}

test "boxy planner keeps checked specialization defaults dynamically represented" {
    const gpa = std.testing.allocator;

    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(2),
        } },
        .{ .flex = .{
            .constraints = .{ .start = 0, .len = 1 },
            .numeric_default_phase = .mono_specialization,
        } },
        .{ .nominal = builtinNominal(.u64, @enumFromInt(2), .{}) },
    };
    const constraints = [_]checked.CheckedStaticDispatchConstraint{
        .{
            .fn_name = @enumFromInt(9),
            .fn_ty = @enumFromInt(fixtureTableIndex(0)),
            .origin = .method_call,
        },
    };
    const view = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .constraint_pool = &constraints,
    };

    var plan = try analyzeCheckedTypes(gpa, view, &.{@as(checked.CheckedTypeId, @enumFromInt(1))}, .{});
    defer plan.deinit();

    const rep = plan.representations.items[@intFromEnum(plan.root_reps.items[0])];
    try std.testing.expectEqual(RepresentationKind{ .dynamic = .flex }, rep.kind);
    try std.testing.expect(rep.contains_dynamic);
    try std.testing.expect(rep.descriptor != null);
    try std.testing.expectEqual(@as(usize, 1), plan.dictionarySlice(rep.dictionaries).len);
}

test "boxy dictionary slots are stable across module ids and requirement subsets" {
    const gpa = std.testing.allocator;

    var root_names = checked_names.CanonicalNameStore.init(gpa);
    defer root_names.deinit();
    var source_names = checked_names.CanonicalNameStore.init(gpa);
    defer source_names.deinit();

    const root_is_eq = try root_names.internMethodName("is_eq");
    const root_to_hash = try root_names.internMethodName("to_hash");
    const source_to_hash = try source_names.internMethodName("to_hash");
    _ = try source_names.internMethodName("is_eq");
    try std.testing.expect(root_to_hash != source_to_hash);

    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.u64, @enumFromInt(fixtureTableIndex(0)), .{}) },
    };
    const checked_types = checked.CheckedTypeStoreView{ .stored_payloads = &payloads };
    const root_key = moduleKey(1);
    const source_key = moduleKey(2);
    const source_views = [_]ModuleView{.{
        .key = source_key,
        .canonical_names = &source_names,
        .checked_types = checked_types,
    }};
    var builder = Builder.init(gpa, .{
        .root_view = .{
            .key = root_key,
            .canonical_names = &root_names,
            .checked_types = checked_types,
        },
        .extra_module_views = &source_views,
    });
    defer builder.deinit();

    const root_span = try builder.appendDictionaryRequirements(
        .{ .module = root_key, .ty = @enumFromInt(fixtureTableIndex(0)) },
        &.{
            .{ .fn_name = root_is_eq, .fn_ty = @enumFromInt(fixtureTableIndex(0)), .origin = .method_call },
            .{ .fn_name = root_to_hash, .fn_ty = @enumFromInt(fixtureTableIndex(0)), .origin = .method_call },
        },
    );
    const source_span = try builder.appendDictionaryRequirements(
        .{ .module = source_key, .ty = @enumFromInt(fixtureTableIndex(0)) },
        &.{
            .{ .fn_name = source_to_hash, .fn_ty = @enumFromInt(fixtureTableIndex(0)), .origin = .method_call },
        },
    );

    const root_requirements = builder.plan.dictionarySlice(root_span);
    const source_requirements = builder.plan.dictionarySlice(source_span);
    try std.testing.expectEqual(@as(usize, 2), builder.plan.dictionary_method_slots.items.len);
    try std.testing.expect(root_requirements[0].slot != root_requirements[1].slot);
    try std.testing.expectEqual(root_requirements[1].slot, source_requirements[0].slot);
}

test "boxy dictionary traversal follows checked evidence order through aliases and nominals" {
    const gpa = std.testing.allocator;
    var plan = ProgramPlan.init(gpa);
    defer plan.deinit();

    try plan.children.appendSlice(gpa, &.{
        .{ .role = .alias_backing, .source_type = rootTypeRef(@enumFromInt(10)), .rep = @enumFromInt(10) },
        .{ .role = .{ .alias_arg = 0 }, .source_type = rootTypeRef(@enumFromInt(11)), .rep = @enumFromInt(11) },
        .{ .role = .nominal_backing, .source_type = rootTypeRef(@enumFromInt(20)), .rep = @enumFromInt(20) },
        .{ .role = .{ .nominal_arg = 0 }, .source_type = rootTypeRef(@enumFromInt(21)), .rep = @enumFromInt(21) },
        .{ .role = .{ .nominal_arg = 1 }, .source_type = rootTypeRef(@enumFromInt(22)), .rep = @enumFromInt(22) },
        .{ .role = .{ .nominal_padding_field = 0 }, .source_type = rootTypeRef(@enumFromInt(23)), .rep = @enumFromInt(23) },
    });
    try plan.representations.appendSlice(gpa, &.{
        .{
            .source_type = rootTypeRef(@enumFromInt(fixtureTableIndex(0))),
            .kind = .alias,
            .children = .{ .start = 0, .len = 2 },
        },
        .{
            .source_type = rootTypeRef(@enumFromInt(1)),
            .kind = .{ .nominal = .transparent },
            .children = .{ .start = 2, .len = 4 },
        },
    });

    try std.testing.expectEqual(@as(TypeRepId, @enumFromInt(11)), plan.dictionaryChildAt(@enumFromInt(fixtureTableIndex(0)), 0).?.rep);
    try std.testing.expectEqual(@as(TypeRepId, @enumFromInt(10)), plan.dictionaryChildAt(@enumFromInt(fixtureTableIndex(0)), 1).?.rep);
    try std.testing.expect(plan.dictionaryChildAt(@enumFromInt(fixtureTableIndex(0)), 2) == null);

    try std.testing.expectEqual(@as(TypeRepId, @enumFromInt(21)), plan.dictionaryChildAt(@enumFromInt(1), 0).?.rep);
    try std.testing.expectEqual(@as(TypeRepId, @enumFromInt(22)), plan.dictionaryChildAt(@enumFromInt(1), 1).?.rep);
    try std.testing.expectEqual(@as(TypeRepId, @enumFromInt(20)), plan.dictionaryChildAt(@enumFromInt(1), 2).?.rep);
    try std.testing.expectEqual(@as(TypeRepId, @enumFromInt(23)), plan.dictionaryChildAt(@enumFromInt(1), 3).?.rep);
    try std.testing.expect(plan.dictionaryChildAt(@enumFromInt(1), 4) == null);
}

test "direct call metadata uses instantiated nominal arguments inside generalized backings" {
    const gpa = std.testing.allocator;
    var builder = Builder.init(gpa, .{});
    defer builder.deinit();

    const worker_nominal: TypeRepId = @enumFromInt(fixtureTableIndex(0));
    const backing: TypeRepId = @enumFromInt(1);
    const worker_arg: TypeRepId = @enumFromInt(2);
    const call_nominal: TypeRepId = @enumFromInt(3);
    const generalized_call_arg: TypeRepId = @enumFromInt(4);
    const exact_arg: TypeRepId = @enumFromInt(5);

    try builder.plan.children.appendSlice(gpa, &.{
        .{ .role = .nominal_backing, .source_type = rootTypeRef(@enumFromInt(1)), .rep = backing },
        .{ .role = .{ .nominal_arg = 0 }, .source_type = rootTypeRef(@enumFromInt(2)), .rep = worker_arg },
        .{ .role = .{ .tuple_elem = 0 }, .source_type = rootTypeRef(@enumFromInt(2)), .rep = worker_arg },
        .{ .role = .nominal_backing, .source_type = rootTypeRef(@enumFromInt(1)), .rep = backing },
        .{ .role = .{ .nominal_arg = 0 }, .source_type = rootTypeRef(@enumFromInt(4)), .rep = generalized_call_arg },
    });
    try builder.plan.nominal_backing_arg_substitutions.appendSlice(gpa, &.{
        .{ .arg_index = 0, .formal_rep = worker_arg, .actual_rep = worker_arg },
        .{ .arg_index = 0, .formal_rep = worker_arg, .actual_rep = exact_arg },
    });
    try builder.plan.dictionaries.append(gpa, .{
        .source_type = rootTypeRef(@enumFromInt(2)),
        .constraint_index = 0,
        .slot = 0,
        .fn_name = @enumFromInt(fixtureTableIndex(0)),
        .fn_ty = rootTypeRef(@enumFromInt(2)),
        .origin = .method_call,
        .binop_negated = false,
        .num_literal = null,
    });
    try builder.plan.representations.appendSlice(gpa, &.{
        .{ .source_type = rootTypeRef(@enumFromInt(fixtureTableIndex(0))), .kind = .{ .nominal = .transparent }, .children = .{ .start = 0, .len = 2 }, .nominal_backing_arg_substitutions = .{ .start = 0, .len = 1 } },
        .{ .source_type = rootTypeRef(@enumFromInt(1)), .kind = .tuple, .children = .{ .start = 2, .len = 1 } },
        .{ .source_type = rootTypeRef(@enumFromInt(2)), .kind = .{ .dynamic = .flex }, .descriptor = @enumFromInt(fixtureTableIndex(0)), .dictionaries = .{ .start = 0, .len = 1 }, .contains_dynamic = true },
        .{ .source_type = rootTypeRef(@enumFromInt(3)), .kind = .{ .nominal = .transparent }, .children = .{ .start = 3, .len = 2 }, .nominal_backing_arg_substitutions = .{ .start = 1, .len = 1 } },
        .{ .source_type = rootTypeRef(@enumFromInt(4)), .kind = .{ .dynamic = .flex }, .descriptor = @enumFromInt(1), .contains_dynamic = true },
        .{ .source_type = rootTypeRef(@enumFromInt(5)), .kind = .{ .primitive = .str } },
    });

    const params = [_]HiddenDescriptorParam{.{
        .source_type = rootTypeRef(@enumFromInt(2)),
        .rep = worker_arg,
        .desc = @enumFromInt(fixtureTableIndex(0)),
    }};
    var pending = std.ArrayList(DirectCallHiddenDescriptorArg).empty;
    defer pending.deinit(gpa);
    var seen_reps = collections.DenseMap(TypeRepId, void).init(gpa);
    defer seen_reps.deinit();
    var seen_descriptor_reps = collections.DenseMap(TypeRepId, void).init(gpa);
    defer seen_descriptor_reps.deinit();
    var substitutions = Builder.CallDescriptorRepSubstitutionMap{};
    defer substitutions.deinit(gpa);
    var next_param: usize = 0;

    try builder.collectCallHiddenDescriptorArgs(
        worker_nominal,
        call_nominal,
        call_nominal,
        call_nominal,
        0,
        &params,
        &next_param,
        &pending,
        &seen_reps,
        &seen_descriptor_reps,
        &substitutions,
        false,
    );

    try std.testing.expectEqual(@as(usize, 1), pending.items.len);
    try std.testing.expectEqual(exact_arg, pending.items[0].rep);
    try std.testing.expectEqual(call_nominal, pending.items[0].source_value_rep.?);

    var dictionary_substitutions = Builder.CallDictionaryRepSubstitutionMap{};
    defer dictionary_substitutions.deinit(gpa);
    var seen_dictionary_pairs = std.AutoHashMap(u64, void).init(gpa);
    defer seen_dictionary_pairs.deinit();
    try builder.collectCallDictionaryRepSubstitutions(
        worker_nominal,
        call_nominal,
        &dictionary_substitutions,
        &seen_dictionary_pairs,
    );
    try std.testing.expectEqual(exact_arg, dictionary_substitutions.get(worker_arg).?);
}

test "direct call descriptors use operand nominal substitutions over generic call types" {
    const gpa = std.testing.allocator;
    var builder = Builder.init(gpa, .{});
    defer builder.deinit();

    const worker_nominal: TypeRepId = @enumFromInt(fixtureTableIndex(0));
    const backing_rep: TypeRepId = @enumFromInt(1);
    const worker_arg: TypeRepId = @enumFromInt(2);
    const call_nominal: TypeRepId = @enumFromInt(3);
    const generalized_call_arg: TypeRepId = @enumFromInt(4);
    const operand_nominal: TypeRepId = @enumFromInt(5);
    const exact_operand_arg: TypeRepId = @enumFromInt(6);
    const call_formal: TypeRepId = @enumFromInt(7);
    const operand_formal: TypeRepId = @enumFromInt(8);

    try builder.plan.children.appendSlice(gpa, &.{
        .{ .role = .nominal_backing, .source_type = rootTypeRef(@enumFromInt(1)), .rep = backing_rep },
        .{ .role = .{ .nominal_arg = 0 }, .source_type = rootTypeRef(@enumFromInt(2)), .rep = worker_arg },
        .{ .role = .{ .tuple_elem = 0 }, .source_type = rootTypeRef(@enumFromInt(2)), .rep = worker_arg },
        .{ .role = .nominal_backing, .source_type = rootTypeRef(@enumFromInt(1)), .rep = backing_rep },
        .{ .role = .{ .nominal_arg = 0 }, .source_type = rootTypeRef(@enumFromInt(4)), .rep = generalized_call_arg },
        .{ .role = .nominal_backing, .source_type = rootTypeRef(@enumFromInt(1)), .rep = backing_rep },
        .{ .role = .{ .nominal_arg = 0 }, .source_type = rootTypeRef(@enumFromInt(6)), .rep = exact_operand_arg },
    });
    try builder.plan.nominal_backing_arg_substitutions.appendSlice(gpa, &.{
        .{ .arg_index = 0, .formal_rep = worker_arg, .actual_rep = worker_arg },
        .{ .arg_index = 0, .formal_rep = call_formal, .actual_rep = generalized_call_arg },
        .{ .arg_index = 0, .formal_rep = operand_formal, .actual_rep = exact_operand_arg },
    });
    try builder.plan.representations.appendSlice(gpa, &.{
        .{ .source_type = rootTypeRef(@enumFromInt(fixtureTableIndex(0))), .kind = .{ .nominal = .builtin_other }, .children = .{ .start = 0, .len = 2 }, .nominal_backing_arg_substitutions = .{ .start = 0, .len = 1 } },
        .{ .source_type = rootTypeRef(@enumFromInt(1)), .kind = .tuple, .children = .{ .start = 2, .len = 1 } },
        .{ .source_type = rootTypeRef(@enumFromInt(2)), .kind = .{ .dynamic = .rigid }, .descriptor = @enumFromInt(fixtureTableIndex(0)), .contains_dynamic = true },
        .{ .source_type = rootTypeRef(@enumFromInt(3)), .kind = .{ .nominal = .builtin_other }, .children = .{ .start = 3, .len = 2 }, .nominal_backing_arg_substitutions = .{ .start = 1, .len = 1 } },
        .{ .source_type = rootTypeRef(@enumFromInt(4)), .kind = .{ .dynamic = .rigid }, .descriptor = @enumFromInt(1), .contains_dynamic = true },
        .{ .source_type = rootTypeRef(@enumFromInt(5)), .kind = .{ .nominal = .builtin_other }, .children = .{ .start = 5, .len = 2 }, .nominal_backing_arg_substitutions = .{ .start = 2, .len = 1 } },
        .{ .source_type = rootTypeRef(@enumFromInt(6)), .kind = .{ .primitive = .str } },
        .{ .source_type = rootTypeRef(@enumFromInt(7)), .kind = .{ .dynamic = .rigid }, .contains_dynamic = true },
        .{ .source_type = rootTypeRef(@enumFromInt(8)), .kind = .{ .dynamic = .rigid }, .contains_dynamic = true },
    });

    const params = [_]HiddenDescriptorParam{.{
        .source_type = rootTypeRef(@enumFromInt(2)),
        .rep = worker_arg,
        .desc = @enumFromInt(fixtureTableIndex(0)),
    }};
    var pending = std.ArrayList(DirectCallHiddenDescriptorArg).empty;
    defer pending.deinit(gpa);
    var seen_reps = collections.DenseMap(TypeRepId, void).init(gpa);
    defer seen_reps.deinit();
    var seen_descriptor_reps = collections.DenseMap(TypeRepId, void).init(gpa);
    defer seen_descriptor_reps.deinit();
    var substitutions = Builder.CallDescriptorRepSubstitutionMap{};
    defer substitutions.deinit(gpa);
    var next_param: usize = 0;

    try builder.collectCallHiddenDescriptorArgs(
        worker_nominal,
        call_nominal,
        call_nominal,
        operand_nominal,
        0,
        &params,
        &next_param,
        &pending,
        &seen_reps,
        &seen_descriptor_reps,
        &substitutions,
        false,
    );

    try std.testing.expectEqual(@as(usize, 1), pending.items.len);
    try std.testing.expectEqual(exact_operand_arg, pending.items[0].rep);
    try std.testing.expectEqual(operand_nominal, pending.items[0].source_value_rep.?);
}

test "evidence representation paths use exact nominal backing substitutions" {
    const gpa = std.testing.allocator;
    var builder = Builder.init(gpa, .{});
    defer builder.deinit();

    const nominal_rep: TypeRepId = @enumFromInt(1);
    const generalized_arg: TypeRepId = @enumFromInt(2);
    const backing_rep: TypeRepId = @enumFromInt(3);
    const exact_arg: TypeRepId = @enumFromInt(4);

    try builder.plan.children.appendSlice(gpa, &.{
        .{ .role = .{ .function_arg = 0 }, .source_type = rootTypeRef(@enumFromInt(1)), .rep = nominal_rep },
        .{ .role = .nominal_backing, .source_type = rootTypeRef(@enumFromInt(3)), .rep = backing_rep },
        .{ .role = .{ .nominal_arg = 0 }, .source_type = rootTypeRef(@enumFromInt(2)), .rep = generalized_arg },
    });
    try builder.plan.nominal_backing_arg_substitutions.append(gpa, .{
        .arg_index = 0,
        .formal_rep = generalized_arg,
        .actual_rep = exact_arg,
    });
    try builder.plan.representations.appendSlice(gpa, &.{
        .{ .source_type = rootTypeRef(@enumFromInt(fixtureTableIndex(0))), .kind = .{ .erased_callable = .pure }, .children = .{ .start = 0, .len = 1 } },
        .{ .source_type = rootTypeRef(@enumFromInt(1)), .kind = .{ .nominal = .builtin_other }, .children = .{ .start = 1, .len = 2 }, .nominal_backing_arg_substitutions = .{ .start = 0, .len = 1 } },
        .{ .source_type = rootTypeRef(@enumFromInt(2)), .kind = .{ .dynamic = .rigid }, .contains_dynamic = true },
        .{ .source_type = rootTypeRef(@enumFromInt(3)), .kind = .empty_record },
        .{ .source_type = rootTypeRef(@enumFromInt(4)), .kind = .{ .primitive = .str } },
    });
    try builder.plan.type_reps.append(gpa, .{
        .source_type = rootTypeRef(@enumFromInt(4)),
        .rep = exact_arg,
    });

    const path = [_]static_dispatch.EvidencePathStep{
        .{ .kind = @intFromEnum(static_dispatch.EvidencePathStep.Kind.fn_arg), .data = 0 },
        .{ .kind = @intFromEnum(static_dispatch.EvidencePathStep.Kind.nominal_arg), .data = 0 },
    };
    try std.testing.expectEqual(
        exact_arg,
        try builder.evidenceCallRepAtPath(
            builder.root_view,
            &path,
            rootTypeRef(@enumFromInt(fixtureTableIndex(0))),
            &.{nominal_rep},
            rootTypeRef(@enumFromInt(fixtureTableIndex(0))),
        ),
    );
    try std.testing.expectEqual(
        exact_arg,
        try builder.evidenceCallRepAtPath(
            builder.root_view,
            &.{},
            rootTypeRef(@enumFromInt(4)),
            &.{},
            rootTypeRef(@enumFromInt(fixtureTableIndex(0))),
        ),
    );
}

test "dictionary method hidden descriptors preserve exact implementation substitutions" {
    const gpa = std.testing.allocator;
    var builder = Builder.init(gpa, .{});
    defer builder.deinit();

    const exact_list_rep: TypeRepId = @enumFromInt(fixtureTableIndex(0));
    const exact_elem_rep: TypeRepId = @enumFromInt(1);
    const worker_left_rep: TypeRepId = @enumFromInt(2);
    const worker_elem_rep: TypeRepId = @enumFromInt(3);
    const worker_right_rep: TypeRepId = @enumFromInt(4);
    const worker_fn_rep: TypeRepId = @enumFromInt(5);
    try builder.plan.representations.appendSlice(gpa, &.{
        .{ .source_type = rootTypeRef(@enumFromInt(fixtureTableIndex(0))), .kind = .list },
        .{ .source_type = rootTypeRef(@enumFromInt(1)), .kind = .{ .primitive = .str } },
        .{ .source_type = rootTypeRef(@enumFromInt(2)), .kind = .{ .dynamic = .flex }, .descriptor = @enumFromInt(fixtureTableIndex(0)), .contains_dynamic = true },
        .{ .source_type = rootTypeRef(@enumFromInt(3)), .kind = .{ .dynamic = .flex }, .descriptor = @enumFromInt(1), .contains_dynamic = true },
        .{ .source_type = rootTypeRef(@enumFromInt(4)), .kind = .{ .dynamic = .flex }, .descriptor = @enumFromInt(2), .contains_dynamic = true },
        .{ .source_type = rootTypeRef(@enumFromInt(5)), .kind = .{ .erased_callable = .pure } },
    });
    try builder.plan.hidden_descriptor_params.appendSlice(gpa, &.{
        .{ .source_type = rootTypeRef(@enumFromInt(2)), .rep = worker_left_rep, .desc = @enumFromInt(fixtureTableIndex(0)) },
        .{ .source_type = rootTypeRef(@enumFromInt(3)), .rep = worker_elem_rep, .desc = @enumFromInt(1) },
        .{ .source_type = rootTypeRef(@enumFromInt(4)), .rep = worker_right_rep, .desc = @enumFromInt(2) },
    });
    try builder.plan.workers.append(gpa, .{
        .id = @enumFromInt(fixtureTableIndex(0)),
        .source = .{ .procedure_template = dummyProcedureTemplate() },
        .checked_type = rootTypeRef(@enumFromInt(5)),
        .rep = worker_fn_rep,
        .hidden_descs = .{ .start = 0, .len = 3 },
    });

    const worker_args_start: u32 = @intCast(builder.plan.direct_call_hidden_desc_args.items.len);
    try builder.plan.direct_call_hidden_desc_args.appendSlice(gpa, &.{
        .{ .worker_desc = @enumFromInt(fixtureTableIndex(0)), .worker_rep = worker_left_rep, .source_type = rootTypeRef(@enumFromInt(fixtureTableIndex(0))), .rep = exact_list_rep, .source_arg_index = 0, .source_value_rep = exact_list_rep },
        .{ .worker_desc = @enumFromInt(1), .worker_rep = worker_elem_rep, .source_type = rootTypeRef(@enumFromInt(1)), .rep = exact_elem_rep, .source_arg_index = 0, .source_value_rep = exact_list_rep },
        .{ .worker_desc = @enumFromInt(2), .worker_rep = worker_right_rep, .source_type = rootTypeRef(@enumFromInt(fixtureTableIndex(0))), .rep = exact_list_rep, .source_arg_index = 1, .source_value_rep = exact_list_rep },
    });
    const requirement_args_start: u32 = @intCast(builder.plan.direct_call_hidden_desc_args.items.len);
    try builder.plan.direct_call_hidden_desc_args.append(gpa, .{
        .worker_desc = @enumFromInt(3),
        .worker_rep = exact_list_rep,
        .source_type = rootTypeRef(@enumFromInt(fixtureTableIndex(0))),
        .rep = exact_list_rep,
        .source_arg_index = 0,
        .source_value_rep = exact_list_rep,
    });

    const sources = try builder.dictionaryMethodHiddenDescriptorSources(
        @enumFromInt(fixtureTableIndex(0)),
        .{ .start = worker_args_start, .len = 3 },
        .{ .start = requirement_args_start, .len = 1 },
    );
    try std.testing.expectEqualSlices(
        DictionaryMethodHiddenDescriptorSource,
        &.{ .{ .call = 0 }, .{ .slot = 0 }, .{ .call = 0 } },
        builder.plan.dictionaryMethodHiddenDescriptorSourceSlice(sources),
    );
}

test "boxy planner propagates dynamic descriptor requirements through records" {
    const gpa = std.testing.allocator;

    const fields = [_]checked.CheckedRecordField{
        .{ .name = @enumFromInt(1), .ty = @enumFromInt(fixtureTableIndex(0)) },
        .{ .name = @enumFromInt(2), .ty = @enumFromInt(1) },
    };
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.u64, @enumFromInt(fixtureTableIndex(0)), .{}) },
        .{ .rigid = .{} },
        .{ .empty_record = {} },
        .{ .record = .{ .fields = .{ .start = 0, .len = fields.len }, .ext = @enumFromInt(2) } },
    };
    const view = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .record_field_pool = &fields,
    };

    var plan = try analyzeCheckedTypes(gpa, view, &.{@as(checked.CheckedTypeId, @enumFromInt(3))}, .{});
    defer plan.deinit();

    const record = plan.representations.items[@intFromEnum(plan.root_reps.items[0])];
    try std.testing.expectEqual(RepresentationKind.record, record.kind);
    try std.testing.expect(record.contains_dynamic);
    try std.testing.expect(record.descriptor != null);
    try std.testing.expectEqual(@as(usize, 2), plan.childSlice(record.children).len);
    try std.testing.expectEqual(@as(usize, 2), plan.descriptors.items.len);
    try std.testing.expectEqual(DescriptorReason.aggregate_contains_dynamic, plan.descriptors.items[@intFromEnum(record.descriptor.?)].reason);
}

test "boxy planner preserves optional record field representation and descriptor" {
    const gpa = std.testing.allocator;

    var canonical_names = checked_names.CanonicalNameStore.init(gpa);
    defer canonical_names.deinit();
    const field_name = try canonical_names.internRecordFieldLabel("value");
    const missing = try canonical_names.internTagLabel("#Missing");
    const present = try canonical_names.internTagLabel("#Present");

    const fields = [_]checked.CheckedRecordField{.{
        .name = field_name,
        .ty = @enumFromInt(fixtureTableIndex(0)),
        .kind = .optional,
    }};
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.u64, @enumFromInt(fixtureTableIndex(0)), .{}) },
        .{ .empty_record = {} },
        .{ .record = .{ .fields = .{ .start = 0, .len = fields.len }, .ext = @enumFromInt(1) } },
    };
    const checked_types = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .record_field_pool = &fields,
    };

    var plan = try analyzeProgram(gpa, .{
        .root_view = .{
            .canonical_names = &canonical_names,
            .checked_types = checked_types,
        },
        .layout_requests = &.{@as(checked.CheckedTypeId, @enumFromInt(2))},
    }, .{});
    defer plan.deinit();

    const record = plan.representations.items[@intFromEnum(plan.root_reps.items[0])];
    const record_children = plan.childSlice(record.children);
    try std.testing.expectEqual(@as(usize, 1), record_children.len);
    try std.testing.expectEqual(checked.CheckedFieldKind.Tag.optional, record_children[0].record_field_kind.tag);
    try std.testing.expect(record.contains_dynamic);
    try std.testing.expect(record.descriptor != null);

    const slot = plan.representations.items[@intFromEnum(record_children[0].rep)];
    try std.testing.expectEqual(RepresentationKind.tag_union, slot.kind);
    try std.testing.expect(slot.contains_dynamic);
    try std.testing.expect(slot.descriptor != null);
    try std.testing.expectEqual(@as(?u16, 1), slot.presence_slot_present_discriminant);
    const variants = plan.tagVariantSlice(slot.tag_variants);
    try std.testing.expectEqual(@as(usize, 2), variants.len);
    try std.testing.expectEqual(missing, variants[0].name);
    try std.testing.expectEqual(present, variants[1].name);
    const present_payloads = plan.childSlice(variants[1].payloads);
    try std.testing.expectEqual(@as(usize, 1), present_payloads.len);
    try std.testing.expectEqual(record_children[0].source_type, present_payloads[0].source_type);
}

test "boxy planner preserves undetermined record field kind identity in a presence slot" {
    const gpa = std.testing.allocator;

    var canonical_names = checked_names.CanonicalNameStore.init(gpa);
    defer canonical_names.deinit();
    const field_name = try canonical_names.internRecordFieldLabel("value");
    _ = try canonical_names.internTagLabel("#Missing");
    _ = try canonical_names.internTagLabel("#Present");

    const kind_var: checked.CheckedTypeId = @enumFromInt(fixtureTableIndex(1));
    const fields = [_]checked.CheckedRecordField{.{
        .name = field_name,
        .ty = @enumFromInt(fixtureTableIndex(0)),
        .kind = .undetermined(kind_var),
    }};
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.u64, @enumFromInt(fixtureTableIndex(0)), .{}) },
        .{ .flex = .{} },
        .{ .empty_record = {} },
        .{ .record = .{ .fields = .{ .start = 0, .len = fields.len }, .ext = @enumFromInt(2) } },
    };
    const checked_types = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .record_field_pool = &fields,
    };

    var plan = try analyzeProgram(gpa, .{
        .root_view = .{
            .canonical_names = &canonical_names,
            .checked_types = checked_types,
        },
        .layout_requests = &.{@as(checked.CheckedTypeId, @enumFromInt(3))},
    }, .{});
    defer plan.deinit();

    const record = plan.representations.items[@intFromEnum(plan.root_reps.items[0])];
    const record_children = plan.childSlice(record.children);
    try std.testing.expectEqual(@as(usize, 1), record_children.len);
    try std.testing.expectEqual(kind_var, record_children[0].record_field_kind.undeterminedVariable().?);

    const slot = plan.representations.items[@intFromEnum(record_children[0].rep)];
    try std.testing.expectEqual(RepresentationKind.tag_union, slot.kind);
    try std.testing.expectEqual(@as(?u16, 1), slot.presence_slot_present_discriminant);
    try std.testing.expect(slot.descriptor != null);
}

test "boxy planner represents open record rows dynamically" {
    const gpa = std.testing.allocator;

    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .record = .{ .fields = .{}, .ext = @enumFromInt(fixtureTableIndex(0)) } },
    };
    const view = checked.CheckedTypeStoreView{ .stored_payloads = &payloads };

    var plan = try analyzeCheckedTypes(gpa, view, &.{@as(checked.CheckedTypeId, @enumFromInt(fixtureTableIndex(0)))}, .{});
    defer plan.deinit();

    const rep = plan.representations.items[@intFromEnum(plan.root_reps.items[0])];
    try std.testing.expectEqual(RepresentationKind{ .dynamic = .flex }, rep.kind);
    try std.testing.expect(rep.contains_dynamic);
    try std.testing.expect(rep.descriptor != null);
}

test "boxy planner represents open tag-union rows dynamically" {
    const gpa = std.testing.allocator;

    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .tag_union = .{ .tags = .{}, .ext = @enumFromInt(fixtureTableIndex(0)) } },
    };
    const view = checked.CheckedTypeStoreView{ .stored_payloads = &payloads };

    var plan = try analyzeCheckedTypes(gpa, view, &.{@as(checked.CheckedTypeId, @enumFromInt(fixtureTableIndex(0)))}, .{});
    defer plan.deinit();

    const rep = plan.representations.items[@intFromEnum(plan.root_reps.items[0])];
    try std.testing.expectEqual(RepresentationKind{ .dynamic = .flex }, rep.kind);
    try std.testing.expect(rep.contains_dynamic);
    try std.testing.expect(rep.descriptor != null);
}

test "boxy planner preserves known variants on open tag-union rows" {
    const gpa = std.testing.allocator;

    const tag_exit: TagLabelId = @enumFromInt(1);
    const type_pool = [_]checked.CheckedTypeId{@enumFromInt(fixtureTableIndex(0))};
    const tags = [_]checked.CheckedTag{
        .{ .name = tag_exit, .args_start = 0, .args_len = 1 },
    };
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.i64, @enumFromInt(fixtureTableIndex(0)), .{}) },
        .{ .flex = .{} },
        .{ .tag_union = .{ .tags = .{ .start = 0, .len = tags.len }, .ext = @enumFromInt(1) } },
    };
    const view = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .type_id_pool = &type_pool,
        .tag_pool = &tags,
    };

    var plan = try analyzeCheckedTypes(gpa, view, &.{@as(checked.CheckedTypeId, @enumFromInt(2))}, .{});
    defer plan.deinit();

    const rep = plan.representations.items[@intFromEnum(plan.root_reps.items[0])];
    try std.testing.expectEqual(RepresentationKind{ .dynamic = .flex }, rep.kind);
    try std.testing.expect(rep.contains_dynamic);
    try std.testing.expect(rep.descriptor != null);

    const variants = plan.tagVariantSlice(rep.tag_variants);
    try std.testing.expectEqual(@as(usize, 1), variants.len);
    try std.testing.expectEqual(tag_exit, variants[0].name);

    const payload_children = plan.childSlice(variants[0].payloads);
    try std.testing.expectEqual(@as(usize, 1), payload_children.len);
    try std.testing.expectEqual(ChildRole{ .tag_payload = .{ .tag = tag_exit, .index = 0 } }, payload_children[0].role);
}

test "boxy planner keeps explicit Box of dynamic payload distinct from dynamic payload representation" {
    const gpa = std.testing.allocator;

    const type_pool = [_]checked.CheckedTypeId{@enumFromInt(fixtureTableIndex(0))};
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .flex = .{} },
        .{ .nominal = builtinNominal(.box, @enumFromInt(1), .{ .start = 0, .len = 1 }) },
    };
    const view = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .type_id_pool = &type_pool,
    };

    var plan = try analyzeCheckedTypes(gpa, view, &.{@as(checked.CheckedTypeId, @enumFromInt(1))}, .{});
    defer plan.deinit();

    const box_rep = plan.representations.items[@intFromEnum(plan.root_reps.items[0])];
    try std.testing.expectEqual(RepresentationKind.box, box_rep.kind);
    try std.testing.expect(box_rep.contains_dynamic);
    try std.testing.expect(box_rep.descriptor != null);
    const children = plan.childSlice(box_rep.children);
    try std.testing.expectEqual(@as(usize, 1), children.len);
    try std.testing.expectEqual(ChildRole.box_payload, children[0].role);
    try std.testing.expectEqual(RepresentationKind{ .dynamic = .flex }, plan.representations.items[@intFromEnum(children[0].rep)].kind);
}

test "boxy planner preserves zero-payload tag variants explicitly" {
    const gpa = std.testing.allocator;

    const tag_a: TagLabelId = @enumFromInt(1);
    const tag_b: TagLabelId = @enumFromInt(2);
    const type_pool = [_]checked.CheckedTypeId{@enumFromInt(fixtureTableIndex(0))};
    const tags = [_]checked.CheckedTag{
        .{ .name = tag_a, .args_start = 0, .args_len = 0 },
        .{ .name = tag_b, .args_start = 0, .args_len = 1 },
    };
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.u64, @enumFromInt(fixtureTableIndex(0)), .{}) },
        .empty_tag_union,
        .{ .tag_union = .{ .tags = .{ .start = 0, .len = tags.len }, .ext = @enumFromInt(1) } },
    };
    const view = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .type_id_pool = &type_pool,
        .tag_pool = &tags,
    };

    var plan = try analyzeCheckedTypes(gpa, view, &.{@as(checked.CheckedTypeId, @enumFromInt(2))}, .{});
    defer plan.deinit();

    const rep = plan.representations.items[@intFromEnum(plan.root_reps.items[0])];
    try std.testing.expectEqual(RepresentationKind.tag_union, rep.kind);

    const variants = plan.tagVariantSlice(rep.tag_variants);
    try std.testing.expectEqual(@as(usize, 2), variants.len);
    try std.testing.expectEqual(tag_a, variants[0].name);
    try std.testing.expectEqual(@as(u32, 0), variants[0].payloads.len);
    try std.testing.expectEqual(tag_b, variants[1].name);
    try std.testing.expectEqual(@as(u32, 1), variants[1].payloads.len);

    const payload_children = plan.childSlice(variants[1].payloads);
    try std.testing.expectEqual(@as(usize, 1), payload_children.len);
    try std.testing.expectEqual(ChildRole{ .tag_payload = .{ .tag = tag_b, .index = 0 } }, payload_children[0].role);
}

test "boxy planner stores tag variants in identity layout order" {
    const gpa = std.testing.allocator;

    var canonical_names = checked_names.CanonicalNameStore.init(gpa);
    defer canonical_names.deinit();

    const ok_tag = try canonical_names.internTagLabel("Ok");
    const err_tag = try canonical_names.internTagLabel("Err");
    const type_pool = [_]checked.CheckedTypeId{
        @enumFromInt(fixtureTableIndex(0)),
        @enumFromInt(1),
    };
    const tags = [_]checked.CheckedTag{
        .{ .name = ok_tag, .args_start = 0, .args_len = 1 },
        .{ .name = err_tag, .args_start = 1, .args_len = 1 },
    };
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.u64, @enumFromInt(fixtureTableIndex(0)), .{}) },
        .{ .nominal = builtinNominal(.u8, @enumFromInt(1), .{}) },
        .empty_tag_union,
        .{ .tag_union = .{ .tags = .{ .start = 0, .len = tags.len }, .ext = @enumFromInt(2) } },
    };
    const view = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .type_id_pool = &type_pool,
        .tag_pool = &tags,
    };

    var plan = try analyzeProgram(gpa, .{
        .root_view = .{
            .canonical_names = &canonical_names,
            .checked_types = view,
        },
        .layout_requests = &.{@as(checked.CheckedTypeId, @enumFromInt(3))},
    }, .{});
    defer plan.deinit();

    const rep = plan.representations.items[@intFromEnum(plan.root_reps.items[0])];
    const variants = plan.tagVariantSlice(rep.tag_variants);
    try std.testing.expectEqual(@as(usize, 2), variants.len);
    try std.testing.expectEqual(err_tag, variants[0].name);
    try std.testing.expectEqual(ok_tag, variants[1].name);

    const err_payloads = plan.childSlice(variants[0].payloads);
    try std.testing.expectEqual(@as(usize, 1), err_payloads.len);
    try std.testing.expectEqual(ChildRole{ .tag_payload = .{ .tag = err_tag, .index = 0 } }, err_payloads[0].role);
    try expectTypeRef(.{}, @enumFromInt(1), err_payloads[0].source_type);

    const ok_payloads = plan.childSlice(variants[1].payloads);
    try std.testing.expectEqual(@as(usize, 1), ok_payloads.len);
    try std.testing.expectEqual(ChildRole{ .tag_payload = .{ .tag = ok_tag, .index = 0 } }, ok_payloads[0].role);
    try expectTypeRef(.{}, @enumFromInt(fixtureTableIndex(0)), ok_payloads[0].source_type);
}

test "boxy planner records nominal declared field order from checked payloads" {
    const gpa = std.testing.allocator;

    const field_a: RecordFieldLabelId = @enumFromInt(1);
    const field_b: RecordFieldLabelId = @enumFromInt(2);
    const type_pool = [_]checked.CheckedTypeId{@enumFromInt(fixtureTableIndex(0))};
    const record_fields = [_]checked.CheckedRecordField{
        .{ .name = field_a, .ty = @enumFromInt(fixtureTableIndex(0)) },
        .{ .name = field_b, .ty = @enumFromInt(1) },
    };
    const declared_fields = [_]checked.CheckedDeclaredField{
        .{ .named = field_a },
        .{ .padding = 0 },
        .{ .named = field_b },
    };
    const nominal_declarations = [_]checked.CheckedNominalDeclaration{.{
        .id = @enumFromInt(fixtureTableIndex(0)),
        .nominal = .{ .module = @enumFromInt(4), .type_name = @enumFromInt(3), .source_decl = null },
        .source_statement = 0,
        .declaration_root = @enumFromInt(4),
        .backing = @enumFromInt(3),
        .pf_start = 0,
        .pf_len = 1,
        .df_start = 0,
        .df_len = declared_fields.len,
    }};
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.u8, @enumFromInt(fixtureTableIndex(0)), .{}) },
        .{ .nominal = builtinNominal(.u16, @enumFromInt(1), .{}) },
        .{ .empty_record = {} },
        .{ .record = .{ .fields = .{ .start = 0, .len = 2 }, .ext = @enumFromInt(2) } },
        .{ .nominal = .{
            .name = @enumFromInt(3),
            .origin_module = @enumFromInt(4),
            .owner_module = .{},
            .is_opaque = false,
            .representation = .{ .local_declaration = @enumFromInt(fixtureTableIndex(0)) },
            .padding_field_types = .{ .start = 0, .len = 1 },
            .declared_fields = .{ .start = 0, .len = 3 },
        } },
    };
    const view = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .nominal_declarations = &nominal_declarations,
        .type_id_pool = &type_pool,
        .record_field_pool = &record_fields,
        .declared_field_pool = &declared_fields,
    };

    var plan = try analyzeCheckedTypes(gpa, view, &.{@as(checked.CheckedTypeId, @enumFromInt(4))}, .{});
    defer plan.deinit();

    const nominal = plan.representations.items[@intFromEnum(plan.root_reps.items[0])];
    try std.testing.expectEqual(RepresentationKind{ .nominal = .transparent }, nominal.kind);
    const fields = plan.declaredFieldSlice(nominal.declared_fields);
    try std.testing.expectEqual(@as(usize, 3), fields.len);
    try std.testing.expectEqual(@as(u16, 0), fields[0].index);
    try std.testing.expect(!fields[0].is_padding);
    try std.testing.expectEqual(@as(u16, 2), fields[1].index);
    try std.testing.expect(fields[1].is_padding);
    try std.testing.expectEqual(@as(u16, 1), fields[2].index);
    try std.testing.expect(!fields[2].is_padding);
}

test "boxy planner resolves local nominal declared order from box payload capability" {
    const gpa = std.testing.allocator;

    const root_key = moduleKey(1);
    const field_a: RecordFieldLabelId = @enumFromInt(1);
    const field_b: RecordFieldLabelId = @enumFromInt(2);
    const nominal_key = checked_names.NominalTypeKey{
        .module = @enumFromInt(4),
        .type_name = @enumFromInt(3),
        .source_decl = 9,
    };
    const type_pool = [_]checked.CheckedTypeId{@enumFromInt(fixtureTableIndex(0))};
    const record_fields = [_]checked.CheckedRecordField{
        .{ .name = field_a, .ty = @enumFromInt(fixtureTableIndex(0)) },
        .{ .name = field_b, .ty = @enumFromInt(1) },
    };
    const declared_fields = [_]checked.CheckedDeclaredField{
        .{ .named = field_a },
        .{ .padding = 0 },
        .{ .named = field_b },
    };
    const declarations = [_]checked.CheckedNominalDeclaration{
        .{
            .id = @enumFromInt(fixtureTableIndex(0)),
            .nominal = nominal_key,
            .source_statement = 9,
            .declaration_root = @enumFromInt(4),
            .backing = @enumFromInt(3),
            .pf_start = 0,
            .pf_len = 1,
            .df_start = 0,
            .df_len = declared_fields.len,
        },
    };
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.u8, @enumFromInt(fixtureTableIndex(0)), .{}) },
        .{ .nominal = builtinNominal(.u16, @enumFromInt(1), .{}) },
        .{ .empty_record = {} },
        .{ .record = .{ .fields = .{ .start = 0, .len = 2 }, .ext = @enumFromInt(2) } },
        .{ .nominal = .{
            .name = nominal_key.type_name,
            .origin_module = nominal_key.module,
            .owner_module = root_key,
            .source_decl = nominal_key.source_decl,
            .is_opaque = false,
            .representation = .{ .local_box_payload_capability = .{ .capability = @enumFromInt(fixtureTableIndex(0)) } },
        } },
    };
    const capability_padding = [_]checked.CheckedTypeId{@enumFromInt(fixtureTableIndex(0))};
    const capabilities = [_]checked.BoxPayloadCapabilityEntry{
        .{
            .id = @enumFromInt(fixtureTableIndex(0)),
            .nominal = nominal_key,
            .source_ty_payload = @enumFromInt(4),
            .source_ty = typeKey(14),
            .backing_ty = @enumFromInt(3),
            .backing_ty_key = typeKey(13),
            .padding_start = 0,
            .padding_len = 1,
            .is_opaque = false,
        },
    };
    const interface_capabilities = checked.ModuleInterfaceCapabilities{
        .boxed_payload_templates = &capabilities,
        .padding_pool = &capability_padding,
    };
    const view = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .nominal_declarations = &declarations,
        .type_id_pool = &type_pool,
        .record_field_pool = &record_fields,
        .declared_field_pool = &declared_fields,
    };

    var plan = try analyzeProgram(gpa, .{
        .root_view = .{
            .key = root_key,
            .checked_types = view,
            .interface_capabilities = &interface_capabilities,
        },
        .layout_requests = &.{@as(checked.CheckedTypeId, @enumFromInt(4))},
    }, .{});
    defer plan.deinit();

    const nominal = plan.representations.items[@intFromEnum(plan.root_reps.items[0])];
    const children = plan.childSlice(nominal.children);
    try std.testing.expectEqual(@as(usize, 2), children.len);
    try std.testing.expectEqual(ChildRole.nominal_backing, children[0].role);
    try std.testing.expectEqual(ChildRole{ .nominal_padding_field = 0 }, children[1].role);
    try expectTypeRef(moduleKey(1), @enumFromInt(fixtureTableIndex(0)), children[1].source_type);

    const fields = plan.declaredFieldSlice(nominal.declared_fields);
    try std.testing.expectEqual(@as(usize, 3), fields.len);
    try std.testing.expectEqual(@as(u16, 0), fields[0].index);
    try std.testing.expectEqual(@as(u16, 2), fields[1].index);
    try std.testing.expect(fields[1].is_padding);
    try expectTypeRef(moduleKey(1), @enumFromInt(fixtureTableIndex(0)), fields[1].source_type);
    try std.testing.expectEqual(@as(u16, 1), fields[2].index);
}

test "boxy planner records imported box payload capability source modules" {
    const gpa = std.testing.allocator;

    var root_names = checked_names.CanonicalNameStore.init(gpa);
    defer root_names.deinit();
    var source_names = checked_names.CanonicalNameStore.init(gpa);
    defer source_names.deinit();

    _ = try root_names.internRecordFieldLabel("different-root-id");
    const root_a = try root_names.internRecordFieldLabel("a");
    const root_b = try root_names.internRecordFieldLabel("b");
    const source_a = try source_names.internRecordFieldLabel("a");
    _ = try source_names.internRecordFieldLabel("different-source-id");
    const source_b = try source_names.internRecordFieldLabel("b");

    const root_key = moduleKey(1);
    const source_key = moduleKey(2);
    const nominal_identity = [_]u8{0x44} ** 32;
    const nominal_module = try root_names.internModuleIdentity(&nominal_identity);
    const source_nominal_module = try source_names.internModuleIdentity(&nominal_identity);
    try std.testing.expectEqual(nominal_module, source_nominal_module);
    const nominal_key = checked_names.NominalTypeKey{
        .module = nominal_module,
        .type_name = @enumFromInt(3),
        .source_decl = 9,
    };

    const root_record_fields = [_]checked.CheckedRecordField{
        .{ .name = root_a, .ty = @enumFromInt(fixtureTableIndex(0)) },
        .{ .name = root_b, .ty = @enumFromInt(1) },
    };
    const root_payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.u8, @enumFromInt(fixtureTableIndex(0)), .{}) },
        .{ .nominal = builtinNominal(.u16, @enumFromInt(1), .{}) },
        .{ .empty_record = {} },
        .{ .record = .{ .fields = .{ .start = 0, .len = 2 }, .ext = @enumFromInt(2) } },
        .{ .nominal = .{
            .name = nominal_key.type_name,
            .origin_module = nominal_key.module,
            .owner_module = source_key,
            .source_decl = nominal_key.source_decl,
            .is_opaque = false,
            .representation = .{ .imported_box_payload_capability = .{
                .artifact = source_key,
                .capability = @enumFromInt(fixtureTableIndex(0)),
            } },
        } },
    };
    const root_roots = [_]checked.CheckedTypeRoot{
        .{ .id = @enumFromInt(fixtureTableIndex(0)), .key = typeKey(15) },
        .{ .id = @enumFromInt(1), .key = typeKey(11) },
        .{ .id = @enumFromInt(2), .key = typeKey(12) },
        .{ .id = @enumFromInt(3), .key = typeKey(13) },
        .{ .id = @enumFromInt(4), .key = typeKey(14) },
    };
    const root_view = checked.CheckedTypeStoreView{
        .roots = &root_roots,
        .stored_payloads = &root_payloads,
        .record_field_pool = &root_record_fields,
    };

    const source_type_pool = [_]checked.CheckedTypeId{@enumFromInt(5)};
    const source_record_fields = [_]checked.CheckedRecordField{
        .{ .name = source_a, .ty = @enumFromInt(5) },
        .{ .name = source_b, .ty = @enumFromInt(1) },
    };
    const source_declared_fields = [_]checked.CheckedDeclaredField{
        .{ .named = source_a },
        .{ .padding = 0 },
        .{ .named = source_b },
    };
    const source_declarations = [_]checked.CheckedNominalDeclaration{
        .{
            .id = @enumFromInt(fixtureTableIndex(0)),
            .nominal = nominal_key,
            .source_statement = 9,
            .declaration_root = @enumFromInt(4),
            .backing = @enumFromInt(3),
            .pf_start = 0,
            .pf_len = 1,
            .df_start = 0,
            .df_len = source_declared_fields.len,
        },
    };
    const source_payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.u8, @enumFromInt(fixtureTableIndex(0)), .{}) },
        .{ .nominal = builtinNominal(.u16, @enumFromInt(1), .{}) },
        .{ .empty_record = {} },
        .{ .record = .{ .fields = .{ .start = 0, .len = 2 }, .ext = @enumFromInt(2) } },
        .{ .nominal = .{
            .name = nominal_key.type_name,
            .origin_module = nominal_key.module,
            .owner_module = source_key,
            .source_decl = nominal_key.source_decl,
            .is_opaque = false,
            .representation = .{ .local_box_payload_capability = .{ .capability = @enumFromInt(fixtureTableIndex(0)) } },
        } },
        .{ .nominal = builtinNominal(.u8, @enumFromInt(5), .{}) },
    };
    const source_roots = [_]checked.CheckedTypeRoot{
        .{ .id = @enumFromInt(fixtureTableIndex(0)), .key = typeKey(10) },
        .{ .id = @enumFromInt(1), .key = typeKey(11) },
        .{ .id = @enumFromInt(2), .key = typeKey(12) },
        .{ .id = @enumFromInt(3), .key = typeKey(13) },
        .{ .id = @enumFromInt(4), .key = typeKey(14) },
        .{ .id = @enumFromInt(5), .key = typeKey(15) },
    };
    const source_capability_padding = [_]checked.CheckedTypeId{@enumFromInt(5)};
    const source_capabilities = [_]checked.BoxPayloadCapabilityEntry{
        .{
            .id = @enumFromInt(fixtureTableIndex(0)),
            .nominal = nominal_key,
            .source_ty_payload = @enumFromInt(4),
            .source_ty = typeKey(14),
            .backing_ty = @enumFromInt(3),
            .backing_ty_key = typeKey(13),
            .padding_start = 0,
            .padding_len = 1,
            .is_opaque = false,
        },
    };
    const source_interface_capabilities = checked.ModuleInterfaceCapabilities{
        .boxed_payload_templates = &source_capabilities,
        .padding_pool = &source_capability_padding,
    };
    const source_view = checked.CheckedTypeStoreView{
        .roots = &source_roots,
        .stored_payloads = &source_payloads,
        .nominal_declarations = &source_declarations,
        .type_id_pool = &source_type_pool,
        .record_field_pool = &source_record_fields,
        .declared_field_pool = &source_declared_fields,
    };

    var plan = try analyzeProgram(gpa, .{
        .root_view = .{
            .key = root_key,
            .canonical_names = &root_names,
            .checked_types = root_view,
        },
        .extra_module_views = &.{
            .{
                .key = source_key,
                .canonical_names = &source_names,
                .checked_types = source_view,
                .interface_capabilities = &source_interface_capabilities,
            },
        },
        .layout_requests = &.{@as(checked.CheckedTypeId, @enumFromInt(4))},
    }, .{});
    defer plan.deinit();

    const nominal = plan.representations.items[@intFromEnum(plan.root_reps.items[0])];
    const children = plan.childSlice(nominal.children);
    try std.testing.expectEqual(@as(usize, 2), children.len);
    try expectTypeRef(source_key, @enumFromInt(3), children[0].source_type);
    try expectTypeRef(source_key, @enumFromInt(5), children[1].source_type);

    const fields = plan.declaredFieldSlice(nominal.declared_fields);
    try std.testing.expectEqual(@as(usize, 3), fields.len);
    try std.testing.expectEqual(@as(u16, 0), fields[0].index);
    try expectTypeRef(source_key, @enumFromInt(5), fields[0].source_type);
    try std.testing.expectEqual(@as(u16, 2), fields[1].index);
    try std.testing.expect(fields[1].is_padding);
    try expectTypeRef(source_key, @enumFromInt(5), fields[1].source_type);
    try std.testing.expectEqual(@as(u16, 1), fields[2].index);
    try expectTypeRef(source_key, @enumFromInt(1), fields[2].source_type);
}

fn moduleKey(byte: u8) checked.ModuleId {
    var key = checked.ModuleId{};
    key.bytes[0] = byte;
    return key;
}

fn rootTypeRef(ty: checked.CheckedTypeId) CheckedTypeIdentity {
    return .{ .ty = ty };
}

fn expectTypeRef(module: checked.ModuleId, ty: checked.CheckedTypeId, actual: CheckedTypeIdentity) error{ TestExpectedEqual, TestUnexpectedResult }!void {
    try std.testing.expect(moduleKeyEqual(module, actual.module));
    try std.testing.expectEqual(ty, actual.ty);
}

fn typeKey(byte: u8) checked_names.CanonicalTypeKey {
    var key = checked_names.CanonicalTypeKey{};
    key.bytes[0] = byte;
    return key;
}

fn typeSchemeKey(byte: u8) checked_names.CanonicalTypeSchemeKey {
    var key = checked_names.CanonicalTypeSchemeKey{};
    key.bytes[0] = byte;
    return key;
}

fn procedureTemplateRef(key: checked.CheckedModuleArtifactKey, raw_template_id: u32) checked_names.ProcedureTemplateRef {
    return .{
        .artifact = .{ .bytes = key.bytes },
        .proc_base = @enumFromInt(raw_template_id),
        .template = @enumFromInt(raw_template_id),
    };
}

fn procedureValueRef(template: checked_names.ProcedureTemplateRef) checked_names.ProcedureValueRef {
    return .{
        .artifact = template.artifact,
        .proc_base = template.proc_base,
    };
}

fn checkedTemplate(
    template_ref: checked_names.ProcedureTemplateRef,
    checked_fn_root: checked.CheckedTypeId,
    body: checked.CheckedBodyId,
    target: checked.ProcTarget,
) checked.CheckedProcedureTemplate {
    return .{
        .proc_base = template_ref.proc_base,
        .template_id = template_ref.template,
        .body = .{ .checked_body = body },
        .checked_fn_scheme = typeSchemeKey(9),
        .checked_fn_root = checked_fn_root,
        .static_dispatch_plans = .{},
        .direct_dispatch_plans = .{},
        .dispatch_relations = .{},
        .resolved_value_refs = .{},
        .top_level_value_uses = .{},
        .nested_proc_sites = .{},
        .target = target,
    };
}

fn testIntNumeral(value: u128) Allocator.Error!can.ModuleEnv.NumeralLiteral {
    var buffer: [16]u8 = undefined;
    var remaining = value;
    var len: usize = 0;
    while (true) {
        buffer[buffer.len - 1 - len] = @truncate(remaining);
        len += 1;
        remaining >>= 8;
        if (remaining == 0) break;
    }

    const env = testNumeralModuleEnv();
    const node = @as(can.CIR.Node.Idx, @enumFromInt(fixtureTableIndex(0)));
    try env.recordNumeralLiteral(node, buffer[buffer.len - len ..], &.{}, 0, false, false, false, true);
    return env.numeralLiteralForNode(node) orelse unreachable;
}

fn testNumeralModuleEnv() *can.ModuleEnv {
    const S = struct {
        var env: ?*can.ModuleEnv = null;
    };
    if (S.env) |existing| return existing;
    const page_allocator = std.heap.page_allocator;
    const env = page_allocator.create(can.ModuleEnv) catch unreachable;
    env.* = can.ModuleEnv.init(page_allocator, "") catch unreachable;
    env.initCIRFields("Test") catch unreachable;
    S.env = env;
    return env;
}

fn minimalCheckedArtifact(allocator: Allocator) checked.CheckedModuleArtifact {
    return .{
        .key = moduleKey(1),
        .canonical_names = checked_names.CanonicalNameStore.init(allocator),
        .module_identity = testModuleIdentity(),
        .checking_context_identity = .{},
        .module_env = .{ .checked_source = testNumeralModuleEnv() },
        .exports = .{},
        .provides_requires = .{},
        .method_registry = .{},
        .static_dispatch_plans = .{},
        .resolved_value_refs = .{},
        .checked_procedure_templates = .{},
        .top_level_procedure_bindings = .{},
        .root_requests = .{},
        .hosted_procs = .{},
        .platform_required_declarations = .{},
        .platform_required_bindings = .{},
        .interface_capabilities = .{},
        .compile_time_roots = .{},
        .top_level_values = .{},
        .hoisted_constants = .{},
        .const_templates = .{},
        .const_store = check.ConstStore.ConstStore.init(allocator),
    };
}

fn testModuleIdentity() checked.ModuleIdentity {
    return .{
        .module_idx = 0,
        .module_name = @enumFromInt(fixtureTableIndex(0)),
        .display_module_name = @enumFromInt(fixtureTableIndex(0)),
        .qualified_module_name = @enumFromInt(fixtureTableIndex(0)),
        .kind = .module,
    };
}

fn dummyProcedureTemplate() checked_names.ProcedureTemplateRef {
    return .{
        .proc_base = @enumFromInt(fixtureTableIndex(0)),
        .template = @enumFromInt(fixtureTableIndex(0)),
    };
}
