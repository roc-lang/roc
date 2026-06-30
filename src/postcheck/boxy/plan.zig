//! Boxy representation planner.
//!
//! The planner records explicit facts consumed by the boxy lowerer: how each
//! checked type is represented internally, which descriptor data dynamic
//! positions require, and which static-dispatch constraints require dictionary
//! arguments. It only consumes checked type data.

const std = @import("std");
const can = @import("can");
const check = @import("check");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const canonical = check.CanonicalNames;
const static_dispatch = check.StaticDispatchRegistry;
const RecordFieldLabelId = @TypeOf(@as(checked.CheckedRecordField, undefined).name);
const TagLabelId = @TypeOf(@as(checked.CheckedTag, undefined).name);
const MethodNameId = @TypeOf(@as(checked.CheckedStaticDispatchConstraint, undefined).fn_name);
const StaticDispatchOrigin = @TypeOf(@as(checked.CheckedStaticDispatchConstraint, undefined).origin);
const NumeralInfo = std.meta.Child(@TypeOf(@as(checked.CheckedStaticDispatchConstraint, undefined).num_literal));

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

pub const TypeRepId = enum(u32) { _ };
pub const RootPlanId = enum(u32) { _ };
pub const WorkerPlanId = enum(u32) { _ };
pub const DescriptorRequirementId = enum(u32) { _ };
pub const DictionaryRequirementId = enum(u32) { _ };

pub const TypeRef = struct {
    module: checked.ModuleId = .{},
    ty: checked.CheckedTypeId,
};

pub const ExprRef = struct {
    module: checked.ModuleId = .{},
    expr: checked.CheckedExprId,
};

const PatternRef = struct {
    module: checked.ModuleId = .{},
    pattern: checked.CheckedPatternId,
};

const StatementRef = struct {
    module: checked.ModuleId = .{},
    statement: checked.CheckedStatementId,
};

pub const Span = extern struct {
    start: u32 = 0,
    len: u32 = 0,

    pub fn empty() Span {
        return .{};
    }
};

pub const DynamicKind = enum {
    flex,
    rigid,
};

pub const NominalKind = enum {
    transparent,
    opaque_nominal,
    builtin_other,
};

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
    empty_record,
    tag_union,
    empty_tag_union,
};

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

pub const RepChild = struct {
    role: ChildRole,
    source_type: TypeRef,
    rep: TypeRepId,
};

pub const TagVariant = struct {
    name: TagLabelId,
    name_module: checked.ModuleId = .{},
    payloads: Span = .{},
};

pub const DeclaredField = struct {
    index: u16,
    source_type: TypeRef,
    rep: TypeRepId,
    is_padding: bool = false,
};

pub const TypeRepBinding = struct {
    source_type: TypeRef,
    rep: TypeRepId,
};

pub const TypeRepresentation = struct {
    source_type: TypeRef,
    kind: RepresentationKind,
    children: Span = .{},
    tag_variants: Span = .{},
    declared_fields: Span = .{},
    dictionaries: Span = .{},
    descriptor: ?DescriptorRequirementId = null,
    contains_dynamic: bool = false,
};

pub const DescriptorReason = enum {
    dynamic_payload,
    aggregate_contains_dynamic,
    list_element_dynamic,
    box_payload_dynamic,
};

pub const DescriptorRequirement = struct {
    source_type: TypeRef,
    rep: TypeRepId,
    reason: DescriptorReason,
};

pub const HiddenDescriptorParam = struct {
    source_type: TypeRef,
    rep: TypeRepId,
    desc: DescriptorRequirementId,
};

pub const HiddenDictionaryParam = struct {
    source_type: TypeRef,
    rep: TypeRepId,
    dictionaries: Span,
};

pub const DirectCallHiddenDescriptorArg = struct {
    worker_desc: DescriptorRequirementId,
    worker_rep: TypeRepId,
    source_type: TypeRef,
    rep: TypeRepId,
    source_arg_index: ?u32 = null,
};

pub const DirectCallHiddenDictionaryArg = struct {
    worker_dictionaries: Span,
    source_type: TypeRef,
    rep: TypeRepId,
};

pub const ErasedCaptureKind = enum {
    captured_value,
    hidden_desc,
    hidden_dict,
};

pub const ErasedCapture = struct {
    kind: ErasedCaptureKind,
    source_type: TypeRef,
    rep: TypeRepId,
    desc: ?DescriptorRequirementId = null,
    dictionaries: Span = .{},
};

pub const DictionaryRequirement = struct {
    source_type: TypeRef,
    constraint_index: u32,
    fn_name: MethodNameId,
    fn_ty: TypeRef,
    origin: StaticDispatchOrigin,
    binop_negated: bool,
    num_literal: ?NumeralInfo,
};

pub const RootWrapperKind = enum {
    private_worker_only,
    host_shaped_wrapper,
};

pub const WorkerSource = union(enum) {
    procedure_template: canonical.ProcedureTemplateRef,
    procedure_binding: checked.ArtifactTopLevelProcedureBindingRef,
    procedure_use: checked.ProcedureUseTemplate,
    nested_expr: ExprRef,
};

pub const WorkerPlan = struct {
    id: WorkerPlanId,
    root_request: ?checked.RootRequest = null,
    source: WorkerSource,
    checked_type: TypeRef,
    rep: TypeRepId,
    hidden_descs: Span = .{},
    hidden_dicts: Span = .{},
    erased_captures: Span = .{},
};

pub const DirectCallPlan = struct {
    call: ExprRef,
    worker: WorkerPlanId,
    source_fn_type: TypeRef,
    hidden_desc_args: Span = .{},
    hidden_dict_args: Span = .{},
};

pub const IteratorCallKind = enum {
    iter,
    next,
};

pub const IteratorCallPlan = struct {
    module: checked.ModuleId,
    for_plan: static_dispatch.IteratorForPlanId,
    kind: IteratorCallKind,
    worker: WorkerPlanId,
    source_fn_type: TypeRef,
    arg_types: Span = .{},
    ret_type: TypeRef,
    hidden_desc_args: Span = .{},
    hidden_dict_args: Span = .{},
};

pub const RootPlan = struct {
    id: RootPlanId,
    request: checked.RootRequest,
    worker: WorkerPlanId,
    wrapper_kind: RootWrapperKind,
    host_type: TypeRef,
    host_rep: TypeRepId,
    worker_rep: TypeRepId,
};

pub const ProgramPlan = struct {
    allocator: Allocator,
    roots: std.ArrayList(RootPlan),
    workers: std.ArrayList(WorkerPlan),
    direct_calls: std.ArrayList(DirectCallPlan),
    iterator_calls: std.ArrayList(IteratorCallPlan),
    iterator_call_arg_types: std.ArrayList(TypeRef),
    root_reps: std.ArrayList(TypeRepId),
    type_reps: std.ArrayList(TypeRepBinding),
    representations: std.ArrayList(TypeRepresentation),
    children: std.ArrayList(RepChild),
    tag_variants: std.ArrayList(TagVariant),
    declared_fields: std.ArrayList(DeclaredField),
    descriptors: std.ArrayList(DescriptorRequirement),
    hidden_descriptor_params: std.ArrayList(HiddenDescriptorParam),
    hidden_dictionary_params: std.ArrayList(HiddenDictionaryParam),
    direct_call_hidden_desc_args: std.ArrayList(DirectCallHiddenDescriptorArg),
    direct_call_hidden_dict_args: std.ArrayList(DirectCallHiddenDictionaryArg),
    erased_captures: std.ArrayList(ErasedCapture),
    dictionaries: std.ArrayList(DictionaryRequirement),

    pub fn init(allocator: Allocator) ProgramPlan {
        return .{
            .allocator = allocator,
            .roots = .empty,
            .workers = .empty,
            .direct_calls = .empty,
            .iterator_calls = .empty,
            .iterator_call_arg_types = .empty,
            .root_reps = .empty,
            .type_reps = .empty,
            .representations = .empty,
            .children = .empty,
            .tag_variants = .empty,
            .declared_fields = .empty,
            .descriptors = .empty,
            .hidden_descriptor_params = .empty,
            .hidden_dictionary_params = .empty,
            .direct_call_hidden_desc_args = .empty,
            .direct_call_hidden_dict_args = .empty,
            .erased_captures = .empty,
            .dictionaries = .empty,
        };
    }

    pub fn deinit(self: *ProgramPlan) void {
        self.dictionaries.deinit(self.allocator);
        self.erased_captures.deinit(self.allocator);
        self.direct_call_hidden_dict_args.deinit(self.allocator);
        self.direct_call_hidden_desc_args.deinit(self.allocator);
        self.hidden_dictionary_params.deinit(self.allocator);
        self.hidden_descriptor_params.deinit(self.allocator);
        self.descriptors.deinit(self.allocator);
        self.declared_fields.deinit(self.allocator);
        self.tag_variants.deinit(self.allocator);
        self.children.deinit(self.allocator);
        self.representations.deinit(self.allocator);
        self.type_reps.deinit(self.allocator);
        self.root_reps.deinit(self.allocator);
        self.iterator_call_arg_types.deinit(self.allocator);
        self.iterator_calls.deinit(self.allocator);
        self.direct_calls.deinit(self.allocator);
        self.workers.deinit(self.allocator);
        self.roots.deinit(self.allocator);
        self.* = ProgramPlan.init(self.allocator);
    }

    pub fn childSlice(self: *const ProgramPlan, span: Span) []const RepChild {
        return self.children.items[span.start .. span.start + span.len];
    }

    pub fn tagVariantSlice(self: *const ProgramPlan, span: Span) []const TagVariant {
        return self.tag_variants.items[span.start .. span.start + span.len];
    }

    pub fn declaredFieldSlice(self: *const ProgramPlan, span: Span) []const DeclaredField {
        return self.declared_fields.items[span.start .. span.start + span.len];
    }

    pub fn dictionarySlice(self: *const ProgramPlan, span: Span) []const DictionaryRequirement {
        return self.dictionaries.items[span.start .. span.start + span.len];
    }

    pub fn hiddenDescriptorParamSlice(self: *const ProgramPlan, span: Span) []const HiddenDescriptorParam {
        return self.hidden_descriptor_params.items[span.start .. span.start + span.len];
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

    pub fn iteratorCallArgTypeSlice(self: *const ProgramPlan, span: Span) []const TypeRef {
        return self.iterator_call_arg_types.items[span.start .. span.start + span.len];
    }

    pub fn erasedCaptureSlice(self: *const ProgramPlan, span: Span) []const ErasedCapture {
        return self.erased_captures.items[span.start .. span.start + span.len];
    }

    pub fn directWorkerForCall(self: *const ProgramPlan, call: ExprRef) ?WorkerPlanId {
        return if (self.directCallPlanForCall(call)) |plan| plan.worker else null;
    }

    pub fn directCallPlanForCall(self: *const ProgramPlan, call: ExprRef) ?DirectCallPlan {
        for (self.direct_calls.items) |direct| {
            if (exprRefEql(direct.call, call)) return direct;
        }
        return null;
    }

    pub fn repForSourceType(self: *const ProgramPlan, source_type: TypeRef) ?TypeRepId {
        for (self.type_reps.items) |binding| {
            if (typeRefEql(binding.source_type, source_type)) return binding.rep;
        }
        return null;
    }

    pub fn workerForSourceType(self: *const ProgramPlan, source: WorkerSource, checked_type: TypeRef) ?WorkerPlanId {
        for (self.workers.items) |worker| {
            if (typeRefEql(worker.checked_type, checked_type) and workerSourceEql(worker.source, source)) {
                return worker.id;
            }
        }
        return null;
    }

    pub fn iteratorCallPlanFor(
        self: *const ProgramPlan,
        module: checked.ModuleId,
        for_plan: static_dispatch.IteratorForPlanId,
        kind: IteratorCallKind,
    ) ?IteratorCallPlan {
        for (self.iterator_calls.items) |call| {
            if (moduleKeyEqual(call.module, module) and call.for_plan == for_plan and call.kind == kind) return call;
        }
        return null;
    }
};

pub const AnalyzeOptions = struct {};

pub const ModuleView = struct {
    key: checked.ModuleId = .{},
    canonical_names: ?*const canonical.CanonicalNameStore = null,
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
};

pub const ProgramInput = struct {
    checked_types: checked.CheckedTypeStoreView = .{},
    root_view: ?ModuleView = null,
    extra_module_views: []const ModuleView = &.{},
    root_module: ?checked.LoweringModuleView = null,
    imports: []const checked.ImportedModuleView = &.{},
    roots: []const checked.RootRequest = &.{},
    layout_requests: []const checked.CheckedTypeId = &.{},
};

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

    builder.propagateDynamicRequirements();
    try builder.materializeWorkerHiddenDictionaryParams();
    try builder.materializeDirectCallHiddenDictionaryArgs();
    try builder.materializeIteratorCallHiddenDictionaryArgs();
    try builder.materializeDescriptorRequirements();
    try builder.materializeWorkerHiddenDescriptorParams();
    try builder.materializeWorkerHiddenDictionaryParams();
    try builder.materializeWorkerErasedCaptures();
    try builder.materializeDirectCallHiddenDescriptorArgs();
    try builder.materializeDirectCallHiddenDictionaryArgs();
    try builder.materializeIteratorCallHiddenDescriptorArgs();
    try builder.materializeIteratorCallHiddenDictionaryArgs();

    const out = builder.plan;
    builder.plan = ProgramPlan.init(allocator);
    return out;
}

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
    allocator: Allocator,
    root_module: ?checked.LoweringModuleView,
    root_view: ModuleView,
    extra_module_views: []const ModuleView,
    imports: []const checked.ImportedModuleView,
    relation_modules: []const checked.ImportedModuleView,
    plan: ProgramPlan,
    by_type: std.AutoHashMap(TypeRef, TypeRepId),
    body_exprs_seen: std.AutoHashMap(ExprRef, void),
    body_patterns_seen: std.AutoHashMap(PatternRef, void),
    body_statements_seen: std.AutoHashMap(StatementRef, void),

    fn init(allocator: Allocator, input: ProgramInput) Builder {
        const root_view = if (input.root_view) |root_view|
            root_view
        else if (input.root_module) |root_module|
            moduleViewFromArtifact(root_module.module)
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
            .by_type = std.AutoHashMap(TypeRef, TypeRepId).init(allocator),
            .body_exprs_seen = std.AutoHashMap(ExprRef, void).init(allocator),
            .body_patterns_seen = std.AutoHashMap(PatternRef, void).init(allocator),
            .body_statements_seen = std.AutoHashMap(StatementRef, void).init(allocator),
        };
    }

    fn deinit(self: *Builder) void {
        self.body_statements_seen.deinit();
        self.body_patterns_seen.deinit();
        self.body_exprs_seen.deinit();
        self.by_type.deinit();
        self.plan.deinit();
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

    fn moduleForArtifactRef(self: *Builder, artifact: anytype) ModuleView {
        return self.moduleForId(.{ .bytes = artifact.bytes });
    }

    fn analyzeRoot(self: *Builder, root: checked.RootRequest) Allocator.Error!void {
        const host_type = typeRef(self.root_view, root.checked_type);
        const rep = try self.analyzeType(self.root_view, root.checked_type);
        const source = workerSourceForRoot(root, self.root_view.key) orelse
            boxyPlanInvariant("boxy root request had no checked procedure worker source");
        const worker_id = try self.ensureWorker(source, host_type, root);

        const id: RootPlanId = @enumFromInt(@as(u32, @intCast(self.plan.roots.items.len)));
        try self.plan.roots.append(self.allocator, .{
            .id = id,
            .request = root,
            .worker = worker_id,
            .wrapper_kind = if (rootRequiresHostWrapper(root)) .host_shaped_wrapper else .private_worker_only,
            .host_type = host_type,
            .host_rep = rep,
            .worker_rep = rep,
        });
        try self.plan.root_reps.append(self.allocator, rep);

        if (rep != self.plan.workers.items[@intFromEnum(worker_id)].rep) {
            boxyPlanInvariant("boxy root worker representation disagreed with root representation");
        }
    }

    fn ensureWorker(
        self: *Builder,
        source: WorkerSource,
        checked_type: TypeRef,
        root_request: ?checked.RootRequest,
    ) Allocator.Error!WorkerPlanId {
        const rep = try self.analyzeType(self.moduleForId(checked_type.module), checked_type.ty);
        for (self.plan.workers.items) |worker| {
            if (typeRefEql(worker.checked_type, checked_type) and workerSourceEql(worker.source, source)) {
                if (root_request) |request| {
                    if (worker.root_request == null) {
                        self.plan.workers.items[@intFromEnum(worker.id)].root_request = request;
                    }
                }
                return worker.id;
            }
        }

        const worker_id: WorkerPlanId = @enumFromInt(@as(u32, @intCast(self.plan.workers.items.len)));
        try self.plan.workers.append(self.allocator, .{
            .id = worker_id,
            .root_request = root_request,
            .source = source,
            .checked_type = checked_type,
            .rep = rep,
        });

        if (self.root_module != null) {
            try self.analyzeWorkerBodyTypes(source);
        }

        return worker_id;
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

    fn dynamicRepresentation(
        self: *Builder,
        source_type: TypeRef,
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
        source_type: TypeRef,
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
        source_type: TypeRef,
        kind: RepresentationKind,
        fields: []const checked.CheckedRecordField,
        ext: ?checked.CheckedTypeId,
    ) Allocator.Error!TypeRepresentation {
        var children = std.ArrayList(RepChild).empty;
        defer children.deinit(self.allocator);
        const closed = try self.appendRecordRowChildren(&children, view, fields, ext);
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

    fn appendRecordRowChildren(
        self: *Builder,
        children: *std.ArrayList(RepChild),
        view: ModuleView,
        fields: []const checked.CheckedRecordField,
        ext: ?checked.CheckedTypeId,
    ) Allocator.Error!bool {
        for (fields) |field| {
            try self.appendPendingChild(children, view, .{ .record_field = field.name }, field.ty);
        }

        var seen = std.AutoHashMap(TypeRef, void).init(self.allocator);
        defer seen.deinit();

        var current = ext;
        while (current) |ext_ty| {
            const source = typeRef(view, ext_ty);
            const entry = try seen.getOrPut(source);
            if (entry.found_existing) return false;

            switch (view.checked_types.payload(ext_ty)) {
                .empty_record => return true,
                .record => |record| {
                    for (record.fields) |field| {
                        try self.appendPendingChild(children, view, .{ .record_field = field.name }, field.ty);
                    }
                    current = record.ext;
                },
                .record_unbound => |tail_fields| {
                    for (tail_fields) |field| {
                        try self.appendPendingChild(children, view, .{ .record_field = field.name }, field.ty);
                    }
                    return true;
                },
                .alias => |alias| current = alias.backing,
                .flex, .rigid => |variable| return variable.row_default == .empty_record,
                else => return false,
            }
        }
        return true;
    }

    fn recordExtensionIsExplicitlyClosed(
        self: *Builder,
        view: ModuleView,
        ext_ty: checked.CheckedTypeId,
    ) Allocator.Error!bool {
        var seen = std.AutoHashMap(TypeRef, void).init(self.allocator);
        defer seen.deinit();
        return try self.recordExtensionIsExplicitlyClosedInner(view, ext_ty, &seen);
    }

    fn recordExtensionIsExplicitlyClosedInner(
        self: *Builder,
        view: ModuleView,
        ext_ty: checked.CheckedTypeId,
        seen: *std.AutoHashMap(TypeRef, void),
    ) Allocator.Error!bool {
        const source = typeRef(view, ext_ty);
        const entry = try seen.getOrPut(source);
        if (entry.found_existing) return false;

        return switch (view.checked_types.payload(ext_ty)) {
            .empty_record => true,
            .alias => |alias| try self.recordExtensionIsExplicitlyClosedInner(view, alias.backing, seen),
            .flex, .rigid => |variable| variable.row_default == .empty_record,
            else => false,
        };
    }

    fn tupleRepresentation(
        self: *Builder,
        view: ModuleView,
        source_type: TypeRef,
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
        source_type: TypeRef,
        nominal: checked.CheckedNominalType,
    ) Allocator.Error!TypeRepresentation {
        if (nominal.builtin) |builtin| {
            switch (checked.builtinRuntimeEncoding(builtin)) {
                .primitive => |primitive| return .{
                    .source_type = source_type,
                    .kind = .{ .primitive = primitive },
                },
                .bool_tag_union => return .{ .source_type = source_type, .kind = .bool_tag_union },
                .list => return try self.builtinUnaryNominalRepresentation(view, source_type, .list, .list_elem, nominal),
                .box => return try self.builtinUnaryNominalRepresentation(view, source_type, .box, .box_payload, nominal),
                .parse_tag_union_spec,
                .fields,
                .field,
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
        if (self.nominalPaddingSource(view, nominal)) |padding_source| {
            for (padding_source.types, 0..) |padding, index| {
                try self.appendPendingChild(
                    &children,
                    padding_source.view,
                    .{ .nominal_padding_field = @intCast(index) },
                    padding,
                );
            }
        }
        const backing = try self.nominalBackingSource(view, nominal);
        const declared_fields = try self.appendNominalDeclaredFields(view, nominal, backing);
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
            .opaque_without_backing,
            => .{ .view = view, .ty = nominal.backing },
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
        if (nominal.declared_fields.len != 0) return .{
            .field_view = view,
            .fields = nominal.declared_fields,
            .padding_view = view,
            .padding_types = nominal.padding_field_types,
        };
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
            else => boxyPlanInvariant("checked nominal declared field order had a non-record backing"),
        };

        var pending = std.ArrayList(DeclaredField).empty;
        defer pending.deinit(self.allocator);
        var padding_ordinal: u16 = 0;
        for (source.fields) |declared| {
            switch (declared) {
                .named => |name| {
                    const field = self.nominalBackingField(source.field_view, backing.view, backing_fields, name) orelse
                        boxyPlanInvariant("checked nominal declared named field was missing from backing row");
                    try pending.append(self.allocator, .{
                        .index = field.index,
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

    const NominalBackingField = struct {
        index: u16,
        ty: checked.CheckedTypeId,
    };

    fn nominalBackingField(
        self: *Builder,
        field_view: ModuleView,
        backing_view: ModuleView,
        backing_fields: []const checked.CheckedRecordField,
        name: RecordFieldLabelId,
    ) ?NominalBackingField {
        for (backing_fields, 0..) |field, index| {
            if (self.recordFieldNameMatches(field_view, name, backing_view, field.name)) return .{
                .index = @intCast(index),
                .ty = field.ty,
            };
        }
        return null;
    }

    fn recordFieldNameMatches(
        _: *Builder,
        source_view: ModuleView,
        source_name: RecordFieldLabelId,
        target_view: ModuleView,
        target_name: RecordFieldLabelId,
    ) bool {
        if (moduleKeyEqual(source_view.key, target_view.key)) return source_name == target_name;
        const source_names = source_view.canonical_names orelse return source_name == target_name;
        const target_names = target_view.canonical_names orelse return source_name == target_name;
        return std.mem.eql(
            u8,
            source_names.recordFieldLabelText(source_name),
            target_names.recordFieldLabelText(target_name),
        );
    }

    fn tagLabelNameMatches(
        _: *Builder,
        source_view: ModuleView,
        source_name: TagLabelId,
        target_view: ModuleView,
        target_name: TagLabelId,
    ) bool {
        if (moduleKeyEqual(source_view.key, target_view.key)) return source_name == target_name;
        const source_names = source_view.canonical_names orelse return source_name == target_name;
        const target_names = target_view.canonical_names orelse return source_name == target_name;
        return checked.tagLabelsMatch(source_names, source_name, target_names, target_name);
    }

    fn builtinUnaryNominalRepresentation(
        self: *Builder,
        view: ModuleView,
        source_type: TypeRef,
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
        source_type: TypeRef,
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
        source_type: TypeRef,
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
            fn lessThan(name_store: *const canonical.CanonicalNameStore, lhs: checked.CheckedTag, rhs: checked.CheckedTag) bool {
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
        var seen = std.AutoHashMap(TypeRef, void).init(self.allocator);
        defer seen.deinit();
        return try self.tagUnionExtensionIsExplicitlyClosedInner(view, ext_ty, &seen);
    }

    fn tagUnionExtensionIsExplicitlyClosedInner(
        self: *Builder,
        view: ModuleView,
        ext_ty: checked.CheckedTypeId,
        seen: *std.AutoHashMap(TypeRef, void),
    ) Allocator.Error!bool {
        const source = typeRef(view, ext_ty);
        const entry = try seen.getOrPut(source);
        if (entry.found_existing) return false;

        return switch (view.checked_types.payload(ext_ty)) {
            .empty_tag_union => true,
            .alias => |alias| try self.tagUnionExtensionIsExplicitlyClosedInner(view, alias.backing, seen),
            .flex, .rigid => |variable| variable.row_default == .empty_tag_union,
            else => false,
        };
    }

    fn rowExtensionIsDefaultClosed(
        self: *Builder,
        view: ModuleView,
        ext_ty: checked.CheckedTypeId,
        expected: checked.RowDefault,
    ) Allocator.Error!bool {
        var seen = std.AutoHashMap(TypeRef, void).init(self.allocator);
        defer seen.deinit();
        return try self.rowExtensionIsDefaultClosedInner(view, ext_ty, expected, &seen);
    }

    fn rowExtensionIsDefaultClosedInner(
        self: *Builder,
        view: ModuleView,
        ext_ty: checked.CheckedTypeId,
        expected: checked.RowDefault,
        seen: *std.AutoHashMap(TypeRef, void),
    ) Allocator.Error!bool {
        const source = typeRef(view, ext_ty);
        const entry = try seen.getOrPut(source);
        if (entry.found_existing) return false;

        return switch (view.checked_types.payload(ext_ty)) {
            .alias => |alias| try self.rowExtensionIsDefaultClosedInner(view, alias.backing, expected, seen),
            .flex, .rigid => |variable| variable.row_default == expected,
            else => false,
        };
    }

    fn appendDictionaryRequirements(
        self: *Builder,
        source_type: TypeRef,
        constraints: []const checked.CheckedStaticDispatchConstraint,
    ) Allocator.Error!Span {
        const start: u32 = @intCast(self.plan.dictionaries.items.len);
        for (constraints, 0..) |constraint, index| {
            try self.plan.dictionaries.append(self.allocator, .{
                .source_type = source_type,
                .constraint_index = @intCast(index),
                .fn_name = constraint.fn_name,
                .fn_ty = .{ .module = source_type.module, .ty = constraint.fn_ty },
                .origin = constraint.origin,
                .binop_negated = constraint.binop_negated,
                .num_literal = constraint.num_literal,
            });
        }
        const view = self.moduleForId(source_type.module);
        for (constraints) |constraint| {
            _ = try self.analyzeType(view, constraint.fn_ty);
        }
        return .{ .start = start, .len = @intCast(constraints.len) };
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

    fn propagateDynamicRequirements(self: *Builder) void {
        var changed = true;
        while (changed) {
            changed = false;
            for (self.plan.representations.items) |*rep| {
                const next = self.representationContainsDynamic(rep.*);
                if (next != rep.contains_dynamic) {
                    rep.contains_dynamic = next;
                    changed = true;
                }
            }
        }
    }

    fn representationContainsDynamic(self: *const Builder, rep: TypeRepresentation) bool {
        switch (rep.kind) {
            .dynamic => return true,
            .in_progress => return false,
            else => {},
        }
        for (self.plan.childSlice(rep.children)) |child| {
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
            if (self.workerResolvesToHosted(worker.source)) {
                self.plan.workers.items[worker_index].hidden_descs = .{};
                continue;
            }

            var pending = std.ArrayList(HiddenDescriptorParam).empty;
            defer pending.deinit(self.allocator);
            var seen_reps = std.AutoHashMap(TypeRepId, void).init(self.allocator);
            defer seen_reps.deinit();
            var seen_descs = std.AutoHashMap(DescriptorRequirementId, void).init(self.allocator);
            defer seen_descs.deinit();

            if (try self.functionChildren(worker.rep)) |function| {
                const children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(function.rep)].children);
                for (children[function.args_start..][0..function.arg_count]) |child| {
                    try self.collectHiddenDescriptorsForRep(child.rep, &pending, &seen_reps, &seen_descs);
                }
                try self.collectHiddenDescriptorsForRep(function.ret, &pending, &seen_reps, &seen_descs);
            } else {
                try self.collectHiddenDescriptorsForRep(worker.rep, &pending, &seen_reps, &seen_descs);
            }

            const start: u32 = @intCast(self.plan.hidden_descriptor_params.items.len);
            try self.plan.hidden_descriptor_params.appendSlice(self.allocator, pending.items);
            self.plan.workers.items[worker_index].hidden_descs = .{
                .start = start,
                .len = @intCast(pending.items.len),
            };
        }
    }

    fn materializeWorkerHiddenDictionaryParams(self: *Builder) Allocator.Error!void {
        for (self.plan.workers.items, 0..) |worker, worker_index| {
            if (self.workerResolvesToHosted(worker.source)) {
                self.plan.workers.items[worker_index].hidden_dicts = .{};
                continue;
            }

            var pending = std.ArrayList(HiddenDictionaryParam).empty;
            defer pending.deinit(self.allocator);
            var seen_reps = std.AutoHashMap(TypeRepId, void).init(self.allocator);
            defer seen_reps.deinit();

            if (try self.functionChildren(worker.rep)) |function| {
                const children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(function.rep)].children);
                for (children[function.args_start..][0..function.arg_count]) |child| {
                    try self.collectHiddenDictionariesForRep(child.rep, &pending, &seen_reps);
                }
                try self.collectHiddenDictionariesForRep(function.ret, &pending, &seen_reps);
            } else {
                try self.collectHiddenDictionariesForRep(worker.rep, &pending, &seen_reps);
            }

            const start: u32 = @intCast(self.plan.hidden_dictionary_params.items.len);
            try self.plan.hidden_dictionary_params.appendSlice(self.allocator, pending.items);
            self.plan.workers.items[worker_index].hidden_dicts = .{
                .start = start,
                .len = @intCast(pending.items.len),
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
                            });
                        }
                    }
                },
                .procedure_template,
                .procedure_binding,
                .procedure_use,
                => {},
            }

            for (self.plan.hiddenDescriptorParamSlice(worker.hidden_descs)) |param| {
                try pending.append(self.allocator, .{
                    .kind = .hidden_desc,
                    .source_type = param.source_type,
                    .rep = param.rep,
                    .desc = param.desc,
                });
            }

            for (self.plan.hiddenDictionaryParamSlice(worker.hidden_dicts)) |param| {
                try pending.append(self.allocator, .{
                    .kind = .hidden_dict,
                    .source_type = param.source_type,
                    .rep = param.rep,
                    .dictionaries = param.dictionaries,
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

    fn workerResolvesToHosted(self: *Builder, source: WorkerSource) bool {
        if (self.root_module == null) return workerSourceIsHosted(source);
        return switch (self.rootWorkerBody(source)) {
            .hosted_proc => true,
            .checked_expr,
            .intrinsic_wrapper,
            => false,
        };
    }

    fn materializeDirectCallHiddenDescriptorArgs(self: *Builder) Allocator.Error!void {
        var direct_index: usize = 0;
        while (direct_index < self.plan.direct_calls.items.len) : (direct_index += 1) {
            const direct = self.plan.direct_calls.items[direct_index];
            const call_view = self.moduleForId(direct.call.module);
            const call_expr = call_view.checked_bodies.expr(direct.call.expr);
            var owned_call_args: []checked.CheckedExprId = &.{};
            defer if (owned_call_args.len != 0) self.allocator.free(owned_call_args);
            const call_args = switch (call_expr.data) {
                .call => |call| call.args,
                .dispatch_call => |maybe_plan| blk: {
                    owned_call_args = try self.checkedArgsForDispatchCall(call_view, maybe_plan);
                    break :blk owned_call_args;
                },
                .type_dispatch_call => |maybe_plan| blk: {
                    owned_call_args = try self.checkedArgsForDispatchCall(call_view, maybe_plan);
                    break :blk owned_call_args;
                },
                .method_eq => |maybe_plan| blk: {
                    owned_call_args = try self.checkedArgsForDispatchCall(call_view, maybe_plan);
                    break :blk owned_call_args;
                },
                else => boxyPlanInvariant("boxy direct call plan referenced a checked expression that is not lowered as a worker call"),
            };

            const arg_types = try self.allocator.alloc(TypeRef, call_args.len);
            defer self.allocator.free(arg_types);
            for (call_args, arg_types) |arg_expr_id, *arg_type| {
                const arg_expr = call_view.checked_bodies.expr(arg_expr_id);
                arg_type.* = typeRef(call_view, arg_expr.ty);
            }
            self.plan.direct_calls.items[direct_index].hidden_desc_args =
                try self.materializeWorkerCallHiddenDescriptorArgs(direct.worker, arg_types, typeRef(call_view, call_expr.ty));
        }
    }

    fn materializeIteratorCallHiddenDescriptorArgs(self: *Builder) Allocator.Error!void {
        for (self.plan.iterator_calls.items, 0..) |call, call_index| {
            self.plan.iterator_calls.items[call_index].hidden_desc_args =
                try self.materializeWorkerCallHiddenDescriptorArgs(call.worker, self.plan.iteratorCallArgTypeSlice(call.arg_types), call.ret_type);
        }
    }

    fn materializeDirectCallHiddenDictionaryArgs(self: *Builder) Allocator.Error!void {
        var direct_index: usize = 0;
        while (direct_index < self.plan.direct_calls.items.len) : (direct_index += 1) {
            const direct = self.plan.direct_calls.items[direct_index];
            const call_view = self.moduleForId(direct.call.module);
            const call_expr = call_view.checked_bodies.expr(direct.call.expr);
            var owned_call_args: []checked.CheckedExprId = &.{};
            defer if (owned_call_args.len != 0) self.allocator.free(owned_call_args);
            const call_args = switch (call_expr.data) {
                .call => |call| call.args,
                .dispatch_call => |maybe_plan| blk: {
                    owned_call_args = try self.checkedArgsForDispatchCall(call_view, maybe_plan);
                    break :blk owned_call_args;
                },
                .type_dispatch_call => |maybe_plan| blk: {
                    owned_call_args = try self.checkedArgsForDispatchCall(call_view, maybe_plan);
                    break :blk owned_call_args;
                },
                .method_eq => |maybe_plan| blk: {
                    owned_call_args = try self.checkedArgsForDispatchCall(call_view, maybe_plan);
                    break :blk owned_call_args;
                },
                else => boxyPlanInvariant("boxy direct call plan referenced a checked expression that is not lowered as a worker call"),
            };

            const arg_types = try self.allocator.alloc(TypeRef, call_args.len);
            defer self.allocator.free(arg_types);
            for (call_args, arg_types) |arg_expr_id, *arg_type| {
                const arg_expr = call_view.checked_bodies.expr(arg_expr_id);
                arg_type.* = typeRef(call_view, arg_expr.ty);
            }
            self.plan.direct_calls.items[direct_index].hidden_dict_args =
                try self.materializeWorkerCallHiddenDictionaryArgs(direct.worker, arg_types, typeRef(call_view, call_expr.ty));
        }
    }

    fn materializeIteratorCallHiddenDictionaryArgs(self: *Builder) Allocator.Error!void {
        for (self.plan.iterator_calls.items, 0..) |call, call_index| {
            self.plan.iterator_calls.items[call_index].hidden_dict_args =
                try self.materializeWorkerCallHiddenDictionaryArgs(call.worker, self.plan.iteratorCallArgTypeSlice(call.arg_types), call.ret_type);
        }
    }

    fn materializeWorkerCallHiddenDescriptorArgs(
        self: *Builder,
        worker_id: WorkerPlanId,
        arg_types: []const TypeRef,
        ret_type: TypeRef,
    ) Allocator.Error!Span {
        const worker = self.plan.workers.items[@intFromEnum(worker_id)];
        const params = self.plan.hiddenDescriptorParamSlice(worker.hidden_descs);
        if (params.len == 0) return .{};

        const worker_function = (try self.functionChildren(worker.rep)) orelse
            boxyPlanInvariant("boxy worker call target with hidden descriptors was not a function worker");
        if (worker_function.arg_count != arg_types.len) {
            boxyPlanInvariant("boxy worker call hidden descriptor mapping saw mismatched function arity");
        }

        var pending = std.ArrayList(DirectCallHiddenDescriptorArg).empty;
        defer pending.deinit(self.allocator);
        var seen_reps = std.AutoHashMap(TypeRepId, void).init(self.allocator);
        defer seen_reps.deinit();
        var next_param: usize = 0;

        const worker_children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(worker_function.rep)].children);
        for (worker_children[worker_function.args_start..][0..worker_function.arg_count], arg_types, 0..) |worker_child, arg_type, arg_index| {
            const arg_rep = self.plan.repForSourceType(arg_type) orelse
                boxyPlanInvariant("boxy worker call argument type was not analyzed");
            try self.collectCallHiddenDescriptorArgs(worker_child.rep, arg_rep, @intCast(arg_index), params, &next_param, &pending, &seen_reps);
        }
        const ret_rep = self.plan.repForSourceType(ret_type) orelse
            boxyPlanInvariant("boxy worker call result type was not analyzed");
        try self.collectCallHiddenDescriptorArgs(worker_function.ret, ret_rep, null, params, &next_param, &pending, &seen_reps);

        if (next_param != params.len or pending.items.len != params.len) {
            boxyPlanInvariant("boxy worker call hidden descriptor mapping did not cover every worker descriptor param");
        }

        const start: u32 = @intCast(self.plan.direct_call_hidden_desc_args.items.len);
        try self.plan.direct_call_hidden_desc_args.appendSlice(self.allocator, pending.items);
        return .{
            .start = start,
            .len = @intCast(pending.items.len),
        };
    }

    fn materializeWorkerCallHiddenDictionaryArgs(
        self: *Builder,
        worker_id: WorkerPlanId,
        arg_types: []const TypeRef,
        ret_type: TypeRef,
    ) Allocator.Error!Span {
        const worker = self.plan.workers.items[@intFromEnum(worker_id)];
        const params = self.plan.hiddenDictionaryParamSlice(worker.hidden_dicts);
        if (params.len == 0) return .{};

        const worker_function = (try self.functionChildren(worker.rep)) orelse
            boxyPlanInvariant("boxy worker call target with hidden dictionaries was not a function worker");
        if (worker_function.arg_count != arg_types.len) {
            boxyPlanInvariant("boxy worker call hidden dictionary mapping saw mismatched function arity");
        }

        var pending = std.ArrayList(DirectCallHiddenDictionaryArg).empty;
        defer pending.deinit(self.allocator);
        var seen_reps = std.AutoHashMap(TypeRepId, void).init(self.allocator);
        defer seen_reps.deinit();
        var next_param: usize = 0;

        const worker_children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(worker_function.rep)].children);
        for (worker_children[worker_function.args_start..][0..worker_function.arg_count], arg_types) |worker_child, arg_type| {
            const arg_rep = self.plan.repForSourceType(arg_type) orelse
                boxyPlanInvariant("boxy worker call argument type was not analyzed");
            try self.collectCallHiddenDictionaryArgs(worker_child.rep, arg_rep, params, &next_param, &pending, &seen_reps);
        }
        const ret_rep = self.plan.repForSourceType(ret_type) orelse
            boxyPlanInvariant("boxy worker call result type was not analyzed");
        try self.collectCallHiddenDictionaryArgs(worker_function.ret, ret_rep, params, &next_param, &pending, &seen_reps);

        if (next_param != params.len or pending.items.len != params.len) {
            boxyPlanInvariant("boxy worker call hidden dictionary mapping did not cover every worker dictionary param");
        }

        const start: u32 = @intCast(self.plan.direct_call_hidden_dict_args.items.len);
        try self.plan.direct_call_hidden_dict_args.appendSlice(self.allocator, pending.items);
        return .{
            .start = start,
            .len = @intCast(pending.items.len),
        };
    }

    fn checkedArgsForDispatchCall(
        self: *Builder,
        view: ModuleView,
        maybe_plan: ?static_dispatch.StaticDispatchPlanId,
    ) Allocator.Error![]checked.CheckedExprId {
        const plan_id = maybe_plan orelse
            boxyPlanInvariant("checked dispatch expression reached boxy direct-call planning without a dispatch plan");
        const raw = @intFromEnum(plan_id);
        if (raw >= view.static_dispatch_plans.plans.len) {
            boxyPlanInvariant("checked dispatch expression referenced a missing dispatch plan");
        }

        const plan = view.static_dispatch_plans.plans[raw];
        const operands = plan.argsSlice(view.static_dispatch_plans);
        const args = try self.allocator.alloc(checked.CheckedExprId, operands.len);
        errdefer self.allocator.free(args);
        for (operands, args) |operand, *arg| {
            arg.* = switch (operand) {
                .checked_expr => |expr| expr,
                .generated_interpolation_iter,
                .generated_numeral,
                .generated_quote,
                => boxyPlanInvariant("generated static-dispatch operand reached boxy worker-call planning before generated operand lowering"),
            };
        }
        return args;
    }

    fn collectHiddenDescriptorsForRep(
        self: *Builder,
        rep_id: TypeRepId,
        pending: *std.ArrayList(HiddenDescriptorParam),
        seen_reps: *std.AutoHashMap(TypeRepId, void),
        seen_descs: *std.AutoHashMap(DescriptorRequirementId, void),
    ) Allocator.Error!void {
        const rep_entry = try seen_reps.getOrPut(rep_id);
        if (rep_entry.found_existing) return;

        const rep = self.plan.representations.items[@intFromEnum(rep_id)];
        if (rep.descriptor) |desc| {
            const desc_entry = try seen_descs.getOrPut(desc);
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

    fn collectHiddenDictionariesForRep(
        self: *Builder,
        rep_id: TypeRepId,
        pending: *std.ArrayList(HiddenDictionaryParam),
        seen_reps: *std.AutoHashMap(TypeRepId, void),
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

        for (self.plan.childSlice(rep.children)) |child| {
            try self.collectHiddenDictionariesForRep(child.rep, pending, seen_reps);
        }
    }

    fn collectCallHiddenDescriptorArgs(
        self: *Builder,
        worker_rep_id: TypeRepId,
        call_rep_id: TypeRepId,
        source_arg_index: ?u32,
        params: []const HiddenDescriptorParam,
        next_param: *usize,
        pending: *std.ArrayList(DirectCallHiddenDescriptorArg),
        seen_reps: *std.AutoHashMap(TypeRepId, void),
    ) Allocator.Error!void {
        const rep_entry = try seen_reps.getOrPut(worker_rep_id);
        if (rep_entry.found_existing) return;

        const worker_rep = self.plan.representations.items[@intFromEnum(worker_rep_id)];
        const call_rep = self.plan.representations.items[@intFromEnum(call_rep_id)];

        if (worker_rep.descriptor) |worker_desc| {
            if (next_param.* >= params.len or params[next_param.*].desc != worker_desc) {
                boxyPlanInvariant("boxy direct call hidden descriptor order disagreed with worker descriptor params");
            }
            next_param.* += 1;
            const desc_arg_rep_id = self.canonicalDescriptorArgRep(call_rep_id);
            const desc_arg_rep = self.plan.representations.items[@intFromEnum(desc_arg_rep_id)];
            try pending.append(self.allocator, .{
                .worker_desc = worker_desc,
                .worker_rep = worker_rep_id,
                .source_type = desc_arg_rep.source_type,
                .rep = desc_arg_rep_id,
                .source_arg_index = source_arg_index,
            });
        }

        if (worker_rep.children.len == 0) return;

        if (call_rep.kind == .empty_tag_union) {
            for (self.plan.childSlice(worker_rep.children)) |worker_child| {
                if (!try self.repSubtreeHasDescriptor(worker_child.rep)) continue;
                try self.collectCallHiddenDescriptorArgs(worker_child.rep, call_rep_id, source_arg_index, params, next_param, pending, seen_reps);
            }
            return;
        }

        const worker_children = self.plan.childSlice(worker_rep.children);
        const call_children = self.plan.childSlice(call_rep.children);
        for (worker_children) |worker_child| {
            if (!try self.repSubtreeHasDescriptor(worker_child.rep)) continue;
            if (self.findMatchingChildByRole(call_children, worker_child)) |call_child| {
                try self.collectCallHiddenDescriptorArgs(worker_child.rep, call_child.rep, source_arg_index, params, next_param, pending, seen_reps);
                continue;
            }
            if (self.structuralWrapperBackingRep(call_rep_id)) |call_backing| {
                const backing_children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(call_backing)].children);
                if (self.findMatchingChildByRole(backing_children, worker_child)) |call_child| {
                    try self.collectCallHiddenDescriptorArgs(worker_child.rep, call_child.rep, source_arg_index, params, next_param, pending, seen_reps);
                    continue;
                }
            }
            if (try self.findMatchingTagPayloadInRowExtension(call_children, worker_child)) |call_child| {
                try self.collectCallHiddenDescriptorArgs(worker_child.rep, call_child.rep, source_arg_index, params, next_param, pending, seen_reps);
                continue;
            }
            if (try self.findMatchingChildBySourceType(call_children, worker_child)) |call_child| {
                try self.collectCallHiddenDescriptorArgs(worker_child.rep, call_child.rep, source_arg_index, params, next_param, pending, seen_reps);
                continue;
            }
            if (try self.workerChildCanMatchUnwrappedCallRep(worker_rep_id, worker_child)) {
                try self.collectCallHiddenDescriptorArgs(worker_child.rep, call_rep_id, source_arg_index, params, next_param, pending, seen_reps);
                continue;
            }
            if (worker_child.role == .tag_ext and call_children.len == 0 and call_rep.descriptor != null) {
                try self.collectCallHiddenDescriptorArgs(worker_child.rep, call_rep_id, source_arg_index, params, next_param, pending, seen_reps);
                continue;
            }
            boxyPlanInvariant("boxy direct call hidden descriptor mapping saw mismatched child roles");
        }
    }

    fn collectCallHiddenDictionaryArgs(
        self: *Builder,
        worker_rep_id: TypeRepId,
        call_rep_id: TypeRepId,
        params: []const HiddenDictionaryParam,
        next_param: *usize,
        pending: *std.ArrayList(DirectCallHiddenDictionaryArg),
        seen_reps: *std.AutoHashMap(TypeRepId, void),
    ) Allocator.Error!void {
        const rep_entry = try seen_reps.getOrPut(worker_rep_id);
        if (rep_entry.found_existing) return;

        const worker_rep = self.plan.representations.items[@intFromEnum(worker_rep_id)];
        const call_rep = self.plan.representations.items[@intFromEnum(call_rep_id)];

        if (worker_rep.dictionaries.len != 0) {
            if (next_param.* >= params.len or !std.meta.eql(params[next_param.*].dictionaries, worker_rep.dictionaries)) {
                boxyPlanInvariant("boxy direct call hidden dictionary order disagreed with worker dictionary params");
            }
            next_param.* += 1;
            const dict_arg_rep_id = self.canonicalDictionaryArgRep(call_rep_id);
            const dict_arg_rep = self.plan.representations.items[@intFromEnum(dict_arg_rep_id)];
            if (dict_arg_rep.dictionaries.len == 0) {
                try self.ensureStaticDictionaryWorkers(dict_arg_rep_id, worker_rep.dictionaries);
            }
            try pending.append(self.allocator, .{
                .worker_dictionaries = worker_rep.dictionaries,
                .source_type = dict_arg_rep.source_type,
                .rep = dict_arg_rep_id,
            });
        }

        if (worker_rep.children.len == 0) return;

        if (call_rep.kind == .empty_tag_union) {
            for (self.plan.childSlice(worker_rep.children)) |worker_child| {
                if (!try self.repSubtreeHasDictionary(worker_child.rep)) continue;
                try self.collectCallHiddenDictionaryArgs(worker_child.rep, call_rep_id, params, next_param, pending, seen_reps);
            }
            return;
        }

        const worker_children = self.plan.childSlice(worker_rep.children);
        const call_children = self.plan.childSlice(call_rep.children);
        for (worker_children) |worker_child| {
            if (!try self.repSubtreeHasDictionary(worker_child.rep)) continue;
            if (self.findMatchingChildByRole(call_children, worker_child)) |call_child| {
                try self.collectCallHiddenDictionaryArgs(worker_child.rep, call_child.rep, params, next_param, pending, seen_reps);
                continue;
            }
            if (self.structuralWrapperBackingRep(call_rep_id)) |call_backing| {
                const backing_children = self.plan.childSlice(self.plan.representations.items[@intFromEnum(call_backing)].children);
                if (self.findMatchingChildByRole(backing_children, worker_child)) |call_child| {
                    try self.collectCallHiddenDictionaryArgs(worker_child.rep, call_child.rep, params, next_param, pending, seen_reps);
                    continue;
                }
            }
            if (try self.findMatchingTagPayloadInRowExtension(call_children, worker_child)) |call_child| {
                try self.collectCallHiddenDictionaryArgs(worker_child.rep, call_child.rep, params, next_param, pending, seen_reps);
                continue;
            }
            if (try self.findMatchingDictionaryChildBySourceType(call_children, worker_child)) |call_child| {
                try self.collectCallHiddenDictionaryArgs(worker_child.rep, call_child.rep, params, next_param, pending, seen_reps);
                continue;
            }
            if (try self.workerChildCanMatchUnwrappedCallRepForDictionaries(worker_rep_id, worker_child)) {
                try self.collectCallHiddenDictionaryArgs(worker_child.rep, call_rep_id, params, next_param, pending, seen_reps);
                continue;
            }
            if (worker_child.role == .tag_ext and call_children.len == 0 and call_rep.dictionaries.len != 0) {
                try self.collectCallHiddenDictionaryArgs(worker_child.rep, call_rep_id, params, next_param, pending, seen_reps);
                continue;
            }
            boxyPlanInvariant("boxy direct call hidden dictionary mapping saw mismatched child roles");
        }
    }

    fn ensureStaticDictionaryWorkers(
        self: *Builder,
        source_rep_id: TypeRepId,
        worker_dictionaries: Span,
    ) Allocator.Error!void {
        const source_rep = self.plan.representations.items[@intFromEnum(source_rep_id)];
        const source_view = self.moduleForId(source_rep.source_type.module);
        const owner = methodOwnerForModuleType(source_view, source_rep.source_type.ty) orelse
            boxyPlanInvariant("static boxy dictionary source type had no checked method owner");

        for (self.plan.dictionarySlice(worker_dictionaries)) |requirement| {
            const requirement_view = self.moduleForId(requirement.source_type.module);
            const lookup = self.lookupMethodTarget(source_view, owner, requirement_view, requirement.fn_name) orelse
                boxyPlanInvariant("static boxy dictionary could not resolve a checked method target");
            const source = self.workerSourceForMethodTarget(lookup);
            const source_fn_type = TypeRef{ .module = lookup.view.key, .ty = lookup.target.callable_ty };
            _ = try self.analyzeType(lookup.view, lookup.target.callable_ty);
            _ = try self.ensureWorker(source, source_fn_type, null);
        }
    }

    const MethodTargetLookup = struct {
        view: ModuleView,
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
        self: *Builder,
        candidate: ModuleView,
        owner_view: ModuleView,
        owner: static_dispatch.MethodOwner,
        method_text: []const u8,
    ) ?MethodTargetLookup {
        _ = self;
        const owner_names = owner_view.canonical_names orelse return null;
        const candidate_names = candidate.canonical_names orelse return null;
        const candidate_owner = methodOwnerInNames(owner_names, candidate_names, owner) orelse return null;
        const candidate_method = candidate_names.lookupMethodName(method_text) orelse return null;
        const target = candidate.method_registry.lookup(.{ .owner = candidate_owner, .method = candidate_method }) orelse return null;
        return .{ .view = candidate, .target = target };
    }

    fn workerSourceForMethodTarget(self: *Builder, lookup: MethodTargetLookup) WorkerSource {
        return switch (lookup.target.kind) {
            .procedure => |procedure| .{ .procedure_template = procedure.template },
            .local_proc => |local| if (self.topLevelProcedureBindingForExpr(lookup.view, local.expr)) |binding|
                .{ .procedure_binding = binding }
            else
                .{ .nested_expr = .{ .module = lookup.view.key, .expr = self.nestedCallableSiteExprForExpr(lookup.view, local.expr) orelse local.expr } },
        };
    }

    fn repSubtreeHasDescriptor(self: *Builder, rep_id: TypeRepId) Allocator.Error!bool {
        var seen = std.AutoHashMap(TypeRepId, void).init(self.allocator);
        defer seen.deinit();
        return try self.repSubtreeHasDescriptorInner(rep_id, &seen);
    }

    fn repSubtreeHasDescriptorInner(
        self: *Builder,
        rep_id: TypeRepId,
        seen: *std.AutoHashMap(TypeRepId, void),
    ) Allocator.Error!bool {
        const entry = try seen.getOrPut(rep_id);
        if (entry.found_existing) return false;

        const rep = self.plan.representations.items[@intFromEnum(rep_id)];
        if (rep.descriptor != null) return true;

        for (self.plan.childSlice(rep.children)) |child| {
            if (try self.repSubtreeHasDescriptorInner(child.rep, seen)) return true;
        }
        return false;
    }

    fn repSubtreeHasDictionary(self: *Builder, rep_id: TypeRepId) Allocator.Error!bool {
        var seen = std.AutoHashMap(TypeRepId, void).init(self.allocator);
        defer seen.deinit();
        return try self.repSubtreeHasDictionaryInner(rep_id, &seen);
    }

    fn repSubtreeHasDictionaryInner(
        self: *Builder,
        rep_id: TypeRepId,
        seen: *std.AutoHashMap(TypeRepId, void),
    ) Allocator.Error!bool {
        const entry = try seen.getOrPut(rep_id);
        if (entry.found_existing) return false;

        const rep = self.plan.representations.items[@intFromEnum(rep_id)];
        if (rep.dictionaries.len != 0) return true;

        for (self.plan.childSlice(rep.children)) |child| {
            if (try self.repSubtreeHasDictionaryInner(child.rep, seen)) return true;
        }
        return false;
    }

    const FunctionChildren = struct {
        rep: TypeRepId,
        args_start: u32,
        arg_count: u32,
        ret: TypeRepId,
    };

    fn functionChildren(self: *Builder, rep_id: TypeRepId) Allocator.Error!?FunctionChildren {
        const canonical_rep = try self.canonicalFunctionRep(rep_id);
        const rep = self.plan.representations.items[@intFromEnum(canonical_rep)];
        return switch (rep.kind) {
            .erased_callable => blk: {
                const children = self.plan.childSlice(rep.children);
                var args_start: ?u32 = null;
                var arg_count: u32 = 0;
                var ret: ?TypeRepId = null;
                for (children, 0..) |child, i| {
                    switch (child.role) {
                        .function_arg => {
                            if (args_start == null) args_start = @intCast(i);
                            arg_count += 1;
                        },
                        .function_ret => ret = child.rep,
                        else => {},
                    }
                }
                break :blk .{
                    .rep = canonical_rep,
                    .args_start = args_start orelse 0,
                    .arg_count = arg_count,
                    .ret = ret orelse boxyPlanInvariant("function representation had no return child"),
                };
            },
            else => null,
        };
    }

    fn canonicalFunctionRep(self: *Builder, rep_id: TypeRepId) Allocator.Error!TypeRepId {
        var current = rep_id;
        var depth: u16 = 0;
        while (true) {
            if (depth == 1024) boxyPlanInvariant("function root alias chain exceeded boxy planner limit");
            depth += 1;

            const rep = self.plan.representations.items[@intFromEnum(current)];
            switch (rep.kind) {
                .alias => current = requiredSingleChild(&self.plan, current, .alias_backing).rep,
                .nominal => |kind| switch (kind) {
                    .transparent => current = requiredSingleChild(&self.plan, current, .nominal_backing).rep,
                    .opaque_nominal, .builtin_other => return current,
                },
                else => return current,
            }
        }
    }

    fn findMatchingChildByRole(
        self: *Builder,
        children: []const RepChild,
        target: RepChild,
    ) ?RepChild {
        for (children) |child| {
            if (self.childRolesMatch(target, child)) return child;
        }
        return null;
    }

    fn findMatchingChildBySourceType(
        self: *Builder,
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

    fn findMatchingDictionaryChildBySourceType(
        self: *Builder,
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

    fn findMatchingTagPayloadInRowExtension(
        self: *Builder,
        children: []const RepChild,
        target: RepChild,
    ) Allocator.Error!?RepChild {
        switch (target.role) {
            .tag_payload => {},
            else => return null,
        }

        var seen = std.AutoHashMap(TypeRepId, void).init(self.allocator);
        defer seen.deinit();
        return try self.findMatchingTagPayloadInRowExtensionInner(children, target, &seen);
    }

    fn findMatchingTagPayloadInRowExtensionInner(
        self: *Builder,
        children: []const RepChild,
        target: RepChild,
        seen: *std.AutoHashMap(TypeRepId, void),
    ) Allocator.Error!?RepChild {
        for (children) |child| {
            switch (child.role) {
                .tag_ext => {},
                else => continue,
            }
            if (try self.findMatchingTagPayloadInRep(child.rep, target, seen)) |match| return match;
        }
        return null;
    }

    fn findMatchingTagPayloadInRep(
        self: *Builder,
        rep_id: TypeRepId,
        target: RepChild,
        seen: *std.AutoHashMap(TypeRepId, void),
    ) Allocator.Error!?RepChild {
        const entry = try seen.getOrPut(rep_id);
        if (entry.found_existing) return null;

        const rep = self.plan.representations.items[@intFromEnum(rep_id)];
        const children = self.plan.childSlice(rep.children);
        if (self.findMatchingChildByRole(children, target)) |match| return match;

        if (self.structuralWrapperBackingRep(rep_id)) |backing_rep| {
            const backing = self.plan.representations.items[@intFromEnum(backing_rep)];
            const backing_children = self.plan.childSlice(backing.children);
            if (self.findMatchingChildByRole(backing_children, target)) |match| return match;
            if (try self.findMatchingTagPayloadInRowExtensionInner(backing_children, target, seen)) |match| return match;
        }

        return try self.findMatchingTagPayloadInRowExtensionInner(children, target, seen);
    }

    fn childRolesMatch(self: *Builder, target: RepChild, candidate: RepChild) bool {
        return switch (target.role) {
            .record_field => |target_name| switch (candidate.role) {
                .record_field => |candidate_name| self.recordFieldNameMatches(
                    self.moduleForId(target.source_type.module),
                    target_name,
                    self.moduleForId(candidate.source_type.module),
                    candidate_name,
                ),
                else => false,
            },
            .tag_payload => |target_payload| switch (candidate.role) {
                .tag_payload => |candidate_payload| target_payload.index == candidate_payload.index and
                    self.tagLabelNameMatches(
                        self.moduleForId(target.source_type.module),
                        target_payload.tag,
                        self.moduleForId(candidate.source_type.module),
                        candidate_payload.tag,
                    ),
                else => false,
            },
            else => std.meta.eql(target.role, candidate.role),
        };
    }

    fn structuralWrapperBackingRep(self: *Builder, rep_id: TypeRepId) ?TypeRepId {
        const rep = self.plan.representations.items[@intFromEnum(rep_id)];
        return switch (rep.kind) {
            .alias => requiredSingleChild(&self.plan, rep_id, .alias_backing).rep,
            .nominal => |kind| switch (kind) {
                .transparent => if (rep.declared_fields.len == 0)
                    requiredSingleChild(&self.plan, rep_id, .nominal_backing).rep
                else
                    null,
                .opaque_nominal, .builtin_other => null,
            },
            else => null,
        };
    }

    fn canonicalDescriptorArgRep(self: *Builder, rep_id: TypeRepId) TypeRepId {
        var current = rep_id;
        var depth: u16 = 0;
        while (true) {
            if (depth == 1024) boxyPlanInvariant("descriptor argument wrapper chain exceeded boxy planner limit");
            depth += 1;

            current = self.structuralWrapperBackingRep(current) orelse return current;
        }
    }

    fn canonicalDictionaryArgRep(self: *Builder, rep_id: TypeRepId) TypeRepId {
        var current = rep_id;
        var depth: u16 = 0;
        while (true) {
            if (depth == 1024) boxyPlanInvariant("dictionary argument wrapper chain exceeded boxy planner limit");
            depth += 1;

            current = self.structuralWrapperBackingRep(current) orelse return current;
        }
    }

    fn workerChildCanMatchUnwrappedCallRep(
        self: *Builder,
        worker_rep_id: TypeRepId,
        worker_child: RepChild,
    ) Allocator.Error!bool {
        const worker_backing = self.structuralWrapperBackingRep(worker_rep_id) orelse return false;
        return worker_child.rep == worker_backing and !try self.repSubtreeHasDescriptorInOtherChildren(worker_rep_id, worker_child);
    }

    fn workerChildCanMatchUnwrappedCallRepForDictionaries(
        self: *Builder,
        worker_rep_id: TypeRepId,
        worker_child: RepChild,
    ) Allocator.Error!bool {
        const worker_backing = self.structuralWrapperBackingRep(worker_rep_id) orelse return false;
        return worker_child.rep == worker_backing and !try self.repSubtreeHasDictionaryInOtherChildren(worker_rep_id, worker_child);
    }

    fn repSubtreeHasDescriptorInOtherChildren(
        self: *Builder,
        rep_id: TypeRepId,
        selected_child: RepChild,
    ) Allocator.Error!bool {
        const rep = self.plan.representations.items[@intFromEnum(rep_id)];
        for (self.plan.childSlice(rep.children)) |child| {
            if (child.rep == selected_child.rep and std.meta.eql(child.role, selected_child.role)) continue;
            if (try self.repSubtreeHasDescriptor(child.rep)) return true;
        }
        return false;
    }

    fn repSubtreeHasDictionaryInOtherChildren(
        self: *Builder,
        rep_id: TypeRepId,
        selected_child: RepChild,
    ) Allocator.Error!bool {
        const rep = self.plan.representations.items[@intFromEnum(rep_id)];
        for (self.plan.childSlice(rep.children)) |child| {
            if (child.rep == selected_child.rep and std.meta.eql(child.role, selected_child.role)) continue;
            if (try self.repSubtreeHasDictionary(child.rep)) return true;
        }
        return false;
    }

    fn analyzeWorkerBodyTypes(self: *Builder, source: WorkerSource) Allocator.Error!void {
        const body = self.rootWorkerBody(source);
        switch (body) {
            .checked_expr => |checked_body| try self.analyzeWorkerRootExprTypes(checked_body.view, checked_body.root_expr),
            .intrinsic_wrapper => |intrinsic| try self.analyzeIntrinsicWrapperTypes(intrinsic.view, intrinsic.wrapper),
            .hosted_proc => |hosted| try self.analyzeHostedProcTypes(hosted.view, hosted.proc),
        }
    }

    fn analyzeWorkerRootExprTypes(
        self: *Builder,
        view: ModuleView,
        root_expr: checked.CheckedExprId,
    ) Allocator.Error!void {
        const expr = view.checked_bodies.expr(root_expr);
        return switch (expr.data) {
            .lambda => |lambda| {
                for (lambda.args) |arg| try self.analyzePatternTypes(view, arg);
                try self.analyzeExprTypes(view, lambda.body);
            },
            else => try self.analyzeExprTypes(view, root_expr),
        };
    }

    const WorkerBody = union(enum) {
        checked_expr: struct {
            view: ModuleView,
            root_expr: checked.CheckedExprId,
        },
        intrinsic_wrapper: struct {
            view: ModuleView,
            wrapper: checked.IntrinsicWrapper,
        },
        hosted_proc: struct {
            view: ModuleView,
            proc: checked.HostedProc,
        },
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
        };
    }

    fn nestedExprWorkerBody(self: *Builder, expr_ref: ExprRef) WorkerBody {
        const view = self.moduleForId(expr_ref.module);
        const expr = view.checked_bodies.expr(expr_ref.expr);
        const root_expr = switch (expr.data) {
            .lambda => expr_ref.expr,
            .closure => |closure| closure.lambda,
            else => boxyPlanInvariant("nested callable worker source did not point at a lambda or closure"),
        };
        return .{ .checked_expr = .{
            .view = view,
            .root_expr = root_expr,
        } };
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

    fn rootProcedureTemplateBody(self: *Builder, template_ref: canonical.ProcedureTemplateRef) WorkerBody {
        const view = self.moduleForArtifactRef(template_ref.artifact);
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
        if (fn_value.captures.len != 0) {
            boxyPlanInvariant("capturing stored function reached runtime boxy body type planning before const capture lowering");
        }
        return switch (fn_value.fn_def) {
            .local_template,
            .imported_template,
            .checked_generated,
            => |template| self.rootProcedureTemplateBody(template),
            .nested => |nested| self.nestedConstFnBody(nested),
            .local_hosted,
            .imported_hosted,
            => boxyPlanInvariant("hosted stored function reached runtime boxy body type planning before hosted wrapper planning"),
            .parser_runtime,
            .encode_to_runtime,
            => boxyPlanInvariant("generated parser/encoder stored function reached runtime boxy body type planning before generated runtime support"),
        };
    }

    fn nestedConstFnBody(self: *Builder, nested: anytype) WorkerBody {
        const view = self.moduleForId(.{ .bytes = canonical.procTemplateModuleDigest(nested.owner).bytes });
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
        };
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
            if (!canonical.procedureTemplateRefEql(site.owner_template, nested.owner)) continue;
            const expr_id = site.checked_expr orelse
                boxyPlanInvariant("stored nested function had no checked expression site");
            const expr = view.checked_bodies.expr(expr_id);
            return switch (expr.data) {
                .lambda => expr_id,
                .closure => |closure| closure.lambda,
                else => boxyPlanInvariant("stored nested function site did not point at a lambda or closure"),
            };
        }
        boxyPlanInvariant("stored nested function referenced a missing checked nested site");
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
        const entry = try self.body_exprs_seen.getOrPut(.{ .module = view.key, .expr = expr_id });
        if (entry.found_existing) return;

        const bodies = view.checked_bodies;
        const expr = bodies.expr(expr_id);
        _ = try self.analyzeType(view, expr.ty);

        switch (expr.data) {
            .pending => boxyPlanInvariant("pending checked expression reached boxy body type planning"),
            .num,
            .frac_f32,
            .frac_f64,
            .dec,
            .dec_small,
            .num_from_numeral,
            .typed_int,
            .typed_frac,
            .typed_num_from_numeral,
            .str_from_quote,
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
                if (call.direct_target == null or local_proc_direct_target) {
                    try self.analyzeExprTypes(view, call.func);
                } else {
                    const func = view.checked_bodies.expr(call.func);
                    _ = try self.analyzeType(view, func.ty);
                }
                try self.analyzeExprSliceTypes(view, call.args);
                _ = try self.analyzeType(view, call.source_fn_ty_payload);
                if (!local_proc_direct_target) {
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
                try self.ensureNestedCallableWorker(view, expr_id);
                try self.analyzeWorkerRootExprTypes(view, closure.lambda);
                for (closure.captures) |capture| try self.analyzePatternTypes(view, capture.pattern);
            },
            .lambda => |lambda| {
                try self.ensureNestedCallableWorker(view, expr_id);
                for (lambda.args) |arg| try self.analyzePatternTypes(view, arg);
                try self.analyzeExprTypes(view, lambda.body);
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

    fn analyzeCallableLookupWorker(
        self: *Builder,
        view: ModuleView,
        expr_id: checked.CheckedExprId,
    ) Allocator.Error!void {
        const expr = view.checked_bodies.expr(expr_id);
        const maybe_ref: ?checked.ResolvedValueRefId = switch (expr.data) {
            .lookup_local => |lookup| lookup.resolved,
            .lookup_external => |resolved| resolved,
            .lookup_required => |resolved| resolved,
            else => boxyPlanInvariant("non-lookup expression reached callable lookup worker planning"),
        };
        const ref_id = maybe_ref orelse return;
        const source = self.workerSourceForProcedureValueRef(view, ref_id) orelse return;
        _ = try self.ensureWorker(source, typeRef(view, expr.ty), null);
    }

    fn ensureNestedCallableWorker(
        self: *Builder,
        view: ModuleView,
        expr_id: checked.CheckedExprId,
    ) Allocator.Error!void {
        const expr = view.checked_bodies.expr(expr_id);
        _ = try self.ensureWorker(.{ .nested_expr = .{ .module = view.key, .expr = expr_id } }, typeRef(view, expr.ty), null);
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
        for (plan.argsSlice(view.static_dispatch_plans)) |operand| {
            switch (operand) {
                .checked_expr => |operand_expr| try self.analyzeExprTypes(view, operand_expr),
                .generated_interpolation_iter,
                .generated_numeral,
                .generated_quote,
                => boxyPlanInvariant("generated static-dispatch operand reached boxy planning before generated operand lowering"),
            }
        }
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
        const target = switch (dispatch.resolution) {
            .resolved_target => |target| target,
            .unresolved_checked_plan => return,
        };
        const lookup = self.dispatchMethodTargetLookup(target);
        const worker = try self.ensureWorker(lookup.source, .{ .module = lookup.view.key, .ty = target.callable_ty }, null);
        const call_ref = ExprRef{ .module = view.key, .expr = call_expr };
        if (self.plan.directWorkerForCall(call_ref)) |existing| {
            if (existing != worker) {
                boxyPlanInvariant("boxy dispatch call plan tried to bind a checked call to two workers");
            }
            return;
        }
        try self.plan.direct_calls.append(self.allocator, .{
            .call = call_ref,
            .worker = worker,
            .source_fn_type = .{ .module = lookup.view.key, .ty = target.callable_ty },
        });
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
        ret_type: TypeRef,
    ) Allocator.Error!void {
        _ = try self.analyzeType(view, call.dispatcher_ty);
        _ = try self.analyzeType(view, call.callable_ty);
        _ = try self.analyzeType(self.moduleForId(ret_type.module), ret_type.ty);
        for (call.argsSlice(view.static_dispatch_plans)) |operand| {
            switch (operand) {
                .checked_expr => |expr| try self.analyzeExprTypes(view, expr),
                .loop_iterator_state => _ = try self.analyzeType(view, plan.iterator_ty),
            }
        }

        const target = switch (call.resolution) {
            .resolved_target => |target| target,
            .unresolved_checked_plan => return,
        };
        const lookup = self.dispatchMethodTargetLookup(target);
        const source_fn_type = TypeRef{ .module = lookup.view.key, .ty = target.callable_ty };
        _ = try self.analyzeType(lookup.view, target.callable_ty);
        const worker = try self.ensureWorker(lookup.source, source_fn_type, null);

        if (self.plan.iteratorCallPlanFor(view.key, plan_id, kind)) |existing| {
            if (existing.worker != worker or !typeRefEql(existing.source_fn_type, source_fn_type)) {
                boxyPlanInvariant("boxy iterator dispatch plan tried to bind a checked iterator call to two workers");
            }
            return;
        }

        const arg_start: u32 = @intCast(self.plan.iterator_call_arg_types.items.len);
        for (call.argsSlice(view.static_dispatch_plans)) |operand| {
            const arg_type = switch (operand) {
                .checked_expr => |expr_id| typeRef(view, view.checked_bodies.expr(expr_id).ty),
                .loop_iterator_state => typeRef(view, plan.iterator_ty),
            };
            try self.plan.iterator_call_arg_types.append(self.allocator, arg_type);
        }
        try self.plan.iterator_calls.append(self.allocator, .{
            .module = view.key,
            .for_plan = plan_id,
            .kind = kind,
            .worker = worker,
            .source_fn_type = source_fn_type,
            .arg_types = .{
                .start = arg_start,
                .len = @intCast(call.argsSlice(view.static_dispatch_plans).len),
            },
            .ret_type = ret_type,
        });
    }

    const DispatchWorkerLookup = struct {
        view: ModuleView,
        source: WorkerSource,
    };

    fn dispatchMethodTargetLookup(
        self: *Builder,
        target: static_dispatch.MethodTarget,
    ) DispatchWorkerLookup {
        return switch (target.kind) {
            .procedure => |procedure| .{
                .view = self.moduleForArtifactRef(procedure.template.artifact),
                .source = .{ .procedure_template = procedure.template },
            },
            .local_proc => boxyPlanInvariant("local procedure dispatch target reached boxy planning before nested procedure worker planning"),
        };
    }

    fn analyzeStatementTypes(self: *Builder, view: ModuleView, statement_id: checked.CheckedStatementId) Allocator.Error!void {
        const entry = try self.body_statements_seen.getOrPut(.{ .module = view.key, .statement = statement_id });
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
        const entry = try self.body_patterns_seen.getOrPut(.{ .module = view.key, .pattern = pattern_id });
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
            .num_literal => |literal| if (literal.conversion) |conversion| try self.analyzeExprTypes(view, conversion),
            .small_dec_literal => |literal| if (literal.conversion) |conversion| try self.analyzeExprTypes(view, conversion),
            .dec_literal => |literal| if (literal.conversion) |conversion| try self.analyzeExprTypes(view, conversion),
            .frac_f32_literal,
            .frac_f64_literal,
            => {},
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
        const call_ref = ExprRef{ .module = view.key, .expr = call_expr };
        if (self.plan.directWorkerForCall(call_ref)) |existing| {
            if (existing != worker) {
                boxyPlanInvariant("boxy direct call plan tried to bind a checked call to two workers");
            }
            return;
        }
        try self.plan.direct_calls.append(self.allocator, .{
            .call = call_ref,
            .worker = worker,
            .source_fn_type = source_fn_type,
        });
    }

    fn directTargetIsLocalProc(
        self: *Builder,
        view: ModuleView,
        target: checked.ResolvedValueId,
    ) bool {
        return switch (self.resolvedValueRecord(view, target).ref) {
            .local_proc => true,
            else => false,
        };
    }

    fn directCallInstantiationSourceFnType(
        self: *Builder,
        view: ModuleView,
        target: checked.ResolvedValueId,
        fallback: checked.CheckedTypeId,
    ) TypeRef {
        const record = self.resolvedValueRecord(view, target);
        return switch (record.ref) {
            .platform_required_proc => |required| .{
                .module = view.key,
                .ty = required.procedure.source_fn_ty_payload orelse
                    boxyPlanInvariant("platform-required procedure call missing relation-owned source function type"),
            },
            else => typeRef(view, fallback),
        };
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
            .platform_required_const,
            => null,
        };
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
            switch (site_expr.data) {
                .closure => |closure| if (closure.lambda == expr) return site_expr_id,
                else => {},
            }
        }
        return null;
    }

    fn topLevelProcedureBindingForExpr(
        self: *Builder,
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
                => continue,
            };
            if (view.checked_bodies.body(body_id).root_expr == expr) {
                _ = self;
                return .{
                    .artifact = view.key,
                    .binding = @enumFromInt(@as(u32, @intCast(index))),
                };
            }
        }
        return null;
    }

    fn workerCheckedTypeForSource(self: *Builder, source: WorkerSource, fallback: TypeRef) TypeRef {
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
                .hosted => fallback,
            },
            .nested_expr => fallback,
        };
    }

    fn checkedTypeForTopLevelBinding(
        self: *Builder,
        binding_ref: checked.ArtifactTopLevelProcedureBindingRef,
    ) TypeRef {
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
    ) TypeRef {
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

    fn checkedTypeForTemplate(self: *Builder, template_ref: canonical.ProcedureTemplateRef) TypeRef {
        const view = self.moduleForArtifactRef(template_ref.artifact);
        const template = view.checked_procedure_templates.get(template_ref.template);
        return typeRef(view, template.checked_fn_root);
    }

    fn workerSourceForProcedureUse(self: *Builder, procedure: checked.ProcedureUseTemplate) WorkerSource {
        return switch (procedure.binding) {
            .top_level => |top_level| blk: {
                const view = self.moduleForId(top_level.artifact);
                _ = self.rootProcedureBindingBody(view, top_level.binding);
                break :blk .{ .procedure_binding = top_level };
            },
            .platform_required => |required| blk: {
                const view = self.moduleForId(required.app_value.artifact);
                _ = self.rootProcedureBindingBody(view, required.procedure_binding);
                break :blk .{ .procedure_binding = .{
                    .artifact = required.app_value.artifact,
                    .binding = required.procedure_binding,
                } };
            },
            .imported => .{ .procedure_use = procedure },
            .hosted => .{ .procedure_use = procedure },
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
        => false,
    };
}

fn workerSourceEql(a: WorkerSource, b: WorkerSource) bool {
    return std.meta.eql(a, b);
}

fn findChildByRole(children: []const RepChild, role: ChildRole) ?RepChild {
    for (children) |child| {
        if (std.meta.eql(child.role, role)) return child;
    }
    return null;
}

fn requiredSingleChild(plan: *const ProgramPlan, rep_id: TypeRepId, role: ChildRole) RepChild {
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
            else => boxyPlanInvariant("checked intrinsic wrapper did not have a function type"),
        }
    }
}

fn hostedProcForTemplate(view: ModuleView, template_ref: canonical.ProcedureTemplateRef) checked.HostedProc {
    for (view.hosted_procs.procs) |hosted| {
        if (canonical.procedureTemplateRefEql(hosted.template, template_ref)) {
            return hosted;
        }
    }
    boxyPlanInvariant("hosted procedure template was missing from the checked hosted proc table");
}

fn hostedRepresentationForTemplate(
    view: ModuleView,
    template_ref: canonical.ProcedureTemplateRef,
) checked.HostedRepresentationCapability {
    for (view.interface_capabilities.hosted_representations) |hosted| {
        if (canonical.procedureTemplateRefEql(hosted.template, template_ref)) {
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
    };
}

fn moduleViewFromArtifact(artifact: *const checked.CheckedModuleArtifact) ModuleView {
    return .{
        .key = artifact.key,
        .canonical_names = &artifact.canonical_names,
        .checked_types = artifact.checked_types.view(),
        .checked_bodies = artifact.checked_bodies.view(),
        .compile_time_roots = &artifact.compile_time_roots,
        .entry_wrappers = &artifact.entry_wrappers,
        .intrinsic_wrappers = &artifact.intrinsic_wrappers,
        .hosted_procs = &artifact.hosted_procs,
        .resolved_value_refs = &artifact.resolved_value_refs,
        .static_dispatch_plans = &artifact.static_dispatch_plans,
        .method_registry = &artifact.method_registry,
        .checked_procedure_templates = &artifact.checked_procedure_templates,
        .nested_proc_sites = &artifact.nested_proc_sites,
        .top_level_procedure_bindings = &artifact.top_level_procedure_bindings,
        .callable_eval_templates = artifact.callable_eval_templates.view(),
        .exported_procedure_bindings = artifact.exported_procedure_bindings.view(),
        .interface_capabilities = &artifact.interface_capabilities,
        .const_store = &artifact.const_store,
    };
}

fn typeRef(view: ModuleView, ty: checked.CheckedTypeId) TypeRef {
    return .{ .module = view.key, .ty = ty };
}

fn typeRefEql(a: TypeRef, b: TypeRef) bool {
    return a.ty == b.ty and moduleKeyEqual(a.module, b.module);
}

fn exprRefEql(a: ExprRef, b: ExprRef) bool {
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
        switch (view.checked_types.payload(current)) {
            .alias => |alias| {
                current = alias.backing;
                continue;
            },
            else => |payload| return methodOwnerForCheckedPayload(payload),
        }
    }
}

fn methodOwnerForCheckedPayload(payload: checked.CheckedTypePayload) ?static_dispatch.MethodOwner {
    return switch (payload) {
        .nominal => |nominal| if (nominal.builtin) |builtin|
            .{ .builtin = builtinOwnerForCheckedBuiltin(builtin) }
        else if (nominal.source_decl) |source_decl|
            .{ .source_decl = .{
                .module_name = nominal.origin_module,
                .statement = source_decl,
            } }
        else
            .{ .nominal = .{
                .module_name = nominal.origin_module,
                .type_name = nominal.name,
                .source_decl = null,
            } },
        else => null,
    };
}

fn builtinOwnerForCheckedBuiltin(builtin: checked.CheckedBuiltinNominal) static_dispatch.BuiltinOwner {
    return switch (builtin) {
        .bool => .bool,
        .str => .str,
        .u8 => .u8,
        .i8 => .i8,
        .u16 => .u16,
        .i16 => .i16,
        .u32 => .u32,
        .i32 => .i32,
        .u64 => .u64,
        .i64 => .i64,
        .u128 => .u128,
        .i128 => .i128,
        .f32 => .f32,
        .f64 => .f64,
        .dec => .dec,
        .list => .list,
        .box => .box,
        .fields => .fields,
        .field => .field,
        .parse_tag_union_spec => .parse_tag_union_spec,
    };
}

fn methodOwnerInNames(
    source_names: *const canonical.CanonicalNameStore,
    target_names: *const canonical.CanonicalNameStore,
    owner: static_dispatch.MethodOwner,
) ?static_dispatch.MethodOwner {
    return switch (owner) {
        .builtin => |builtin| .{ .builtin = builtin },
        .source_decl => |decl| .{ .source_decl = .{
            .module_name = target_names.lookupModuleName(source_names.moduleNameText(decl.module_name)) orelse return null,
            .statement = decl.statement,
        } },
        .nominal => |nominal| .{ .nominal = .{
            .module_name = target_names.lookupModuleName(source_names.moduleNameText(nominal.module_name)) orelse return null,
            .type_name = target_names.lookupTypeName(source_names.typeNameText(nominal.type_name)) orelse return null,
            .source_decl = nominal.source_decl,
        } },
    };
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
        else => null,
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
        .{ .nominal = builtinNominal(.u64, @enumFromInt(0), .{}) },
    };
    const view = checked.CheckedTypeStoreView{ .stored_payloads = &payloads };
    const roots = [_]checked.RootRequest{
        .{
            .order = 3,
            .module_idx = 0,
            .kind = .provided_export,
            .source = .{ .def = @enumFromInt(4) },
            .checked_type = @enumFromInt(0),
            .abi = .roc,
            .exposure = .exported,
            .procedure_template = dummyProcedureTemplate(),
        },
    };

    var plan = try analyzeProgram(gpa, .{ .checked_types = view, .roots = &roots }, .{});
    defer plan.deinit();

    try std.testing.expectEqual(@as(usize, 1), plan.roots.items.len);
    try std.testing.expectEqual(@as(usize, 1), plan.workers.items.len);
    try std.testing.expectEqual(WorkerSource{ .procedure_template = dummyProcedureTemplate() }, plan.workers.items[0].source);
    try std.testing.expectEqual(plan.workers.items[0].id, plan.roots.items[0].worker);
    try std.testing.expectEqual(RootWrapperKind.host_shaped_wrapper, plan.roots.items[0].wrapper_kind);
    try std.testing.expectEqual(@as(u32, 3), plan.roots.items[0].request.order);
    try std.testing.expectEqual(plan.roots.items[0].host_rep, plan.roots.items[0].worker_rep);
    try std.testing.expectEqual(@as(usize, 1), plan.root_reps.items.len);
}

test "boxy planner walks callable eval finalized const function bodies" {
    const gpa = std.testing.allocator;

    const root_key = moduleKey(1);
    const template_ref = dummyProcedureTemplate();
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .empty_record = {} },
        .{ .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        } },
    };
    const exprs = [_]checked.StoredCheckedExpr{
        .{
            .id = @enumFromInt(0),
            .ty = @enumFromInt(1),
            .source_region = .zero(),
            .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
        },
        .{
            .id = @enumFromInt(1),
            .ty = @enumFromInt(0),
            .source_region = .zero(),
            .data = .empty_record,
        },
    };
    const callable_templates = [_]checked.CallableEvalTemplate{
        .{
            .id = @enumFromInt(0),
            .module_idx = 0,
            .pattern = @enumFromInt(0),
            .root = @enumFromInt(0),
            .source_scheme = .{},
            .checked_fn_root = @enumFromInt(1),
        },
    };
    var const_store = check.ConstStore.ConstStore.init(gpa);
    defer const_store.deinit();
    const fn_id = try const_store.appendFn(.{
        .fn_def = .{ .nested = .{
            .owner = template_ref,
            .site = @enumFromInt(0),
        } },
        .source_fn_ty = @enumFromInt(1),
        .source_fn_key = typeKey(1),
    });
    var compile_time_roots = [_]checked.CompileTimeRoot{
        .{
            .id = @enumFromInt(0),
            .module_idx = 0,
            .kind = .callable_binding,
            .source = .{ .def = @enumFromInt(0) },
            .pattern = @enumFromInt(0),
            .expr = @enumFromInt(0),
            .checked_type = @enumFromInt(1),
            .payload = .{ .fn_value = fn_id },
        },
    };
    var compile_time_root_table = checked.CompileTimeRootTable{ .roots = &compile_time_roots };
    var nested_sites = [_]checked.NestedProcSite{
        .{
            .site = @enumFromInt(0),
            .owner_template = template_ref,
            .path_start = 0,
            .path_len = 0,
            .kind = .local_function,
            .checked_expr = @enumFromInt(0),
            .checked_pattern = null,
        },
    };
    var nested_proc_site_table = checked.NestedProcSiteTable{ .sites = &nested_sites };
    var bindings = [_]checked.TopLevelProcedureBinding{
        .{
            .source_scheme = .{},
            .body = .{ .callable_eval_template = @enumFromInt(0) },
        },
    };
    var binding_table = checked.TopLevelProcedureBindingTable{ .bindings = &bindings };
    const roots = [_]checked.RootRequest{
        .{
            .order = 0,
            .module_idx = 0,
            .kind = .runtime_entrypoint,
            .source = .{ .def = @enumFromInt(0) },
            .checked_type = @enumFromInt(1),
            .abi = .roc,
            .exposure = .private,
            .procedure_binding = @enumFromInt(0),
        },
    };

    var plan = try analyzeProgram(gpa, .{
        .root_view = .{
            .key = root_key,
            .checked_types = .{ .stored_payloads = &payloads },
            .checked_bodies = .{ .stored_exprs = &exprs },
            .compile_time_roots = &compile_time_root_table,
            .nested_proc_sites = &nested_proc_site_table,
            .top_level_procedure_bindings = &binding_table,
            .callable_eval_templates = .{ .templates = &callable_templates },
            .const_store = &const_store,
        },
        .roots = &roots,
    }, .{});
    defer plan.deinit();

    try std.testing.expectEqual(@as(usize, 1), plan.workers.items.len);
    try std.testing.expectEqual(WorkerSource{ .procedure_binding = .{ .artifact = root_key, .binding = @enumFromInt(0) } }, plan.workers.items[0].source);
    try expectTypeRef(root_key, @enumFromInt(1), plan.workers.items[0].checked_type);
    try std.testing.expect(plan.repForSourceType(.{ .module = root_key, .ty = @enumFromInt(0) }) != null);
}

test "boxy planner does not add hidden descriptor params to imported hosted workers" {
    const gpa = std.testing.allocator;

    var root_artifact = minimalCheckedArtifact(gpa);
    defer root_artifact.canonical_names.deinit();
    defer root_artifact.checked_types.deinit(gpa);
    defer root_artifact.checked_bodies.deinit(gpa);

    var import_artifact = minimalCheckedArtifact(gpa);
    import_artifact.key = moduleKey(2);
    defer import_artifact.canonical_names.deinit();
    defer import_artifact.checked_types.deinit(gpa);

    try import_artifact.checked_types.type_id_pool.append(gpa, @as(checked.CheckedTypeId, @enumFromInt(0)));
    try import_artifact.checked_types.payloads.append(gpa, .{ .flex = .{ .constraints = .{} } });
    try import_artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{ .start = 0, .len = 1 },
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });

    const import_template = procedureTemplateRef(import_artifact.key, 0);
    var import_templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(import_template, @enumFromInt(1), @enumFromInt(0), .hosted),
    };
    import_artifact.checked_procedure_templates = .{ .templates = &import_templates };

    const hosted_order_key = "Import.dynamic_hosted";
    var hosted_procs = [_]checked.HostedProc{
        .{
            .module_idx = 0,
            .def_idx = @enumFromInt(0),
            .expr_idx = @enumFromInt(0),
            .external_symbol_name = @enumFromInt(0),
            .deterministic_index = 0,
            .order_key_start = 0,
            .order_key_len = hosted_order_key.len,
            .proc = procedureValueRef(import_template),
            .template = import_template,
        },
    };
    import_artifact.hosted_procs = .{
        .procs = &hosted_procs,
        .order_key_bytes = hosted_order_key,
    };
    var hosted_representations = [_]checked.HostedRepresentationCapability{
        .{
            .id = @enumFromInt(0),
            .external_symbol_name = @enumFromInt(0),
            .proc = procedureValueRef(import_template),
            .template = import_template,
            .host_checked_fn_root = @enumFromInt(1),
        },
    };
    import_artifact.interface_capabilities.hosted_representations = &hosted_representations;

    const imported_binding = checked.ImportedProcedureBindingRef{
        .artifact = import_artifact.key,
        .def = @enumFromInt(0),
        .pattern = @enumFromInt(0),
    };
    var exported_bindings = [_]checked.ImportedProcedureBindingView{
        .{
            .binding = imported_binding,
            .source_scheme = typeSchemeKey(2),
            .body = .{ .direct_template = .{
                .proc_value = procedureValueRef(import_template),
                .template = .{ .checked = import_template },
            } },
            .template_closure = .{},
        },
    };
    import_artifact.exported_procedure_bindings = .{ .bindings = &exported_bindings };

    try root_artifact.checked_types.type_id_pool.append(gpa, @as(checked.CheckedTypeId, @enumFromInt(0)));
    try root_artifact.checked_types.type_id_pool.append(gpa, @as(checked.CheckedTypeId, @enumFromInt(0)));
    try root_artifact.checked_types.payloads.append(gpa, .{ .flex = .{ .constraints = .{} } });
    try root_artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{ .start = 0, .len = 1 },
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });
    try root_artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{ .start = 1, .len = 1 },
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });

    try root_artifact.checked_bodies.pattern_id_pool.append(gpa, @as(checked.CheckedPatternId, @enumFromInt(0)));
    try root_artifact.checked_bodies.expr_id_pool.append(gpa, @as(checked.CheckedExprId, @enumFromInt(3)));
    try root_artifact.checked_bodies.stored_patterns.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(0),
        .source_region = .zero(),
        .data = .{ .assign = @enumFromInt(0) },
    });
    try root_artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(1),
        .source_region = .zero(),
        .data = .{ .lambda = .{
            .args = .{ .start = 0, .len = 1 },
            .body = @enumFromInt(1),
        } },
    });
    try root_artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = .zero(),
        .data = .{ .call = .{
            .func = @enumFromInt(2),
            .args = .{ .start = 0, .len = 1 },
            .called_via = .apply,
            .source_fn_ty_payload = @enumFromInt(2),
            .direct_target = @enumFromInt(0),
        } },
    });
    try root_artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(2),
        .source_region = .zero(),
        .data = .{ .lookup_external = @enumFromInt(0) },
    });
    try root_artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(3),
        .ty = @enumFromInt(0),
        .source_region = .zero(),
        .data = .{ .lookup_local = .{ .pattern = @enumFromInt(0), .resolved = null } },
    });

    const root_template = procedureTemplateRef(root_artifact.key, 0);
    try root_artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(0),
        .owner_template = root_template,
    });
    var root_templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(root_template, @enumFromInt(1), @enumFromInt(0), .roc),
    };
    root_artifact.checked_procedure_templates = .{ .templates = &root_templates };

    const imported_use = checked.ProcedureUseTemplate{
        .binding = .{ .imported = imported_binding },
        .source_fn_ty_template = typeKey(2),
        .source_fn_ty_payload = @enumFromInt(2),
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
        @as(checked.ResolvedValueRefId, @enumFromInt(0)),
        null,
    };
    root_artifact.resolved_value_refs = .{
        .records = &resolved_records,
        .by_checked_expr = &refs_by_expr,
    };

    const imports = [_]checked.ImportedModuleView{
        .{
            .key = import_artifact.key,
            .module_env = undefined,
            .canonical_names = &import_artifact.canonical_names,
            .module_identity = undefined,
            .exports = .{},
            .checked_types = import_artifact.checked_types.view(),
            .checked_bodies = import_artifact.checked_bodies.view(),
            .exhaustiveness_sites = undefined,
            .checked_const_bodies = undefined,
            .checked_procedure_templates = &import_artifact.checked_procedure_templates,
            .compile_time_roots = undefined,
            .entry_wrappers = undefined,
            .intrinsic_wrappers = undefined,
            .resolved_value_refs = &import_artifact.resolved_value_refs,
            .nested_proc_sites = undefined,
            .static_dispatch_plans = undefined,
            .hosted_procs = &import_artifact.hosted_procs,
            .exported_procedure_templates = .{},
            .exported_procedure_bindings = import_artifact.exported_procedure_bindings.view(),
            .exported_const_templates = .{},
            .provided_exports = undefined,
            .top_level_procedure_bindings = &import_artifact.top_level_procedure_bindings,
            .platform_required_declarations = undefined,
            .platform_required_bindings = undefined,
            .callable_eval_templates = .{},
            .hoisted_constants = undefined,
            .const_templates = undefined,
            .method_registry = undefined,
            .interface_capabilities = &import_artifact.interface_capabilities,
            .const_store = undefined,
        },
    };

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(1),
        .abi = .roc,
        .exposure = .private,
        .procedure_template = root_template,
    };
    var plan = try analyzeProgram(gpa, .{
        .root_module = .{ .module = &root_artifact, .roots = undefined },
        .imports = &imports,
        .roots = &.{root},
    }, .{});
    defer plan.deinit();

    try std.testing.expect(plan.descriptors.items.len != 0);
    const call_ref = ExprRef{ .module = root_artifact.key, .expr = @enumFromInt(1) };
    const direct = plan.directCallPlanForCall(call_ref) orelse return error.TestUnexpectedResult;
    const callee_worker = plan.workers.items[@intFromEnum(direct.worker)];
    try std.testing.expectEqual(WorkerSource{ .procedure_use = imported_use }, callee_worker.source);
    try std.testing.expectEqual(@as(usize, 0), plan.hiddenDescriptorParamSlice(callee_worker.hidden_descs).len);
    try std.testing.expectEqual(@as(usize, 0), plan.directCallHiddenDescriptorArgSlice(direct.hidden_desc_args).len);
}

test "boxy planner records relation-owned source type for platform-required direct calls" {
    const gpa = std.testing.allocator;

    var platform_artifact = minimalCheckedArtifact(gpa);
    defer platform_artifact.canonical_names.deinit();
    defer platform_artifact.checked_types.deinit(gpa);
    defer platform_artifact.checked_bodies.deinit(gpa);

    var app_artifact = minimalCheckedArtifact(gpa);
    app_artifact.key = moduleKey(3);
    defer app_artifact.canonical_names.deinit();
    defer app_artifact.checked_types.deinit(gpa);
    defer app_artifact.checked_bodies.deinit(gpa);

    try app_artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try app_artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });
    const app_template = procedureTemplateRef(app_artifact.key, 0);
    try app_artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(1),
        .source_region = .zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try app_artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = .zero(),
        .data = .{ .num = .{ .value = intValue(1), .kind = .u64 } },
    });
    try app_artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(0),
        .owner_template = app_template,
    });
    var app_templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(app_template, @enumFromInt(1), @enumFromInt(0), .roc),
    };
    app_artifact.checked_procedure_templates = .{ .templates = &app_templates };
    var app_bindings = [_]checked.TopLevelProcedureBinding{
        .{
            .source_scheme = typeSchemeKey(4),
            .body = .{ .direct_template = .{
                .proc_value = procedureValueRef(app_template),
                .template = .{ .checked = app_template },
            } },
        },
    };
    app_artifact.top_level_procedure_bindings = .{ .bindings = &app_bindings };

    try platform_artifact.checked_types.payloads.append(gpa, .{
        .nominal = builtinNominal(.u64, @enumFromInt(0), .{}),
    });
    try platform_artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });
    try platform_artifact.checked_types.payloads.append(gpa, .{
        .function = .{
            .kind = .pure,
            .args = .{},
            .ret = @enumFromInt(0),
            .needs_instantiation = false,
        },
    });

    const platform_template = procedureTemplateRef(platform_artifact.key, 0);
    try platform_artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(0),
        .ty = @enumFromInt(1),
        .source_region = .zero(),
        .data = .{ .lambda = .{ .args = .{}, .body = @enumFromInt(1) } },
    });
    try platform_artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(1),
        .ty = @enumFromInt(0),
        .source_region = .zero(),
        .data = .{ .call = .{
            .func = @enumFromInt(2),
            .args = .{},
            .called_via = .apply,
            .source_fn_ty_payload = @enumFromInt(1),
            .direct_target = @enumFromInt(0),
        } },
    });
    try platform_artifact.checked_bodies.stored_exprs.append(gpa, .{
        .id = @enumFromInt(2),
        .ty = @enumFromInt(1),
        .source_region = .zero(),
        .data = .{ .lookup_required = @as(?checked.ResolvedValueRefId, @enumFromInt(0)) },
    });
    try platform_artifact.checked_bodies.bodies.append(gpa, .{
        .id = @enumFromInt(0),
        .root_expr = @enumFromInt(0),
        .owner_template = platform_template,
    });
    var platform_templates = [_]checked.CheckedProcedureTemplate{
        checkedTemplate(platform_template, @enumFromInt(1), @enumFromInt(0), .roc),
    };
    platform_artifact.checked_procedure_templates = .{ .templates = &platform_templates };

    const required = checked.RequiredAppProcedureRef{
        .artifact = app_artifact.key,
        .app_value = .{
            .artifact = app_artifact.key,
            .pattern = @enumFromInt(0),
        },
        .procedure_binding = @enumFromInt(0),
    };
    const required_use = checked.ProcedureUseTemplate{
        .binding = .{ .platform_required = required },
        .source_fn_ty_template = typeKey(5),
        .source_fn_ty_payload = @enumFromInt(2),
    };
    var resolved_records = [_]checked.ResolvedValueRefRecord{
        .{
            .expr = @enumFromInt(2),
            .ref = .{ .platform_required_proc = .{
                .binding = @enumFromInt(0),
                .procedure = required_use,
            } },
            .checked_ty = @enumFromInt(1),
            .scope_depth = 0,
        },
    };
    var refs_by_expr = [_]?checked.ResolvedValueRefId{
        null,
        null,
        @as(checked.ResolvedValueRefId, @enumFromInt(0)),
    };
    platform_artifact.resolved_value_refs = .{
        .records = &resolved_records,
        .by_checked_expr = &refs_by_expr,
    };

    const imports = [_]checked.ImportedModuleView{
        .{
            .key = app_artifact.key,
            .module_env = undefined,
            .canonical_names = &app_artifact.canonical_names,
            .module_identity = undefined,
            .exports = .{},
            .checked_types = app_artifact.checked_types.view(),
            .checked_bodies = app_artifact.checked_bodies.view(),
            .exhaustiveness_sites = undefined,
            .checked_const_bodies = undefined,
            .checked_procedure_templates = &app_artifact.checked_procedure_templates,
            .compile_time_roots = undefined,
            .entry_wrappers = undefined,
            .intrinsic_wrappers = undefined,
            .resolved_value_refs = &app_artifact.resolved_value_refs,
            .nested_proc_sites = undefined,
            .static_dispatch_plans = undefined,
            .hosted_procs = &app_artifact.hosted_procs,
            .exported_procedure_templates = .{},
            .exported_procedure_bindings = app_artifact.exported_procedure_bindings.view(),
            .exported_const_templates = .{},
            .provided_exports = undefined,
            .top_level_procedure_bindings = &app_artifact.top_level_procedure_bindings,
            .platform_required_declarations = undefined,
            .platform_required_bindings = undefined,
            .callable_eval_templates = .{},
            .hoisted_constants = undefined,
            .const_templates = undefined,
            .method_registry = undefined,
            .interface_capabilities = &app_artifact.interface_capabilities,
            .const_store = undefined,
        },
    };

    const root = checked.RootRequest{
        .order = 0,
        .module_idx = 0,
        .kind = .runtime_entrypoint,
        .source = .{ .def = @enumFromInt(0) },
        .checked_type = @enumFromInt(1),
        .abi = .roc,
        .exposure = .private,
        .procedure_template = platform_template,
    };
    var plan = try analyzeProgram(gpa, .{
        .root_module = .{ .module = &platform_artifact, .roots = undefined },
        .imports = &imports,
        .roots = &.{root},
    }, .{});
    defer plan.deinit();

    const direct = plan.directCallPlanForCall(.{
        .module = platform_artifact.key,
        .expr = @enumFromInt(1),
    }) orelse return error.TestUnexpectedResult;
    try expectTypeRef(platform_artifact.key, @enumFromInt(2), direct.source_fn_type);
    try std.testing.expect(plan.repForSourceType(.{ .module = platform_artifact.key, .ty = @enumFromInt(2) }) != null);
}

test "boxy planner records explicit source type representation bindings" {
    const gpa = std.testing.allocator;

    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.u64, @enumFromInt(0), .{}) },
        .{ .nominal = builtinNominal(.u8, @enumFromInt(1), .{}) },
        .{ .function = .{
            .kind = .pure,
            .args = .{ .start = 0, .len = 1 },
            .ret = @enumFromInt(1),
            .needs_instantiation = false,
        } },
    };
    const type_pool = [_]checked.CheckedTypeId{@enumFromInt(0)};
    const view = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .type_id_pool = &type_pool,
    };

    var plan = try analyzeCheckedTypes(gpa, view, &.{@as(checked.CheckedTypeId, @enumFromInt(2))}, .{});
    defer plan.deinit();

    try std.testing.expectEqual(@as(usize, 3), plan.type_reps.items.len);
    try std.testing.expectEqual(plan.root_reps.items[0], plan.repForSourceType(rootTypeRef(@enumFromInt(2))).?);
    try std.testing.expect(plan.repForSourceType(rootTypeRef(@enumFromInt(0))) != null);
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
            .needs_instantiation = false,
        } },
        .{ .flex = .{ .constraints = .{ .start = 0, .len = 1 } } },
        .{ .nominal = builtinNominal(.u64, @enumFromInt(2), .{}) },
    };
    const constraints = [_]checked.CheckedStaticDispatchConstraint{
        .{
            .fn_name = @enumFromInt(9),
            .fn_ty = @enumFromInt(0),
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

test "boxy planner propagates dynamic descriptor requirements through records" {
    const gpa = std.testing.allocator;

    const fields = [_]checked.CheckedRecordField{
        .{ .name = @enumFromInt(1), .ty = @enumFromInt(0) },
        .{ .name = @enumFromInt(2), .ty = @enumFromInt(1) },
    };
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.u64, @enumFromInt(0), .{}) },
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

test "boxy planner represents open record rows dynamically" {
    const gpa = std.testing.allocator;

    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .record = .{ .fields = .{}, .ext = @enumFromInt(0) } },
    };
    const view = checked.CheckedTypeStoreView{ .stored_payloads = &payloads };

    var plan = try analyzeCheckedTypes(gpa, view, &.{@as(checked.CheckedTypeId, @enumFromInt(0))}, .{});
    defer plan.deinit();

    const rep = plan.representations.items[@intFromEnum(plan.root_reps.items[0])];
    try std.testing.expectEqual(RepresentationKind{ .dynamic = .flex }, rep.kind);
    try std.testing.expect(rep.contains_dynamic);
    try std.testing.expect(rep.descriptor != null);
}

test "boxy planner represents open tag-union rows dynamically" {
    const gpa = std.testing.allocator;

    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .tag_union = .{ .tags = .{}, .ext = @enumFromInt(0) } },
    };
    const view = checked.CheckedTypeStoreView{ .stored_payloads = &payloads };

    var plan = try analyzeCheckedTypes(gpa, view, &.{@as(checked.CheckedTypeId, @enumFromInt(0))}, .{});
    defer plan.deinit();

    const rep = plan.representations.items[@intFromEnum(plan.root_reps.items[0])];
    try std.testing.expectEqual(RepresentationKind{ .dynamic = .flex }, rep.kind);
    try std.testing.expect(rep.contains_dynamic);
    try std.testing.expect(rep.descriptor != null);
}

test "boxy planner preserves known variants on open tag-union rows" {
    const gpa = std.testing.allocator;

    const tag_exit: TagLabelId = @enumFromInt(1);
    const type_pool = [_]checked.CheckedTypeId{@enumFromInt(0)};
    const tags = [_]checked.CheckedTag{
        .{ .name = tag_exit, .args_start = 0, .args_len = 1 },
    };
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.i64, @enumFromInt(0), .{}) },
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

    const type_pool = [_]checked.CheckedTypeId{@enumFromInt(0)};
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
    const type_pool = [_]checked.CheckedTypeId{@enumFromInt(0)};
    const tags = [_]checked.CheckedTag{
        .{ .name = tag_a, .args_start = 0, .args_len = 0 },
        .{ .name = tag_b, .args_start = 0, .args_len = 1 },
    };
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.u64, @enumFromInt(0), .{}) },
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

test "boxy planner stores tag variants in canonical layout order" {
    const gpa = std.testing.allocator;

    var canonical_names = canonical.CanonicalNameStore.init(gpa);
    defer canonical_names.deinit();

    const ok_tag = try canonical_names.internTagLabel("Ok");
    const err_tag = try canonical_names.internTagLabel("Err");
    const type_pool = [_]checked.CheckedTypeId{
        @enumFromInt(0),
        @enumFromInt(1),
    };
    const tags = [_]checked.CheckedTag{
        .{ .name = ok_tag, .args_start = 0, .args_len = 1 },
        .{ .name = err_tag, .args_start = 1, .args_len = 1 },
    };
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.u64, @enumFromInt(0), .{}) },
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
    try expectTypeRef(.{}, @enumFromInt(0), ok_payloads[0].source_type);
}

test "boxy planner records nominal declared field order from checked payloads" {
    const gpa = std.testing.allocator;

    const field_a: RecordFieldLabelId = @enumFromInt(1);
    const field_b: RecordFieldLabelId = @enumFromInt(2);
    const type_pool = [_]checked.CheckedTypeId{@enumFromInt(0)};
    const record_fields = [_]checked.CheckedRecordField{
        .{ .name = field_a, .ty = @enumFromInt(0) },
        .{ .name = field_b, .ty = @enumFromInt(1) },
    };
    const declared_fields = [_]checked.CheckedDeclaredField{
        .{ .named = field_a },
        .{ .padding = 0 },
        .{ .named = field_b },
    };
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.u8, @enumFromInt(0), .{}) },
        .{ .nominal = builtinNominal(.u16, @enumFromInt(1), .{}) },
        .{ .empty_record = {} },
        .{ .record = .{ .fields = .{ .start = 0, .len = 2 }, .ext = @enumFromInt(2) } },
        .{ .nominal = .{
            .name = @enumFromInt(3),
            .origin_module = @enumFromInt(4),
            .is_opaque = false,
            .backing = @enumFromInt(3),
            .representation = .{ .local_declaration = @enumFromInt(0) },
            .padding_field_types = .{ .start = 0, .len = 1 },
            .declared_fields = .{ .start = 0, .len = 3 },
        } },
    };
    const view = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
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

    const field_a: RecordFieldLabelId = @enumFromInt(1);
    const field_b: RecordFieldLabelId = @enumFromInt(2);
    const nominal_key = canonical.NominalTypeKey{
        .module_name = @enumFromInt(4),
        .type_name = @enumFromInt(3),
        .source_decl = 9,
    };
    const type_pool = [_]checked.CheckedTypeId{@enumFromInt(0)};
    const record_fields = [_]checked.CheckedRecordField{
        .{ .name = field_a, .ty = @enumFromInt(0) },
        .{ .name = field_b, .ty = @enumFromInt(1) },
    };
    const declared_fields = [_]checked.CheckedDeclaredField{
        .{ .named = field_a },
        .{ .padding = 0 },
        .{ .named = field_b },
    };
    const declarations = [_]checked.CheckedNominalDeclaration{
        .{
            .id = @enumFromInt(0),
            .nominal = nominal_key,
            .declaration_root = @enumFromInt(4),
            .backing = @enumFromInt(3),
            .pf_start = 0,
            .pf_len = 1,
            .df_start = 0,
            .df_len = declared_fields.len,
        },
    };
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.u8, @enumFromInt(0), .{}) },
        .{ .nominal = builtinNominal(.u16, @enumFromInt(1), .{}) },
        .{ .empty_record = {} },
        .{ .record = .{ .fields = .{ .start = 0, .len = 2 }, .ext = @enumFromInt(2) } },
        .{ .nominal = .{
            .name = nominal_key.type_name,
            .origin_module = nominal_key.module_name,
            .source_decl = nominal_key.source_decl,
            .is_opaque = false,
            .backing = @enumFromInt(3),
            .representation = .{ .local_box_payload_capability = .{ .capability = @enumFromInt(0) } },
        } },
    };
    const capability_padding = [_]checked.CheckedTypeId{@enumFromInt(0)};
    const capabilities = [_]checked.BoxPayloadCapabilityEntry{
        .{
            .id = @enumFromInt(0),
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
            .key = moduleKey(1),
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
    try expectTypeRef(moduleKey(1), @enumFromInt(0), children[1].source_type);

    const fields = plan.declaredFieldSlice(nominal.declared_fields);
    try std.testing.expectEqual(@as(usize, 3), fields.len);
    try std.testing.expectEqual(@as(u16, 0), fields[0].index);
    try std.testing.expectEqual(@as(u16, 2), fields[1].index);
    try std.testing.expect(fields[1].is_padding);
    try expectTypeRef(moduleKey(1), @enumFromInt(0), fields[1].source_type);
    try std.testing.expectEqual(@as(u16, 1), fields[2].index);
}

test "boxy planner records imported box payload capability source modules" {
    const gpa = std.testing.allocator;

    var root_names = canonical.CanonicalNameStore.init(gpa);
    defer root_names.deinit();
    var source_names = canonical.CanonicalNameStore.init(gpa);
    defer source_names.deinit();

    _ = try root_names.internRecordFieldLabel("different-root-id");
    const root_a = try root_names.internRecordFieldLabel("a");
    const root_b = try root_names.internRecordFieldLabel("b");
    const source_a = try source_names.internRecordFieldLabel("a");
    _ = try source_names.internRecordFieldLabel("different-source-id");
    const source_b = try source_names.internRecordFieldLabel("b");

    const root_key = moduleKey(1);
    const source_key = moduleKey(2);
    const nominal_key = canonical.NominalTypeKey{
        .module_name = @enumFromInt(4),
        .type_name = @enumFromInt(3),
        .source_decl = 9,
    };

    const root_record_fields = [_]checked.CheckedRecordField{
        .{ .name = root_a, .ty = @enumFromInt(0) },
        .{ .name = root_b, .ty = @enumFromInt(1) },
    };
    const root_payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.u8, @enumFromInt(0), .{}) },
        .{ .nominal = builtinNominal(.u16, @enumFromInt(1), .{}) },
        .{ .empty_record = {} },
        .{ .record = .{ .fields = .{ .start = 0, .len = 2 }, .ext = @enumFromInt(2) } },
        .{ .nominal = .{
            .name = nominal_key.type_name,
            .origin_module = nominal_key.module_name,
            .source_decl = nominal_key.source_decl,
            .is_opaque = false,
            .backing = @enumFromInt(3),
            .representation = .{ .imported_box_payload_capability = .{
                .artifact = source_key,
                .capability = @enumFromInt(0),
            } },
        } },
    };
    const root_roots = [_]checked.CheckedTypeRoot{
        .{ .id = @enumFromInt(0), .key = typeKey(15) },
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
            .id = @enumFromInt(0),
            .nominal = nominal_key,
            .declaration_root = @enumFromInt(4),
            .backing = @enumFromInt(3),
            .pf_start = 0,
            .pf_len = 1,
            .df_start = 0,
            .df_len = source_declared_fields.len,
        },
    };
    const source_payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.u8, @enumFromInt(0), .{}) },
        .{ .nominal = builtinNominal(.u16, @enumFromInt(1), .{}) },
        .{ .empty_record = {} },
        .{ .record = .{ .fields = .{ .start = 0, .len = 2 }, .ext = @enumFromInt(2) } },
        .{ .nominal = .{
            .name = nominal_key.type_name,
            .origin_module = nominal_key.module_name,
            .source_decl = nominal_key.source_decl,
            .is_opaque = false,
            .backing = @enumFromInt(3),
            .representation = .{ .local_box_payload_capability = .{ .capability = @enumFromInt(0) } },
        } },
        .{ .nominal = builtinNominal(.u8, @enumFromInt(5), .{}) },
    };
    const source_roots = [_]checked.CheckedTypeRoot{
        .{ .id = @enumFromInt(0), .key = typeKey(10) },
        .{ .id = @enumFromInt(1), .key = typeKey(11) },
        .{ .id = @enumFromInt(2), .key = typeKey(12) },
        .{ .id = @enumFromInt(3), .key = typeKey(13) },
        .{ .id = @enumFromInt(4), .key = typeKey(14) },
        .{ .id = @enumFromInt(5), .key = typeKey(15) },
    };
    const source_capability_padding = [_]checked.CheckedTypeId{@enumFromInt(5)};
    const source_capabilities = [_]checked.BoxPayloadCapabilityEntry{
        .{
            .id = @enumFromInt(0),
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

fn builtinNominal(
    builtin: checked.CheckedBuiltinNominal,
    backing: checked.CheckedTypeId,
    args: checked.CheckedTypeRange,
) checked.StoredNominal {
    return .{
        .name = @enumFromInt(0),
        .origin_module = @enumFromInt(0),
        .builtin = builtin,
        .is_opaque = false,
        .backing = backing,
        .representation = .{ .builtin = builtin },
        .args = args,
    };
}

fn moduleKey(byte: u8) checked.ModuleId {
    var key = checked.ModuleId{};
    key.bytes[0] = byte;
    return key;
}

fn rootTypeRef(ty: checked.CheckedTypeId) TypeRef {
    return .{ .ty = ty };
}

fn expectTypeRef(module: checked.ModuleId, ty: checked.CheckedTypeId, actual: TypeRef) !void {
    try std.testing.expect(moduleKeyEqual(module, actual.module));
    try std.testing.expectEqual(ty, actual.ty);
}

fn typeKey(byte: u8) canonical.CanonicalTypeKey {
    var key = canonical.CanonicalTypeKey{};
    key.bytes[0] = byte;
    return key;
}

fn typeSchemeKey(byte: u8) canonical.CanonicalTypeSchemeKey {
    var key = canonical.CanonicalTypeSchemeKey{};
    key.bytes[0] = byte;
    return key;
}

fn procedureTemplateRef(key: checked.CheckedModuleArtifactKey, raw_template_id: u32) canonical.ProcedureTemplateRef {
    return .{
        .artifact = .{ .bytes = key.bytes },
        .proc_base = @enumFromInt(raw_template_id),
        .template = @enumFromInt(raw_template_id),
    };
}

fn procedureValueRef(template: canonical.ProcedureTemplateRef) canonical.ProcedureValueRef {
    return .{
        .artifact = template.artifact,
        .proc_base = template.proc_base,
    };
}

fn checkedTemplate(
    template_ref: canonical.ProcedureTemplateRef,
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
        .resolved_value_refs = .{},
        .top_level_value_uses = .{},
        .nested_proc_sites = .{},
        .target = target,
    };
}

fn intValue(value: i128) can.CIR.IntValue {
    return .{
        .bytes = @bitCast(value),
        .kind = .i128,
    };
}

fn minimalCheckedArtifact(allocator: Allocator) checked.CheckedModuleArtifact {
    return .{
        .key = moduleKey(1),
        .canonical_names = canonical.CanonicalNameStore.init(allocator),
        .module_identity = undefined,
        .checking_context_identity = undefined,
        .module_env = undefined,
        .exports = undefined,
        .provides_requires = undefined,
        .method_registry = undefined,
        .static_dispatch_plans = undefined,
        .resolved_value_refs = undefined,
        .checked_procedure_templates = undefined,
        .top_level_procedure_bindings = undefined,
        .root_requests = undefined,
        .hosted_procs = .{},
        .platform_required_declarations = undefined,
        .platform_required_bindings = undefined,
        .interface_capabilities = .{},
        .compile_time_roots = undefined,
        .top_level_values = undefined,
        .hoisted_constants = undefined,
        .const_templates = undefined,
        .const_store = undefined,
    };
}

fn dummyProcedureTemplate() canonical.ProcedureTemplateRef {
    return .{
        .proc_base = @enumFromInt(0),
        .template = @enumFromInt(0),
    };
}
