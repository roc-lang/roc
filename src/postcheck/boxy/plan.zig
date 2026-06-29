//! Boxy representation planner.
//!
//! The planner records explicit facts consumed by the boxy lowerer: how each
//! checked type is represented internally, which descriptor data dynamic
//! positions require, and which static-dispatch constraints require dictionary
//! arguments. It only consumes checked type data.

const std = @import("std");
const check = @import("check");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const canonical = check.CanonicalNames;
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
};

pub const WorkerPlan = struct {
    id: WorkerPlanId,
    root_request: ?checked.RootRequest = null,
    source: WorkerSource,
    checked_type: TypeRef,
    rep: TypeRepId,
};

pub const DirectCallPlan = struct {
    call: ExprRef,
    worker: WorkerPlanId,
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
    root_reps: std.ArrayList(TypeRepId),
    type_reps: std.ArrayList(TypeRepBinding),
    representations: std.ArrayList(TypeRepresentation),
    children: std.ArrayList(RepChild),
    tag_variants: std.ArrayList(TagVariant),
    declared_fields: std.ArrayList(DeclaredField),
    descriptors: std.ArrayList(DescriptorRequirement),
    dictionaries: std.ArrayList(DictionaryRequirement),

    pub fn init(allocator: Allocator) ProgramPlan {
        return .{
            .allocator = allocator,
            .roots = .empty,
            .workers = .empty,
            .direct_calls = .empty,
            .root_reps = .empty,
            .type_reps = .empty,
            .representations = .empty,
            .children = .empty,
            .tag_variants = .empty,
            .declared_fields = .empty,
            .descriptors = .empty,
            .dictionaries = .empty,
        };
    }

    pub fn deinit(self: *ProgramPlan) void {
        self.dictionaries.deinit(self.allocator);
        self.descriptors.deinit(self.allocator);
        self.declared_fields.deinit(self.allocator);
        self.tag_variants.deinit(self.allocator);
        self.children.deinit(self.allocator);
        self.representations.deinit(self.allocator);
        self.type_reps.deinit(self.allocator);
        self.root_reps.deinit(self.allocator);
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

    pub fn directWorkerForCall(self: *const ProgramPlan, call: ExprRef) ?WorkerPlanId {
        for (self.direct_calls.items) |direct| {
            if (exprRefEql(direct.call, call)) return direct.worker;
        }
        return null;
    }

    pub fn repForSourceType(self: *const ProgramPlan, source_type: TypeRef) ?TypeRepId {
        for (self.type_reps.items) |binding| {
            if (typeRefEql(binding.source_type, source_type)) return binding.rep;
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
    resolved_value_refs: *const checked.ResolvedValueRefTable = &empty_resolved_value_refs,
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
    try builder.materializeDescriptorRequirements();

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
        if (ext) |ext_ty| {
            if (!try self.recordExtensionIsExplicitlyClosed(view, ext_ty)) {
                return try self.dynamicRepresentation(source_type, &.{}, .flex);
            }
        }

        var children = std.ArrayList(RepChild).empty;
        defer children.deinit(self.allocator);
        for (fields) |field| {
            try self.appendPendingChild(&children, view, .{ .record_field = field.name }, field.ty);
        }
        if (ext) |ext_ty| {
            try self.appendPendingChild(&children, view, .record_ext, ext_ty);
        }
        return .{
            .source_type = source_type,
            .kind = kind,
            .children = try self.commitPendingChildren(children.items),
        };
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
        var children = std.ArrayList(RepChild).empty;
        defer children.deinit(self.allocator);
        for (tag_union.tags) |tag| {
            for (tag.argsSlice(view.checked_types), 0..) |arg, index| {
                try self.appendPendingChild(&children, view, .{ .tag_payload = .{ .tag = tag.name, .index = @intCast(index) } }, arg);
            }
        }
        try self.appendPendingChild(&children, view, .tag_ext, tag_union.ext);
        const child_span = try self.commitPendingChildren(children.items);

        const variant_start: u32 = @intCast(self.plan.tag_variants.items.len);
        var payload_start = child_span.start;
        for (tag_union.tags) |tag| {
            try self.plan.tag_variants.append(self.allocator, .{
                .name = tag.name,
                .payloads = .{ .start = payload_start, .len = tag.args_len },
            });
            payload_start += tag.args_len;
        }

        return .{
            .source_type = source_type,
            .kind = .tag_union,
            .children = child_span,
            .tag_variants = .{ .start = variant_start, .len = @intCast(tag_union.tags.len) },
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

    fn analyzeWorkerBodyTypes(self: *Builder, source: WorkerSource) Allocator.Error!void {
        const body = self.rootWorkerBody(source);
        try self.analyzeExprTypes(body.view, body.root_expr);
    }

    const WorkerBody = struct {
        view: ModuleView,
        root_expr: checked.CheckedExprId,
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
                .hosted => boxyPlanInvariant("hosted procedure use reached boxy body type planning before hosted wrapper planning is implemented"),
            },
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
        const root_expr = switch (template.body) {
            .checked_body => |body| view.checked_bodies.body(body).root_expr,
            .intrinsic_wrapper => boxyPlanInvariant("intrinsic wrapper reached boxy body type planning"),
            .entry_wrapper => boxyPlanInvariant("compile-time entry wrapper reached runtime boxy body type planning"),
        };
        return .{
            .view = view,
            .root_expr = root_expr,
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
        return .{
            .view = view,
            .root_expr = expr_id,
        };
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
            .lookup_local,
            .lookup_external,
            .lookup_required,
            .empty_list,
            .empty_record,
            .zero_argument_tag,
            .dispatch_call,
            .method_eq,
            .type_dispatch_call,
            .runtime_error,
            .crash,
            .ellipsis,
            .anno_only,
            .break_,
            => {},
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
                try self.analyzeExprTypes(view, call.func);
                try self.analyzeExprSliceTypes(view, call.args);
                _ = try self.analyzeType(view, call.source_fn_ty_payload);
                try self.analyzeDirectCallTarget(view, expr_id, call);
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
                try self.analyzeExprTypes(view, closure.lambda);
                for (closure.captures) |capture| try self.analyzePatternTypes(view, capture.pattern);
            },
            .lambda => |lambda| {
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
            .tuple_access => |access| try self.analyzeExprTypes(view, access.tuple),
            .expect_err => |expect_err| try self.analyzeExprTypes(view, expect_err.expr),
            .return_ => |ret| try self.analyzeExprTypes(view, ret.expr),
            .for_ => |for_| {
                try self.analyzePatternTypes(view, for_.pattern);
                try self.analyzeExprTypes(view, for_.expr);
                try self.analyzeExprTypes(view, for_.body);
            },
            .hosted_lambda => |hosted| for (hosted.args) |arg| try self.analyzePatternTypes(view, arg),
            .run_low_level => |run| try self.analyzeExprSliceTypes(view, run.args),
        }
    }

    fn analyzeExprSliceTypes(self: *Builder, view: ModuleView, exprs: []const checked.CheckedExprId) Allocator.Error!void {
        for (exprs) |expr| try self.analyzeExprTypes(view, expr);
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
        });
    }

    fn workerSourceForDirectTarget(self: *Builder, view: ModuleView, target: checked.ResolvedValueId) WorkerSource {
        const record = self.resolvedValueRecord(view, target);
        return switch (record.ref) {
            .top_level_proc,
            .promoted_top_level_proc,
            => |procedure| self.workerSourceForProcedureUse(procedure),
            .platform_required_proc => |required| self.workerSourceForProcedureUse(required.procedure),
            .local_proc => boxyPlanInvariant("local procedure direct call reached boxy planning before nested procedure worker planning"),
            .imported_proc => |procedure| self.workerSourceForProcedureUse(procedure),
            .hosted_proc => boxyPlanInvariant("hosted direct call reached boxy planning before hosted wrapper planning"),
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
            .hosted => boxyPlanInvariant("hosted procedure use reached boxy planning before hosted wrapper planning"),
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

fn workerSourceEql(a: WorkerSource, b: WorkerSource) bool {
    return std.meta.eql(a, b);
}

fn moduleViewFromImported(imported: checked.ImportedModuleView) ModuleView {
    return .{
        .key = imported.key,
        .canonical_names = imported.canonical_names,
        .checked_types = imported.checked_types,
        .checked_bodies = imported.checked_bodies,
        .compile_time_roots = imported.compile_time_roots,
        .resolved_value_refs = imported.resolved_value_refs,
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
        .resolved_value_refs = &artifact.resolved_value_refs,
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
    try std.testing.expectEqual(@as(usize, 3), plan.childSlice(record.children).len);
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

fn dummyProcedureTemplate() canonical.ProcedureTemplateRef {
    return .{
        .proc_base = @enumFromInt(0),
        .template = @enumFromInt(0),
    };
}
