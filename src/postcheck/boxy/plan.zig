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

pub const TypeRepId = enum(u32) { _ };
pub const RootPlanId = enum(u32) { _ };
pub const WorkerPlanId = enum(u32) { _ };
pub const DescriptorRequirementId = enum(u32) { _ };
pub const DictionaryRequirementId = enum(u32) { _ };

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
    source_type: checked.CheckedTypeId,
    rep: TypeRepId,
};

pub const TagVariant = struct {
    name: TagLabelId,
    payloads: Span = .{},
};

pub const DeclaredField = struct {
    index: u16,
    source_type: checked.CheckedTypeId,
    rep: TypeRepId,
    is_padding: bool = false,
};

pub const TypeRepBinding = struct {
    source_type: checked.CheckedTypeId,
    rep: TypeRepId,
};

pub const TypeRepresentation = struct {
    source_type: checked.CheckedTypeId,
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
    source_type: checked.CheckedTypeId,
    rep: TypeRepId,
    reason: DescriptorReason,
};

pub const DictionaryRequirement = struct {
    source_type: checked.CheckedTypeId,
    constraint_index: u32,
    fn_name: MethodNameId,
    fn_ty: checked.CheckedTypeId,
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
    procedure_binding: checked.TopLevelProcedureBindingRef,
    procedure_use: checked.ProcedureUseTemplate,
};

pub const WorkerPlan = struct {
    id: WorkerPlanId,
    request: checked.RootRequest,
    source: WorkerSource,
    checked_type: checked.CheckedTypeId,
    rep: TypeRepId,
};

pub const RootPlan = struct {
    id: RootPlanId,
    request: checked.RootRequest,
    worker: WorkerPlanId,
    wrapper_kind: RootWrapperKind,
    host_type: checked.CheckedTypeId,
    host_rep: TypeRepId,
    worker_rep: TypeRepId,
};

pub const ProgramPlan = struct {
    allocator: Allocator,
    roots: std.ArrayList(RootPlan),
    workers: std.ArrayList(WorkerPlan),
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

    pub fn repForSourceType(self: *const ProgramPlan, source_type: checked.CheckedTypeId) ?TypeRepId {
        for (self.type_reps.items) |binding| {
            if (binding.source_type == source_type) return binding.rep;
        }
        return null;
    }
};

pub const AnalyzeOptions = struct {};

pub const ModuleView = struct {
    key: checked.ModuleId = .{},
    canonical_names: ?*const canonical.CanonicalNameStore = null,
    checked_types: checked.CheckedTypeStoreView,
    interface_capabilities: *const checked.ModuleInterfaceCapabilities = &empty_interface_capabilities,
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
        try builder.plan.root_reps.append(allocator, try builder.analyzeType(layout_request));
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
    root_view: ModuleView,
    extra_module_views: []const ModuleView,
    imports: []const checked.ImportedModuleView,
    relation_modules: []const checked.ImportedModuleView,
    plan: ProgramPlan,
    by_type: std.AutoHashMap(checked.CheckedTypeId, TypeRepId),

    fn init(allocator: Allocator, input: ProgramInput) Builder {
        const root_view = if (input.root_view) |root_view|
            root_view
        else if (input.root_module) |root_module|
            moduleViewFromImported(checked.importedView(root_module.module))
        else
            ModuleView{ .checked_types = input.checked_types };

        return .{
            .allocator = allocator,
            .root_view = root_view,
            .extra_module_views = input.extra_module_views,
            .imports = if (input.root_module != null) input.imports else &.{},
            .relation_modules = if (input.root_module) |root_module| root_module.relation_modules else &.{},
            .plan = ProgramPlan.init(allocator),
            .by_type = std.AutoHashMap(checked.CheckedTypeId, TypeRepId).init(allocator),
        };
    }

    fn deinit(self: *Builder) void {
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

    fn typeInRootView(
        self: *Builder,
        source_view: ModuleView,
        source_ty: checked.CheckedTypeId,
    ) Allocator.Error!checked.CheckedTypeId {
        if (moduleKeyEqual(source_view.key, self.root_view.key)) return source_ty;
        return self.root_view.checked_types.rootForKey(source_view.checked_types.rootKey(source_ty)) orelse
            boxyPlanInvariant("checked capability type was not projected into the root checked type store");
    }

    fn analyzeRoot(self: *Builder, root: checked.RootRequest) Allocator.Error!void {
        const rep = try self.analyzeType(root.checked_type);
        const source = workerSourceForRoot(root) orelse
            boxyPlanInvariant("boxy root request had no checked procedure worker source");
        const worker_id: WorkerPlanId = @enumFromInt(@as(u32, @intCast(self.plan.workers.items.len)));
        try self.plan.workers.append(self.allocator, .{
            .id = worker_id,
            .request = root,
            .source = source,
            .checked_type = root.checked_type,
            .rep = rep,
        });

        const id: RootPlanId = @enumFromInt(@as(u32, @intCast(self.plan.roots.items.len)));
        try self.plan.roots.append(self.allocator, .{
            .id = id,
            .request = root,
            .worker = worker_id,
            .wrapper_kind = if (rootRequiresHostWrapper(root)) .host_shaped_wrapper else .private_worker_only,
            .host_type = root.checked_type,
            .host_rep = rep,
            .worker_rep = rep,
        });
        try self.plan.root_reps.append(self.allocator, rep);
    }

    fn analyzeType(self: *Builder, ty: checked.CheckedTypeId) Allocator.Error!TypeRepId {
        const entry = try self.by_type.getOrPut(ty);
        if (entry.found_existing) return entry.value_ptr.*;

        const rep_id: TypeRepId = @enumFromInt(@as(u32, @intCast(self.plan.representations.items.len)));
        entry.value_ptr.* = rep_id;
        try self.plan.representations.append(self.allocator, .{
            .source_type = ty,
            .kind = .in_progress,
        });
        try self.plan.type_reps.append(self.allocator, .{
            .source_type = ty,
            .rep = rep_id,
        });

        const rep = try self.buildRepresentation(ty);
        self.plan.representations.items[@intFromEnum(rep_id)] = rep;
        return rep_id;
    }

    fn buildRepresentation(self: *Builder, ty: checked.CheckedTypeId) Allocator.Error!TypeRepresentation {
        const payload = self.root_view.checked_types.payload(ty);
        return switch (payload) {
            .pending => boxyPlanInvariant("checked type payload was pending during boxy planning"),
            .flex => |flex| try self.dynamicRepresentation(ty, .flex, flex.constraints),
            .rigid => |rigid| try self.dynamicRepresentation(ty, .rigid, rigid.constraints),
            .alias => |alias| try self.aliasRepresentation(ty, alias),
            .record => |record| try self.recordRepresentation(ty, .record, record.fields, record.ext),
            .record_unbound => |fields| try self.recordRepresentation(ty, .record_unbound, fields, null),
            .tuple => |elems| try self.tupleRepresentation(ty, elems),
            .nominal => |nominal| try self.nominalRepresentation(ty, nominal),
            .function => |function| try self.functionRepresentation(ty, function),
            .empty_record => .{ .source_type = ty, .kind = .empty_record },
            .tag_union => |tag_union| try self.tagUnionRepresentation(ty, tag_union),
            .empty_tag_union => .{ .source_type = ty, .kind = .empty_tag_union },
        };
    }

    fn dynamicRepresentation(
        self: *Builder,
        ty: checked.CheckedTypeId,
        kind: DynamicKind,
        constraints: []const checked.CheckedStaticDispatchConstraint,
    ) Allocator.Error!TypeRepresentation {
        const dictionaries = try self.appendDictionaryRequirements(ty, constraints);
        return .{
            .source_type = ty,
            .kind = .{ .dynamic = kind },
            .dictionaries = dictionaries,
            .contains_dynamic = true,
        };
    }

    fn aliasRepresentation(
        self: *Builder,
        ty: checked.CheckedTypeId,
        alias: checked.CheckedAliasType,
    ) Allocator.Error!TypeRepresentation {
        var children = std.ArrayList(RepChild).empty;
        defer children.deinit(self.allocator);
        try self.appendPendingChild(&children, .alias_backing, alias.backing);
        for (alias.args, 0..) |arg, index| {
            try self.appendPendingChild(&children, .{ .alias_arg = @intCast(index) }, arg);
        }
        return .{
            .source_type = ty,
            .kind = .alias,
            .children = try self.commitPendingChildren(children.items),
        };
    }

    fn recordRepresentation(
        self: *Builder,
        ty: checked.CheckedTypeId,
        kind: RepresentationKind,
        fields: []const checked.CheckedRecordField,
        ext: ?checked.CheckedTypeId,
    ) Allocator.Error!TypeRepresentation {
        var children = std.ArrayList(RepChild).empty;
        defer children.deinit(self.allocator);
        for (fields) |field| {
            try self.appendPendingChild(&children, .{ .record_field = field.name }, field.ty);
        }
        if (ext) |ext_ty| {
            try self.appendPendingChild(&children, .record_ext, ext_ty);
        }
        return .{
            .source_type = ty,
            .kind = kind,
            .children = try self.commitPendingChildren(children.items),
        };
    }

    fn tupleRepresentation(
        self: *Builder,
        ty: checked.CheckedTypeId,
        elems: []const checked.CheckedTypeId,
    ) Allocator.Error!TypeRepresentation {
        var children = std.ArrayList(RepChild).empty;
        defer children.deinit(self.allocator);
        for (elems, 0..) |elem, index| {
            try self.appendPendingChild(&children, .{ .tuple_elem = @intCast(index) }, elem);
        }
        return .{
            .source_type = ty,
            .kind = .tuple,
            .children = try self.commitPendingChildren(children.items),
        };
    }

    fn nominalRepresentation(
        self: *Builder,
        ty: checked.CheckedTypeId,
        nominal: checked.CheckedNominalType,
    ) Allocator.Error!TypeRepresentation {
        if (nominal.builtin) |builtin| {
            switch (checked.builtinRuntimeEncoding(builtin)) {
                .primitive => |primitive| return .{
                    .source_type = ty,
                    .kind = .{ .primitive = primitive },
                },
                .bool_tag_union => return .{ .source_type = ty, .kind = .bool_tag_union },
                .list => return try self.builtinUnaryNominalRepresentation(ty, .list, .list_elem, nominal),
                .box => return try self.builtinUnaryNominalRepresentation(ty, .box, .box_payload, nominal),
                .parse_tag_union_spec,
                .fields,
                .field,
                => {},
            }
        }

        var children = std.ArrayList(RepChild).empty;
        defer children.deinit(self.allocator);
        if (nominal.representation != .opaque_without_backing) {
            try self.appendPendingChild(&children, .nominal_backing, try self.nominalBackingType(nominal));
        }
        for (nominal.args, 0..) |arg, index| {
            try self.appendPendingChild(&children, .{ .nominal_arg = @intCast(index) }, arg);
        }
        if (self.nominalPaddingSource(nominal)) |padding_source| {
            for (padding_source.types, 0..) |padding, index| {
                try self.appendPendingChild(
                    &children,
                    .{ .nominal_padding_field = @intCast(index) },
                    try self.typeInRootView(padding_source.view, padding),
                );
            }
        }
        const backing_ty = try self.nominalBackingType(nominal);
        const declared_fields = try self.appendNominalDeclaredFields(nominal, backing_ty);
        return .{
            .source_type = ty,
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

    fn nominalBackingType(
        self: *Builder,
        nominal: checked.CheckedNominalType,
    ) Allocator.Error!checked.CheckedTypeId {
        return switch (nominal.representation) {
            .local_box_payload_capability => |capability| self.root_view.interface_capabilities.boxPayloadCapability(capability.capability).backing_ty,
            .imported_box_payload_capability => |capability| blk: {
                const source_view = self.moduleForId(checked.importedBoxPayloadCapabilityModuleId(capability));
                const source_capability = source_view.interface_capabilities.boxPayloadCapability(capability.capability);
                break :blk try self.typeInRootView(source_view, source_capability.backing_ty);
            },
            .builtin,
            .local_declaration,
            .imported_declaration,
            .opaque_without_backing,
            => nominal.backing,
        };
    }

    fn nominalPaddingSource(self: *Builder, nominal: checked.CheckedNominalType) ?NominalPaddingSource {
        if (nominal.padding_field_types.len != 0) return .{
            .view = self.root_view,
            .types = nominal.padding_field_types,
        };
        const lookup = self.nominalDeclarationFor(nominal) orelse return null;
        if (lookup.padding_types.len == 0) return null;
        return .{
            .view = lookup.padding_view,
            .types = lookup.padding_types,
        };
    }

    fn nominalDeclaredSource(self: *Builder, nominal: checked.CheckedNominalType) ?NominalDeclaredSource {
        if (nominal.declared_fields.len != 0) return .{
            .field_view = self.root_view,
            .fields = nominal.declared_fields,
            .padding_view = self.root_view,
            .padding_types = nominal.padding_field_types,
        };
        const lookup = self.nominalDeclarationFor(nominal) orelse return null;
        const fields = lookup.declaration.declaredFields(lookup.view.checked_types);
        if (fields.len == 0) return null;
        return .{
            .field_view = lookup.view,
            .fields = fields,
            .padding_view = lookup.padding_view,
            .padding_types = lookup.padding_types,
        };
    }

    fn nominalDeclarationFor(self: *Builder, nominal: checked.CheckedNominalType) ?NominalDeclarationLookup {
        return switch (nominal.representation) {
            .local_declaration => |id| blk: {
                const declaration = self.root_view.checked_types.nominalDeclarationById(id);
                break :blk .{
                    .view = self.root_view,
                    .declaration = declaration,
                    .padding_view = self.root_view,
                    .padding_types = declaration.paddingFieldTypes(self.root_view.checked_types),
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
                const capability = self.root_view.interface_capabilities.boxPayloadCapability(capability_ref.capability);
                const declaration = self.root_view.checked_types.nominalDeclaration(capability.nominal) orelse break :blk null;
                break :blk .{
                    .view = self.root_view,
                    .declaration = declaration,
                    .padding_view = self.root_view,
                    .padding_types = capability.paddingFieldTys(self.root_view.interface_capabilities),
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
        nominal: checked.CheckedNominalType,
        backing_ty: checked.CheckedTypeId,
    ) Allocator.Error!Span {
        const source = self.nominalDeclaredSource(nominal) orelse return Span.empty();
        const backing_fields = switch (self.root_view.checked_types.payload(backing_ty)) {
            .record => |record| record.fields,
            else => boxyPlanInvariant("checked nominal declared field order had a non-record backing"),
        };

        var pending = std.ArrayList(DeclaredField).empty;
        defer pending.deinit(self.allocator);
        var padding_ordinal: u16 = 0;
        for (source.fields) |declared| {
            switch (declared) {
                .named => |name| {
                    const field = self.nominalBackingField(source.field_view, backing_fields, name) orelse
                        boxyPlanInvariant("checked nominal declared named field was missing from backing row");
                    try pending.append(self.allocator, .{
                        .index = field.index,
                        .source_type = field.ty,
                        .rep = try self.analyzeType(field.ty),
                    });
                },
                .padding => |index| {
                    const raw_index: usize = @intCast(index);
                    if (raw_index >= source.padding_types.len) {
                        boxyPlanInvariant("checked nominal declared padding field index was out of range");
                    }
                    const padding_ty = try self.typeInRootView(source.padding_view, source.padding_types[raw_index]);
                    try pending.append(self.allocator, .{
                        .index = @intCast(backing_fields.len + padding_ordinal),
                        .source_type = padding_ty,
                        .rep = try self.analyzeType(padding_ty),
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
        backing_fields: []const checked.CheckedRecordField,
        name: RecordFieldLabelId,
    ) ?NominalBackingField {
        for (backing_fields, 0..) |field, index| {
            if (self.recordFieldNameMatches(field_view, name, self.root_view, field.name)) return .{
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
        ty: checked.CheckedTypeId,
        kind: RepresentationKind,
        role: ChildRole,
        nominal: checked.CheckedNominalType,
    ) Allocator.Error!TypeRepresentation {
        if (nominal.args.len != 1) {
            boxyPlanInvariant("builtin unary nominal had an unexpected checked argument count");
        }
        var children = std.ArrayList(RepChild).empty;
        defer children.deinit(self.allocator);
        try self.appendPendingChild(&children, role, nominal.args[0]);
        return .{
            .source_type = ty,
            .kind = kind,
            .children = try self.commitPendingChildren(children.items),
        };
    }

    fn functionRepresentation(
        self: *Builder,
        ty: checked.CheckedTypeId,
        function: checked.CheckedFunctionType,
    ) Allocator.Error!TypeRepresentation {
        var children = std.ArrayList(RepChild).empty;
        defer children.deinit(self.allocator);
        for (function.args, 0..) |arg, index| {
            try self.appendPendingChild(&children, .{ .function_arg = @intCast(index) }, arg);
        }
        try self.appendPendingChild(&children, .function_ret, function.ret);
        return .{
            .source_type = ty,
            .kind = .{ .erased_callable = checked.finalizedFunctionKind(function.kind) },
            .children = try self.commitPendingChildren(children.items),
        };
    }

    fn tagUnionRepresentation(
        self: *Builder,
        ty: checked.CheckedTypeId,
        tag_union: checked.CheckedTagUnionType,
    ) Allocator.Error!TypeRepresentation {
        var children = std.ArrayList(RepChild).empty;
        defer children.deinit(self.allocator);
        for (tag_union.tags) |tag| {
            for (tag.argsSlice(self.root_view.checked_types), 0..) |arg, index| {
                try self.appendPendingChild(&children, .{ .tag_payload = .{ .tag = tag.name, .index = @intCast(index) } }, arg);
            }
        }
        try self.appendPendingChild(&children, .tag_ext, tag_union.ext);
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
            .source_type = ty,
            .kind = .tag_union,
            .children = child_span,
            .tag_variants = .{ .start = variant_start, .len = @intCast(tag_union.tags.len) },
        };
    }

    fn appendDictionaryRequirements(
        self: *Builder,
        ty: checked.CheckedTypeId,
        constraints: []const checked.CheckedStaticDispatchConstraint,
    ) Allocator.Error!Span {
        const start: u32 = @intCast(self.plan.dictionaries.items.len);
        for (constraints, 0..) |constraint, index| {
            try self.plan.dictionaries.append(self.allocator, .{
                .source_type = ty,
                .constraint_index = @intCast(index),
                .fn_name = constraint.fn_name,
                .fn_ty = constraint.fn_ty,
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
        role: ChildRole,
        source_type: checked.CheckedTypeId,
    ) Allocator.Error!void {
        try pending.append(self.allocator, .{
            .role = role,
            .source_type = source_type,
            .rep = try self.analyzeType(source_type),
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
};

fn rootRequiresHostWrapper(root: checked.RootRequest) bool {
    return root.abi != .roc or root.exposure != .private;
}

fn workerSourceForRoot(root: checked.RootRequest) ?WorkerSource {
    if (root.procedure_template) |template| return .{ .procedure_template = template };
    if (root.procedure_binding) |binding| return .{ .procedure_binding = binding };
    if (root.procedure_use) |procedure| return .{ .procedure_use = procedure };
    return null;
}

fn moduleViewFromImported(imported: checked.ImportedModuleView) ModuleView {
    return .{
        .key = imported.key,
        .canonical_names = imported.canonical_names,
        .checked_types = imported.checked_types,
        .interface_capabilities = imported.interface_capabilities,
    };
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
    try std.testing.expectEqual(plan.root_reps.items[0], plan.repForSourceType(@enumFromInt(2)).?);
    try std.testing.expect(plan.repForSourceType(@enumFromInt(0)) != null);
    try std.testing.expect(plan.repForSourceType(@enumFromInt(1)) != null);
    try std.testing.expect(plan.repForSourceType(@enumFromInt(99)) == null);
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
    try std.testing.expectEqual(@as(checked.CheckedTypeId, @enumFromInt(0)), children[1].source_type);

    const fields = plan.declaredFieldSlice(nominal.declared_fields);
    try std.testing.expectEqual(@as(usize, 3), fields.len);
    try std.testing.expectEqual(@as(u16, 0), fields[0].index);
    try std.testing.expectEqual(@as(u16, 2), fields[1].index);
    try std.testing.expect(fields[1].is_padding);
    try std.testing.expectEqual(@as(checked.CheckedTypeId, @enumFromInt(0)), fields[1].source_type);
    try std.testing.expectEqual(@as(u16, 1), fields[2].index);
}

test "boxy planner maps imported box payload capability types into the root view" {
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
    try std.testing.expectEqual(@as(checked.CheckedTypeId, @enumFromInt(3)), children[0].source_type);
    try std.testing.expectEqual(@as(checked.CheckedTypeId, @enumFromInt(0)), children[1].source_type);

    const fields = plan.declaredFieldSlice(nominal.declared_fields);
    try std.testing.expectEqual(@as(usize, 3), fields.len);
    try std.testing.expectEqual(@as(u16, 0), fields[0].index);
    try std.testing.expectEqual(@as(checked.CheckedTypeId, @enumFromInt(0)), fields[0].source_type);
    try std.testing.expectEqual(@as(u16, 2), fields[1].index);
    try std.testing.expect(fields[1].is_padding);
    try std.testing.expectEqual(@as(checked.CheckedTypeId, @enumFromInt(0)), fields[1].source_type);
    try std.testing.expectEqual(@as(u16, 1), fields[2].index);
    try std.testing.expectEqual(@as(checked.CheckedTypeId, @enumFromInt(1)), fields[2].source_type);
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
