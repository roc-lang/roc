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
const RecordFieldLabelId = @TypeOf(@as(checked.CheckedRecordField, undefined).name);
const TagLabelId = @TypeOf(@as(checked.CheckedTag, undefined).name);
const MethodNameId = @TypeOf(@as(checked.CheckedStaticDispatchConstraint, undefined).fn_name);
const StaticDispatchOrigin = @TypeOf(@as(checked.CheckedStaticDispatchConstraint, undefined).origin);
const NumeralInfo = std.meta.Child(@TypeOf(@as(checked.CheckedStaticDispatchConstraint, undefined).num_literal));

pub const TypeRepId = enum(u32) { _ };
pub const RootPlanId = enum(u32) { _ };
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

pub const DeclaredField = struct {
    index: u16,
    source_type: checked.CheckedTypeId,
    rep: TypeRepId,
    is_padding: bool = false,
};

pub const TypeRepresentation = struct {
    source_type: checked.CheckedTypeId,
    kind: RepresentationKind,
    children: Span = .{},
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

pub const RootPlan = struct {
    id: RootPlanId,
    request: checked.RootRequest,
    wrapper_kind: RootWrapperKind,
    host_type: checked.CheckedTypeId,
    host_rep: TypeRepId,
    worker_rep: TypeRepId,
};

pub const ProgramPlan = struct {
    allocator: Allocator,
    roots: std.ArrayList(RootPlan),
    root_reps: std.ArrayList(TypeRepId),
    representations: std.ArrayList(TypeRepresentation),
    children: std.ArrayList(RepChild),
    declared_fields: std.ArrayList(DeclaredField),
    descriptors: std.ArrayList(DescriptorRequirement),
    dictionaries: std.ArrayList(DictionaryRequirement),

    pub fn init(allocator: Allocator) ProgramPlan {
        return .{
            .allocator = allocator,
            .roots = .empty,
            .root_reps = .empty,
            .representations = .empty,
            .children = .empty,
            .declared_fields = .empty,
            .descriptors = .empty,
            .dictionaries = .empty,
        };
    }

    pub fn deinit(self: *ProgramPlan) void {
        self.dictionaries.deinit(self.allocator);
        self.descriptors.deinit(self.allocator);
        self.declared_fields.deinit(self.allocator);
        self.children.deinit(self.allocator);
        self.representations.deinit(self.allocator);
        self.root_reps.deinit(self.allocator);
        self.roots.deinit(self.allocator);
        self.* = ProgramPlan.init(self.allocator);
    }

    pub fn childSlice(self: *const ProgramPlan, span: Span) []const RepChild {
        return self.children.items[span.start .. span.start + span.len];
    }

    pub fn declaredFieldSlice(self: *const ProgramPlan, span: Span) []const DeclaredField {
        return self.declared_fields.items[span.start .. span.start + span.len];
    }

    pub fn dictionarySlice(self: *const ProgramPlan, span: Span) []const DictionaryRequirement {
        return self.dictionaries.items[span.start .. span.start + span.len];
    }
};

pub const AnalyzeOptions = struct {};

pub const ProgramInput = struct {
    checked_types: checked.CheckedTypeStoreView,
    roots: []const checked.RootRequest = &.{},
    layout_requests: []const checked.CheckedTypeId = &.{},
};

pub fn analyzeProgram(
    allocator: Allocator,
    input: ProgramInput,
    _: AnalyzeOptions,
) Allocator.Error!ProgramPlan {
    var builder = Builder.init(allocator, input.checked_types);
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
    checked_types: checked.CheckedTypeStoreView,
    plan: ProgramPlan,
    by_type: std.AutoHashMap(checked.CheckedTypeId, TypeRepId),

    fn init(allocator: Allocator, checked_types: checked.CheckedTypeStoreView) Builder {
        return .{
            .allocator = allocator,
            .checked_types = checked_types,
            .plan = ProgramPlan.init(allocator),
            .by_type = std.AutoHashMap(checked.CheckedTypeId, TypeRepId).init(allocator),
        };
    }

    fn deinit(self: *Builder) void {
        self.by_type.deinit();
        self.plan.deinit();
    }

    fn analyzeRoot(self: *Builder, root: checked.RootRequest) Allocator.Error!void {
        const rep = try self.analyzeType(root.checked_type);
        const id: RootPlanId = @enumFromInt(@as(u32, @intCast(self.plan.roots.items.len)));
        try self.plan.roots.append(self.allocator, .{
            .id = id,
            .request = root,
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

        const rep = try self.buildRepresentation(ty);
        self.plan.representations.items[@intFromEnum(rep_id)] = rep;
        return rep_id;
    }

    fn buildRepresentation(self: *Builder, ty: checked.CheckedTypeId) Allocator.Error!TypeRepresentation {
        const payload = self.checked_types.payload(ty);
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
        const start = self.childStart();
        try self.appendChild(.alias_backing, alias.backing);
        for (alias.args, 0..) |arg, index| {
            try self.appendChild(.{ .alias_arg = @intCast(index) }, arg);
        }
        return .{
            .source_type = ty,
            .kind = .alias,
            .children = self.childSpanFrom(start),
        };
    }

    fn recordRepresentation(
        self: *Builder,
        ty: checked.CheckedTypeId,
        kind: RepresentationKind,
        fields: []const checked.CheckedRecordField,
        ext: ?checked.CheckedTypeId,
    ) Allocator.Error!TypeRepresentation {
        const start = self.childStart();
        for (fields) |field| {
            try self.appendChild(.{ .record_field = field.name }, field.ty);
        }
        if (ext) |ext_ty| {
            try self.appendChild(.record_ext, ext_ty);
        }
        return .{
            .source_type = ty,
            .kind = kind,
            .children = self.childSpanFrom(start),
        };
    }

    fn tupleRepresentation(
        self: *Builder,
        ty: checked.CheckedTypeId,
        elems: []const checked.CheckedTypeId,
    ) Allocator.Error!TypeRepresentation {
        const start = self.childStart();
        for (elems, 0..) |elem, index| {
            try self.appendChild(.{ .tuple_elem = @intCast(index) }, elem);
        }
        return .{
            .source_type = ty,
            .kind = .tuple,
            .children = self.childSpanFrom(start),
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

        const start = self.childStart();
        if (nominal.representation != .opaque_without_backing) {
            try self.appendChild(.nominal_backing, nominal.backing);
        }
        for (nominal.args, 0..) |arg, index| {
            try self.appendChild(.{ .nominal_arg = @intCast(index) }, arg);
        }
        for (nominal.padding_field_types, 0..) |padding, index| {
            try self.appendChild(.{ .nominal_padding_field = @intCast(index) }, padding);
        }
        const declared_fields = try self.appendNominalDeclaredFields(nominal);
        return .{
            .source_type = ty,
            .kind = .{ .nominal = if (nominal.representation == .opaque_without_backing)
                .opaque_nominal
            else if (nominal.builtin != null)
                .builtin_other
            else
                .transparent },
            .children = self.childSpanFrom(start),
            .declared_fields = declared_fields,
        };
    }

    fn appendNominalDeclaredFields(
        self: *Builder,
        nominal: checked.CheckedNominalType,
    ) Allocator.Error!Span {
        if (nominal.declared_fields.len == 0) return Span.empty();
        const backing_fields = switch (self.checked_types.payload(nominal.backing)) {
            .record => |record| record.fields,
            else => boxyPlanInvariant("checked nominal declared field order had a non-record backing"),
        };

        const start: u32 = @intCast(self.plan.declared_fields.items.len);
        var padding_ordinal: u16 = 0;
        for (nominal.declared_fields) |declared| {
            switch (declared) {
                .named => |name| {
                    const field = self.nominalBackingField(backing_fields, name) orelse
                        boxyPlanInvariant("checked nominal declared named field was missing from backing row");
                    try self.plan.declared_fields.append(self.allocator, .{
                        .index = field.index,
                        .source_type = field.ty,
                        .rep = try self.analyzeType(field.ty),
                    });
                },
                .padding => |index| {
                    const raw_index: usize = @intCast(index);
                    if (raw_index >= nominal.padding_field_types.len) {
                        boxyPlanInvariant("checked nominal declared padding field index was out of range");
                    }
                    const padding_ty = nominal.padding_field_types[raw_index];
                    try self.plan.declared_fields.append(self.allocator, .{
                        .index = @intCast(backing_fields.len + padding_ordinal),
                        .source_type = padding_ty,
                        .rep = try self.analyzeType(padding_ty),
                        .is_padding = true,
                    });
                    padding_ordinal += 1;
                },
            }
        }
        return .{
            .start = start,
            .len = @intCast(self.plan.declared_fields.items.len - start),
        };
    }

    const NominalBackingField = struct {
        index: u16,
        ty: checked.CheckedTypeId,
    };

    fn nominalBackingField(
        _: *Builder,
        backing_fields: []const checked.CheckedRecordField,
        name: RecordFieldLabelId,
    ) ?NominalBackingField {
        for (backing_fields, 0..) |field, index| {
            if (field.name == name) return .{
                .index = @intCast(index),
                .ty = field.ty,
            };
        }
        return null;
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
        const start = self.childStart();
        try self.appendChild(role, nominal.args[0]);
        return .{
            .source_type = ty,
            .kind = kind,
            .children = self.childSpanFrom(start),
        };
    }

    fn functionRepresentation(
        self: *Builder,
        ty: checked.CheckedTypeId,
        function: checked.CheckedFunctionType,
    ) Allocator.Error!TypeRepresentation {
        const start = self.childStart();
        for (function.args, 0..) |arg, index| {
            try self.appendChild(.{ .function_arg = @intCast(index) }, arg);
        }
        try self.appendChild(.function_ret, function.ret);
        return .{
            .source_type = ty,
            .kind = .{ .erased_callable = checked.finalizedFunctionKind(function.kind) },
            .children = self.childSpanFrom(start),
        };
    }

    fn tagUnionRepresentation(
        self: *Builder,
        ty: checked.CheckedTypeId,
        tag_union: checked.CheckedTagUnionType,
    ) Allocator.Error!TypeRepresentation {
        const start = self.childStart();
        for (tag_union.tags) |tag| {
            for (tag.argsSlice(self.checked_types), 0..) |arg, index| {
                try self.appendChild(.{ .tag_payload = .{ .tag = tag.name, .index = @intCast(index) } }, arg);
            }
        }
        try self.appendChild(.tag_ext, tag_union.ext);
        return .{
            .source_type = ty,
            .kind = .tag_union,
            .children = self.childSpanFrom(start),
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

    fn appendChild(self: *Builder, role: ChildRole, source_type: checked.CheckedTypeId) Allocator.Error!void {
        try self.plan.children.append(self.allocator, .{
            .role = role,
            .source_type = source_type,
            .rep = try self.analyzeType(source_type),
        });
    }

    fn childStart(self: *const Builder) u32 {
        return @intCast(self.plan.children.items.len);
    }

    fn childSpanFrom(self: *const Builder, start: u32) Span {
        return .{
            .start = start,
            .len = @intCast(self.plan.children.items.len - start),
        };
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
        },
    };

    var plan = try analyzeProgram(gpa, .{ .checked_types = view, .roots = &roots }, .{});
    defer plan.deinit();

    try std.testing.expectEqual(@as(usize, 1), plan.roots.items.len);
    try std.testing.expectEqual(RootWrapperKind.host_shaped_wrapper, plan.roots.items[0].wrapper_kind);
    try std.testing.expectEqual(@as(u32, 3), plan.roots.items[0].request.order);
    try std.testing.expectEqual(plan.roots.items[0].host_rep, plan.roots.items[0].worker_rep);
    try std.testing.expectEqual(@as(usize, 1), plan.root_reps.items.len);
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
