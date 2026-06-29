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

pub const TypeRepresentation = struct {
    source_type: checked.CheckedTypeId,
    kind: RepresentationKind,
    children: Span = .{},
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

pub const ProgramPlan = struct {
    allocator: Allocator,
    root_reps: std.ArrayList(TypeRepId),
    representations: std.ArrayList(TypeRepresentation),
    children: std.ArrayList(RepChild),
    descriptors: std.ArrayList(DescriptorRequirement),
    dictionaries: std.ArrayList(DictionaryRequirement),

    pub fn init(allocator: Allocator) ProgramPlan {
        return .{
            .allocator = allocator,
            .root_reps = .empty,
            .representations = .empty,
            .children = .empty,
            .descriptors = .empty,
            .dictionaries = .empty,
        };
    }

    pub fn deinit(self: *ProgramPlan) void {
        self.dictionaries.deinit(self.allocator);
        self.descriptors.deinit(self.allocator);
        self.children.deinit(self.allocator);
        self.representations.deinit(self.allocator);
        self.root_reps.deinit(self.allocator);
        self.* = ProgramPlan.init(self.allocator);
    }

    pub fn childSlice(self: *const ProgramPlan, span: Span) []const RepChild {
        return self.children.items[span.start .. span.start + span.len];
    }

    pub fn dictionarySlice(self: *const ProgramPlan, span: Span) []const DictionaryRequirement {
        return self.dictionaries.items[span.start .. span.start + span.len];
    }
};

pub const AnalyzeOptions = struct {};

pub fn analyzeCheckedTypes(
    allocator: Allocator,
    checked_types: checked.CheckedTypeStoreView,
    roots: []const checked.CheckedTypeId,
    _: AnalyzeOptions,
) Allocator.Error!ProgramPlan {
    var builder = Builder.init(allocator, checked_types);
    defer builder.deinit();

    for (roots) |root| {
        try builder.plan.root_reps.append(allocator, try builder.analyzeType(root));
    }

    builder.propagateDynamicRequirements();
    try builder.materializeDescriptorRequirements();

    const out = builder.plan;
    builder.plan = ProgramPlan.init(allocator);
    return out;
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
        return .{
            .source_type = ty,
            .kind = .{ .nominal = if (nominal.representation == .opaque_without_backing)
                .opaque_nominal
            else if (nominal.builtin != null)
                .builtin_other
            else
                .transparent },
            .children = self.childSpanFrom(start),
        };
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
