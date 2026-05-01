//! Lower checked artifact type payloads into specialization-local mono MIR types.
//!
//! Mono lowering consumes only the immutable checked artifact graph. It does not
//! inspect the checker `types.Store`, raw `Var`s, or module-local identifiers.

const std = @import("std");
const builtin = @import("builtin");
const check = @import("check");

const Type = @import("type.zig");
const ArtifactNames = @import("../artifact_names.zig");

const Allocator = std.mem.Allocator;
const checked_artifact = check.CheckedArtifact;
const canonical = check.CanonicalNames;

const CheckedTypeId = checked_artifact.CheckedTypeId;

pub const Lowerer = struct {
    allocator: Allocator,
    source: checked_artifact.CheckedTypeStoreView,
    dest: *Type.Store,
    name_resolver: ?*ArtifactNames.ArtifactNameResolver = null,
    artifact: ?checked_artifact.CheckedModuleArtifactKey = null,
    lowered: std.AutoHashMap(CheckedTypeId, Type.TypeId),

    pub fn init(
        allocator: Allocator,
        source: checked_artifact.CheckedTypeStoreView,
        dest: *Type.Store,
    ) Lowerer {
        return .{
            .allocator = allocator,
            .source = source,
            .dest = dest,
            .name_resolver = null,
            .artifact = null,
            .lowered = std.AutoHashMap(CheckedTypeId, Type.TypeId).init(allocator),
        };
    }

    pub fn initWithResolver(
        allocator: Allocator,
        source: checked_artifact.CheckedTypeStoreView,
        dest: *Type.Store,
        name_resolver: *ArtifactNames.ArtifactNameResolver,
        artifact: checked_artifact.CheckedModuleArtifactKey,
    ) Lowerer {
        return .{
            .allocator = allocator,
            .source = source,
            .dest = dest,
            .name_resolver = name_resolver,
            .artifact = artifact,
            .lowered = std.AutoHashMap(CheckedTypeId, Type.TypeId).init(allocator),
        };
    }

    pub fn deinit(self: *Lowerer) void {
        self.lowered.deinit();
    }

    pub fn lowerChecked(self: *Lowerer, id: CheckedTypeId) Allocator.Error!Type.TypeId {
        if (self.lowered.get(id)) |existing| return existing;

        const placeholder = try self.dest.addType(.placeholder);
        try self.lowered.put(id, placeholder);
        const lowered = try self.lowerPayload(self.payload(id));
        self.dest.setType(placeholder, lowered);
        self.dest.debugValidateTypeGraph(placeholder);
        return try self.dest.internTypeId(placeholder);
    }

    fn payload(self: *const Lowerer, id: CheckedTypeId) checked_artifact.CheckedTypePayload {
        const raw = @intFromEnum(id);
        if (raw >= self.source.payloads.len) {
            invariantViolation("mono type lowering received a checked type id outside the published artifact payload graph");
        }
        return self.source.payloads[raw];
    }

    fn lowerPayload(self: *Lowerer, payload: checked_artifact.CheckedTypePayload) Allocator.Error!Type.Content {
        return switch (payload) {
            .pending => invariantViolation("mono type lowering received an unpublished checked type payload"),
            .flex => invariantViolation("mono type lowering received an unsolved flex type variable"),
            .rigid => invariantViolation("mono type lowering received an unsolved rigid type variable"),
            .alias => |alias| .{ .link = try self.lowerChecked(alias.backing) },
            .record_unbound => invariantViolation("mono type lowering received an unfinalized open record row"),
            .record => |record| try self.lowerRecord(record),
            .tuple => |elems| .{ .tuple = try self.lowerTypeIds(elems) },
            .nominal => |nominal| try self.lowerNominal(nominal),
            .function => |func| try self.lowerFunc(func),
            .empty_record => .{ .record = .{ .fields = &.{} } },
            .tag_union => |tag_union| try self.lowerTagUnion(tag_union),
            .empty_tag_union => .{ .tag_union = .{ .tags = &.{} } },
        };
    }

    fn lowerFunc(self: *Lowerer, func: checked_artifact.CheckedFunctionType) Allocator.Error!Type.Content {
        if (func.needs_instantiation) {
            invariantViolation("mono type lowering received a function type that still needs instantiation");
        }
        return .{ .func = .{
            .args = try self.lowerTypeIds(func.args),
            .lambdas = &.{},
            .ret = try self.lowerChecked(func.ret),
        } };
    }

    fn lowerRecord(
        self: *Lowerer,
        record: checked_artifact.CheckedRecordType,
    ) Allocator.Error!Type.Content {
        var source_fields = std.ArrayList(checked_artifact.CheckedRecordField).empty;
        defer source_fields.deinit(self.allocator);

        try self.collectRecordFields(record.fields, record.ext, &source_fields);

        const fields = try self.allocator.alloc(Type.Field, source_fields.items.len);
        errdefer self.allocator.free(fields);
        for (source_fields.items, 0..) |field, i| {
            fields[i] = .{
                .name = try self.recordFieldLabel(field.name),
                .ty = try self.lowerChecked(field.ty),
            };
        }
        std.mem.sort(Type.Field, fields, {}, typeFieldLessThan);

        return .{ .record = .{ .fields = fields } };
    }

    fn collectRecordFields(
        self: *Lowerer,
        fields: []const checked_artifact.CheckedRecordField,
        ext: CheckedTypeId,
        out: *std.ArrayList(checked_artifact.CheckedRecordField),
    ) Allocator.Error!void {
        try out.appendSlice(self.allocator, fields);

        var current = ext;
        while (true) {
            switch (self.payload(current)) {
                .alias => |alias| current = alias.backing,
                .empty_record => return,
                .record_unbound => |ext_fields| {
                    try out.appendSlice(self.allocator, ext_fields);
                    return;
                },
                .record => |ext_record| {
                    try out.appendSlice(self.allocator, ext_record.fields);
                    current = ext_record.ext;
                },
                .pending => invariantViolation("record row extension resolved to an unpublished checked type payload"),
                .flex => invariantViolation("record row extension stayed as flex after checking"),
                .rigid => invariantViolation("record row extension stayed as rigid after checking"),
                else => invariantViolation("record row extension resolved to a non-record type"),
            }
        }
    }

    fn lowerTagUnion(
        self: *Lowerer,
        tag_union: checked_artifact.CheckedTagUnionType,
    ) Allocator.Error!Type.Content {
        var source_tags = std.ArrayList(checked_artifact.CheckedTag).empty;
        defer source_tags.deinit(self.allocator);

        try self.collectTags(tag_union.tags, tag_union.ext, &source_tags);

        const tags = try self.allocator.alloc(Type.Tag, source_tags.items.len);
        @memset(tags, .{ .name = @enumFromInt(0), .args = &.{} });
        errdefer {
            for (tags[0..source_tags.items.len]) |tag| {
                if (tag.args.len > 0) self.allocator.free(tag.args);
            }
            self.allocator.free(tags);
        }

        for (source_tags.items, 0..) |tag, i| {
            tags[i] = .{
                .name = try self.tagLabel(tag.name),
                .args = try self.lowerTypeIds(tag.args),
            };
        }
        std.mem.sort(Type.Tag, tags, {}, typeTagLessThan);

        return .{ .tag_union = .{ .tags = tags } };
    }

    fn collectTags(
        self: *Lowerer,
        tags: []const checked_artifact.CheckedTag,
        ext: CheckedTypeId,
        out: *std.ArrayList(checked_artifact.CheckedTag),
    ) Allocator.Error!void {
        try out.appendSlice(self.allocator, tags);

        var current = ext;
        while (true) {
            switch (self.payload(current)) {
                .alias => |alias| current = alias.backing,
                .empty_tag_union => return,
                .tag_union => |ext_tags| {
                    try out.appendSlice(self.allocator, ext_tags.tags);
                    current = ext_tags.ext;
                },
                .pending => invariantViolation("tag-union extension resolved to an unpublished checked type payload"),
                .flex => invariantViolation("tag-union extension stayed as flex after checking"),
                .rigid => invariantViolation("tag-union extension stayed as rigid after checking"),
                else => invariantViolation("tag-union extension resolved to a non-tag-union type"),
            }
        }
    }

    fn lowerNominal(self: *Lowerer, nominal: checked_artifact.CheckedNominalType) Allocator.Error!Type.Content {
        if (nominal.builtin) |builtin_nominal| {
            switch (builtin_nominal) {
                .bool => return .{ .primitive = .bool },
                .str => return .{ .primitive = .str },
                .u8 => return .{ .primitive = .u8 },
                .i8 => return .{ .primitive = .i8 },
                .u16 => return .{ .primitive = .u16 },
                .i16 => return .{ .primitive = .i16 },
                .u32 => return .{ .primitive = .u32 },
                .i32 => return .{ .primitive = .i32 },
                .u64 => return .{ .primitive = .u64 },
                .i64 => return .{ .primitive = .i64 },
                .u128 => return .{ .primitive = .u128 },
                .i128 => return .{ .primitive = .i128 },
                .f32 => return .{ .primitive = .f32 },
                .f64 => return .{ .primitive = .f64 },
                .dec => return .{ .primitive = .dec },
                .list => {
                    if (nominal.args.len != 1) invariantViolation("List nominal type did not have exactly one argument");
                    return .{ .list = try self.lowerChecked(nominal.args[0]) };
                },
                .box => {
                    if (nominal.args.len != 1) invariantViolation("Box nominal type did not have exactly one argument");
                    return .{ .box = try self.lowerChecked(nominal.args[0]) };
                },
            }
        }

        return .{ .nominal = .{
            .nominal = .{
                .module_name = try self.moduleName(nominal.origin_module),
                .type_name = try self.typeName(nominal.name),
            },
            .is_opaque = nominal.is_opaque,
            .args = try self.lowerTypeIds(nominal.args),
            .backing = try self.lowerChecked(nominal.backing),
        } };
    }

    fn lowerTypeIds(self: *Lowerer, ids: []const CheckedTypeId) Allocator.Error![]const Type.TypeId {
        if (ids.len == 0) return &.{};
        const out = try self.allocator.alloc(Type.TypeId, ids.len);
        errdefer self.allocator.free(out);
        for (ids, 0..) |id, i| {
            out[i] = try self.lowerChecked(id);
        }
        return out;
    }

    fn moduleName(self: *Lowerer, id: canonical.ModuleNameId) Allocator.Error!canonical.ModuleNameId {
        const resolver = self.name_resolver orelse return id;
        const artifact = self.artifact orelse invariantViolation("mono type lowering had a name resolver without an artifact key");
        return try resolver.moduleName(artifact, id);
    }

    fn typeName(self: *Lowerer, id: canonical.TypeNameId) Allocator.Error!canonical.TypeNameId {
        const resolver = self.name_resolver orelse return id;
        const artifact = self.artifact orelse invariantViolation("mono type lowering had a name resolver without an artifact key");
        return try resolver.typeName(artifact, id);
    }

    fn recordFieldLabel(self: *Lowerer, id: canonical.RecordFieldLabelId) Allocator.Error!canonical.RecordFieldLabelId {
        const resolver = self.name_resolver orelse return id;
        const artifact = self.artifact orelse invariantViolation("mono type lowering had a name resolver without an artifact key");
        return try resolver.recordFieldLabel(artifact, id);
    }

    fn tagLabel(self: *Lowerer, id: canonical.TagLabelId) Allocator.Error!canonical.TagLabelId {
        const resolver = self.name_resolver orelse return id;
        const artifact = self.artifact orelse invariantViolation("mono type lowering had a name resolver without an artifact key");
        return try resolver.tagLabel(artifact, id);
    }
};

fn typeFieldLessThan(_: void, a: Type.Field, b: Type.Field) bool {
    return @intFromEnum(a.name) < @intFromEnum(b.name);
}

fn typeTagLessThan(_: void, a: Type.Tag, b: Type.Tag) bool {
    return @intFromEnum(a.name) < @intFromEnum(b.name);
}

fn invariantViolation(comptime message: []const u8) noreturn {
    if (builtin.mode == .Debug) {
        std.debug.panic(message, .{});
    }
    unreachable;
}

test "mono type lowerer declarations are referenced" {
    std.testing.refAllDecls(@This());
}
