//! Row-finalized mono MIR shape store.
//!
//! This module interns record and tag-union row shapes immediately after mono
//! MIR lowering. Later stages should consume these ids instead of performing
//! field-name, tag-name, payload-position, or display-name lookup.

const std = @import("std");
const base = @import("base");
const Mono = @import("../mono/mod.zig");
const ids = @import("../ids.zig");
const verify = @import("../debug_verify.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const MonoType = Mono.Type;
const TypeId = MonoType.TypeId;

pub const RecordShapeId = ids.RecordShapeId;
pub const RecordFieldId = ids.RecordFieldId;
pub const TagUnionShapeId = ids.TagUnionShapeId;
pub const TagId = ids.TagId;
pub const TagPayloadId = ids.TagPayloadId;

pub const RecordField = struct {
    name: Ident.Idx,
    ty: TypeId,
    logical_index: u32,
};

pub const RecordShape = struct {
    fields: ids.Span(RecordFieldId),
};

pub const TagPayload = struct {
    ty: TypeId,
    logical_index: u32,
};

pub const Tag = struct {
    name: Ident.Idx,
    logical_index: u32,
    payloads: ids.Span(TagPayloadId),
};

pub const TagUnionShape = struct {
    tags: ids.Span(TagId),
};

pub const Store = struct {
    allocator: Allocator,
    record_shapes: std.ArrayList(RecordShape),
    record_fields: std.ArrayList(RecordField),
    record_shape_fields: std.ArrayList(RecordFieldId),
    tag_union_shapes: std.ArrayList(TagUnionShape),
    tags: std.ArrayList(Tag),
    tag_union_tags: std.ArrayList(TagId),
    tag_payloads: std.ArrayList(TagPayload),
    tag_payload_ids: std.ArrayList(TagPayloadId),
    record_shape_by_key: std.StringHashMap(RecordShapeId),
    tag_union_shape_by_key: std.StringHashMap(TagUnionShapeId),
    scratch_key: std.ArrayList(u8),

    pub fn init(allocator: Allocator) Store {
        return .{
            .allocator = allocator,
            .record_shapes = .empty,
            .record_fields = .empty,
            .record_shape_fields = .empty,
            .tag_union_shapes = .empty,
            .tags = .empty,
            .tag_union_tags = .empty,
            .tag_payloads = .empty,
            .tag_payload_ids = .empty,
            .record_shape_by_key = std.StringHashMap(RecordShapeId).init(allocator),
            .tag_union_shape_by_key = std.StringHashMap(TagUnionShapeId).init(allocator),
            .scratch_key = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        freeStringHashMapKeys(&self.record_shape_by_key, self.allocator);
        self.record_shape_by_key.deinit();
        freeStringHashMapKeysTag(&self.tag_union_shape_by_key, self.allocator);
        self.tag_union_shape_by_key.deinit();
        self.scratch_key.deinit(self.allocator);
        self.tag_payload_ids.deinit(self.allocator);
        self.tag_payloads.deinit(self.allocator);
        self.tag_union_tags.deinit(self.allocator);
        self.tags.deinit(self.allocator);
        self.tag_union_shapes.deinit(self.allocator);
        self.record_shape_fields.deinit(self.allocator);
        self.record_fields.deinit(self.allocator);
        self.record_shapes.deinit(self.allocator);
        self.* = Store.init(self.allocator);
    }

    pub fn internRecordShapeFromType(self: *Store, types: *const MonoType.Store, ty: TypeId) Allocator.Error!RecordShapeId {
        return switch (types.getType(ty)) {
            .record => |record| try self.internRecordShape(record.fields),
            else => std.debug.panic("mono_row expected record type for row finalization", .{}),
        };
    }

    pub fn internTagUnionShapeFromType(self: *Store, types: *const MonoType.Store, ty: TypeId) Allocator.Error!TagUnionShapeId {
        return switch (types.getType(ty)) {
            .tag_union => |tag_union| try self.internTagUnionShape(tag_union.tags),
            else => std.debug.panic("mono_row expected tag-union type for row finalization", .{}),
        };
    }

    pub fn internRecordShape(self: *Store, fields: MonoType.Fields) Allocator.Error!RecordShapeId {
        try self.buildRecordKey(fields);
        if (self.record_shape_by_key.get(self.scratch_key.items)) |shape_id| return shape_id;

        const shape_id: RecordShapeId = @enumFromInt(@as(u32, @intCast(self.record_shapes.items.len)));
        const key = try self.allocator.dupe(u8, self.scratch_key.items);
        errdefer self.allocator.free(key);

        const start: u32 = @intCast(self.record_shape_fields.items.len);
        for (fields, 0..) |field, i| {
            const field_id: RecordFieldId = @enumFromInt(@as(u32, @intCast(self.record_fields.items.len)));
            try self.record_fields.append(self.allocator, .{
                .name = field.name,
                .ty = field.ty,
                .logical_index = @intCast(i),
            });
            try self.record_shape_fields.append(self.allocator, field_id);
        }

        try self.record_shapes.append(self.allocator, .{
            .fields = .{ .start = start, .len = @intCast(fields.len) },
        });
        try self.record_shape_by_key.put(key, shape_id);
        return shape_id;
    }

    pub fn internTagUnionShape(self: *Store, source_tags: MonoType.Tags) Allocator.Error!TagUnionShapeId {
        try self.buildTagUnionKey(source_tags);
        if (self.tag_union_shape_by_key.get(self.scratch_key.items)) |shape_id| return shape_id;

        const shape_id: TagUnionShapeId = @enumFromInt(@as(u32, @intCast(self.tag_union_shapes.items.len)));
        const key = try self.allocator.dupe(u8, self.scratch_key.items);
        errdefer self.allocator.free(key);

        const tag_start: u32 = @intCast(self.tag_union_tags.items.len);
        for (source_tags, 0..) |source_tag, tag_i| {
            const payload_start: u32 = @intCast(self.tag_payload_ids.items.len);
            for (source_tag.args, 0..) |payload_ty, payload_i| {
                const payload_id: TagPayloadId = @enumFromInt(@as(u32, @intCast(self.tag_payloads.items.len)));
                try self.tag_payloads.append(self.allocator, .{
                    .ty = payload_ty,
                    .logical_index = @intCast(payload_i),
                });
                try self.tag_payload_ids.append(self.allocator, payload_id);
            }

            const tag_id: TagId = @enumFromInt(@as(u32, @intCast(self.tags.items.len)));
            try self.tags.append(self.allocator, .{
                .name = source_tag.name,
                .logical_index = @intCast(tag_i),
                .payloads = .{ .start = payload_start, .len = @intCast(source_tag.args.len) },
            });
            try self.tag_union_tags.append(self.allocator, tag_id);
        }

        try self.tag_union_shapes.append(self.allocator, .{
            .tags = .{ .start = tag_start, .len = @intCast(source_tags.len) },
        });
        try self.tag_union_shape_by_key.put(key, shape_id);
        return shape_id;
    }

    pub fn recordShape(self: *const Store, id: RecordShapeId) RecordShape {
        return self.record_shapes.items[@intFromEnum(id)];
    }

    pub fn recordShapeFields(self: *const Store, id: RecordShapeId) []const RecordFieldId {
        return self.recordShape(id).fields.get(self.record_shape_fields.items);
    }

    pub fn recordField(self: *const Store, id: RecordFieldId) RecordField {
        return self.record_fields.items[@intFromEnum(id)];
    }

    pub fn tagUnionShape(self: *const Store, id: TagUnionShapeId) TagUnionShape {
        return self.tag_union_shapes.items[@intFromEnum(id)];
    }

    pub fn tagUnionTags(self: *const Store, id: TagUnionShapeId) []const TagId {
        return self.tagUnionShape(id).tags.get(self.tag_union_tags.items);
    }

    pub fn tag(self: *const Store, id: TagId) Tag {
        return self.tags.items[@intFromEnum(id)];
    }

    pub fn tagPayloads(self: *const Store, id: TagId) []const TagPayloadId {
        return self.tag(id).payloads.get(self.tag_payload_ids.items);
    }

    pub fn tagPayload(self: *const Store, id: TagPayloadId) TagPayload {
        return self.tag_payloads.items[@intFromEnum(id)];
    }

    fn buildRecordKey(self: *Store, fields: MonoType.Fields) Allocator.Error!void {
        self.scratch_key.clearRetainingCapacity();
        try self.scratch_key.writer(self.allocator).print("record:{d}|", .{fields.len});
        for (fields) |field| {
            try self.scratch_key.writer(self.allocator).print("{d}:{d}|", .{
                @as(u32, @bitCast(field.name)),
                @intFromEnum(field.ty),
            });
        }
    }

    fn buildTagUnionKey(self: *Store, source_tags: MonoType.Tags) Allocator.Error!void {
        self.scratch_key.clearRetainingCapacity();
        try self.scratch_key.writer(self.allocator).print("tag_union:{d}|", .{source_tags.len});
        for (source_tags) |source_tag| {
            try self.scratch_key.writer(self.allocator).print("{d}:{d}|", .{
                @as(u32, @bitCast(source_tag.name)),
                source_tag.args.len,
            });
            for (source_tag.args) |payload_ty| {
                try self.scratch_key.writer(self.allocator).print("{d},", .{@intFromEnum(payload_ty)});
            }
            try self.scratch_key.append(self.allocator, '|');
        }
    }
};

pub const Result = struct {
    mono: Mono.Lower.Result,
    shapes: Store,

    pub fn deinit(self: *Result) void {
        self.mono.deinit();
        self.shapes.deinit();
    }

    pub fn takeMono(self: *Result, allocator: Allocator) Allocator.Error!Mono.Lower.Result {
        return try self.mono.take(allocator);
    }
};

pub fn run(allocator: Allocator, mono: *Mono.Lower.Result) Allocator.Error!Result {
    var owned_mono = try mono.take(allocator);
    errdefer owned_mono.deinit();

    var shapes = Store.init(allocator);
    errdefer shapes.deinit();

    for (owned_mono.types.types.items, 0..) |_, raw_i| {
        const ty: TypeId = @enumFromInt(@as(u32, @intCast(raw_i)));
        switch (owned_mono.types.getType(ty)) {
            .record => |record| _ = try shapes.internRecordShape(record.fields),
            .tag_union => |tag_union| _ = try shapes.internTagUnionShape(tag_union.tags),
            else => {},
        }
    }

    const result = Result{
        .mono = owned_mono,
        .shapes = shapes,
    };
    verifyResult(&result);
    return result;
}

pub fn verifyResult(result: *const Result) void {
    if (!verify.enabled()) return;

    for (result.shapes.record_shapes.items) |shape| {
        const fields = shape.fields.get(result.shapes.record_shape_fields.items);
        for (fields) |field_id| {
            verify.assertFmt(@intFromEnum(field_id) < result.shapes.record_fields.items.len, "invalid record field id {d}", .{@intFromEnum(field_id)});
        }
    }

    for (result.shapes.tag_union_shapes.items) |shape| {
        const tag_ids = shape.tags.get(result.shapes.tag_union_tags.items);
        for (tag_ids) |tag_id| {
            verify.assertFmt(@intFromEnum(tag_id) < result.shapes.tags.items.len, "invalid tag id {d}", .{@intFromEnum(tag_id)});
            const payload_ids = result.shapes.tag(tag_id).payloads.get(result.shapes.tag_payload_ids.items);
            for (payload_ids) |payload_id| {
                verify.assertFmt(@intFromEnum(payload_id) < result.shapes.tag_payloads.items.len, "invalid tag payload id {d}", .{@intFromEnum(payload_id)});
            }
        }
    }
}

fn freeStringHashMapKeys(map: *std.StringHashMap(RecordShapeId), allocator: Allocator) void {
    var keys = map.keyIterator();
    while (keys.next()) |key| allocator.free(key.*);
}

fn freeStringHashMapKeysTag(map: *std.StringHashMap(TagUnionShapeId), allocator: Allocator) void {
    var keys = map.keyIterator();
    while (keys.next()) |key| allocator.free(key.*);
}

test "mono_row store interns empty record shape once" {
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const first = try store.internRecordShape(&.{});
    const second = try store.internRecordShape(&.{});

    try std.testing.expectEqual(first, second);
    try std.testing.expectEqual(@as(usize, 1), store.record_shapes.items.len);
}
