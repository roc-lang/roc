//! Executable MIR value types.

const std = @import("std");
const check = @import("check");
const solved = @import("../lambda_solved/mod.zig");
const row = @import("../mono_row/mod.zig");

const canonical = check.CanonicalNames;
const repr = solved.Representation;

pub const TypeId = enum(u32) { _ };
pub const TypeIds = []const TypeId;
pub const Prim = solved.Type.Prim;

pub const CallableSetType = struct {
    key: repr.CanonicalCallableSetKey,
};

pub const ErasedFnType = struct {
    sig_key: repr.ErasedFnSigKey,
    capture_shape: repr.CaptureShapeKey,
};

pub const RecordFieldType = struct {
    field: row.RecordFieldId,
    ty: TypeId,
};

pub const RecordType = struct {
    shape: row.RecordShapeId,
    fields: []const RecordFieldType,
};

pub const TagPayloadType = struct {
    payload: row.TagPayloadId,
    ty: TypeId,
};

pub const TagType = struct {
    tag: row.TagId,
    payloads: []const TagPayloadType,
};

pub const TagUnionType = struct {
    shape: row.TagUnionShapeId,
    tags: []const TagType,
};

pub const Content = union(enum) {
    placeholder,
    link: TypeId,
    primitive: Prim,
    nominal: struct {
        nominal: canonical.NominalTypeKey,
        backing: TypeId,
    },
    list: TypeId,
    box: TypeId,
    tuple: []const TypeId,
    record: RecordType,
    tag_union: TagUnionType,
    callable_set: CallableSetType,
    erased_fn: ErasedFnType,
};

pub const Store = struct {
    allocator: std.mem.Allocator,
    types: std.ArrayList(Content),

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .types = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        for (self.types.items) |content| {
            self.freeOwnedContent(content);
        }
        self.types.deinit(self.allocator);
        self.* = Store.init(self.allocator);
    }

    fn freeOwnedContent(self: *Store, content: Content) void {
        switch (content) {
            .tuple => |items| {
                if (items.len > 0) self.allocator.free(items);
            },
            .record => |record| {
                if (record.fields.len > 0) self.allocator.free(record.fields);
            },
            .tag_union => |tag_union| {
                for (tag_union.tags) |tag| {
                    if (tag.payloads.len > 0) self.allocator.free(tag.payloads);
                }
                if (tag_union.tags.len > 0) self.allocator.free(tag_union.tags);
            },
            else => {},
        }
    }

    pub fn addType(self: *Store, content: Content) std.mem.Allocator.Error!TypeId {
        const idx: u32 = @intCast(self.types.items.len);
        try self.types.append(self.allocator, content);
        return @enumFromInt(idx);
    }

    pub fn getType(self: *const Store, id: TypeId) Content {
        var current = id;
        while (true) {
            switch (self.types.items[@intFromEnum(current)]) {
                .link => |next| current = next,
                else => |content| return content,
            }
        }
    }
};

test "executable type tests" {
    std.testing.refAllDecls(@This());
}
