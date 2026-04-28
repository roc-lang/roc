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
    capture_shape: repr.CaptureShapeKey,
};

pub const ErasedFnType = struct {
    sig_key: repr.ErasedFnSigKey,
    capture_shape: repr.CaptureShapeKey,
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
    record: row.RecordShapeId,
    tag_union: row.TagUnionShapeId,
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
        self.types.deinit(self.allocator);
        self.* = Store.init(self.allocator);
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
