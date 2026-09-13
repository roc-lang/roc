//! Persistent field bindings for records under construction. Leaves name
//! evaluated LIR values; changing one field copies only its tree path.

const std = @import("std");
const LIR = @import("lir_core").LIR;

/// Store-local identity of one immutable field tree.
pub const Id = enum(u32) { _ };
const Node = union(enum) {
    leaf: LIR.LocalId,
    branch: struct { left: Id, right: Id },
};

/// Procedure-local storage shared by immutable record versions.
pub const Store = struct {
    allocator: std.mem.Allocator,
    nodes: std.ArrayList(Node) = .empty,

    pub fn deinit(self: *Store) void {
        self.nodes.deinit(self.allocator);
    }

    fn add(self: *Store, node: Node) std.mem.Allocator.Error!Id {
        const id: Id = @enumFromInt(@as(u32, @intCast(self.nodes.items.len)));
        try self.nodes.append(self.allocator, node);
        return id;
    }

    pub fn build(self: *Store, fields: []const LIR.LocalId) std.mem.Allocator.Error!Id {
        std.debug.assert(fields.len != 0);
        if (fields.len == 1) return self.add(.{ .leaf = fields[0] });
        const mid = fields.len / 2;
        const left = try self.build(fields[0..mid]);
        const right = try self.build(fields[mid..]);
        return self.add(.{ .branch = .{ .left = left, .right = right } });
    }

    pub fn update(self: *Store, root: Id, len: usize, index: usize, value: LIR.LocalId) std.mem.Allocator.Error!Id {
        std.debug.assert(index < len);
        if (len == 1) return self.add(.{ .leaf = value });
        var branch = self.nodes.items[@intFromEnum(root)].branch;
        const mid = len / 2;
        if (index < mid) {
            branch.left = try self.update(branch.left, mid, index, value);
        } else {
            branch.right = try self.update(branch.right, len - mid, index - mid, value);
        }
        return self.add(.{ .branch = branch });
    }

    pub fn get(self: *const Store, root: Id, len: usize, index: usize) LIR.LocalId {
        std.debug.assert(index < len);
        if (len == 1) return self.nodes.items[@intFromEnum(root)].leaf;
        const branch = self.nodes.items[@intFromEnum(root)].branch;
        const mid = len / 2;
        return if (index < mid)
            self.get(branch.left, mid, index)
        else
            self.get(branch.right, len - mid, index - mid);
    }

    pub fn write(self: *const Store, root: Id, out: []LIR.LocalId) void {
        std.debug.assert(out.len != 0);
        if (out.len == 1) {
            out[0] = self.nodes.items[@intFromEnum(root)].leaf;
            return;
        }
        const branch = self.nodes.items[@intFromEnum(root)].branch;
        const mid = out.len / 2;
        self.write(branch.left, out[0..mid]);
        self.write(branch.right, out[mid..]);
    }
};

test "record field versions share unchanged bindings with logarithmic update work" {
    var store = Store{ .allocator = std.testing.allocator };
    defer store.deinit();
    var fields: [50]LIR.LocalId = undefined;
    for (&fields, 0..) |*field, i| field.* = @enumFromInt(@as(u32, @intCast(i)));
    const original = try store.build(&fields);
    const initial_count = store.nodes.items.len;
    var current = original;
    for (0..500) |i| {
        current = try store.update(current, fields.len, i % fields.len, @enumFromInt(@as(u32, @intCast(i + 50))));
    }
    try std.testing.expect(store.nodes.items.len - initial_count <= 500 * 7);
    var actual: [50]LIR.LocalId = undefined;
    store.write(original, &actual);
    try std.testing.expectEqualSlices(LIR.LocalId, &fields, &actual);
    store.write(current, &actual);
    for (actual, 0..) |value, i| {
        try std.testing.expectEqual(@as(u32, @intCast(500 + i)), @intFromEnum(value));
        try std.testing.expectEqual(value, store.get(current, fields.len, i));
    }
}
