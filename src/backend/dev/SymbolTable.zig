//! Store-local identities for native code relocation targets.
//!
//! Producers intern linker declarations once and pass compact IDs to emitters.
//! Names are borrowed from the producer and live until its code is consumed.

const std = @import("std");

/// Index in one code generator's symbol-name column.
pub const Id = enum(u32) { _ };

/// Append-only symbol declarations, shared by code generation and object emission.
pub const Table = struct {
    names: std.ArrayList([]const u8) = .empty,
    required_definitions: std.ArrayList(Id) = .empty,
    indices: std.StringHashMapUnmanaged(Id) = .empty,

    pub fn deinit(self: *Table, allocator: std.mem.Allocator) void {
        self.names.deinit(allocator);
        self.required_definitions.deinit(allocator);
        self.indices.deinit(allocator);
    }

    pub fn intern(self: *Table, allocator: std.mem.Allocator, name: []const u8) std.mem.Allocator.Error!Id {
        const entry = try self.indices.getOrPut(allocator, name);
        if (!entry.found_existing) {
            errdefer _ = self.indices.remove(name);
            const id: Id = @enumFromInt(self.names.items.len);
            try self.names.append(allocator, name);
            entry.value_ptr.* = id;
        }
        return entry.value_ptr.*;
    }

    /// Declare an internal target once at its producer's identity-cache insertion.
    pub fn internInternal(self: *Table, allocator: std.mem.Allocator, name: []const u8) std.mem.Allocator.Error!Id {
        const id = try self.intern(allocator, name);
        try self.required_definitions.append(allocator, id);
        return id;
    }

    pub fn clearRetainingCapacity(self: *Table) void {
        self.names.clearRetainingCapacity();
        self.required_definitions.clearRetainingCapacity();
        self.indices.clearRetainingCapacity();
    }
};

test "symbol declarations retain IDs across growth and reset" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, exerciseTable, .{});
}

fn exerciseTable(allocator: std.mem.Allocator) std.mem.Allocator.Error!void {
    var table: Table = .{};
    defer table.deinit(allocator);
    const first = try table.internInternal(allocator, "first");
    const other = try table.intern(allocator, "other");
    var same_bytes = [_]u8{ 'f', 'i', 'r', 's', 't' };
    std.debug.assert(try table.intern(allocator, &same_bytes) == first);
    std.debug.assert(first != other and table.names.items.len == 2);
    var names: [128][16]u8 = undefined;
    for (&names, 0..) |*buffer, index| {
        const name = std.fmt.bufPrint(buffer, "symbol_{d}", .{index}) catch unreachable;
        _ = try table.intern(allocator, name);
    }
    std.debug.assert(try table.intern(allocator, "first") == first);
    table.clearRetainingCapacity();
    std.debug.assert(table.names.items.len == 0 and table.required_definitions.items.len == 0);
    std.debug.assert(@intFromEnum(try table.intern(allocator, "after_reset")) == 0);
}
