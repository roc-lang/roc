//! Top-level bindings for compile-time evaluation.
//!
//! Maps pattern indices directly to memory addresses where evaluated
//! constants are stored. Used during compilation to track top-level
//! constant values that have already been evaluated.

const std = @import("std");

/// Environment for top-level compile-time evaluation.
/// Maps pattern indices directly to memory addresses.
pub const TopLevelBindings = struct {
    /// pattern_idx -> address of evaluated value
    addresses: std.AutoHashMap(u32, [*]u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) TopLevelBindings {
        return .{
            .addresses = std.AutoHashMap(u32, [*]u8).init(allocator),
            .allocator = allocator,
        };
    }

    /// Bind a pattern index to its evaluated value's address.
    pub fn bind(self: *TopLevelBindings, pattern_idx: u32, address: [*]u8) !void {
        try self.addresses.put(pattern_idx, address);
    }

    /// Look up the address of an evaluated value by pattern index.
    pub fn lookup(self: *const TopLevelBindings, pattern_idx: u32) ?[*]u8 {
        return self.addresses.get(pattern_idx);
    }

    pub fn deinit(self: *TopLevelBindings) void {
        self.addresses.deinit();
    }
};

test "TopLevelBindings bind and lookup" {
    var bindings = TopLevelBindings.init(std.testing.allocator);
    defer bindings.deinit();

    var buffer: [8]u8 = undefined;
    const ptr: [*]u8 = &buffer;

    try bindings.bind(42, ptr);

    const looked_up = bindings.lookup(42);
    try std.testing.expect(looked_up != null);
    try std.testing.expectEqual(ptr, looked_up.?);
}

test "TopLevelBindings lookup missing returns null" {
    var bindings = TopLevelBindings.init(std.testing.allocator);
    defer bindings.deinit();

    try std.testing.expectEqual(@as(?[*]u8, null), bindings.lookup(999));
}
