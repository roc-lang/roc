//! Lambda Mono specialization queue.

const std = @import("std");
const Common = @import("../common.zig");
const Type = @import("type.zig");

/// Lambda Mono function specialization request.
pub const Spec = struct {
    symbol: Common.Symbol,
    solved_fn_ty: Type.TypeId,
    capture_shape: ?Type.TypeId,
};

/// Work queue for Lambda Mono specialization.
pub const Queue = struct {
    entries: std.ArrayList(Spec),

    pub fn init() Queue {
        return .{ .entries = .empty };
    }

    pub fn deinit(self: *Queue, allocator: std.mem.Allocator) void {
        self.entries.deinit(allocator);
    }

    /// Add a specialization request if the exact request is not already queued.
    pub fn enqueue(self: *Queue, allocator: std.mem.Allocator, spec: Spec) std.mem.Allocator.Error!bool {
        for (self.entries.items) |existing| {
            if (std.meta.eql(existing, spec)) return false;
        }

        try self.entries.append(allocator, spec);
        return true;
    }
};

test "lambda mono specialize declarations are referenced" {
    std.testing.refAllDecls(@This());
}

test "lambda mono specialize queue keeps exact requests once" {
    var queue = Queue.init();
    defer queue.deinit(std.testing.allocator);

    const spec = testSpec(0, 0, null);

    try std.testing.expect(try queue.enqueue(std.testing.allocator, spec));
    try std.testing.expect(!try queue.enqueue(std.testing.allocator, spec));
    try std.testing.expectEqual(@as(usize, 1), queue.entries.items.len);
}

test "lambda mono specialize queue keeps distinct capture shapes" {
    var queue = Queue.init();
    defer queue.deinit(std.testing.allocator);

    try std.testing.expect(try queue.enqueue(std.testing.allocator, testSpec(0, 0, null)));
    try std.testing.expect(try queue.enqueue(std.testing.allocator, testSpec(0, 0, @enumFromInt(1))));
    try std.testing.expect(try queue.enqueue(std.testing.allocator, testSpec(1, 0, null)));
    try std.testing.expectEqual(@as(usize, 3), queue.entries.items.len);
}

fn testSpec(comptime symbol_index: u32, comptime fn_ty_index: u32, capture_shape: ?Type.TypeId) Spec {
    return .{
        .symbol = @enumFromInt(symbol_index),
        .solved_fn_ty = @enumFromInt(fn_ty_index),
        .capture_shape = capture_shape,
    };
}
