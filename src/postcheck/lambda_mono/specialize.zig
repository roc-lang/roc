//! Lambda Mono specialization queue.

const std = @import("std");
const Common = @import("../common.zig");
const Type = @import("type.zig");

pub const Spec = struct {
    symbol: Common.Symbol,
    solved_fn_ty: Type.TypeId,
    capture_shape: ?Type.TypeId,
};

pub const Queue = struct {
    entries: std.ArrayList(Spec),

    pub fn init() Queue {
        return .{ .entries = .empty };
    }

    pub fn deinit(self: *Queue, allocator: std.mem.Allocator) void {
        self.entries.deinit(allocator);
    }
};

test "lambda mono specialize declarations are referenced" {
    std.testing.refAllDecls(@This());
}
