//! Monotype specialization worklist.

const std = @import("std");
const Ast = @import("ast.zig");
const Type = @import("type.zig");
const Common = @import("../common.zig");

/// Monotype function template paired with its requested function type.
pub const Spec = struct {
    fn_def: Ast.FnTemplate,
    ty: Type.TypeId,
};

/// Work queue for Monotype specialization.
pub const Queue = struct {
    entries: std.ArrayList(Spec),

    pub fn init() Queue {
        return .{ .entries = .empty };
    }

    pub fn deinit(self: *Queue, allocator: std.mem.Allocator) void {
        self.entries.deinit(allocator);
    }
};

test "monotype specialize declarations are referenced" {
    std.testing.refAllDecls(@This());
    _ = Common.Symbol;
}
