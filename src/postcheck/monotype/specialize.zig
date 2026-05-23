//! Monotype specialization worklist.

const std = @import("std");
const Ast = @import("ast.zig");
const Type = @import("type.zig");

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

    /// Add a specialization request if the exact request is not already queued.
    pub fn enqueue(self: *Queue, allocator: std.mem.Allocator, spec: Spec) std.mem.Allocator.Error!bool {
        for (self.entries.items) |existing| {
            if (specEql(existing, spec)) return false;
        }

        try self.entries.append(allocator, spec);
        return true;
    }
};

fn specEql(left: Spec, right: Spec) bool {
    return Ast.fnTemplateIdentityEql(left.fn_def, right.fn_def) and left.ty == right.ty;
}

test "monotype specialize declarations are referenced" {
    std.testing.refAllDecls(@This());
}

test "monotype specialize queue keeps exact requests once" {
    var queue = Queue.init();
    defer queue.deinit(std.testing.allocator);

    const spec = testSpec(0, 0, 0);

    try std.testing.expect(try queue.enqueue(std.testing.allocator, spec));
    try std.testing.expect(!try queue.enqueue(std.testing.allocator, spec));
    try std.testing.expectEqual(@as(usize, 1), queue.entries.items.len);
}

test "monotype specialize queue keeps distinct requests" {
    var queue = Queue.init();
    defer queue.deinit(std.testing.allocator);

    try std.testing.expect(try queue.enqueue(std.testing.allocator, testSpec(0, 0, 0)));
    try std.testing.expect(try queue.enqueue(std.testing.allocator, testSpec(0, 0, 1)));
    try std.testing.expect(try queue.enqueue(std.testing.allocator, testSpec(0, 1, 0)));
    try std.testing.expectEqual(@as(usize, 3), queue.entries.items.len);
}

fn testSpec(comptime proc_index: u32, comptime source_digest_byte: u8, comptime ty_index: u32) Spec {
    return .{
        .fn_def = .{
            .fn_def = .{ .local_template = .{
                .proc_base = @enumFromInt(proc_index),
                .template = @enumFromInt(proc_index + 1),
            } },
            .source_fn_ty = @enumFromInt(ty_index + 1),
            .source_fn_key = digestWithFirstByte(source_digest_byte),
            .mono_fn_ty = @enumFromInt(ty_index),
        },
        .ty = @enumFromInt(ty_index),
    };
}

fn digestWithFirstByte(comptime byte: u8) @import("check").CheckedNames.TypeDigest {
    var digest: @import("check").CheckedNames.TypeDigest = .{};
    digest.bytes[0] = byte;
    return digest;
}
