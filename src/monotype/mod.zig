//! Cor-style monotype lowering from solved checker types.

const std = @import("std");

pub const Type = @import("type.zig");
pub const Ast = @import("ast.zig");
pub const Lower = @import("lower.zig");

test "monotype tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(Type);
    std.testing.refAllDecls(Ast);
    std.testing.refAllDecls(Lower);
}
