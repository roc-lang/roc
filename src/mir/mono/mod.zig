//! Mono MIR.

const std = @import("std");

pub const Type = @import("type.zig");
pub const Ast = @import("ast.zig");
pub const LowerType = @import("lower_type.zig");
pub const Specialize = @import("specialize.zig");

test "mono tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(Type);
    std.testing.refAllDecls(Ast);
    std.testing.refAllDecls(LowerType);
    std.testing.refAllDecls(Specialize);
}
